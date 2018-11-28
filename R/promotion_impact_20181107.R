#### promotionImpact ####
## 함수 리스트 ##
# promotionImpact : 메인 함수
# promotion.model : 모델링 함수
# create.dummy.vars : dummy 변수 생성 함수
# create.smooth.vars : smoothing 변수 생성 함수
# value_fit_plot : plotting용 함수
# format_time : 시간값 변환 함수


#' promotionImpact
#' 
#' promotionImpact is for analysis & measurement of the effectiveness of promotions, controlling for some prespeficied or estimated control variables.
#' 
#' @param data Dataframe containing date, target variable, and some additional time dummies that the researcher wants to account for.
#' @param promotion Dataframe containing promotion ID, start date, end date, promotion tag(type). Might include daily payments associated with the promotion.
#' @param time.field Specify the date field of 'data'.
#' @param target.field Specify the target field of 'data'.
#' @param dummy.field Specify the additional time dummies of 'data'.
#' @param trend TRUE to incorporate trend component, FALSE to exclude the trend component.
#' @param period NULL to exclude any periodicity from the model, 'auto' to automatically determine the period, certain numeric value to manually specify the period
#' @param trend.param Flexibility of trend component. Default is 0.05, and as this value becomes larger, the trend component will be more flexible.
#' @param period.param Flexibility of period component. Default is 3, and as this value becomes larger, the period component will be more flexible.
#' @param var.type 'smooth' to use smoothed promotion variables, 'dummy' to use dummy promotion variables
#' @param smooth.except.date Date value that will be excluded from the smoothing process. eg) '01' to exclude every start day of a month
#' @param smooth.bandwidth Bandwidth of local polynomial regression used in the smoothing process. Default value is 2.
#' @param smooth.origin 'all' to estimate a global smoothing function for all promotions. 'tag' to estimate different smoothing functions for different promotion types(tags).
#' @param smooth.var.sum If TRUE, the smoothing values for times when multiple promotions in a single tag overlap will be the values from the latest promotion. Otherwise, the values will be added(default).
#' @param logged TRUE to take logs to the target variable and the trend/period component
#' @param differencing TRUE to first difference the target variable, smoothed regressors, and the trend/period component values
#' 
#' @import dplyr
#' @import Rcpp
#' @import prophet
#' @import ggpubr
#' @import reshape2
#' @import ggplot2
#' @import scales
#' @import strucchange
#' @import KernSmooth
#' @import data.table
#' @import stringr

#' @export


#### promotionImpact 메인 함수 ####

promotionImpact <- function(data, promotion
                            ,time.field = 'date', target.field = 'value', dummy.field = NULL
                            ,trend = FALSE, period = NULL, structural.change = FALSE
                            ,trend.param = 0.05, period.param = 3
                            ,var.type = 'smooth', smooth.except.date = NULL
                            ,smooth.bandwidth = 2, smooth.origin = 'all', smooth.var.sum = TRUE
                            ,logged = FALSE, differencing = FALSE) {
  
  
  suppressMessages(require(dplyr))
  data <- as.data.frame(data)
  
  ## 입력한 필드가 데이터에 포함되었는지 체크 
  check.fields <- function(data, field) {
    if (!all(field %in% names(data))) {
      stop(sprintf("'%s' contains a field which is in the input data - (Fields of data: %s)", 
                   paste(field, collapse=","), paste(names(data), collapse=", ")))
    }
  }
  
  check.fields(data, time.field)
  check.fields(data, target.field)
  check.fields(data, dummy.field)
  
  ## 일자 및 타겟 지표 컬럼명 지정
  names(data)[1:2] <- c('date','value')
  
  ## 시간변수 변환 및 정렬
  data[,'date'] <- format_time(data[,'date'])
  data <- data[order(data[,'date']), ]
  
  if (var.type == 'smooth') {
    
    if (ncol(promotion) == 4) {  
      
      ## 프로모션 일정 데이터의 컬럼명 지정 
      names(promotion) <- c('pro_id','start_date','end_date','pro_tag')
      
      ## 프로모션 일정 데이터의 시간 변수 변환
      promotion['start_date'] <- format_time(unclass(promotion['start_date'])[[1]])
      promotion['end_date'] <- format_time(unclass(promotion['end_date'])[[1]])
      
      ## 프로모션별 일별 한 row씩 나오도록 변환
      promotion$duration <- promotion$end_date - promotion$start_date + 1
      promotion <- promotion[rep(seq(nrow(promotion)), promotion$duration),]
      promotion$date <- promotion$start_date
      
      ## dayPassed : 프로모션 기간 중 며칠째인지 나타냄
      for (i in seq(1:nrow(promotion))) {
        if (i == 1) {
          promotion[i,'dayPassed'] <- 1
        } else if (promotion[i,'pro_id'] != promotion[i-1,'pro_id']) {
          promotion[i,'dayPassed'] <- 1
        } else {
          promotion[i,'dayPassed'] <- promotion[i-1,'dayPassed'] + 1
        }
      }
      
      ## 프로모션별 일별 데이터로 구성
      promotion$date <- promotion$start_date + ((promotion$dayPassed - 1) * 86400)
      promotion <- promotion %>% dplyr::select(-duration, -dayPassed)
      
      
      # 통제변수만 넣고 모형 적합
      invisible(
        capture.output(
          suppressMessages(
          control <- promotion.model(data, time.field = 'date', target.field = 'value', dummy.field = dummy.field,
                                     trend = trend, period = period, structural.change = structural.change, 
                                     trend.param = trend.param, period.param = period.param, logged=F, differencing=F)
          )
        )
      )

      # residual : 일별 잔차 데이터. 여기서의 잔차는 control$model의 잔차
      residual <- data.frame(date = data[,'date'])
      residual[,'residual'] <- control$model$residuals
      
      # residual 값을 기존 promotion에 join
      promotion <- promotion %>% dplyr::left_join(residual, by = c('date'='date'))
      
      promotion <- promotion %>% dplyr::filter(start_date >= min(data$date) & end_date <= max(data$date))
      
      # promotion의 잔차를 min-max scale
      promotion$residual <- (promotion$residual - min(promotion$residual)) / (max(promotion$residual) - min(promotion$residual))
      
      # 겹치는 날짜의 잔차를 균등 배분
      promotion <- promotion %>% dplyr::add_count(date)
      promotion <- promotion %>% dplyr::mutate(residual = residual/n)
      promotion <- promotion %>% dplyr::select(-n)
      
    } else if (ncol(promotion) == 6) {
      
      names(promotion) <- c('pro_id','start_date','end_date','pro_tag','date','value')
      
    }
    
    ## 여기까지 하면, promotion은 프로모션별 일별 '매출(결제금액)' 또는 프로모션별 일별 '통제변수 모형의 잔차(효과)' 데이터로 완성
    
    # smoothed variables 생성
    smoothvar <- create.smooth.vars(target.data = data[ ,c('date', 'value')], promotion.data = promotion,
                                    smooth.except.date = smooth.except.date, smooth.bandwidth = smooth.bandwidth,
                                    smooth.origin = smooth.origin, smooth.var.sum = smooth.var.sum)
    
    # 최종 입력 데이터
    input.data <- data %>% dplyr::left_join(smoothvar$data, by = c('date'='date', 'value'='value'))
    
    
    # 모델링
    model <- promotion.model(input.data, time.field = 'date', target.field = 'value', dummy.field = dummy.field,
                             trend = trend, period = period, structural.change = structural.change,
                             trend.param = trend.param, period.param = period.param, logged=logged, differencing=differencing)
    
    
    # 회귀계수로부터 효과 환산
    
    effects <- data.frame(matrix(nrow = 0, ncol = length(unique(promotion$pro_tag))))
    names(effects) <- unique(promotion$pro_tag)
    
    if (logged == F) {
      
      for (i in unique(promotion$pro_tag)) {
        effects[1, i] <- model$model$coefficients[i][[1]] * smoothvar$smooth_value_mean[[i]]  # 로그변환 없을 시 절대 효과 출력
      }
      
    } else if (logged == T) {
      
      for (i in unique(promotion$pro_tag)) {
        effects[1, i] <- ( exp(model$model$coefficients[i][[1]] * smoothvar$smooth_value_mean[[i]]) - 1 ) * 100  # 로그변환 했을 시 상대 효과(증가율) 출력
      }
      
    }
    
  } else if (var.type == 'dummy') {
    
    ## 프로모션 일정 데이터의 컬럼명 지정 
    names(promotion) <- c('pro_id','start_date','end_date','pro_tag')
    
    # 더미변수 생성
    dummyvar <- create.dummy.vars(data[ c('date','value')], promotion, tovar.col = 'pro_tag')
    
    # 최종 입력 데이터
    input.data <- dummyvar %>% dplyr::left_join(data, by = c('date'='date', 'value'='value'))
    
    # 모델링
    model <- promotion.model(input.data, time.field = 'date', target.field = 'value', dummy.field = dummy.field,
                             trend = trend, period = period, structural.change = structural.change,
                             trend.param = trend.param, period.param = period.param, logged=logged, differencing=differencing)
    
    if (differencing == T) {
      warning("Differencing with dummy promotion variables is not recommended. The interpretation of the effects will be significantly different.")
    }
    
    # 회귀계수로부터 효과 환산
    effects <- data.frame(matrix(nrow = 0, ncol = length(unique(promotion$pro_tag))))
    names(effects) <- unique(promotion$pro_tag)
    
    if (logged == F) {
      
      for (i in unique(promotion$pro_tag)) {
        effects[1, i] <- model$model$coefficients[i][[1]]  # 절대 효과
      }
      
    } else if (logged == T) {
      
      for (i in unique(promotion$pro_tag)) {
        effects[1, i] <- ( exp(model$model$coefficients[i][[1]]) - 1 ) * 100  # 상대 효과(증가율)
      }
    }
    
  }
  
  result <- list(
    model = model,
    effects = effects
  )
  
  if (var.type == 'smooth') {
    result$smoothvar <- smoothvar
  }
  
  return(result)
  
}


#### promotion.model : 모델링 함수 ####
# data : 일자(time.field), 타겟 지표(target.field), 기타 사용자 입력 dummy(dummy.field)로 구성된 데이터
# logged : 타겟지표 및 연속형 독립변수에 대한 로그 변환 여부
# differencing : 타겟지표 및 연속형 독립변수에 대한 차분 변환 여부
# trend : 트렌드 포함 여부(TRUE이면 트렌드 있음, FALSE이면 트렌드 없음)
# period : 주기성(NULL이면 주기성 없음, 'auto'이면 주기성 자동 추정, 기타 숫자값을 입력하면 해당 주기로 모델링)
# structural.change : 구조 변화 포함 여부(TRUE이면 구조변화 포함, FALSE이면 구조변화 포함하지 않음)
# trend.param : 트렌드 컴포넌트의 유연성을 조정하는 파라미터. 이 값이 클수록 동적으로 변하는 트렌드 적합.
# period.param : 주기성 컴포넌트의 유연성을 조정하는 파라미터. 아 값이 클수록 동적으로 변하는 주기성 적합.

promotion.model <- function(data, time.field = 'date', target.field = 'value', dummy.field = NULL
                            ,logged = FALSE, differencing = FALSE
                            ,trend = FALSE, period = NULL, structural.change = FALSE
                            ,trend.param = 0.05, period.param = 3) {
  
  suppressMessages(require(dplyr))
  data.fields <- names(data)
  data <- as.data.frame(data)
  
  ## 독립변수 군 생성
  # 입력한 독립변수 중 dummy.field에 포함되지 않는 것들은 conti.field로
  promotion.field <- names(data)[-c(grep(time.field, names(data)), grep(target.field, names(data)))]
  promotion.field <- promotion.field[!promotion.field %in% dummy.field]
  
  ## 시간변수 변환 및 정렬
  data[,time.field] <- format_time(data[,time.field])
  data <- data[order(data[time.field]),]
  
  ## 트렌드/주기성 컴포넌트 계산
  if (trend == TRUE | !is.null(period) == TRUE) {
    message('Estimating trend/periodicity components..')
    suppressMessages(require(Rcpp))
    prophet.data <- data.frame(ds = data[,time.field], y = data[,target.field])  # prophet에서의 트렌드, 주기성 추정을 위한 데이터
    
    if(!is.null(period) == TRUE){
      if (period == 'auto') {
        prophet.model <- suppressMessages(prophet::prophet(prophet.data, changepoint.prior.scale = trend.param))
      } else if (class(period) == 'numeric' & length(period) == 1){
        prophet.model <- prophet::prophet(weekly.seasonality = F, yearly.seasonality = F, changepoint.prior.scale = trend.param)
        prophet.model <- prophet::add_seasonality(prophet.model, name = 'custom_period', period, fourier.order = period.param)
        prophet.model <- suppressMessages(prophet::fit.prophet(prophet.model, prophet.data))
      } else {
        stop("Invalid period value. Possible period values might be NULL, 'auto', or some numeric value.")
      }
    } else {
      prophet.model <- suppressMessages(prophet::prophet(prophet.data, changepoint.prior.scale = trend.param))
    }
    
    prophet.fitted <- predict(prophet.model, prophet.data)
    
    period.cnt <- length(prophet.model$seasonalities)  # 주기성의 갯수
    
    trend.component <- prophet.fitted[,'trend']             # 트렌드 컴포넌트
    period.component <- prophet.fitted[,'additive_terms']   # 주기성 컴포넌트
    
    # 트렌드+주기성 값을 데이터에 포함하고 각 컴포넌트 별 그래프 생성
    trend.period.graph <- list()
    
    if (!is.null(period) == FALSE) {
      data[,'trend_value'] <- trend.component
      trend.period.field <- 'trend_value'
      trend.period.graph <- prophet::prophet_plot_components(prophet.model, prophet.fitted, render_plot = F)[[1]]
      
    } else if (trend == FALSE) {
      data[,'period_value'] <- period.component + mean(data[,target.field])
      trend.period.field <- 'period_value'
      for(i in seq(2,period.cnt+1)){
        trend.period.graph[[i-1]] <- prophet::prophet_plot_components(prophet.model, prophet.fitted, render_plot = F)[[i]]
      }
      trend.period.graph <- ggpubr::ggarrange(plotlist = trend.period.graph, nrow = period.cnt, ncol = 1)
      
    } else {
      data[,'trend_period_value'] <- trend.component + period.component
      trend.period.field <- 'trend_period_value'
      trend.period.graph <- ggpubr::ggarrange(plotlist = prophet::prophet_plot_components(prophet.model, prophet.fitted, render_plot = F), nrow = period.cnt+1, ncol = 1)
    }
    
    
    ## 일별 타겟변수 시계열과 트렌드+주기성 컴포넌트를 포함하는 plot 생성 (trend.period.graph.with.target)
    trend.period.df <- prophet.data
    trend.period.df[,'trend_period'] <- data[,trend.period.field]
    
    trend.period.df <- reshape2::melt(trend.period.df[c('ds','y','trend_period')], id.vars = 'ds')
    trend.period.df$variable <- as.character(trend.period.df$variable)
    trend.period.df$variable[trend.period.df$variable == 'y'] <- 'target value'
    trend.period.df$variable[trend.period.df$variable == 'trend_period'] <- 'trend+period value'
    trend.period.df$variable <- factor(trend.period.df$variable, levels = c('target value', 'trend+period value'))
    
    trend.period.graph.with.target <- ggplot2::ggplot(trend.period.df, ggplot2::aes(ds, value, col=variable, linetype=variable, size=variable))+
      ggplot2::geom_line()+ggplot2::theme_bw()+ggplot2::scale_color_manual(values = c('black','#356dc6'))+
      ggplot2::scale_linetype_manual(values = c('solid','solid'))+
      ggplot2::scale_size_manual(values = c(0.7,0.7))+
      ggplot2::scale_x_datetime(labels = scales::date_format("%y.%m.%d"), breaks = scales::date_breaks('weeks'))+
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n=10), labels = scales::comma)+
      ggplot2::ggtitle('Trend/Periodicity Component with Target Values')+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     axis.text.x = ggplot2::element_text(vjust=0.25, angle=90))
  }
  
  
  ## 구조변화점 탐지
  if (structural.change == TRUE) {
    
    target.ts <- as.ts(data[target.field])
    
    breakpoints <- strucchange::breakpoints(target.ts ~ 1)  # 타겟변수 시계열에 대한 절편(베이스라인) 변화 탐지 (breakpoints)
    
    # 구조변화점이 탐지되는 경우 구조변수 생성
    
    if(is.na(breakpoints$breakpoints)[1]){
      message("No apparent structural breakpoints detected. Model was fitted without structural breakpoints.")
    }else{
      
      data$structure <- length(breakpoints$breakpoints) + 1
      
      for(i in seq(1:length(breakpoints$breakpoints))){
        breakpoint <- breakpoints$breakpoints[i]
        for(j in seq(1:nrow(data))){
          if(data[j,time.field] <= data[breakpoint,time.field]){
            data[j,'structure'] <- data[j,'structure'] - 1
          }
        }
      }
      
      data$structure <- as.factor(data$structure)
      structure.field <- 'structure'
      structural.breakpoint <- data[breakpoints$breakpoints, time.field]
    }
  }
  
  
  ## 로그 변환
  if (logged == TRUE) {
    data[target.field] <- log(data[target.field])
    if(trend == TRUE | !is.null(period) == TRUE){
      data[trend.period.field] <- log(data[trend.period.field])
    }
  }
  
  ## 차분 변환
  if (differencing == TRUE) {
    data[2:nrow(data), target.field] <- diff(as.matrix(data[target.field]))
    data[2:nrow(data), promotion.field] <- diff(as.matrix(data[promotion.field]))
    if(trend == TRUE | !is.null(period) == TRUE){
      data[2:nrow(data), trend.period.field] <- diff(as.matrix(data[trend.period.field]))
    }
    data <- data %>% dplyr::slice(-1)
  }
  
  
  ## 최종적으로 formula에 들어갈 변수들 지정(regressor.field)
  
  regressor.field <- c()
  
  if (all(is.null(promotion.field) == FALSE)) {
    regressor.field <- append(regressor.field, promotion.field)
  }
  
  if (all(is.null(dummy.field) == FALSE)) {
    regressor.field <- append(regressor.field, dummy.field)
  }
  
  if (structural.change == TRUE) {
    if (is.na(breakpoints$breakpoints)[1] != TRUE) {
      regressor.field <- append(regressor.field, structure.field)
    }
  }
  
  if (trend == TRUE | !is.null(period) == TRUE) {
    regressor.field <- append(regressor.field, trend.period.field)
  }
  
  ## lm 모델링
  
  result.formula <- paste(target.field, paste(regressor.field, collapse ='+'), sep="~")
  model <- lm(data, formula = as.formula(result.formula))
  
  model$call <- sprintf('lm(data, formula = %s)',result.formula)
  
  ## target vs fitted plot
  inspection <- data.frame(date = data[, time.field], value = data[, target.field])
  inspection$fit <- model$fitted.values
  
  fit.plot <- value_fit_plot(inspection)
  
  ## 결과 반환 (structural.change나 trend/period 유무에 따라 structural.breakpoint와 trend.period.graph 반환)
  result <- list(
    model = model,
    final_input_data = data,
    fit_plot = fit.plot
  )
  if (structural.change == TRUE) {
    if (is.na(breakpoints$breakpoints)[1] != TRUE) {
      result$structural_breakpoint <- structural.breakpoint
    }
  }
  if (trend == TRUE | !is.null(period) == TRUE) {
    result$trend_period_graph <- trend.period.graph
    result$trend_period_graph_with_target <- trend.period.graph.with.target
  }
  
  return(result)
  
}

## value_fit_plot : 종속변수 대비 통제변수모형의 fit을 보기 위한 plotting 함수 ##
value_fit_plot <- function(data){
  names(data) <- c('dt','value','fit')
  m.data <- reshape2::melt(data, id.vars = 'dt')
  ggplot2::ggplot(m.data, ggplot2::aes(dt, value, col = variable, linetype = variable, size = variable))+ggplot2::geom_line()+ggplot2::theme_bw()+
    ggplot2::scale_color_manual(values = c('black','blue'))+
    ggplot2::scale_linetype_manual(values = c('solid', 'solid'))+
    ggplot2::scale_size_manual(values = c(0.7, 0.7))+
    ggplot2::scale_x_datetime(labels = scales::date_format("%y-%m-%d", tz = 'Asia/Seoul'), breaks = scales::date_breaks('weeks'))+
    ggplot2::scale_y_continuous(labels = ggplot2::waiver())+
    ggplot2::ggtitle("Fitted Values Against Target Values")+
    ggplot2::xlab('date')+ggplot2::ylab('value')+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = ggplot2::element_text(vjust=0.25, angle=90))
}



#### create.dummy.vars : 프로모션 기간별로 1, 0 더미 변수 생성하는 함수 ####
## 전체 y값으로 쓸 기간별 종속변수 데이터(=target.data)와 프로모션 일정(=promotion.data) 데이터를 입력받음

# target.data : 일자와 타겟 변수로 이루어진 데이터
# promotion.data : 프로모션 일정 데이터
# tovar.col : promotion.data에서, 프로모션 더미변수로 만들 컬럼명


create.dummy.vars <- function(target.data, promotion.data, tovar.col = 'pro_id') {
  
  ## 더미변수로 변환할 컬럼명과 데이터 필드명이 일치하는지 확인
  if (!tovar.col %in% names(promotion.data)) {
    stop(sprintf('%s is not a promotion field of promotion.data - (promotion.data fields: %s)', tovar.col, paste(names(promotion.data), collapse=", ")))
  }
  
  names(target.data) <- c('date','value')
  
  tovar.col <- grep(tovar.col, colnames(promotion.data))
  
  names(promotion.data) <- c('pro_id','start_date','end_date','pro_tag')
  
  
  ## 프로모션 시작-종료일에 NA가 있는지 확인 (NA가 있는 경우, 시간 포맷 변환 및 더미변수 생성 시, 에러 발생)
  if (sum(is.na(promotion.data[,'start_date'])) != 0) {
    stop(print('promotion start date included NA values. please fill date values for all rows of the start date field'))
  }
  if (sum(is.na(promotion.data[,'end_date'])) != 0) {
    stop(print('promotion end date included NA values. please fill date values for all rows of the end date field'))
  }
  
  ## 시간값 변환
  target.data['date'] <- format_time(unclass(target.data['date'])[[1]])
  target.data <- target.data[order(target.data$date),]
  promotion.data['start_date'] <- format_time(unclass(promotion.data['start_date'])[[1]])
  promotion.data['end_date'] <- format_time(unclass(promotion.data['end_date'])[[1]])
  
  ## 이벤트 더미 변수 생성
  event.dummy <- data.frame(date=unique(target.data[,'date']))
  event.dummy[,levels(as.factor(unclass(unique(promotion.data[tovar.col]))[[1]]))] <- 0 # 초기값은 모두 0
  
  
  for (i in seq(1:nrow(promotion.data))) {
    start <- promotion.data[i, 'start_date']
    end <- promotion.data[i, 'end_date']
    tag <- as.character(promotion.data[i, tovar.col])
    
    for (j in seq(1:nrow(event.dummy))) {
      if (event.dummy[j, tag] == 0) {
        if (event.dummy[j, 'date'] >= start & event.dummy[j, 'date'] <= end) {
          event.dummy[j, tag] <- 1
        }
      }
    }
  }
  
  ## date별 y값에 dummy 변수 붙이기
  target.data.dummy <- merge(target.data, event.dummy, by='date', all.x=T)
  
  return(target.data.dummy)
  
}




#### create.smooth.vars : 일자별 매출 데이터와 프로모션 유형별 매출 데이터를 받아서 smoothing function 생성 ####

# target.data : 일자와 타겟 변수로 이루어진 데이터
# promotion.data : 프로모션 유형별 일별 매출 데이터(smoothing 시 사용)
# smooth.except.date : smoothing에서 제외할 일자(예 - 월초일을 smoothing에서 제외하려면 '01' 또는 '1' 입력)
# smooth.bandwidth : local regression 기반의 smoothing에서 얼마나 local하게 regression fit을 구하여 이을 것인지를 결정하는 bandwidth. 
#                    값이 높을수록 더 함수가 더 smooth해지며, NULL로 지정 시 최적의 bandwidth 값을 찾아서 plug-in
# smooth.origin : 'all'이면 전체 프로모션의 smoothing function 평균을 사용 / 'tag'이면 프로모션 유형별 smoothing function 평균을 사용
#                    프로모션 유형 간 비교 목적인 경우 'all', 같은 유형의 프로모션 내에서 개별 프로모션 간 비교 목적인 경우 'tag'
# smooth.var.sum : 'TRUE'  : 같은 프로모션 유형이 겹치는 기간에 프로모션 변수를 합산하여 적용
#                  'FALSE' : 같은 프로모션 유형이 겹치는 기간에 더 최근 프로모션의 변수값이 이전 값을 대체


create.smooth.vars <- function(target.data, promotion.data, smooth.except.date = NULL, smooth.bandwidth = 2,
                               smooth.origin = 'all', smooth.var.sum = TRUE) {
  
  suppressMessages(require(dplyr))
  
  ## smooth.except.date를 숫자로 준 경우 및 01일자를 1로만 기입한 경우 01로 만드는 처리
  smooth.except.date <- as.character(smooth.except.date)
  
  if (1 %in% nchar(smooth.except.date) == TRUE) {
    for (n in 1:length(smooth.except.date)) {
      if (nchar(smooth.except.date[n]) == 1) {
        smooth.except.date[n] <- paste0('0', smooth.except.date[n])
      }
    }
  }
  
  ## smooth.origin에 유효한 값 넣었는지 체크 ('all'이나 'tag'만 가능)
  if (!smooth.origin %in% c('all', 'tag')) {
    stop(print('fill the correct smoothing origin parameter: "all" or "tag" (default is "all")'))
  }
  
  ## 컬럼명 변경
  names(target.data) <- c('date','value')
  names(promotion.data) <- c('pro_id','start_date','end_date','pro_tag','date','value')
  
  
  ## 프로모션 시작-종료일에 NA가 있는지 확인 (NA가 있는 경우, 시간 포맷 변환 및 더미변수 생성 시, 에러 발생)
  if (sum(is.na(promotion.data[c('start_date')])) != 0) {
    stop(print('promotion start date included NA values. please fill date values for all rows of the start date field'))
  }
  if (sum(is.na(promotion.data[c('end_date')])) != 0) {
    stop(print('promotion end date included NA values. please fill date values for all rows of the end date field'))
  }
  if (sum(is.na(promotion.data[c('date')])) != 0) {
    stop(print('promotion date included NA values. please fill date values for all rows of the date field'))
  }
  
  ## 시간값 변환
  target.data['date'] <- format_time(unclass(target.data['date'])[[1]])
  target.data <- target.data[order(target.data$date),]
  promotion.data['start_date'] <- format_time(unclass(promotion.data['start_date'])[[1]])
  promotion.data['end_date'] <- format_time(unclass(promotion.data['end_date'])[[1]])
  promotion.data['date'] <- format_time(unclass(promotion.data['date'])[[1]])
  
  ## promotion.data 분리
  # 프로모션 기간 중 끊어지는 프로모션
  promotion.data.cut <- promotion.data[promotion.data$start_date < min(target.data$date) | promotion.data$end_date > max(target.data$date), ]
  # 프로모션 기간이 모델링 기간 내 모두 포함되는 프로모션
  promotion.data <- promotion.data[promotion.data$start_date >= min(target.data$date) & promotion.data$end_date <= max(target.data$date), ]
  
  ## 프로모션명과 종류 추출
  # 프로모션 기간 중 끊어지는 프로모션
  pro.nm.uid.cut <- unique(promotion.data.cut$pro_id)
  pro.tag.uid.cut <- unique(promotion.data.cut$pro_tag)
  
  # 프로모션 기간이 모델링 기간 내 모두 포함되는 프로모션
  pro.nm.uid <- unique(promotion.data$pro_id)
  pro.tag.uid <- unique(promotion.data$pro_tag)
  
  ## smooth function 에서 제외하는 일자를 확인하여 해당 일자만 제외한 프로모션별 일별 매출 데이터 생성 
  if (!is.null(smooth.except.date)) {
    # 제외 일자 있는 경우는 제외
    except.date.data <- subset(promotion.data, !substr(as.character(date),9,10) %in% smooth.except.date)
  } else {
    except.date.data <- promotion.data
  }
  
  ## Kernsmooth로 smoothing function 만들기
  smooth.list <- list() # 각 프로모션별 일매출 양상을 smoothing한 값
  smooth.means.tag <- list() # 프로모션 종류별로 smoothing 평균을 낸 값
  smooth.means.all <- data.frame(index=1:401) # 전체 프로모션의 smoothing 평균을 낸 값 (401 : 'locpoly'함수가 반환하는 smoothing 함수값 갯수의 default)
  
  for (i in pro.tag.uid) {
    smooth.list[[i]] <- list()
    
    for (j in pro.nm.uid) {
      if (promotion.data[promotion.data[,'pro_id']==j,][1, 'pro_tag'][[1]] == i) {
        sub.data <- subset(except.date.data, pro_tag==i & pro_id ==j)
        
        if(all(is.finite(smooth.bandwidth)) == 'FALSE') {
          stop(print('Smoothing bandwidth must be specified as numeric/integer type.'))
        }
        
        locpoly.result <- KernSmooth::locpoly(as.numeric(sub.data$date)/86400, as.numeric(sub.data$value),
                                              bandwidth = smooth.bandwidth)$y
        
        # 데이터 간격에 비해 bandwidth 값이 너무 작으면, smoothing 결과에 NaN이나 -Inf가 나올 수 있어 체크
        if (all(is.finite(locpoly.result))=='FALSE') {
          if (is.null(smooth.except.date)=='FALSE') {
            stop(print('Local regression result(smoothed value) included NaN or -Inf value. Need to specify a bigger bandwidth parameter or reduce the number of smooth.except.date.'))
          } else {
            stop(print('Local regression result(smoothed value) included NaN or -Inf value. Need to specify a bigger bandwidth parameter.'))
          }
        }
        
        # smooth.list에 smoothing function 값 저장 및 scaling
        smooth.list[[i]][[j]] <- locpoly.result
        
        smooth.list[[i]][[j]] <- smooth.list[[i]][[j]] / max(smooth.list[[i]][[j]])
        # smooth.list[[i]][[j]] <- ( smooth.list[[i]][[j]] - min(smooth.list[[i]][[j]]) )/ ( max(smooth.list[[i]][[j]]) - min(smooth.list[[i]][[j]]) )
        
        smooth.means.all[paste(i,j,sep='_')] <- smooth.list[[i]][[j]]   # 전체 프로모션에 대하여 smoothing 평균하는 경우를 위한 데이터
      
      }
    }
    
    # i번째 태깅에 대해 smoothing function 평균값을 저장(smoothing.means.tag)
    smooth.means.tag[[i]] <- data.frame(index = 1:401, smooth_mean = rowMeans(sapply(smooth.list[[i]], unlist)))
    
    # smooth.means.tag[[i]]$smooth_mean <- (smooth.means.tag[[i]]$smooth_mean - min(smooth.means.tag[[i]]$smooth_mean)) / (max(smooth.means.tag[[i]]$smooth_mean) - min(smooth.means.tag[[i]]$smooth_mean))
    smooth.means.tag[[i]]$smooth_mean <- smooth.means.tag[[i]]$smooth_mean / max(smooth.means.tag[[i]]$smooth_mean)
    
  }
  
  ## 전체 프로모션에 대한 smoothing function 평균값을 저장(smoothing.means.all)
  smooth.means.all['smooth_mean'] <- rowMeans(smooth.means.all[,2:ncol(smooth.means.all)])
  smooth.means.all <- smooth.means.all %>% dplyr::select(index, smooth_mean)
  # smooth.means.all$smooth_mean <- (smooth.means.all$smooth_mean - min(smooth.means.all$smooth_mean)) / (max(smooth.means.all$smooth_mean) - min(smooth.means.all$smooth_mean))
  smooth.means.all$smooth_mean <- smooth.means.all$smooth_mean / max(smooth.means.all$smooth_mean)
  
  ## smoothing function 결과 plot 저장(smoothing.graph)
  smoothing.graph <- list()
  
  if (smooth.origin == 'tag') {
    for (i in pro.tag.uid) {
      smoothing.graph[[i]] <- ggplot2::ggplot(smooth.means.tag[[i]], ggplot2::aes(index, smooth_mean))+ggplot2::geom_line()+
        ggplot2::theme_bw()+ggplot2::scale_y_continuous(limits = c(0,1))+
        ggplot2::ggtitle(sprintf("Smoothed Means - promotion '%s'", i))+
        ggplot2::theme(plot.title=ggplot2::element_text(size=ggplot2::rel(1.2), face="bold", hjust=0.5))
    }
  } else {
    smoothing.graph[['all']] <- ggplot2::ggplot(smooth.means.all, ggplot2::aes(index, smooth_mean))+ggplot2::geom_line()+
      ggplot2::theme_bw()+ggplot2::scale_y_continuous(limits = c(0,1))+
      ggplot2::ggtitle('Smoothed Means - all promotions')+
      ggplot2::theme(plot.title=ggplot2::element_text(size=ggplot2::rel(1.2), face="bold", hjust=0.5))
  }
  
  ## 전체 프로모션 데이터, 프로모션 종류 및 이름 복원
  promotion.data <- rbind(promotion.data, promotion.data.cut)
  pro.nm.uid <- unique(promotion.data$pro_id)
  pro.tag.uid <- unique(promotion.data$pro_tag)
  
  ## 프로모션별 기간에 따라 평균 smoothing 수치를 가져온 후, 다시 max scale함
  smooth.value <- list() # 각 프로모션 별로 실제 변수값에 들어가는 수치
  dt.tagvalue <- data.frame() # 태깅별 일별 smoothing 값 합친 것 (프로모션별 rbind)
  smooth.value.vector <- list() # 아래 값을 계산하기 위해 필요한 임시 벡터
  smooth.value.mean <- list() # 태깅별 변수값의 평균 (회귀계수 해석 시 필요 - delta x)
  
  k <- 1
  for (i in pro.tag.uid) {
    smooth.value[[i]] <- list()
    
    for (j in pro.nm.uid) {
      if (promotion.data[promotion.data[,'pro_id']==j,][1, 'pro_tag'] == i) {
        
        start <- promotion.data[promotion.data[,'pro_id']==j,][1, 'start_date'][[1]]
        end <- promotion.data[promotion.data[,'pro_id']==j,][1, 'end_date'][[1]]
        
        smooth.value[[i]][[j]] <- data.frame()
        
        for(l in seq(1:as.integer(end-start+1))) {
          # 프로모션 기간 중 며칠째인지('l')에 대응하는 smoothing function의 상대 위치값 계산
          ## 1~401 구간을 프로모션 기간 만큼 균등으로 나눈 후, 해당 시점의 smooth 값 가져옴
          
          if (l == 1) {
            smooth.index <- 1     # 1일차인 경우 첫 번째 함수값을 가져옴
          } else {
            smooth.index <- round((l-1)/as.integer(end-start) * 401)   # 다른 일차인 경우
          }
          
          smooth.value[[i]][[j]][l,1] <- data.frame(index=l)
          
          # 전체 프로모션 양상(all)인지, 태깅별 프로모션 양상(tag)인지 체크
          if (smooth.origin == 'all') {
            smooth.value[[i]][[j]][l,2] <- data.frame(smoothing.value=smooth.means.all[smooth.index, 'smooth_mean']) # 상대 위치값(smooth.index)에 해당하는 smooth value 가져옴
          } else if (smooth.origin == 'tag') {
            if (j %in% pro.nm.uid.cut) {   # 기간 중 끊어지는 프로모션이면 smoothing function을 'all'로부터
              smooth.value[[i]][[j]][l,2] <- data.frame(smoothing.value=smooth.means.all[smooth.index, 'smooth_mean']) # 상대 위치값(smooth.index)에 해당하는 smooth value 가져옴
            } else {                     # 기간 중 다 들어가는 프로모션이면 smoothing function을 'tag'로부터
              smooth.value[[i]][[j]][l,2] <- data.frame(smoothing.value=smooth.means.tag[[i]][smooth.index, 'smooth_mean']) # 상대 위치값(smooth.index)에 해당하는 smooth value 가져옴
            }
          } else {
            stop(print("Error: Fill the correct smoothing origin parameter: 'all' or 'tag' (default is 'all')"))
          }
        }
        
        smooth.value[[i]][[j]][,2] <- smooth.value[[i]][[j]][,2]/max(smooth.value[[i]][[j]][,2]) # max scale 변경
        smooth.value[[i]][[j]][,3] <- data.frame(date=start) 
        smooth.value[[i]][[j]][,3] <- smooth.value[[i]][[j]][,3]+as.difftime(smooth.value[[i]][[j]][,1]-1, unit="days") # 각 일자 정보 계산
        smooth.value[[i]][[j]][,4] <- data.frame(pro_tag=i)
        
        if (j %in% pro.nm.uid.cut) {
          smooth.value[[i]][[j]] <- smooth.value[[i]][[j]] %>% dplyr::filter(date >= min(target.data$date) & date <= max(target.data$date))
        }
        
        smooth.value.vector[[i]] <- c(smooth.value.vector[[i]], smooth.value[[i]][[j]][,2])  # i번째 태깅에 들어가는 변수값 누적
        
        if (k == 1) {
          dt.tagvalue <- data.frame(smooth.value[[i]][[j]][c('date','pro_tag','smoothing.value')])
          k <- k + 1
        } else if (k > 1 & k <= length(pro.nm.uid)) {
          dt.tagvalue <- rbind(dt.tagvalue, data.frame(smooth.value[[i]][[j]][c('date','pro_tag','smoothing.value')]))
          k <- k + 1
        }
      }
    }
    smooth.value.mean[[i]] <- mean(smooth.value.vector[[i]])  # i번째 태깅에 대하여 들어가는 모든 smoothing 변수값들의 평균 - 회귀계수 해석 시 필요
  }
  
  
  ## 변수 생성 (pro.vars)
  if (smooth.var.sum == FALSE) {
    pro.vars <- data.frame(date=target.data$date)
    pro.vars[,levels(pro.tag.uid)] <- 0   # BM변수의 초기값은 0으로 놓는다
    
    for (i in pro.tag.uid) {
      for (j in pro.nm.uid) {
        if (promotion.data[promotion.data[,'pro_id']==j,][1, 'pro_tag'] == i) {
          
          start <- promotion.data[promotion.data[,'pro_id']==j,][1, 'start_date']
          end <- promotion.data[promotion.data[,'pro_id']==j,][1, 'end_date']
          
          for (k in seq(1:nrow(pro.vars))) {
            if (pro.vars[k,'date'] >= start & pro.vars[k,'date'] <= end) {
              pro.vars[k,i] <- subset(smooth.value[[i]][[j]], date==pro.vars[k,'date'])$smoothing.value
            }
          }
        }
      }
    }
  } else {
    pro.vars <- merge(data.frame(date=target.data$date), data.table::dcast(dt.tagvalue, date~pro_tag, value.var="smoothing.value", sum), by='date', all.x=T) 
    pro.vars[is.na(pro.vars)] <- 0 # 변수값 없는건 0으로 변환
  }
  
  ## 일자와 종속변수가 들어있는 target.data에, smoothing한 변수 붙이기
  target.data.with.vars <- merge(target.data, pro.vars, by='date', all.x=T)
  
  if (smooth.origin == 'tag') {
    result <- list(data = target.data.with.vars,
                   smooth_except_date = smooth.except.date,
                   smoothing_means = smooth.means.tag,
                   smoothing_graph = smoothing.graph,
                   smooth_value = smooth.value,
                   smooth_value_mean = smooth.value.mean)
  } else {
    result <- list(data = target.data.with.vars,
                   smooth_except_date = smooth.except.date,
                   smoothing_means = smooth.means.all,
                   smoothing_graph = smoothing.graph,
                   smooth_value = smooth.value,
                   smooth_value_mean = smooth.value.mean)
  }
  
  return(result)
}



#### format_time : 일자값 변환 함수 ####

format_time <- function(data) {
  if (class(data)[1] == "POSIXlt" | class(data)[1] == "POSIXct" ) {
    return(data)
  }
  if (stringr::str_detect(data[1], "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2} \\+\\d{4}$")) {
    data <- as.POSIXct(strptime(data, format="%Y-%m-%d %H:%M:%S", tz="UTC"))
  }
  else if (stringr::str_detect(data[1], "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$")) {
    data <- as.POSIXct(strptime(data, format="%Y-%m-%d %H:%M:%S", tz="UTC"))
  }
  else if (stringr::str_detect(data[1], "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}$")) {
    data <- as.POSIXct(strptime(data, format="%Y-%m-%d %H:%M", tz="UTC"))
  }
  else if (stringr::str_detect(data[1], "^\\d{4}-\\d{2}-\\d{2} \\d{1}$")) {
    data <- as.POSIXct(strptime(data, format="%Y-%m-%d %H", tz="UTC"))
  }
  else if (stringr::str_detect(data[1], "^\\d{4}-\\d{2}-\\d{2} \\d{2}$")) {
    data <- as.POSIXct(strptime(data, format="%Y-%m-%d %H", tz="UTC"))
  }
  else if (stringr::str_detect(data[1], "^\\d{4}\\d{2}\\d{2} \\d{2}$")) {
    data <- as.POSIXct(strptime(data, format="%Y%m%d %H", tz="UTC"))
  }
  else if (stringr::str_detect(data[1], "^\\d{4}-\\d{2}-\\d{2}$")) {
    data <- as.POSIXct(strptime(data, format="%Y-%m-%d", tz="UTC"))
  }
  else if (stringr::str_detect(data[1], "^\\d{2}/\\d{2}/\\d{2}$")) {
    data <- as.POSIXct(strptime(data, format="%m/%d/%y", tz="UTC"))
  }
  else if (stringr::str_detect(data[1], "^\\d{2}/\\d{2}/\\d{4}$")) {
    data <- as.POSIXct(strptime(data, format="%m/%d/%Y", tz="UTC"))
  }
  else if (stringr::str_detect(data[1], "^\\d{4}\\d{2}\\d{2}$")) {
    data <- as.POSIXct(strptime(data, format="%Y%m%d", tz="UTC"))
  }
  else if (stringr::str_detect(data[1], "^\\d{4}/\\d{2}/\\d{2}/\\d{2}$")) {
    data <- as.POSIXct(strptime(data, format="%Y/%m/%d/%H", tz="UTC"))
  }
  else if( stringr::str_detect(data[1],"^\\d{4}-\\d{2}$")){
    data <- as.POSIXct(strptime(paste0(data, "-01"), format="%Y-%m-%d", tz="UTC"))
  }
  else if( stringr::str_detect(data[1],"^\\d{4}/\\d{2}$")){
    data <- as.POSIXct(strptime(paste0(data, "/01"), format="%Y/%m/%d", tz="UTC"))
  }
  
  return(data)
}








