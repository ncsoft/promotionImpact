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
#' @param period NULL to exclude any periodicity from the model, 'auto' to automatically determine the period, certain numeric value(e.g. '30.5' for month) to manually specify the period
#' @param trend.param Flexibility of trend component. Default is 0.05, and as this value becomes larger, the trend component will be more flexible.
#' @param period.param Flexibility of period component. Default is 3, and as this value becomes larger, the period component will be more flexible.
#' @param var.type 'smooth' to use smoothed promotion variables, 'dummy' to use dummy promotion variables
#' @param structural.change TRUE to incorporate structural changes in the intercept(baseline)
#' @param smooth.except.date Date value that will be excluded from the smoothing process. eg) '01' to exclude every start day of a month
#' @param smooth.bandwidth Bandwidth of local polynomial regression used in the smoothing process. Default value is 2.
#' @param smooth.origin 'all' to estimate a global smoothing function for all promotions. 'tag' to estimate different smoothing functions for different promotion types(tags).
#' @param smooth.var.sum If TRUE, the smoothing values for times when multiple promotions in a single tag overlap will be the values from the latest promotion. Otherwise, the values will be added(default).
#' @param logged TRUE to take logs to the target variable and the trend/period component
#' @param differencing TRUE to first difference the target variable, smoothed regressors, and the trend/period component values
#' @param synergy.promotion TRUE to incorporate synergy between promotion tags.
#' @param synergy.var Specify the synergy variables. 'names of fields' between each promotion tag and other variables. eg) c('month_start') to incorparate synergy between each promotion tag and 'month_start'.
#' @param allow.missing TRUE to allow missing data in promotion sales during the promotion period
#' @import dplyr
#' @import Rcpp
#' @import prophet
#' @import ggplot2
#' @import scales
#' @importFrom ggpubr ggarrange
#' @importFrom KernSmooth locpoly
#' @importFrom stringr str_detect
#' @importFrom strucchange breakpoints
#' @importFrom reshape2 melt
#' @importFrom data.table dcast
#' @importFrom utils capture.output
#' @export promotionImpact


#### promotionImpact 메인 함수 ####

promotionImpact <- function(data, promotion
                            ,time.field = 'date', target.field = 'value', dummy.field = NULL
                            ,trend = TRUE, period = 'auto', structural.change = FALSE
                            ,trend.param = 0.05, period.param = 3
                            ,var.type = 'smooth', smooth.except.date = NULL
                            ,smooth.bandwidth = 2, smooth.origin = 'all', smooth.var.sum = TRUE
                            ,logged = TRUE, differencing = TRUE, synergy.promotion = FALSE, synergy.var = NULL, allow.missing = TRUE) {
  
  
  data <- as.data.frame(data)
  promotion <- as.data.frame(promotion)
  
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
      if (is.null(dummy.field) == TRUE & trend == FALSE & is.null(period) == TRUE & structural.change == FALSE) {
        
        residuals <- data$value   # 통제변수가 없다면 타겟 지표 자체가 프로모션의 일별 효과
        
      } else {
        invisible(
          capture.output(
            suppressMessages(
              control <- promotion.model(data, time.field = 'date', target.field = 'value', dummy.field = dummy.field,
                                         trend = trend, period = period, structural.change = structural.change,
                                         trend.param = trend.param, period.param = period.param, logged=FALSE, differencing=FALSE)
            )
          )
        )
        
        residuals <- control$model$residuals   # 통제변수가 있다면 통제변수만 넣은 모형의 잔차가 프로모션의 일별 효과
        
      }
      
      # residual : 일별 잔차 데이터. 이들 잔차를 프로모션의 일별 효과로 간주.
      residual <- data.frame(date = data[,'date'])
      residual[,'residual'] <- residuals
      
      # residual 값을 기존 promotion에 join
      promotion <- promotion %>% dplyr::left_join(residual, by = c('date'='date'))
      
      promotion <- promotion %>% dplyr::filter(start_date >= min(data$date) & end_date <= max(data$date))
      
      # promotion의 잔차를 min-max scale
      promotion$residual <- (promotion$residual - min(promotion$residual)) / (max(promotion$residual) - min(promotion$residual))
      
      # 겹치는 날짜의 잔차를 균등 배분
      promotion <- promotion %>% dplyr::add_count(date)
      promotion <- promotion %>% dplyr::mutate(residual = residual/n)
      promotion <- promotion %>% dplyr::select(-n)
      
      # smoothed variables 생성
      smoothvar <- create.smooth.vars(target.data = data[ ,c('date', 'value')], promotion.data = promotion,
                                      smooth.except.date = smooth.except.date, smooth.bandwidth = smooth.bandwidth,
                                      smooth.origin = smooth.origin, smooth.var.sum = smooth.var.sum, smooth.scale = 'minmax')
      
    } else if (ncol(promotion) == 6) {
      
      names(promotion) <- c('pro_id','start_date','end_date','pro_tag','date','value')
      
      # 프로모션 기간 중 판매내역이 없는 날짜가 있는지 Check
      pro_list <- unique(promotion$pro_id)
      max_dt <- as.Date(max(data$date))
      min_dt <- as.Date(min(data$date))
      
      for(pid in pro_list){
        start_dt <- promotion[promotion$pro_id == pid,'start_date'][1]
        end_dt <- promotion[promotion$pro_id == pid,'end_date'][1]
        if( sum(!seq.Date(max(min_dt,start_dt),min(max_dt,end_dt),1) %in% promotion[promotion$pro_id == pid,'date']) > 0 ){
          if(allow.missing == TRUE){
            warning(sprintf("There is a date without sales data during the promotion period : promotion = '%s'", pid))
          }else{
            stop(sprintf("There is a date without sales data during the promotion period : promotion = '%s'. If you want to ignore missing data, change the option : allow.missing=TRUE", pid))
          }
          
        }
      }
      
      # smoothed variables 생성
      smoothvar <- create.smooth.vars(target.data = data[ ,c('date', 'value')], promotion.data = promotion,
                                      smooth.except.date = smooth.except.date, smooth.bandwidth = smooth.bandwidth,
                                      smooth.origin = smooth.origin, smooth.var.sum = smooth.var.sum, smooth.scale = 'max')
      
    }
    
    # 최종 입력 데이터
    input.data <- data %>% dplyr::left_join(smoothvar$data, by = c('date'='date', 'value'='value'))
    bmname <- unique(promotion$pro_tag)
    
    if( synergy.promotion == TRUE ){
      #프로모션간 시너지 효과 고려
      synergy <- combn(colnames(input.data[,colnames(input.data) %in% bmname]),2)
      for(cb in 1:ncol(synergy)){
        input.data <- cbind(input.data, apply(input.data[,colnames(input.data) %in% (synergy[,cb])],1,prod))
      }
      colnames(input.data)[(ncol(input.data)-ncol(synergy)+1):ncol(input.data)] <- apply(synergy,2,paste,collapse="and")
    }
    
    if( sum(synergy.var %in% names(input.data)[-c(grep('date', names(input.data)), grep('value', names(input.data)))]) > 0 ){
      #프로모션과 월초 등 기타변수의 시너지 효과 고려
      synergy <- NULL  
      for(b in colnames(input.data)[colnames(input.data) %in% bmname]){
        for(d in synergy.var[synergy.var %in% names(input.data)[-c(grep('date', names(input.data)), grep('value', names(input.data)))]]){
          input.data <- cbind(input.data, input.data[,b]*input.data[,d])
          synergy <- append(synergy,paste(b, d, sep='and'))
        }
      }
      colnames(input.data)[(ncol(input.data)-sum(colnames(input.data) %in% bmname)*length(synergy.var[synergy.var %in% names(input.data)[-c(grep('date', names(input.data)), grep('value', names(input.data)))]])+1):ncol(input.data)] <- synergy
    }
    
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
# period.param : 주기성 컴포넌트의 유연성을 조정하는 파라미터. 이 값이 클수록 동적으로 변하는 주기성 적합.

promotion.model <- function(data, time.field = 'date', target.field = 'value', dummy.field = NULL
                            ,logged = TRUE, differencing = TRUE
                            ,trend = TRUE, period = 'auto', structural.change = FALSE
                            ,trend.param = 0.05, period.param = 3) {
  
  requireNamespace("Rcpp", quietly = TRUE)
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
                               smooth.origin = 'all', smooth.var.sum = TRUE, smooth.scale = 'minmax') {
  
  
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
        
        smooth.means.all[paste(i,j,sep='_')] <- smooth.list[[i]][[j]]   # 전체 프로모션에 대하여 smoothing 평균하는 경우를 위한 데이터
        
      }
    }
    
    # i번째 태깅에 대해 smoothing function 평균값을 저장(smoothing.means.tag)
    smooth.means.tag[[i]] <- data.frame(index = 1:401, smooth_mean = rowMeans(sapply(smooth.list[[i]], unlist)))
    
    if (smooth.scale == 'minmax') {
      smooth.means.tag[[i]]$smooth_mean <- (smooth.means.tag[[i]]$smooth_mean - min(smooth.means.tag[[i]]$smooth_mean)) / (max(smooth.means.tag[[i]]$smooth_mean) - min(smooth.means.tag[[i]]$smooth_mean))
    } else if (smooth.scale == 'max') {
      smooth.means.tag[[i]]$smooth_mean <- smooth.means.tag[[i]]$smooth_mean / max(smooth.means.tag[[i]]$smooth_mean)
    }
  }
  
  ## 전체 프로모션에 대한 smoothing function 평균값을 저장(smoothing.means.all)
  smooth.means.all['smooth_mean'] <- rowMeans(smooth.means.all[,2:ncol(smooth.means.all)])
  smooth.means.all <- smooth.means.all %>% dplyr::select(index, smooth_mean)
  
  if (smooth.scale == 'minmax') {
    smooth.means.all$smooth_mean <- (smooth.means.all$smooth_mean - min(smooth.means.all$smooth_mean)) / (max(smooth.means.all$smooth_mean) - min(smooth.means.all$smooth_mean))
  } else if (smooth.scale == 'max') {
    smooth.means.all$smooth_mean <- smooth.means.all$smooth_mean / max(smooth.means.all$smooth_mean)
  }
  
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

#' detectOutliers
#'
#' detectOutliers extracts outliers which affect the average effects of promotions.
#'
#' @param model Execution result object : promotionImpact
#' @param threshold List of threshold values to be determined as outliers if greater than the written values
#' @param option The number of indicators that must be greater than the threshold values to be outliers.
#' @export detectOutliers

detectOutliers<-function(model, threshold=list(cooks.distance=1, dfbetas=1, dffits=2), option=2){
  
  ckdist<-cooks.distance(model$model$model)
  betas<-as.data.frame(dfbetas(model$model$model))
  fits<-dffits(model$model$model)
  outlier1 <- model$model$final_input_data[which(ckdist>threshold[['cooks.distance']]),'date']
  outlier2 <- model$model$final_input_data[rowSums(abs(betas)>threshold[['dfbetas']])>0,'date']
  outlier3 <- model$model$final_input_data[which(abs(fits)>threshold[['dffits']]),'date']
  
  if(option==1){
    #적어도 하나 이상 outlier로 판명될 때
    outliers <- union(union(outlier1, outlier2), outlier3)
    
  }else if(option==2){
    #적어도 두개 이상 outlier로 판명될 때
    outliers <- union(intersect(outlier1, union(outlier2, outlier3)), intersect(outlier2, outlier3))
    
  }else if(option==3){
    #세개 모두 outlier로 판명될 때
    outliers <- intersect(intersect(outlier1, outlier2), outlier3)
    
  }
  
  if(as.Date(model$model$final_input_data$date %>% tail(1)) %in% as.Date(as.POSIXct(outliers, origin='1970-01-01'))){
    #outlier날짜에 오늘이 있으면, 오늘을 제외한 outlier만 제거
    outliers <- as.POSIXct(outliers, origin='1970-01-01')[! as.Date(model$model$final_input_data$date %>% 
                                                                      tail(1)) %in% as.Date(as.POSIXct(outliers, origin='1970-01-01'))]
  }
  
  index <- which(model$model$final_input_data$date %in% outliers)
  outlier_dat <- model$model$final_input_data[model$model$final_input_data$date %in% outliers, c('date','value')]
  outlier_dat <- cbind(outlier_dat, ckdist=ckdist[index], dfbetas=betas[index,], dffits=fits[index])
  
  return(list(outliers=outlier_dat, cooks.distance = ckdist, dfbetas = betas, dffits = fits))
  
}


#' compareModels
#'
#' compareModels compares several models under user-defined conditions and suggests the best options.
#'
#' @param data Dataframe containing date, target variable, and some additional time dummies that the researcher wants to account for.
#' @param promotion Dataframe containing promotion ID, start date, end date, promotion tag(type). Might include daily payments associated with the promotion.
#' @param fix A List of constraints to find the best model. Constraints can only be in following list: 'period','trend','logged','synergy.var','differencing','smooth.origin','structural.change','synergy.promotion'
#' @param time.field Specify the date field of 'data'.
#' @param target.field Specify the target field of 'data'.
#' @param dummy.field Specify the additional time dummies of 'data'.
#' @param trend.param Flexibility of trend component. Default is 0.05, and as this value becomes larger, the trend component will be more flexible.
#' @param period.param Flexibility of period component. Default is 3, and as this value becomes larger, the period component will be more flexible.
#' @param var.type 'smooth' to use smoothed promotion variables, 'dummy' to use dummy promotion variables
#' @param smooth.except.date Date value that will be excluded from the smoothing process. eg) '01' to exclude every start day of a month
#' @param smooth.bandwidth Bandwidth of local polynomial regression used in the smoothing process. Default value is 2.
#' @param smooth.var.sum If TRUE, the smoothing values for times when multiple promotions in a single tag overlap will be the values from the latest promotion. Otherwise, the values will be added(default).
#' @param allow.missing TRUE to allow missing data in promotion sales during the promotion period
#' @importFrom lmtest bptest dwtest
#' @importFrom crayon italic bold green
#' @export compareModels



compareModels <- function(data, promotion, fix=list(logged = T, differencing = T), 
                          time.field = 'dt', target.field = 'sales', dummy.field = NULL,
                          trend.param = 0.05, period.param = 3, var.type = 'smooth', smooth.except.date = NULL,
                          smooth.bandwidth = 2, smooth.var.sum = TRUE, allow.missing = TRUE){
  
  if( !all(names(fix) %in% c('period','trend','logged','synergy.var','differencing','smooth.origin','structural.change','synergy.promotion')) ){
    #고정할 요소 이름 체크 
    stop('fix can only be in following list: "differencing", "logged", "smooth.origin", "synergy.var", "trend", "period", "structural.change", "synergy.promotion"')
  }
  # 옵션 input 잘못 입력 체크
  if('period' %in% names(fix)){
    if(length(fix[['period']])!=0){
      if(fix[['period']]!='auto' & !is.numeric(fix[['period']])){
        stop('fix[["period"]] can only be NULL or "auto" or numeric.')
      }
    }
  }
  if('smooth.origin' %in% names(fix)){
    if(!fix[['smooth.origin']] %in% c('all','tag')){
      stop('fix[["smooth.origin"]] can only be "all" or "tag".')
    }
  }
  
  # Y 결정 스텝
  decide_y <- data.frame(differencing = rep(c(TRUE,FALSE),each = 2), logged=rep(c(TRUE,FALSE),2),
                         period = rep('auto', 4), smooth.origin = rep('all', 4),
                         synergy.var = rep('NULL', 4), trend = rep(TRUE, 4),
                         structural.change = rep(TRUE, 4), synergy.promotion = rep(FALSE, 4)
  )
  
  if(length(names(fix)[!names(fix) %in% c('logged','differencing')])==0){
    synergy_var <- NULL
  }
  if('period' %in% names(fix) & is.null(fix[['period']])==TRUE){
    decide_y[,'period'] <- 'NULL'
    fix[['period']] <- 'NULL'
  }
  
  for(name in names(fix)[!names(fix) %in% c('logged','differencing')]){
    #사용자 지정 옵션 
    if(length(fix[[name]])==1 & name!='synergy.var'){
      decide_y[,name] <- fix[[name]]
      synergy_var <- NULL
    }else if(name == 'synergy.var'){
      synergy_var <- fix[[name]]
    }else{
      stop(sprintf('length of fix[[%s]] must be 1 except "synergy.var".', name))
    }
    
  }
  
  residPlots <- function(data, title){
    ggdat <- data.frame(time=1:length(data), residual=data)
    ggplot(ggdat, aes(time,residual))+ggtitle(title)+theme_bw()+scale_y_continuous(labels=comma)+theme(plot.title = element_text(hjust=0.5))+
      stat_smooth(data=ggdat,aes(time, residual),method = 'loess',level = 1-0.1^10, span = length(data)/5000, color='#d6d6d6')+
      geom_hline(aes(yintercept = 0), color = 'red')+geom_point(size = 0.7)+geom_line()
  }
  qqPlots <- function(data, title){
    ggplot(data.frame(sample=data), aes(sample = sample))+geom_qq()+stat_qq_line(color='red')+theme_bw()+
      ggtitle(title)+theme(plot.title = element_text(hjust=0.5))
  }
  acfPlots <- function(data, title){
    acf_dat <- data.frame(lag = 0:30, acf=acf(data,lag.max = 30, plot = FALSE)$acf)
    ggplot(acf_dat, aes(lag, acf))+geom_hline(aes(yintercept = 0), color='red')+geom_segment(mapping = aes(xend = lag, yend = 0))+
      theme_bw()+ggtitle(title)+theme(plot.title = element_text(hjust=0.5))+
      geom_hline(aes(yintercept = 1.96/sqrt(length(data))), color = 'blue', linetype = 'dashed')+
      geom_hline(aes(yintercept = -1.96/sqrt(length(data))), color = 'blue', linetype = 'dashed')
  }
  histPlots <- function(data, title){
    dat <- data.frame(x = data)
    dat2 <- data.frame(x=density(data)$x, y=density(data)$y)
    ggplot(dat, aes(x=x))+geom_histogram(color='white',aes(y=..density..),fill='grey', bins = nrow(dat)*0.3)+theme_bw()+
      geom_line(aes(x=x, y=y, color = 'kernel'), data = dat2, size=1)+geom_line(aes(x,dnorm(x, sd = sd(data)), color = 'normal'),data=dat,size=1)+
      scale_colour_manual('density',values=c(kernel='black', normal='steelblue'))+xlab('residual')+
      ggtitle(title)+theme(plot.title = element_text(hjust=0.5))
  }
  
  
  model_y <- list()
  plot_resid <- list()
  plot_qq <- list()
  plot_acf <- list()
  plot_hist <- list()
  message('Finding proper response variable...')
  for(i in 1:nrow(decide_y)){
    if(decide_y[i,'period']=='NULL'){
      period_var <- NULL
    }else{
      period_var <- 'auto'
    }
    model_y[[i]] <- suppressMessages(promotionImpact(data, promotion, 
                                                     time.field = time.field, target.field = target.field, dummy.field = dummy.field,
                                                     trend = decide_y[i,'trend'], period = period_var, structural.change = decide_y[i,'structural.change'],
                                                     trend.param = trend.param, period.param = period.param,
                                                     var.type = var.type, smooth.except.date = smooth.except.date,
                                                     smooth.bandwidth = smooth.bandwidth, smooth.origin = decide_y[i,'smooth.origin'], smooth.var.sum = smooth.var.sum,
                                                     logged = decide_y[i,'logged'], differencing = decide_y[i,'differencing'], 
                                                     synergy.promotion = decide_y[i,'synergy.promotion'], synergy.var = synergy_var, allow.missing = allow.missing))
    
    plot_resid <- append(plot_resid, list(residPlots(model_y[[i]]$model$model$residuals, sprintf('residual - differencing=%s, logged=%s',decide_y[i,1],decide_y[i,2]))))
    plot_qq <- append(plot_qq, list(qqPlots(model_y[[i]]$model$model$residuals, sprintf('Normal Q-Q Plot - differencing=%s, logged=%s', decide_y[i,1], decide_y[i,2]))))
    plot_acf <- append(plot_acf, list(acfPlots(model_y[[i]]$model$model$residuals, sprintf('ACF Plot - differencing=%s, logged=%s', decide_y[i,1], decide_y[i,2]))))
    plot_hist <- append(plot_hist, list(histPlots(model_y[[i]]$model$model$residuals, sprintf('Histogram of Residuals - differencing=%s, logged-%s', decide_y[i,1], decide_y[i,2]))))
    message(paste(round(i/nrow(decide_y)*100),'% Completed',sep=''))
  }
  
  #Y의 경우, 옵션 받은 걸로 넘김
  if(sum(names(fix) %in% c('logged','differencing'))==2){
    index <- which(decide_y$logged==fix[['logged']] & decide_y$differencing==fix[['differencing']])
    y_cond <- list(logged = fix[['logged']], differencing = fix[['differencing']])
  }else if(sum(names(fix) %in% c('logged'))==1){
    index <- which(decide_y$logged==fix[['logged']] & decide_y$differencing==TRUE)
    y_cond <- list(logged = fix[['logged']], differencing = TRUE)
  }else if(sum(names(fix) %in% c('differencing'))==1){
    index <- which(decide_y$logged==TRUE & decide_y$differencing==fix[['differencing']])
    y_cond <- list(logged = TRUE, differencing = fix[['differencing']])
  }else{
    index <- which(decide_y$logged==TRUE & decide_y$differencing==TRUE)
    y_cond <- list(logged = TRUE, differencing = TRUE)
  }
  
  message('Finding proper independent variables...')
  ## X 결정
  params <- data.frame(1)
  while(min(unlist(apply(params,2,table)))<4){
    #가능한 모든 경우의 수
    params <- data.frame(differencing = rep(decide_y[index,'differencing'], 2^5),
                         logged = rep(decide_y[index,'logged'], 2^5),
                         smooth.origin = rep(c('all','tag'), each = 2^4),
                         synergy.promotion = rep(rep(c(TRUE, FALSE), each = 2^3),2^1),
                         trend = rep(rep(c(TRUE, FALSE), each = 2^2),2^2),
                         period = rep(rep(c('auto','NULL'), each = 2^1),2^3),
                         structural.change = rep(c(TRUE,FALSE),2^4)
    )
    params$smooth.origin <- as.character(params$smooth.origin)
    params$period <- as.character(params$period)
    
    for(name in names(fix)[! names(fix) %in% c('synergy.var','period')]){
      #사용자 지정 옵션 
      params <- params[params[,name]==fix[[name]],]
    }
    if('period' %in% names(fix)){
      if(fix[['period']] %in% c('NULL','auto')){
        params <- params[params[,'period']==fix[['period']],]
      }else{
        params[,'period'] <- fix[['period']]
      }
    }
    params <- unique(params)
    #최대 10종류만
    params <- params[sample(1:nrow(params),min(10,nrow(params))),]
    #경우의 수 4개 이하일 경우 무한루프 방지 
    if(nrow(params)<=4) break
  }
  rownames(params) <- 1:nrow(params)
  
  model <- list()
  for(i in 1:nrow(params)){
    if(params[i,'period']=='NULL'){
      period_var <- NULL
    }else{
      period_var <- 'auto'
    }
    model[[i]] <- suppressMessages(promotionImpact(data, promotion, 
                                                   time.field = time.field, target.field = target.field, dummy.field = dummy.field,
                                                   trend = params[i,'trend'], period = period_var,
                                                   structural.change = params[i,'structural.change'],
                                                   trend.param = trend.param, period.param = period.param,
                                                   var.type = var.type, smooth.except.date = smooth.except.date,
                                                   smooth.bandwidth = smooth.bandwidth, smooth.origin = params[i,'smooth.origin'], smooth.var.sum = smooth.var.sum,
                                                   logged = params[i,'logged'], differencing = params[i,'differencing'],
                                                   synergy.promotion = params[i,'synergy.promotion'], synergy.var = synergy_var, allow.missing = allow.missing))
    
    params[i, 'AIC'] <- AIC(model[[i]]$model$model)
    params[i, 'RMSE'] <- sqrt(mean(model[[i]]$model$model$residuals^2))
    params[i, 'MAE'] <- mean(abs(model[[i]]$model$model$residuals))
    params[i, 'p'] <- length(model[[i]]$model$model$coefficients)
    message(paste(round(i/nrow(params)*100),'% Completed',sep=''))
  }
  
  test_list <- list(
    'log=T, diff=T'=list(heteroscedasticity = lmtest::bptest(model_y[[1]]$model$model), 
                         normality = shapiro.test(model_y[[1]]$model$model$residuals), 
                         autocorrelation = lmtest::dwtest(model_y[[1]]$model$model, alternative = 'two.sided')),
    'log=F, diff=T'=list(heteroscedasticity = lmtest::bptest(model_y[[2]]$model$model), 
                         normality = shapiro.test(model_y[[2]]$model$model$residuals), 
                         autocorrelation = lmtest::dwtest(model_y[[2]]$model$model, alternative = 'two.sided')),
    'log=T, diff=F'=list(heteroscedasticity = lmtest::bptest(model_y[[3]]$model$model), 
                         normality = shapiro.test(model_y[[3]]$model$model$residuals), 
                         autocorrelation = lmtest::dwtest(model_y[[3]]$model$model, alternative = 'two.sided')),
    'log=F, diff=F'=list(heteroscedasticity = lmtest::bptest(model_y[[4]]$model$model), 
                         normality = shapiro.test(model_y[[4]]$model$model$residuals), 
                         autocorrelation = lmtest::dwtest(model_y[[4]]$model$model, alternative = 'two.sided'))
  )
  
  report <- paste(c('Analysis report: To satisfy the assumption of residuals, we recommand ',paste(paste(names(y_cond),y_cond,sep="="),collapse=', '),
                    ' transformation on the response variable. And the most appropriate options for independent variables are ',
                    paste(paste(names(params)[3:7],params[which.min(params$AIC),3:7],sep="="),collapse = ", "),
                    ' under ',paste(paste(names(fix),fix,sep="="),collapse=', '),' condition. But this may be local optimum not global optimum.'), collapse = "")
  
  writeLines(c('Analysis report',
               sprintf('To satisfy the assumption of residuals, we recommand %s transformation on the response variable.',
                       crayon::italic(crayon::green(crayon::bold(paste(paste(names(y_cond),y_cond,sep="="),collapse=', '))))), 
               sprintf('And the most appropriate options for independent variables are %s under %s condition.',
                       crayon::italic(crayon::green(crayon::bold(paste(paste(names(params)[3:7],params[which.min(params$AIC),3:7],sep="="),collapse = ", ")))),
                       crayon::italic(crayon::green(crayon::bold(paste(paste(names(fix),fix,sep="="),collapse=', '))))
               ),'But this may be local optimum not global optimum.'))
  
  fitted_data <- predict(model[[which.min(params$AIC)]]$model$model, interval = 'confidence')
  fitted_data <- cbind(fitted_data, model[[which.min(params$AIC)]]$model$final_input_data[,c('date','value')])
  fitted_data$date <- as.Date(fitted_data$date)
  final_plot<-ggplot(fitted_data %>% tail(100), aes(date, value))+theme_bw()+
    geom_ribbon(aes(ymin=lwr,ymax=upr, fill='confidence'),alpha=0.7)+
    geom_point(aes(date, fit, color='fitted'))+geom_line(aes(date,fit,color='fitted'))+
    geom_point(aes(color='value'))+geom_line(aes(color='value'))+scale_x_date(date_breaks = '1 week')+scale_y_continuous(labels= comma)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = 'top')+
    scale_colour_manual('',values=c(value='black', fitted='red'))+scale_fill_manual('',values=c(confidence='lightpink2'),labels=c('95% confidence interval'))
  
  params[which.min(params$AIC),'_'] <- '*final*'
  params[-which.min(params$AIC),'_'] <- ''
  colnames(params)[ncol(params)] <- ''
  
  return(list(residualPlot = plot_resid,
              qqPlot = plot_qq,
              acfPlot = plot_acf,
              histPlot = plot_hist,
              models_y = model_y,
              residualTest = test_list,
              params = params, 
              models = model,
              report = report,
              final_model = model[[which.min(params$AIC)]],
              final_plot = final_plot
  ))
}

