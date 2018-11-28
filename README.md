# promotionImpact

promotionImpact는 타겟 지표(일매출, DAU 등)에 대한 프로모션의 효과를 측정/비교하기 위한 패키지입니다. 

프로모션 효과 비교에 있어 중요하게 통제되어야 할 트렌드, 주기성, 구조변화 등을 추정하고, 프로모션 효과의 시간에 따른 변화 양상을 반영하여 프로모션 효과를 측정 및 비교할 수 있도록 구성되어 있습니다.

## 설치 방법
R에서 아래 코드를 실행하시면 됩니다.

```
devtools::install_github("ncsoft/promotionImpact")
```

## 사용 방법
먼저, 아래의 데이터들이 필요합니다.

 - 일별 타겟 지표 데이터 (예 - 일별 매출, 일별 AU)

 - 프로모션 일정 데이터 (프로모션 ID, 시작/끝 날짜, 프로모션 유형 포함)

```
setwd('./promotionImpact/resources') # Rdata 포함한 directory
sim.data  # 일별 매출 시뮬레이션 데이터
```

|     dt     | simulated_sales |
| :--------: | :-------------: |
| 2015-02-11 |  1,601,948,810  |
| 2015-02-12 |  2,048,650,675  |
| 2015-02-13 |  2,288,870,304  |
|    ...     |       ...       |
| 2017-09-25 |  1,492,506,224  |

	sim.promotion # 프로모션 일정 시뮬레이션 데이터
| pro_id        | start_dt     | end_dt     | tag_info   |
|:----------------------:|:-------------:|:-----------:|:-----------:|
| pro_1_1  | 2015-02-16 | 2015-03-14 |    A     |
| pro_1_2  | 2015-06-07 | 2015-06-25 |    A     |
|   ...    |    ...     |    ...     |   ...    |
| pro_5_10 | 2017-04-02 | 2017-04-26 |    E     |

예제 데이터에서 프로모션은 2015-02-11 ~ 2017-09-25 동안 총 50번 진행되었으며, A, B, C, D, E 다섯 가지 유형에 대하여 각각 10번씩 진행되었습니다.

예제 데이터의 일 매출은 이들 프로모션의 효과 및 트렌드/주기성 요소, 월초일(매월 1일)의 급증 효과 및 기타 랜덤하게 발생하는 오차를 포함하고 있습니다.

<img src="https://github.com/ncsoft/promotionImpact/blob/master/resources/simulated_daily_sales.png?raw=true">

이 일 매출 데이터로부터 각 프로모션 유형별 효과를 분리하여 추정하고자 하는 것이 목표입니다.

먼저, 분석자가 월초일 효과를 통제하고 싶다면 아래와 같이 월초일 dummy 변수를 추가해 줍니다.

```
library(dplyr)
sim.data <- sim.data %>% 
dplyr::mutate(month_start = ifelse(substr(as.character(dt),9,10) == '01', 1, 0))
```

이와 같이, 프로모션 효과 비교 시 고려해야 한다고 판단하는 time dummy를 얼마든지 추가할 수 있습니다.

이제 아래와 같이 모델을 생성합니다.

```
pri1 <- promotionImpact(data=sim.data, promotion=sim.promotion, 
                        time.field = 'dt', target.field = 'simulated_sales', 
                        dummy.field = 'month_start',
                        trend = T, period = 30.5, trend.param = 0.02, period.param = 2,
                        logged = T, differencing = T)
```

위에서 쓰인 각 파라미터들에 대한 설명은 아래와 같습니다.

- data : 일자(time.field), 타겟지표(target.field) 및 기타 dummy변수(dummy.field)를 포함한 데이터
- promotion : 프로모션 일정 데이터
- trend : T이면 트렌드 있음 / F이면 트렌드 없음
- period : NULL 이면 주기성 없음, 'auto'이면 주기를 자동 추정, 특정 숫자값을 지정하면 입력한 주기로 추정
- trend.param : 트렌드 컴포넌트의 유연함을 조정하는 파라미터. 이 값이 높을수록 동적으로 변하는 트렌드.
- period.param : 주기성 컴포넌트의 유연함을 조정하는 파라미터. 이 값이 높을수록 동적으로 변하는 주기성.
- logged = T : 타겟 지표 및 연속형 독립변수에 대한 로그 변환 여부
- differencing = T : 타겟 지표 및 연속형 독립변수에 대한 차분 변환 여부

이를 통해 얻어진 각 프로모션 유형 별 효과는 아래에서 확인할 수 있습니다.

```
pri1$effects
         A        B        C        D        E
1 19.34965 13.40238 10.46531 7.764716 4.015453
```

로그 변환을 했기 때문에, 각 유형 별 효과는 '프로모션 기간 중 일 매출 증가율(%)'로 출력됩니다. 

예를 들어, A 유형 프로모션이 진행중인 기간의 일 매출은 다른 기간에 비해 약 19.3% 증가합니다.

위 결과를 보면 프로모션 유형 A가 가장 효과가 크고, 유형 E로 갈수록 효과가 감소함을 알 수 있습니다. 

-----------------------------------------------------------------------------------------------------------------------------------------------------------

이같은 효과 추정치를 얻어내기 위해, promotionImpact에서는 일련의 변수 처리 과정을 거칩니다. 

기본적으로는, 프로모션 기간 동안 프로모션 효과의 크기가 변화하는 평균적인 양상을 일종의 smoothing function으로 '본떠서', 이를 프로모션 기간 중의 변수값으로 입력하는 방식입니다. 

예를 들면, 위 모델의 경우 프로모션 효과의 변화 양상은 아래와 같이 추정되었습니다.

```
pri1$smoothvar$smoothing_graph
```

<p align="center">
<img width="450" height="400" src="https://github.com/ncsoft/promotionImpact/blob/master/resources/smoothing_function.png?raw=true" style="float: center; zoom:60%">
</p>

프로모션 시작일의 효과가 가장 크고, 이후 프로모션이 진행됨에 따라 효과가 감소하는 형태입니다. 

이같은 프로모션 효과의 '모양'을 프로모션 기간 내의 해당 프로모션 유형의 일별 변수값으로 입력받게 됩니다.

이 과정에 관한 자세한 정보는 아래를 통해 확인할 수 있습니다.

```
pri1$smoothvar$data   # 최종적으로 얻어진 일별 smoothing 변수값
pri1$smoothvar$smooth_except_date   # smoothing function을 만들 때 제외한 일자
pri1$smoothvar$smoothing_means   # smoothing function의 함수값
pri1$smoothvar$smoothing_graph   # 위 함수값에 대한 plot
pri1$smoothvar$smooth_value   # 각 프로모션별로 계산된 smoothing 변수값
pri1$smoothvar$smooth_value_mean   # 위 값에 대한 각 유형 별 평균
```

최종적인 모델링 결과는 아래를 통해 확인할 수 있습니다.

```
pri1$model$model  # 최종적으로 만들어진 선형모형 객체 (일반적인 lm object의 요소들을 포함)
pri1$model$final_input_data   # 해당 모형에 입력된 데이터 (변수변환 등 전처리 이후 데이터)
pri1$model$fit_plot   # 모델의 target vs fitted plot
```
<img src="https://github.com/ncsoft/promotionImpact/blob/master/resources/model_call_new.PNG?raw=true" style="float: center; zoom:60%">

<img src="https://github.com/ncsoft/promotionImpact/blob/master/resources/fit_plot.png?raw=true" style="float: center; zoom:60%">

위 그래프는 로그 및 차분 변환 이후의 타겟 지표에 대한 model fit을 보여 줍니다.

모델에 사용된 트렌드나 주기성 컴포넌트의 적절성은 아래의 plot을 통해 시각적으로 확인할 수 있습니다.

```
pri1$model$trend_period_graph_with_target   # 트렌드+주기성 컴포넌트를 타겟 지표와 함께 보기
```

<img src="https://github.com/ncsoft/promotionImpact/blob/master/resources/trend_periodicity_with_target.png?raw=true" style="float: center; zoom:60%">



-----------------------------------------------------------------------------------------------------------------------------------------------------------

위의 예제에서는 프로모션 별 일정 데이터(시작/종료일 및 유형)만 가지고 있어서, 각 프로모션의 효과 양상을 일별 타겟 지표 자체로부터 추정하였습니다. 그런데, 사용자가 각 프로모션별로 일자별 효과가 어떻게 되는지에 대한 데이터까지 가지고 있는 경우도 있을 수 있습니다. (예- 각 프로모션 별로 일별 결제금액을 집계할 수 있는 경우)

이를 활용하여 프로모션 효과의 양상을 추정하려면, 프로모션 데이터를 아래와 같이 입력합니다.

```
sim.promotion.sales  # 프로모션별 일별 결제금액 시뮬레이션 데이터
```

|  pro_id  |  start_dt  |   end_dt   | tag_info |     dt     |    payment    |
| :------: | :--------: | :--------: | :------: | :--------: | :-----------: |
| pro_1_1  | 2015-02-16 | 2015-03-14 |    A     | 2015-02-16 | 1,033,921,614 |
| pro_1_1  | 2015-02-16 | 2015-03-14 |    A     | 2015-02-17 |  971,764,194  |
|   ...    |    ...     |    ...     |   ...    |    ...     |      ...      |
| pro_5_10 | 2017-04-02 | 2017-04-26 |    E     | 2017-04-26 |  54,212,694   |

각 프로모션 별로, 일자별 결제금액('payment' 컬럼)이 입력되어 있는 데이터입니다.

이 경우에는 프로모션 효과의 시간에 따른 양상을 나타내는 smoothing function을 입력한 프로모션별 일별 결제금액으로부터 추정하게 되며, 이후 과정은 위에서 설명한 예제와 동일합니다.

```
pri2 <- promotionImpact(data=sim.data, promotion=sim.promotion.sales, 
                        time.field = 'dt', target.field = 'simulated_sales',
                        dummy.field = 'month_start',
                        trend = T, period = 30.5, trend.param = 0.02, period.param = 2,
                        logged = T, differencing = T)
```



-----------------------------------------------------------------------------------------------------------------------------------------------------------

한편, 지금까지처럼 프로모션 효과의 시간에 따른 변화 양상을 smoothing function 형태로 입력하는 게 아니라, 간단하게 dummy 변수로 입력할 수도 있습니다. 이 경우에는 아래와 같이 var.type 옵션을 'dummy'로 주면 됩니다.

```
pri3 <- promotionImpact(data=sim.data, promotion=sim.promotion, 
                        time.field = 'dt', target.field = 'simulated_sales', 
                        dummy.field = 'month_start', var.type = 'dummy',
                        trend = T, period = 30.5, trend.param = 0.02, period.param = 2,
                        structural.change = T, logged = F, differencing = F)
```

또한 이번에는, 트렌드/주기성 컴포넌트에 더하여, 시계열의 '구조변화(structural change)' 요소를 도입하여 보았습니다. structural.change 옵션을 TRUE로 주게 되면, 일별 타겟 지표의 급격한 수준 변화 지점을 탐지하여 이를 변수로 추가해 줍니다.

```
pri3$structural_breakpoint
"2015-09-16 UTC" "2016-02-23 UTC" "2016-11-22 UTC" "2017-04-20 UTC"
```

위의 날짜들에, 일 매출의 급격한 변화가 있었음을 알 수 있습니다. 주의할 점은, 프로모션이 타겟 지표에 미치는 효과의 양상이 뚜렷하고 급격한 경우, 프로모션의 출시 등으로 인한 급격한 효과를 타겟 시계열 평균의 구조적 변화로 오인할 수 있다는 점입니다(이에 대해서는 도메인 지식에 근거한 분석자의 적절한 판단이 필요합니다).

최종적으로 모델에 입력된 데이터를 보면 아래와 같습니다.

```
pri3$model$final_input_data
```

<img src="https://github.com/ncsoft/promotionImpact/blob/master/resources/dummy_variables.PNG?raw=true" style="float: center; zoom:100%">

프로모션 A, B, C, D, E의 변수가, 프로모션 진행중이면 1, 아니면 0의 dummy변수로 입력되었음을 알 수 있습니다. 'structure' 변수는 1에서 시작하여 구조변화점마다 2, 3, ... 등으로 증가하는 factor변수입니다.

로그 변환을 하지 않았으므로, 효과 추정치는 아래와 같이 상대효과(증가율)가 아닌 절대효과로 출력됩니다.

```
pri3$effects
          A         B         C         D         E
1 383088749 154422868 108831741 113017212 -13252524
```

기존 결과와는 프로모션 유형 간 효과의 순위가 다소 바뀌었음을 볼 수 있고(C<->D), E유형 프로모션의 경우 오히려 음(-)의 효과를  낸다고 나옵니다(이는 시뮬레이션 데이터를 생성할 때 입력한 프로모션 효과의 수준 및 순위와는 상이합니다). 현재까지의 사용 케이스에서는, 프로모션 유형 별 dummy 변수보다는 프로모션 효과의 변화 양상을 반영한 smoothed 변수가 일반적으로는 더 정확하게 프로모션 효과를 추정하는 것으로 보입니다.

-----------------------------------------------------------------------------------------------------------------------------------------------------------

요약하면, promotionImpact는 타겟 지표의 변동이 아래와 같은 세 가지 구성요소로 설명될 수 있을 때, 프로모션 효과를 측정 및 비교할 수 있게 해 줍니다.

<p align="center">
<img width="500" height="280" src="https://github.com/ncsoft/promotionImpact/blob/master/resources/model_concept.png?raw=true" />
</p>

특히, 타겟 시계열(일별 매출, AU 등)로부터 트렌드/주기성/구조변화 컴포넌트를 추정하여 이를 통제한 상태에서, 프로모션 효과의 시간에 따른 변화 양상을 고려한(smoothing) 변수처리를 통해 프로모션 효과를 분리/측정하는 것이 핵심 기능이라고 할 수 있습니다.
