---
output:
  html_document:
    highlight: tango
    keep_md: yes
    theme: spacelab
---


</br>

***

<center>
## "요기요 음식리뷰를 통한 가성비 음식점 찾아내기"
*WooJun Kang*
</center>

***

<center>
### **오늘 뭐 먹지?**
</center>

배달 앱을 키며 유저들이 하는 생각입니다.
어떤 음식을 배달시킬 지 각자의 기준이 있을 것입니다.
우선 치킨을 먹을지 피자를 먹을지 정하고, 어떤 치킨을 먹을지 정합니다.
여기서 음식점 선택의 공통적 기준은 '맛' 입니다. 하지만 돈이 많지 않는 대학생들에게는 맛도 맛이지만 얼마냐 양이 많은지도 중요할 것입니다.

돈이 없는 학생에게 배달 음식 결정에서 주된 선택기준은 
저렴하고 양이 많은 음식입니다.
하지만 기존 배달앱에 정렬 방식에는 '최소 주문 금액순' 정도입니다.

따라서 음식점 리뷰를 통해 가성비가 좋은 음식점을 가려냄으로써
'가성비 순'이란 새로운 필터 방식을 제안합니다.


***

</br>

> #### Load Library

사용할 라이브러리를 불러옵니다.


```r
library(tidyverse)
library(httr)
library(jsonlite)
library(rvest)
library(urltools)
library(NLP4kec)
library(tm)
library(RWeka)
```

</br>

***

</br>

> ### **1. 리뷰 수집**

수집대상: 9개의 기존 카테고리 안에서 '리뷰가 많은 순'으로 상위 10개의 가게 리뷰 수집
`1인분 주문`, `야식`, `프랜차이즈`는 겹치는 가게가 많아 수집대상에서 제외했습니다.

수집하는 9개의 카테고리는 아래와 같습니다.
`치킨`, `피자/양식`, `중국집`, `한식`, `일식/돈까스`, `족발/보쌈`, `야식`, `분식`, `카페/디저트`

주소는 연세대(서울특별시 서대문구 연세로 50) 기준.

</br>

> #### *1-1. 가게 정보 수집*

개별 가게 페이지로 이동 후 리뷰를 수집하기 위해, 먼저 요기요 측에서 부여한 `가게 고유 번호`를 크롤링해와야 합니다. 

```r
category <- c('치킨', '피자양식', '중식', '한식', '일식돈까스', '족발보쌈', '분식', '카페디저트')
store_info <- data.frame()

## 음식 카테고리, 가게 교유 ID, 가게 이름, 배달비, 최소 주문 금액, 총 리뷰 수를 크롤링 합니다.
for(i in 1:length(category)){
  
  # cat('카테고리 [', category[i], '] 가게 고유 번호 수집중...\n')
  
  # 리뷰 많은 순으로 정렬했을때 상위 20곳의 음식점에 대한 정보 http 요청
  res <- GET(url = 'https://www.yogiyo.co.kr/api/v1/restaurants-geo/',
             query = list(category = category[i] %>% url_encode() %>% toupper() %>% I(),
                          items = 20,
                          lat = 37.5565050755347,
                          lng = 126.939656244325,
                          order = 'review_count',
                          page = 0),
             add_headers('X-ApiSecret' = 'fe5183cc3dea12bd0ce299cf110a75a2',
                         'X-ApiKey' = 'iphoneap'))
  
  # cat('[', category[i], '] http 요청 상태 코드:', res$status_code, '\n')
  
  
  # JSON형태로 가게 정보 추출
  temp <- res %>% httr::content(as = 'text', encoding = 'UTF-8') %>% fromJSON()
  
  # 카테고리별 가게 정보 하나의 데이터 프레임으로  
  store <- data.frame(category = category[i], # 카테고리
                      id = temp$restaurants$id, # 가게 ID
                      name = temp$restaurants$name, # 가게 이름
                      delivery_fee = temp$restaurants$delivery_fee, # 배달비
                      min_order_amount = temp$restaurants$min_order_amount, # 최소주문금액
                      review_count = temp$restaurants$review_count, # 총 리뷰 수
                      stringsAsFactors = FALSE) 
  
  # 전체 카테고리 하나의 데이터 프레임으로 
  store_info <- rbind(store_info, store, stringsAsFactors = FALSE)
  
  Sys.sleep(5)

}
```

</br>
카테고리가 중복되는 음식점은 하나의 카테고리만 갖게 합니다.


```r
n_distinct(store_info$id) 
```

```
## [1] 130
```

```r
store_info <- store_info[!duplicated(x = store_info$id), ]
nrow(store_info)
```

```
## [1] 130
```

</br>
`리뷰 수`가 너무 적으면 분석의 정확도가 떨어질 수 있어, `리뷰 수`가 100개가 안되는 음식점은 제외시켰습니다.


```r
summary(store_info$review_count)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0   107.5   376.0   559.2   813.2  4489.0
```

```r
store_info <- store_info %>% filter(review_count >= 100)
```

</br>
최종 음식점 정보 데이터 셋


```r
str(store_info)
```

```
## 'data.frame':	101 obs. of  6 variables:
##  $ category        : chr  "치킨" "치킨" "치킨" "치킨" ...
##  $ id              : int  225071 241433 230513 234960 26338 236657 257772 10962 246043 259782 ...
##  $ name            : chr  "BHC-신촌점" "BHC-신수서강대점" "범벅치킨-신촌점" "롯데리아-신촌로터리점" ...
##  $ delivery_fee    : int  2000 2000 0 0 1000 2000 0 0 0 2000 ...
##  $ min_order_amount: int  14000 15000 15000 11000 19000 15000 10000 14000 11000 14000 ...
##  $ review_count    : int  2093 1862 1764 1413 1263 1226 1224 1210 1128 1104 ...
```


```
## 최종 분석할 음식점의 수는 101 입니다.
```

</br>

> #### *1-2. 음식점 리뷰 수집 *

`for loop`를 돌리기 위해 최대 리뷰 수를 10으로 나눠 `리뷰 최대 페이지`를 알아냅니다.


```r
max_review_page <- (store_info$review_count / 10) %>% ceiling()
```


```r
# 빈 데이터 프레임 
store_review <- data.frame()

for (i in 1:nrow(store_info)) {
  
  # cat('##### [', i, '/', nrow(store_info), '] 음식점 리뷰 수집 시작! #####\n')
  
  # 음식점 리뷰 페이지 url 할당
  url <- str_c('https://www.yogiyo.co.kr/api/v1/reviews/', store_info$id[i], sep = '')
  # 리뷰 페이지 수
  n <- max_review_page[i]
  
  for (j in 1:n) {
    
    #  cat('[', store_info$name[i], '] 리뷰 수집 중...(', j, '/', n, ')\n')
    
    # http 요청
    res <- GET(url = url,
               query = list(count = 10,
                            only_photo_review = 'false',
                            page = j,
                            sort = 'time'))
    
    # cat('http 요청 상태 코드:', res$status_code, '\n')
    
    # JSON 형태로 가게 리뷰 추출
    temp_review <- res %>% httr::content(as = 'text', encoding = 'UTF-8') %>% fromJSON()
    
    # 필요한 정보만 가져와 하나의 데이터 프레임으로
    reviews <- data.frame(id = store_info$id[i],
                          name = store_info$name[i], 
                          comment = temp_review$comment, # 음식점 리뷰
                          rating_quantity = temp_review$rating_quantity, # 양 평점
                          stringsAsFactors = FALSE)
    
    # 모든 리뷰 하나의 데이터 프레임으로
    store_review <- rbind(store_review, reviews, stringsAsFactors = FALSE)
    
  }
}
```


```
## 분석할 음식점의 리뷰 수는 71543 입니다.
```

</br>

***

</br>

> ### **2. 리뷰 분석**

</br>

> #### *2-1. '가성비 있는 음식점'이란 의미를 내포하는 리뷰 추출*

'가성비가 좋다'라는 의미를 직접적으로 내포하는 리뷰와
'양이 많다'는 의미를 내포하는 리뷰를 
가성비 있는 음식점의 리뷰라 가정했습니다.
 
따라서 '가성비'란 단어를 포함한 리뷰와
'양'과 '많'이란 단어를 포함한 리뷰만 추출했습니다.

또한 가성비가 좋다는 말인 '혜자'라는 단어 포함한 것도
가성비와 관련된다 판단했습니다.


```r
# '양'이란 단어와 '많'이란 단어가 포함된 리뷰만 추출
store_quantity <- store_review %>% 
  filter(str_detect(.$comment, '양') & str_detect(.$comment, '많')) 

# '가성비'란 단어가 포함된 리뷰만 추출
store_co_ef <- store_review %>% 
  filter(str_detect(.$comment, '가성비'))

# '혜자'란 단어가 포함된 리뷰만 추출
store_haeja <- store_review %>% 
  filter(str_detect(.$comment, '혜자'))

# 데이터 프레임을 합치고 중복되는 리뷰를 제거합니다.
total_cost_ef <- store_quantity %>% 
  rbind(store_co_ef, 
        stringsAsFactors = FALSE) %>% 
  rbind(store_haeja,
        stringsAsFactors = FALSE) %>%
  `[`(!duplicated(.$comment), )
```

</br>
단순하게 단어의 포함 여부만으로 리뷰를 가려내다보니깐
*'가성비가 뛰어나지 않다'*, *'양이 많은건 아니다'*, *'양이 조금만 더 많았으면'*과 같은 
`부정적인 리뷰`도 추출됐습니다.


```r
head(total_cost_ef$comment)
```

```
## [1] "양도 많고 배달도 빠르구 칸초 과자까지 서비스로 주셨어요\U0001f496\U0001f496\U0001f496감사해용"
## [2] "치레카 너무 맛있어요!!ㅠㅠ양도 많고 마늘튀김 좋아요!!"                                        
## [3] "믿먹 뿌링클입니다요 둘이 먹었는데 보기보다 양이 많아서 놀람 배달은 좀 걸렸지만 맛있어서 만족" 
## [4] "맛도 있었고 양도 많고 배달 1시간 걸릴거갘다고 했는데 30~40분만에 와서 좋았어요~"              
## [5] "양은 그렇게 많은건 아닌거 같지만 먹다보면 혼자서 다 못억어요"                                 
## [6] "배달도 빨랐고 치킨 양도 많아서 좋았어요!! 감사합니다~"
```

```r
tail(total_cost_ef$comment)
```

```
## [1] "이가격에 이정도양에 구성에 다른 브랜드랑 비교가된다.혜자다!!!매운맛도 자극적이지 않아서 좋다^^"                                                                                                            
## [2] "배달 95분 걸린다고 해서 절망하고는 그래도 열심히 기다리고 있었는데 한시간 정도 만에 왔어요! 오뎅대신 떡만 달라고 했는데 요구사항도 잘 들어주시고 치즈츄가도 아주 혜자스러웠슴니다 !"                       
## [3] "세상 맛있고 세상 혜자스럽습니다ㅜㅜ\n치즈추가 필수예여!!\n사장님 오래오래 장사해주세요~~~❤"                                                                                                                
## [4] "맛있고 양도 완전 혜자!! 다음에 또 시켜먹을께요!"                                                                                                                                                           
## [5] "벼락 떡볶이는 사랑입니다ㅜㅠ\n오늘은 처음으로 치즈추가해봤는데 양이 혜자예요^^\n야끼만두 안먹어서 안주셔도 된다했더니 김말이로 대체해 주셔서 맛있게 먹었습니다\n여기 참깨김밥 떡볶이와 같이 먹으면 핵존맛!"
## [6] "50퍼 혜자 배달도 빨리옴"
```

</br>

> #### *2-2. 텍스트 마이닝*

</br>

> ##### 2-2-1. 형태소 분석

형태소 분석은 `은전한닢 프로젝트`로 개발된 형태소 분석 패키지인 `NLP4kec`를 이용해 진행했습니다.
comment 컬럼의 공백을 모두 제거합니다.
`은전한닢` 형태소 분석기는 띄어쓰기 를 자체적으로 구분합니다.


```r
total_cost_ef$comment <- total_cost_ef$comment %>% str_remove_all(pattern = '\\s+')
```

</br>
형태소를 분석하여 객체에 할당합니다.


```r
total_parsed <- r_parser_r(contentVector = total_cost_ef$comment, language = 'ko')
```

</br>
형태소 분석 전 리뷰와 분석 후 리뷰를 출력해봅니다.


```r
total_cost_ef$comment[1]
```

```
## [1] "양도많고배달도빠르구칸초과자까지서비스로주셨어요\U0001f496\U0001f496\U0001f496감사해용"
```

```r
total_parsed[1]
```

```
## [1] "양 많다 배달 빠르다 카다 초과 서비스 주다 "
```

</br>
'양이 작다'와 같은 가성비 의미에 반대되는 텍스트 추출하기 위해
인접한 2개의 단어렬 결합한 `bigram`을 생성합니다.

`말뭉치` 생성을 위해 형태소 분석된 문자 벡터를 벡터 소스로 변경해야 합니다.
벡터 소스는 벡터의 개별 원소를 각각의 문서로 인식합니다.


```r
total_corpus <- total_parsed %>% VectorSource() %>% VCorpus()
```

</br>
제대로 생성됐는지 확인해봅니다.


```r
identical(x = nrow(total_cost_ef),
          y = length(total_corpus))
```

```
## [1] TRUE
```

</br>
생성된 말뭉치는 `content`와 `meta`라는 2개의 원소를 갖는 리스트입니다.


```r
total_corpus[[1]]$content
```

```
## [1] "양 많다 배달 빠르다 카다 초과 서비스 주다 "
```

```r
total_corpus[[1]]$meta 
```

```
##   author       : character(0)
##   datetimestamp: 2019-04-02 08:16:06
##   description  : character(0)
##   heading      : character(0)
##   id           : 1
##   language     : en
##   origin       : character(0)
```

</br>

> ##### 2-2-2. 사전생성

`bigram` 생성을 위해 사용자함수를 만듭니다.
`min`과 `max`에 할당할 숫자를 바꾸면 원하는 N-gram을 만들 수 있습니다.


```r
bigram <- function(x, min = 2, max = 2){
  NGramTokenizer(x = x, control = Weka_control(min = min, max = max))
}

# bigram으로 DTM 생성
bigramList <- total_corpus %>% 
  TermDocumentMatrix(control = list(tokenize = bigram)) %>% 
  apply(MARGIN = 1, FUN = sum) %>% 
  sort(decreasing = TRUE)
```


```r
head(bigramList)
```

```
##     양 많다 많다 맛있다     것 같다 배달 빠르다 시키다 먹다   맛있다 양 
##        3582         925         879         678         675         600
```

```r
tail(bigramList)
```

```
## ㅓ서다 생하다         ㅣ 듯         ㅣ 뷰       ㅣ 짜다     ㅣ 치키다 
##             1             1             1             1             1 
##       ㅣ 혜자 
##             1
```

</br>
사전 제작을 위해 bigramList를 txt파일로 내보냅니다.


```r
write.table(x = names(bigramList),
            quote = FALSE,
            file = './data/yogiyo_for_dic.txt',
            row.names = FALSE,
            col.names = FALSE)
```

</br>
노가다(?)로 '가성비'에 반대되는 의미를 갖는 것만 남깁니다.


```r
neg_dic <- readLines(file('./data/yogiyo_neg_dic.txt'))
```

</br>
`가성비 반대 사전`을 출력해봅니다.


```r
head(neg_dic)
```

```
## [1] "양 적다"   "밥 적다"   "많다 않다" "적다 느낌" "적다 주다" "적다 양"
```

```r
tail(neg_dic)
```

```
## [1] "서비스 작다" "세트 작다"   "소세지 작다" "솔 작다"     "시양 작다"  
## [6] "아구 작다"
```

</br>
`가성비 반대 사전`에 있는 bigram을 포함하고 있으면 가성비가 없다고 평가한 리뷰로 간주했습니다.


```r
total_neg_coef <- data.frame()
for (i in 1:length(total_parsed)) {
  
  # 형태소 중 '가성비 반대 사전'에 있는 bigram갯수 할당
  neg_coef <- total_parsed[i] %>% 
    str_detect(pattern = neg_dic) %>% 
    sum()
  
  total_neg_coef <- rbind(total_neg_coef, neg_coef, stringsAsFactors = FALSE)
  
}

total_cost_ef$cost_efficiency <- ifelse(total_neg_coef == 0, 1, 0) %>% as.vector()
```

</br>
가성비 없다고 한 리뷰를 일부 출력해 봅니다.

```r
total_cost_ef %>% 
  filter(cost_efficiency == 0) %>% 
  select(comment) %>% 
  sample_n(5)
```

```
##                                                                                                                                                                                                                                                                                       comment
## 231 배달초밥치고.....진짜ㅡㅡ굿원래초밥많이못먹는데도시킨거다먹고적은양이지만8p회도굿이고..뭐전체적으로나쁜거하나도없음배달도빨랐고음식도괜찮고그리고짠거싫어하시는분들은무침회에초장조금만친구는다뿌려서다먹었는데저는회만빼먹고다남겼어요ㅠㅠ배만안불렀으면밥넣고회덮밥처럼먹을텐데무튼굿굿
## 129                                                                                                                                                                                  쟁반짜장면양이좀많이작고배달도1시간20분지나서겨우왔습니다..그래서기분이많이좋진않았는데맛도평범했구요...
## 192                                                                                                                                                  맛있어요낙지양이너무적은게좀아쉬워요~!!국물양에비해소면이너무많아요.아구찜자주시켜먹는데아구찜양에비하먄낙지볶음은좀많이적은느낌이에요..
## 210                                                삼겹살냄새진동싫어서주문해봤는데너무맛있네요혼자먹기에는양이너무많고같이온김치찌개맛있었구요밑반찬도좋았어요상추양은솔직히너무작았는데담에는쌈만추가하면좋을꺼같아요속는셈치고시켜본건데너무깔끔하고좋아요담에도또주문할꺼같아요맛있어요^^
## 269                                                                                                                                                                                                                                볶음밥에기름이너무많아요양도적었구요김치우동은달아요국물이
```

</br>
`가성비 순`으로 정렬하기 위해 해당 음식점 리뷰 중 가성비가 좋다고 한 리뷰 갯수 파악합니다.


```r
num_cost_ef <- total_cost_ef %>% 
  group_by(id) %>% 
  summarise(num_cost_ef = sum(cost_efficiency)) 

total <- left_join(x = store_info,
                   y = num_cost_ef,
                   by = 'id')
```


```r
str(total)
```

```
## 'data.frame':	101 obs. of  7 variables:
##  $ category        : chr  "치킨" "치킨" "치킨" "치킨" ...
##  $ id              : int  225071 241433 230513 234960 26338 236657 257772 10962 246043 259782 ...
##  $ name            : chr  "BHC-신촌점" "BHC-신수서강대점" "범벅치킨-신촌점" "롯데리아-신촌로터리점" ...
##  $ delivery_fee    : int  2000 2000 0 0 1000 2000 0 0 0 2000 ...
##  $ min_order_amount: int  14000 15000 15000 11000 19000 15000 10000 14000 11000 14000 ...
##  $ review_count    : int  2093 1862 1764 1413 1263 1226 1224 1210 1128 1104 ...
##  $ num_cost_ef     : num  109 118 204 28 106 51 198 141 36 55 ...
```

</br>
가성비 리뷰 개수를 전체 리뷰 개수로 나누어 `가성비 리뷰 비율` 계산합니다.


```r
total$ratio_cost_ef <-total$num_cost_ef / total$review_count
```

</br>
`가싱비 리뷰 비율 높은 순`으로 출력해 봅니다.


```
## ## 연세대 인근 가성비 좋은 음식점 상위 10곳 입니다 ##
##  1 : 동대문엽기떡볶이-광흥창점 
##  2 : 족발의품격-신촌본점 
##  3 : The피그맛나면돼지-마포점 
##  4 : 망원꽃돼지-망원본점 
##  5 : 웁스떡볶이-신촌점 
##  6 : 타베타이-태국음식전문점 
##  7 : 홍대미남보쌈족발-서교점 
##  8 : 정성이가득찬집밥-이대점 
##  9 : 홍대칼국수와족발-홍대본점 
##  10: 혼밥혼술-신촌점
```

</br>
한식 음식점 `가성비 높은 순`으로 출력합니다.


```
## ## 연세대 인근 가성비 좋은 음식점(한식) 상위 10곳 입니다 ##
##  1 : 동대문엽기떡볶이-광흥창점 
##  2 : The피그맛나면돼지-마포점 
##  3 : 혼밥혼술-신촌점 
##  4 : 쫄면주는삼겹본능-신촌점 
##  5 : 24시뼈다귀해장국-강북 
##  6 : 배달돼지-신촌본점 
##  7 : 지붕위의닭 
##  8 : 미스n불닭발-홍대본점 
##  9 : 오떡순한식도시락-이대신촌점 
##  10: 원조안동찜닭,닭도리탕
```

</br>

***

</br>

> #### **3. 한계점**

1) 우선 가성비를 양이 많다는 것으로 단순하게 정의했다. 실제로 가성비를 좀 더 명확하게 정의 할 필요가 있다.
2) 배달비나 최소주문금액 등 가격에 미치는 다른 요소는 고려하지 않았다.
3) 가성비 관련 사전을 생성할 때도 부정, 중립, 긍정과 같이 총 3가지 정도의 사전을 제작해 감성 분석을 한다면, 긍정적 혹은 부정적 입장인지 파악할 때 더 정확했을 것이다.

</br>
</br>
</br>
</br>
</br>
