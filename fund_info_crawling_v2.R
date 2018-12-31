library(jsonlite)
library(dplyr)
library(ggplot2)

# 펀드정보 크롤링 From Fundsupermarket
url <- 'http://www.fundsupermarket.co.kr/fmm/FMM1010301/selectFundList.do?fsType01=B1&fstChk02=BB&fstChk03=&fstChk04=T&fstChk05=T&fstChk06=T&fstChk07=T&rlzRt=&type01_con1=-9999&type01_con2=9999&type02_con1=0&type02_con2=-999&type03_con1=0&type03_con2=20&saleRate=ON&afrcvRate=ON&gradeGbn=1&zeroin=T&mstar=&kfr=&fng=&type04_con1=0&type04_con2=100&type05_con1=0&type05_con2=100&type06_con1=0&type06_con2=100&sClass=N&eClass=N&fundName2=&pageNo=1&pageCnt=2000&openAbleYn=Y'
url %>%
  readLines(encoding = "UTF-8") %>%
  fromJSON() -> tmp
tmp$json$resList -> fund_info
rm(tmp, url)
fund_info %>%
  arrange(desc(RLZ_RT1), desc(RLZ_RT3)) -> fund_info
fund_info %>%
  head()

# 펀드별 기준가 크롤링 From Fundsupermarket
url <- "http://m.fundsupermarket.co.kr/fmm/FMM1030101/selectDayNavResult.do?fpCode="
get_nav <- function(i){
  nav_url <- paste0(url, fund_info$FP_CODE[i])
  nav_url %>%
    read_json -> tmp
  tmp$json$dayNavInfo %>%
    toJSON() %>%
    fromJSON %>%
    select(STD_DATE, NAV) -> nav
  nav$STD_DATE %>%
    as.character %>%
    as.Date(format="%Y%m%d") -> nav$STD_DATE
  nav$NAV %>%
    as.numeric -> nav$NAV
  cat("\n",i)
  return(nav)
}
fund_nav<- lapply(1:1914, get_nav)
names(fund_nav) <- fund_info$FP_CODE

##################################################################

# 펀드 유형별 subset을 만들어서 일반펀드만 분석해보자
# (연금펀드와 일반펀드는 클래스만 다르며 운용 측면에서는 차이가 없다고 할 수 있다.)
for(i in fund_info$INVTRST_DETAIL_CODE %>% unique){
  print(i)
  fund_info %>%
    filter(INVTRST_DETAIL_CODE==i) %>%
    head() %>%
    select(FP_KRN_NAME) -> tmp_fund_name
  print(tmp_fund_name)
}
# 61=일반클래스, 77=연금 클래스, 84=코스닥벤처
fund_info %>%
  filter(INVTRST_DETAIL_CODE==77) -> fund_info_p
fund_info %>%
  filter(INVTRST_DETAIL_CODE!=77) -> fund_info_np


# 유형별로 최근 1개월간 가장 높은 수익률을 기록한 펀드의 차트를 그려보자
fund_info_np %>%
  select(TYPE_NAME) %>%
  unique %>%
  t %>%
  as.vector %>%
  sort -> fund_type


top_fund_chart <- function(type="ALL"){
  if(type=="ALL"){
    fund_subset <- fund_info_np
  }else{
    fund_info_np %>%
      filter(TYPE_NAME==type) -> fund_subset
  }
  fund_subset$FP_KRN_NAME[1] -> top_fund_name
  fund_subset$FP_CODE[1] -> top_fund_ind
  fund_nav[[top_fund_ind]] -> top_fund_nav
  ggplot(top_fund_nav, aes(x=top_fund_nav$STD_DATE, y=top_fund_nav$NAV)) +
    geom_line() +
    labs(title=paste0("유형 = ",type,"\n", "펀드명 = ",top_fund_name),
         x = "일자",
         y = "기준가격") +
    theme(plot.title = element_text(face='bold', hjust = 0.5, size=15, color = 'darkblue'))
}

top_fund_chart("러시아주식")
