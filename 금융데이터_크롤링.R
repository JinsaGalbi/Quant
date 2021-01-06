# 종목티커 가져오기

## 네이버 금융에서 코스피 종목번호 가져오기
library(dplyr)
library(httr)
library(rvest)

### 1) 마지막막 페이지 번호 가져오기
i=0  # 코스피 or 코스닥
j=1
url = paste0('https://finance.naver.com/sise/sise_market_sum.nhn?sosok=',i,'&page=',j)
down_table = GET(url)

navi.final = down_table %>% 
  read_html(encoding = 'EUC-KR') %>% 
  html_nodes('.pgRR') %>% 
  html_nodes('a') %>% 
  html_attr('href') %>% 
  strsplit('=') %>% 
  unlist() %>% 
  tail(1) %>% 
  as.numeric()  # 마지막 페이지번호


### 2) 테이블 읽기
Sys.setlocale('LC_ALL', 'English')

table = read_html(down_table, encoding='EUC-KR') %>% 
  html_table(fill=T) %>% 
  .[[2]]


Sys.setlocale('LC_ALL', 'Korean')

print(head(table,10))

table %<>%
  select(-토론실) %>% 
  na.omit()

### 3) 종목번호 가져오기
i=0  # 코스피 or 코스닥
j=1

data = c()
for(i in 0:1){
  for(j in 1:navi.final){
    url = paste0('https://finance.naver.com/sise/sise_market_sum.nhn?sosok=',i,'&page=',j)
    temp = GET(url) %>% 
      read_html(encoding = 'EUC-KR') %>% 
      html_nodes('tbody') %>% 
      html_nodes('td') %>% 
      html_nodes('a') %>% 
      html_attr('href') %>% 
      strsplit('=') %>% 
      unlist() %>% 
      tail(1) %>% 
      unique()
    data = c(data,temp)
  }
}

data

i=j=1
url = paste0('https://finance.naver.com/sise/sise_market_sum.nhn?sosok=',i,'&page=',j)
temp = GET(url) %>% 
  read_html(encoding = 'EUC-KR') %>% 
  html_nodes('tbody') %>% 
  html_nodes('td') %>% 
  html_nodes('a') %>% 
  html_attr('href')

?substr
