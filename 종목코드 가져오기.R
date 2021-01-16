# 네이버 금융에서 최신정보 가져오기
library(dplyr)
library(httr)
library(rvest)
library(magrittr)
library(stringr)

# 마지막 페이지 번호 가져오기
get_pagenumber = function(market=0){# market = 0(KOSPI), 1(KOSDAQ)
  if (market %in% c('kospi','KOSPI','코스피')) {market = 0}
  if (market %in% c('kosdaq','KOSDAQ','코스닥')) {market = 1}
  
  url = paste0('https://finance.naver.com/sise/sise_market_sum.nhn?sosok=',
               market,'&page=1')
  down_table = GET(url)
  navi.final = down_table %>% 
    read_html(encoding = 'EUC-KR') %>% 
    html_nodes('.pgRR') %>% 
    html_nodes('a') %>% 
    html_attr('href') %>% 
    strsplit('=') %>% 
    unlist() %>% 
    tail(1) %>% 
    as.numeric()
  
  return(navi.final)
}

# 오늘의 주식 테이블 가져오기
get_todaystock = function(market=0){# market = 0(KOSPI), 1(KOSDAQ)
  pg_num = get_pagenumber(market)
  ticker = list()
  for(p in 1:pg_num){
    url = paste0('https://finance.naver.com/sise/sise_market_sum.nhn?sosok=',
                 market,'&page=',p)
    down_table = GET(url)
    
    # 네이버 증권 테이블 읽기
    Sys.setlocale('LC_ALL', 'English')
    table = read_html(down_table, encoding = 'EUC-KR') %>%  
      html_table(fill = T) %>% 
      .[[2]]
    table[c(1,ncol(table))] = NULL
    table %<>% na.omit()
    Sys.setlocale('LC_ALL', 'Korean')
    
    # 종목코드 읽기
    symbol = GET(url) %>% 
      read_html(encoding = 'EUC-KR') %>% 
      html_nodes('tbody') %>% 
      html_nodes('td') %>% 
      html_nodes('a') %>% 
      html_attr('href')
    
    symbol = sapply(symbol, function(x){
      substr(x, nchar(x)-5, nchar(x))
    })
    symbol = unique(symbol)
    
    # 테이블에 종목코드 병합
    table$'종목코드' = symbol
    rownames(table) = NULL
    ticker[[p]] = table
  }
  ticker = do.call(rbind,ticker)
  return(ticker)
  
  Sys.sleep(0.1)
} 

stock = get_todaystock()
head(stock)
stock %>% glimpse()

stock %<>%
  mutate_if(function(x) is.character(x), funs(str_replace_all(.,',|%',''))) %>% 
  mutate_at(vars(-종목명,-종목코드), funs(as.numeric(.))) %>% 
  mutate_at(vars(등락률, 외국인비율), funs(./100)) %>% 
  arrange(desc(시가총액))

stock %>% 
  filter(str_detect(종목명,''))

write.csv(stock,'c:/Users/USER/Desktop/Quant study/file/종목번호.csv',row.names = F)
