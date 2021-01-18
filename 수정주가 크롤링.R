# 네이버 금융에서 주가 크롤링
get_stockprice = function(start_time,end_time=Sys.Date(), type = 0){ # type = 0(kospi), 1(kosdaq)
  if (type ==0) stock = read.csv('C:/Users/USER/Desktop/Quant study/code/Quant/거래소/코스피상장현황_20210118.csv')
  else stock = read.csv('C:/Users/USER/Desktop/Quant study/code/Quant/거래소/코스닥상장현황_20210118.csv')
  stock %<>% 
    filter(주식종류 == '보통주') %>% 
    select(단축코드, 한글.종목약명, 상장일) %>% 
    rename('종목코드'=단축코드,'종목명'=한글.종목약명) %>% 
    mutate(상장일 = ymd(상장일)) %>% 
    filter(!str_detect(종목명,'스팩')) %>% 
    arrange()
  
  stock_price = data.frame(날짜 = ymd(Sys.Date()))
  
  for(i in 1:nrow(stock)){
    code = stock[i,1]
    url = paste0('https://fchart.stock.naver.com/sise.nhn?symbol=',code,'&requestType=1&startTime=',start_time,'&endTime=',gsub('-','',end_time),'&timeframe=day')
    ref = 'https://finance.naver.com/'
    data = GET(url, header=add_headers(referer=ref))
    data_html = read_html(data, encoding='EUC-KR') %>% 
      html_nodes('item') %>% 
      html_attr('data')
    
    price = data_html %>% 
      str_split('[|]') %>% 
      do.call(rbind,.) %>% 
      data.frame(.,stringsAsFactors = F) %>% 
      .[c(1,5)]
    colnames(price) = c('날짜', as.character(stock[i,2]))
    
    price %<>%
      mutate(날짜 = ymd(날짜))
    
    stock_price %<>% full_join(price, by='날짜')
  }
  stock_price %<>%
    mutate_at(vars(-날짜), as.numeric) %>% 
    arrange(날짜)
  return(stock_price)
}

# 수정주가 최신화
past_stock = read.csv('C:/Users/USER/Desktop/Quant study/code/Quant/수정주가/수정주가_20210118.csv') %>% 
  mutate(날짜 = ymd(날짜)) %>% 
  arrange(날짜)

past_day = past_stock %>%
  select(날짜) %>%
  tail(1) %>%
  .[[1]] %>% 
  str_replace_all('-','')

new_stock = get_stockprice(start_time = past_day, end_time = Sys.Date())

stock_csv = past_stock %>%
  bind_rows(new_stock)

write.csv(stock_csv, paste0('C:/Users/USER/Desktop/Quant study/code/Quant/수정주가/수정주가_',gsub('-','',Sys.Date()),'.csv'), row.names=F)
