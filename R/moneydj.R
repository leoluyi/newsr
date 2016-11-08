getPageUrlMoneyDJ <- function(){
  wantURL <- sprintf('http://www.moneydj.com/KMDJ/News/NewsRealList.aspx?index1=%s&a=MB00',1)
  res <- GET(wantURL, encoding='utf8')
  res2 <- content(res, encoding='utf8')
  maxPage <- cssApply(res2, 'table.paging3 > tr > td> a', cssLink)
  maxPage <- str_replace(str_extract(maxPage[length(maxPage)],'[0-9]+&a=MB00'),'&a=MB00', '')
  maxpage <- as.numeric(maxPage)  
  wantPages <- sprintf('http://www.moneydj.com/KMDJ/News/NewsRealList.aspx?index1=%s&a=MB00', 1:maxPage)
  return(wantPages)
}

getUrlListMoneyDJ <- function(URL, sleepTime=0 ){
  Sys.sleep(sleepTime)
  res <- GET(URL)
  res2 <- content(res, encoding='utf8')
  Xpath <- "//*[@id='ctl00_ctl00_MainContent_Contents_sl_gvList']/tr/td/a"
  UrlList <- xpathSApply(res2, Xpath, xmlAttrs) # xmlAttrs
  UrlList <-  UrlList[rownames(UrlList)=='href',]
  getNewUrL <- function(URL){
    wantURL <- (sprintf('http://www.moneydj.com%s',URL))
  }
  UrlList <- unlist(lapply(UrlList, getNewUrL))
  return(UrlList)
}


getNewsMoneyDJ <- function(URL, sleepTime=0){
  Sys.sleep(sleepTime)
  res <- content(GET(URL), encoding='utf8')
  newsTitle <- cssApply(res, "#hmviewerbox > div.viewer_tl", cssCharacter) 
  newsText <- cssApply(res, "#highlight>article", cssCharacter)
  newsCate <- cssApply(res, "#hmviewerbox > div.viewer_ft", cssCharacter)
  newsCate <- str_replace(str_replace(newsCate,'分類主題：',''),'(\r\n)','')
  newsAuthor <-str_replace(str_replace(str_extract(newsText,'記者(\r|\n| ).+(\r|\n| )報導'),'記者 ',''),' 報導','')
  newsDate <- cssApply(res, "#ctl00_ctl00_MainContent_Contents_lbDate", cssCharacter)
  newsTime <- str_extract(newsDate,'[0-9]{2}:[0-9]{2}')
  newsTime <- str_replace_all(newsTime, ':', '')
  newsDate <-str_extract(newsDate,'[0-9]{4}/[0-9]{0,2}/[0-9]{0,2}')
  newsDate <- str_replace_all(newsDate, '/', '')
  newsClick <- cssApply(res, "span.viewcount", cssCharacter)
  result <- try(data.frame(newsWebsite='MoneyDJ', newsCate=newsCate, newsAuthor=newsAuthor, 
                           newsDate=newsDate, newsTime=newsTime, 
                           newsTitle=newsTitle, newsText=newsText,
                           newsClick=newsClick, newsShare=NA,
                           href=URL, createTime=format(Sys.time(), '%Y%m%d%H%M%S'),
                           stringsAsFactors=FALSE ), silent = TRUE)
  if( inherits(result, 'try-error')){
    result <- data.frame(newsWebsite='MoneyDJ', newsCate=NA, newsAuthor=NA, newsDate=NA, newsTime=NA, 
                         newsTitle=NA, newsText=NA,
                         newsClick=NA, newsShare=NA,
                         href=URL, createTime=format(Sys.time(), '%Y%m%d%H%M%S'),
                         stringsAsFactors=FALSE )
  }
  return(result)
}

getAllNewsMoneyDJ <- function(sleepBetween = 5, sleepTime = 1, printSts=TRUE){
  PageUrl <- getPageUrlMoneyDJ()
  UrlList <- lapply(PageUrl, getUrlListMoneyDJ)
  UrlList <- unlist(UrlList)
  newsList <- list()
  for(i in 1:length(UrlList)){
    newsList[[i]] <- getNewsMoneyDJ(UrlList[i])
    if ( (i %% sleepBetween) ==0) Sys.sleep(sleepTime)
    if ( printSts&(i%%100==0)) cat('MoneyDJ ', i, 'is ok', '\n')
  }
  MoneyDJnews <- do.call(rbind, newsList)
}