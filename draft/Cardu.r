getAllCateCardu <- function(){
  wantURL <- 'http://www.cardu.com.tw/news/news.php'
  res <- content(GET(wantURL),encoding='utf8')
  cateCardu <-xpathSApply(res, '//*[@id="con_navi"]/ul/li/a', xmlValue)
  cateURL <-xpathSApply(res, '//*[@id="con_navi"]/ul/li/a', xmlAttrs)
  cateURL <- cateURL[rownames(cateURL)=='href',]
  cateURL <- sprintf('http://www.cardu.com.tw/%s',cateURL)
  CateList <- as.data.frame(cbind(cateURL, cateCardu), stringsAsFactors=FALSE)
}

getPageUrlCardu <- function(URL){
  res <- GET(URL)
  res2 <- content(res, encoding='utf8')
  Pages<- xpathSApply(res2, '//*[@id="main_content"]/div/form/span[1]', xmlValue)
  Pages <- str_replace_all(str_extract(Pages,'\\(.+\\)'), '\\(|\\)|共|頁', '')
  wantPages <- sprintf(paste0(URL, '&PageNo=%s'), 1:Pages)
  return(wantPages)
}

getUrlListCardu <- function(URL, sleepTime = 0){
  Sys.sleep(sleepTime)
  res <- GET(URL, encoding='utf8')
  res2 <- content(res, encoding='utf8')
  UrlList <- xpathSApply(res2, '//*[@id="lists"]/ul/li/h2/a', xmlAttrs)
  UrlList <- UrlList[rownames(UrlList)=='href',]
  UrlList2 <- sprintf('http://www.cardu.com.tw/%s', UrlList)
  return(UrlList2)
}


getNewsCardu <- function(URL, sleepTime = 0){
  Sys.sleep(sleepTime)
  res <- content(GET(URL), encoding='utf8')
  newsCate <- xpathSApply(res, '//*[@id="building"]', xmlValue)
  newsCate <- str_replace(newsCate,'首頁>新聞>', '')
  newsTitle <- xpathSApply(res, '//*[@id="page"]/h3', xmlValue)
  newsText <- xpathSApply(res, '//*[@id="page_content"]/p', xmlValue)
  newsText <- str_replace_all(newsText, '[:space:]+','')
  newsText <- paste(newsText, collapse= '\n')
  newsAuthor <- xpathSApply(res, '//*[@id="page"]/h5', xmlValue)
  newsDate <- str_replace_all(str_extract(newsAuthor, '[0-9]+/[0-9]+/[0-9]+'), '/', '')
  newsAuthor <- str_extract(newsAuthor, '記.+報導')
  newsAuthor <- str_replace_all(newsAuthor, '記者|報導| ', '')
  result <- try(data.frame(newsWebsite='卡優網', newsCate=newsCate, newsAuthor=newsAuthor,
                           newsDate=newsDate, newsTime=NA,
                           newsTitle=newsTitle, newsText=newsText,
                           newsClick=NA, newsShare=NA,
                           href=URL, createTime=format(Sys.time(), '%Y%m%d%H%M%S'),
                           stringsAsFactors=FALSE ), silent = TRUE)
  if( inherits(result, 'try-error')){
    result <- data.frame(newsWebsite='卡優網', newsCate=NA, newsAuthor=NA, newsDate=NA, newsTime=NA,
                         newsTitle=NA, newsText=NA,
                         newsClick=NA, newsShare=NA,
                         href=URL, createTime=format(Sys.time(), '%Y%m%d%H%M%S'),
                         stringsAsFactors=FALSE )
  }
  return(result)
}



getAllNewsCardu <- function(sleepBetween = 5, sleepTime = 1, printSts=TRUE){
  allCate <- getAllCateCardu()
  PageUrl <- unlist(lapply(allCate$cateURL, getPageUrlCardu))
  UrlList <- unlist(lapply(PageUrl,getUrlListCardu))
  newsList <- list()
  for( i in 1:length(UrlList)){
    newsList[[i]] <- getNewsCardu(UrlList[i])
    if ( (i %% sleepBetween) ==0) Sys.sleep(sleepTime)
    if ( printSts&(i%%100 ==0) ) cat('卡優網 ', i, 'is ok', '\n')
  }
  CarduNews <- do.call(rbind, newsList)
}
