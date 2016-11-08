
getAllCatecnYES <- function(){
  res <- GET('http://news.cnyes.com/express/list.shtml?ga=nav', user_agent='Mozilla/5.0')
  res <- content(res,encoding='utf8')
  cateCN <-xpathSApply(res, '//li[@class="one-hover"]/a', xmlValue)
  cateURL <-xpathSApply(res, '//li[@class="one-hover"]/a', xmlAttrs)["href",]
  cateURL <- str_replace_all(str_extract(cateURL, '/([a-zA-z]|_)+/'), '/', '')
  CateList <- as.data.frame(cbind(cateURL, cateCN), stringsAsFactors=FALSE)
  CateList <- CateList[!(CateList$cateCN %in% c('焦點', '總覽', '排行', '生活', '研究報告')),]
  CateList[CateList$cateCN=='頭條',1]='headline'
  CateList
}

# dateStart <- dateWant
# dateStart <- '20150703'
# dateEnd <- '20150703'
# dateStart <- gsub('-','',as.Date(Sys.time()))
# dateEnd <- gsub('-','',as.Date(Sys.time()))
# cate <- 'headline'

getPageUrlcnYES <- function(cate, dateStart, dateEnd=dateStart){
  wantURL <- sprintf('http://news.cnyes.com/%s/sonews_%s%s_%d.htm', cate, dateStart, dateEnd, 1)
  res <- GET(wantURL, user_agent='Mozilla/5.0')
  res2 <- content(res, encoding='utf8')
  (maxPage<- xpathSApply(res2, '//*[@id="listArea"]/ul[1]/li/text()', xmlValue))
  maxPage <- maxPage[str_detect(maxPage, '共[0-9]+頁')]
  maxPage <- str_replace_all(str_extract(maxPage,'共[0-9]+頁'), '共|頁','') 
  if(maxPage>1){  
    Pages <- 1:maxPage 
  }else{
    Pages <-1
  }
  wantPages<- sprintf('http://news.cnyes.com/%s/sonews_%s%s_%d.htm', cate, dateStart, dateEnd, Pages)
  return(wantPages)
}


# (PageUrl<- getPageUrlcnYES(cate, dateStart))
# URL <- PageUrl[i]


getUrlListcnYES <- function(URL, sleepTime = 0){
  Sys.sleep(sleepTime)
  res <- GET(URL, user_agent='Mozilla/5.0')
  res2 <- content(res, encoding='utf8')
  UrlList <- xpathSApply(res2, '//*[@id="listArea"]/ul/li/a', xmlAttrs)
  UrlList <- lapply(UrlList, toUTF8)
  UrlList2 <- UrlList[sapply(UrlList,length)==1]
  UrlList2 <- lapply(UrlList2, function(URL) URLencode(sprintf('http://news.cnyes.com%s',URL) ))
  return(UrlList2)
}


# UrlList <- getUrlListcnYES(PageUrl[i])
# UrlList[1:2]

# URL <- UrlList2[[32]]
# URL <- 'http://news.cnyes.com/20150702/%e5%85%a8%e7%90%83%e5%a0%b1%e6%91%98-%e4%ba%9e%e6%b4%b2%e8%8f%af%e7%88%be%e8%a1%97%e6%97%a5%e5%a0%b1%e9%87%8d%e8%a6%81%e6%96%b0%e8%81%9e-092519440386210.shtml?c=headline'


getNewscnYES <- function(URL, sleepTime = 0){
  Sys.sleep(sleepTime)
  res <- try(content(GET(URL, user_agent='Mozilla/5.0'), encoding='utf8'), silent = TRUE)
  if( inherits(res, 'try-error')){
    result <- data.frame(newsWebsite='鉅亨網', newsCate=NA, newsAuthor=NA, 
                         newsDate=NA, 
                         newsTime=NA, 
                         newsTitle=NA, 
                         newsText=NA,
                         newsClick=NA, 
                         newsShare=NA,
                         href=URL, createTime=Sys.time(),
                         stringsAsFactors=FALSE )
    return(result)
  }
  newsTitle <- xpathSApply(res, '//*[@id="container"]/div[4]/div[1]/div[3]/h1', xmlValue)
  newsCate <- xpathSApply(res, '//*[@id="container"]/div[4]/div[1]/div[1]/span/a[2]', xmlValue)
  newsText <- xpathSApply(res, '//*[@id="newsText"]', xmlValue)
  info <- xpathSApply(res, '//div[@class="info"]', xmlValue)
  if( is.list(info)) info <-''
  newsDate <- str_replace_all(str_extract(info, '[0-9]{4}-[0-9]{2}-[0-9]{2}'), '-', '')
  newsTime <- str_replace_all(str_extract(info, '[0-9]{2}:[0-9]{2}'), ':', '')
  newsAuthor <- str_extract(info, '(鉅亨網編譯.+　　)|(鉅亨網.+　　)|(鉅亨網新聞中心)')
  newsAuthor <- str_replace_all(newsAuthor, '(^鉅亨網)|編譯|　　|外電報導| |記者|綜合外電|台北|綜合報導', '')
  result<- try(data.frame(newsWebsite='鉅亨網', newsCate=newsCate, newsAuthor=newsAuthor, newsDate=newsDate, newsTime=newsTime, 
                          newsTitle=newsTitle, newsText=newsText,
                          newsClick=NA, newsShare=NA,
                          href=URL, createTime=format(Sys.time(), '%Y%m%d%H%M%S'),
                          stringsAsFactors=FALSE ), silent = TRUE)
  if( inherits(result, 'try-error')){
    result <- data.frame(newsWebsite='鉅亨網', newsCate=NA, newsAuthor=NA, newsDate=NA, newsTime=NA, 
                         newsTitle=NA, newsText=NA,
                         newsClick=NA, newsShare=NA,
                         href=URL, createTime=format(Sys.time(), '%Y%m%d%H%M%S'),
                         stringsAsFactors=FALSE )
  }
  return(result)
}


# dateWant <- gsub('-','',as.Date(Sys.time()))

getAllNewscnYES <- function(dateWant, sleepBetween = 5, sleepTime = 1, printSts=TRUE){
  allcate <- getAllCatecnYES()
  UrlList <- list()
  for(i in 1:nrow(allcate)){
    PageUrl<- getPageUrlcnYES(allcate$cateURL[i], dateWant)
    UrlListTp <- lapply(PageUrl, getUrlListcnYES)
    UrlListTp <- lapply(UrlListTp, unlist)
    UrlList[[i]] <- data.frame(href=unlist(UrlListTp), newsCate=allcate$cateCN[i], stringsAsFactors=FALSE)
    if ( (i %% sleepBetween) ==0) Sys.sleep(sleepTime)
  }
  UrlList <- do.call(rbind, UrlList)
  newsList <- list()
  for(i in 1:nrow(UrlList)){
    newsList[[i]] <- getNewscnYES(UrlList$href[i])
    if ( (i %% sleepBetween) ==0) Sys.sleep(sleepTime)
    if ( printSts&(i%%100==0)) cat('鉅亨網 ', i,'is ok', '\n')
  }
  cnYESnews <- do.call(rbind, newsList)  
}
