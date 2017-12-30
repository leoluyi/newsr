#' CTEE 工商時報 (工商e報)
#'
#' @param dateWant \code{"yyyymmdd"}.
#'
#' @return
#' data.table object with columns:
#'
#' @examples
#' getAllNewsUdn("20161001")
#'
#' @export
getAllNewsCT <- function(dateWant, sleepBetween = 5, sleepTime = 1, printSts=TRUE){
  PageUrl<- getPageUrlCT(dateWant)
  UrlList <- lapply(PageUrl, getUrlListCT)
  UrlList <- do.call(rbind,UrlList)
  newsList <- list()
  for(i in 1:length(UrlList$URL)){
    newsList[[i]] <- getNewsCT(UrlList$URL[i])
    if ( (i %% sleepBetween) ==0) Sys.sleep(sleepTime)
    if ( printSts&(i%%100==0)) cat('工商時報 ', i,'is ok', '\n')
  }
  do.call(rbind, newsList)
}


getPageUrlCT <- function(dateWant){
  wantURL <- sprintf('http://www.chinatimes.com/history-by-date/%s-%s-%s-2602?page=%s', substr(dateWant,1,4), substr(dateWant,5,6), substr(dateWant,7,8), 1)
  res <- GET(wantURL, encoding='utf8')
  res2 <- content(res, encoding='utf8')
  (maxPage<- xpathSApply(res2, '//div[@class="pagination clear-fix"]/ul/li/a', xmlAttrs))

  maxPage <- str_replace(str_extract(maxPage[length(maxPage)],'\\?page=[0-9]+$'),'\\?page=', '')

  wantPages<- sapply(1:maxPage,
                     function(wantPage) sprintf('http://www.chinatimes.com/history-by-date/%s-%s-%s-2602?page=%s', substr(dateWant,1,4), substr(dateWant,5,6), substr(dateWant,7,8), wantPage)
  )
  return(wantPages)
}

getUrlListCT <- function(URL, sleepTime=0){
  Sys.sleep(sleepTime)
  res <- GET(URL)
  res2 <- content(res, encoding='utf8')
  UrlList <- xpathSApply(res2, '//div[@class="listRight"]/ul/li/h2/a', xmlAttrs)
  UrlList <-  UrlList[rownames(UrlList)=='href',]
  UrlListCate <- xpathSApply(res2, '//div[@class="listRight"]/ul/li/div[@class="kindOf"]/a', xmlValue)
  UrlListCate <- str_replace_all(UrlListCate, '[:space:]', '')
  getNewUrL <- function(URL){
    wantURL <- URLencode(sprintf('http://www.chinatimes.com%s',URL))
  }
  UrlList2 <- unlist(lapply(UrlList, getNewUrL))
  return(data.frame(URL=UrlList2,cate=UrlListCate, stringsAsFactors = FALSE))
}


getNewsCT <- function(URL, sleepTime=0){
  Sys.sleep(sleepTime)
  res <- content(GET(URL), encoding='utf8')
  newsCate <- xpathSApply(res, '//article[@class="clear-fix"]/ul/li/h6', xmlValue)
  newsCate <- newsCate[length(newsCate)]
  newsCate <- str_replace_all(newsCate, '[:space:]', '')
  newsTitle <- xpathSApply(res, '//article[@class="clear-fix"]/header/h1', xmlValue)
  newsTitle <- str_replace_all(newsTitle, '[:space:]', '')
  newsText <- xpathSApply(res, '//article[@class="clear-fix"]/article[@class="clear-fix"]/p', xmlValue)
  newsText <- paste(newsText, collapse = '\n')
  newsAuthor <- xpathSApply(res, '//div[@class="reporter"]/div', xmlValue)
  newsAuthor <- str_replace_all(newsAuthor, '整理|記者|／.+|/.+|.+基金經理人|.+部門|主管', '')
  newsAuthor <- paste(newsAuthor, collapse = '&')
  newsDate <- xpathSApply(res, '//div[@class="reporter"]/time', xmlValue)
  newsDate <- str_replace_all(newsDate, '[:space:]', '')
  newsTime <- str_replace_all(str_extract(newsDate, '[0-9]{2}:[0-9]{2}'), ':', '')
  newsDate <- str_replace_all(str_extract(newsDate, '[0-9]{4}年[0-9]{2}月[0-9]{2}日'), '年|月|日', '')
  newsClick <- xpathSApply(res, '//div[@class="article_star clear-fix"]/div[@class="art_click clear-fix"]/span[@class="num"]', xmlValue)[1]
  newsClick <- ifelse(is.list(newsClick),0,newsClick)
#   URLQry <- str_replace_all(str_replace_all(URL, '/', '%2F'), ':' ,'%3A')
#   URLnewsShare1 <- paste0('http://graph.facebook.com/fql?q=SELECT%20share_count,%20like_count,%20comment_count,%20total_count,%20comments_fbid,%20click_count%20FROM%20link_stat%20WHERE%20url=%22',URLQry,'%22','&callback=_ate.cbs.rcb_httpwwwchinatimescom0')
#   newsShare1Content <- str_extract(rawToChar(GET(URLnewsShare1)$content), '\\{.+\\}')
#   newsShare1Content <- rjson::fromJSON(newsShare1Content)
#   newsShare1 <- newsShare1Content$data[[1]]$total_count
#   newsShare1 <- ifelse(is.null(newsShare1),0,newsShare1)
#
#   URLnewsShare2 <- paste(c('https://cdn.api.twitter.com/1/urls/count.json?url=',URLQry,'&callback=_ate.cbs.rcb_httpwwwchinatimescom0'), collapse = '')
#   newsShare2Content <- str_extract(rawToChar(GET(URLnewsShare2)$content), '\\{.+\\}')
#   newsShare2Content <- rjson::fromJSON(newsShare2Content)
#   newsShare2 <- newsShare2Content$count
#   newsShare2 <- ifelse(is.null(newsShare2),0,newsShare2)
#
#   URLnewsShare3 <- paste0('http://api-public.addthis.com/url/shares.json?url=',URLQry, '&callback=_ate.cbs.rcb_httpwwwchinatimescom0')
#   newsShare3Content <- rawToChar(GET(URLnewsShare3)$content)
#   newsShare3Content <- str_extract(newsShare3Content, '\\{.+\\}')
#   newsShare3Content <- rjson::fromJSON(newsShare3Content)
#   newsShare3 <- newsShare3Content$shares
#   newsShare3 <- ifelse(is.null(newsShare3),0,newsShare3)

  result <- try(data.frame(newsWebsite='工商時報', newsCate=newsCate, newsAuthor=newsAuthor,
                           newsDate=newsDate, newsTime=newsTime,
                           newsTitle=newsTitle, newsText=newsText,
                           newsClick=newsClick, newsShare=NA,
                           href=URL, createTime=format(Sys.time(), '%Y%m%d%H%M%S'),
                           stringsAsFactors=FALSE ), silent = TRUE)
  if( inherits(result, 'try-error')){
    result <- data.frame(newsWebsite='工商時報', newsCate=NA, newsAuthor=NA, newsDate=NA, newsTime=NA,
                         newsTitle=NA, newsText=NA,
                         newsClick=NA, newsShare=NA,
                         href=URL, createTime=format(Sys.time(), '%Y%m%d%H%M%S'),
                         stringsAsFactors=FALSE )
  }
  return(result)
}

