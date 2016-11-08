#' UDN
#'
#' @param keyword Vector of search strings.
#' @param date_from Begin of date of news to get with format \code{"yyyy-mm-dd"}.
#' @param date_to End of date of news to get with format \code{"yyyy-mm-dd"}.
#'
#' @return
#' data.table object with columns:
#'
#' @examples
#' getAllNewsUdn("20161001")
#'
#' @export
getAllNewsUdn <- function(dateWant, sleepBetween = 5, sleepTime = 1,
                          printSts=TRUE) {
  CurDate <- gsub('-','',Sys.Date())
  if (CurDate == dateWant) {
    UdnNews <- getAllNewsUdnCur(sleepBetween = sleepBetween,
                                sleepTime = sleepTime,
                                printSts = printSts)
  } else {
    UdnNews <- getAllNewsUdnHs(dateWant, sleepBetween = sleepBetween, sleepTime = sleepTime, printSts=printSts)
  }
  return(UdnNews)
}

getPageUrlUdnCur <- function(){
  wantURL <- "http://udn.com/news/breaknews/1/99"
  res <- GET(wantURL)
  res2 <- content(res, encoding='utf8')
  cateURL <- html_nodes(res2, '//*[@id="breaknews_head"]/dl/dt/a', xmlAttrs)
  cateURL <- cateURL[rownames(cateURL)=='href',]
  cateName <- html_nodes(res2, '//*[@id="breaknews_head"]/dl/dt/a', xmlValue)
  cateURL <- sprintf('http://udn.com%s', cateURL)
  pageList <- list()
  for(i in 1:length(cateURL)){
    res <- GET(cateURL[i], encoding='utf8')
    res2 <- content(res, encoding='utf8')
    maxPage<- html_nodes(res2, '//*[@id="ranking"]/div/span[@class="total"]', xmlValue)
    maxPage <- str_extract(maxPage,"[0-9]+")
    wantPages<- sprintf(paste0(cateURL[i],'/%s'),  1:maxPage)
    pageList[[i]] <- data.frame(cateName=cateName[i], URL = wantPages, stringsAsFactors = FALSE)
  }
  pageList <- do.call(rbind, pageList)
  return(pageList)
}


getUrlListUdnCur <- function(URL, sleepTime = 0){
  Sys.sleep(sleepTime)
  res <- GET(URL)
  res2 <- content(res, encoding='utf8')
  UrlList <- html_nodes(res2,'//*[@id="breaknews_body"]/dl/dt/a',xmlAttrs)
  UrlList <-  UrlList[rownames(UrlList)=='href',]
  newsClick <- html_nodes(res2, '//*[@id="breaknews_body"]/dl/dt/a/div[@class="info"]/div[2]',xmlValue )
  newsClick <- str_replace_all(newsClick, '(\r|\n| )+', '')
  UrlList <- sprintf('http://udn.com%s', UrlList)
  newsTitle <- html_nodes(res2,'//*[@id="breaknews_body"]/dl/dt/a/h2',xmlValue)
  return(data.frame(newsTitle=newsTitle, URL=UrlList,
                    newsClick=newsClick, stringsAsFactors = FALSE))
}



getPageUrlUdnHs <- function(dateWant, sleepTime = 0){
  # Sys.sleep(sleepTime)
  dateWant = "20161001"

  wantURL <- sprintf('http://udn.com/news/archive/%s/%s/%s/%s',
                     substr(dateWant,1,4),
                     substr(dateWant,5,6),
                     substr(dateWant,7,8), 1)
  res <- GET(wantURL, encoding='utf8')
  res2 <- content(res, encoding='utf8')
  cateURL <- res2 %>%
    html_nodes("#ranking_head a") %>%
    html_attr("href")
  cateName <- res2 %>%
    html_nodes("#ranking_head a") %>%
    html_text()

  pageList <- list()
  for(i in seq_along(cateURL)){
    # i = 1
    res <- GET(cateURL[i], encoding='utf8')
    res2 <- content(res, encoding='utf8')
    maxPage <- res2 %>% html_node(".total") %>% html_text() %>%
      str_extract("[0-9]+")
    wantPages <- sprintf(paste0(cateURL[i],'/%s'),  1:maxPage)
    pageList[[i]] <- data.frame(cateName = cateName[i],
                                URL = wantPages,
                                stringsAsFactors = FALSE)
  }

  pageList <- data.table::rbindlist(pageList)
  return(pageList)
}


getUrlListUdnHs <- function(URL, sleepTime = 0){
  # sleepTime = 0
  Sys.sleep(sleepTime)

  res <- GET(URL)
  res2 <- content(res, encoding='utf8')
  UrlList <- html_nodes(res2,"//*[@id='ranking_table']/tr/td/a", xmlAttrs)
  UrlListCate <- html_nodes(res2, "//*[@id='ranking_table']/tr/td[3]", xmlValue )
  UrlListCate <- str_replace_all(UrlListCate, '(\r|\n| )+', '')
  newsClick <- html_nodes(res2, "//*[@id='ranking_table']/tr/td[5]", xmlValue )
  newsClick <- str_replace_all(newsClick, '(\r|\n| )+', '')
  UrlList2 <- sprintf('http://udn.com%s',UrlList)
  newsTitle <- html_nodes(res2,"//*[@id='ranking_table']/tr/td/a", xmlValue)
  return(data.frame(newsTitle = newsTitle,
                    cate = UrlListCate,
                    URL = UrlList2,
                    newsClick = newsClick,
                    stringsAsFactors = FALSE))
}


# scrape data page
getNewsUdn <- function(dataVector, sleepTime = 0){
  Sys.sleep(sleepTime)
  URL <- as.character(dataVector[1])
  newsClick <- as.character(dataVector[2])
  res <- content(GET(URL), encoding='utf8')
  newsCate <- html_nodes(res, '//*[@id="nav"]/a', xmlValue)
  newsCate <- newsCate[length(newsCate)]
  newsTitle <- html_nodes(res, '//*[@id="story_art_title"]') %>%
    html_text()
  newsText1 <- html_nodes(res, '//*[@id="story_body_content"]/text()') %>%
    html_text() %>%
    str_replace_all('(\r|\n| )+', '')
  newsText1 <- newsText1[str_detect(newsText1, '[:alnum:]')]
  newsText1 <- paste(newsText1, collapse = '\n')

  newsText2 <- html_nodes(res, '//*[@id="story_body_content"]/p', xmlValue)
  newsText2 <- str_replace_all(newsText2, '(\r|\n| )+', '')
  newsText2 <- newsText2[str_detect(newsText2, '[:alnum:]')]
  newsText2 <- paste(newsText2, collapse = '\n')

  newsText <- paste(newsText1,newsText2, sep='\n')
  newsText <- str_replace(newsText, '^(\n)+', '')

  newsAuthor <- html_nodes(res, '//*[@id="story_bady_info"]/h3/span', xmlValue)
  newsAuthor <- str_replace_all(newsAuthor, '(╱|／).+', '')
  newsAuthor <- ifelse(length(newsAuthor)!=0, newsAuthor, '')

  newsDate <- html_nodes(res, '//*[@id="story_bady_info"]/h3', xmlValue)
  newsTime <- str_replace_all(str_extract(newsDate,'[0-9]{2}:[0-9]{2}'), ':', '')
  newsDate <- str_replace_all(str_extract(newsDate, '[0-9]{4}\\-+[0-9]{2}\\-+[0-9]{2}'), '\\-', '')

  result <- try({
    data.frame(newsWebsite='聯合報', newsCate=newsCate, newsAuthor=newsAuthor, newsDate=newsDate, newsTime=newsTime,
               newsTitle=newsTitle, newsText=newsText,
               newsClick=newsClick, newsShare=NA,
               href=URL, createTime= format(Sys.time(), '%Y%m%d%H%M%S'),
               stringsAsFactors=FALSE )
  }, silent = TRUE)

  if(inherits(result, 'try-error')) {
    result <- data.frame(newsWebsite='聯合報', newsCate=NA, newsAuthor=NA, newsDate=NA, newsTime=NA,
                         newsTitle=NA, newsText=NA,
                         newsClick=NA, newsShare=NA,
                         href=URL, createTime= format(Sys.time(), '%Y%m%d%H%M%S'),
                         stringsAsFactors=FALSE )
  }
  return(result)
}


##### Current
getAllNewsUdnCur <- function(sleepBetween = 5, sleepTime = 1, printSts=TRUE) {
  PageUrl<- getPageUrlUdnCur()
  UrlList <- lapply(PageUrl$URL, getUrlListUdnCur)
  UrlList <- do.call(rbind, UrlList)
  UrlList <- UrlList[!duplicated(UrlList$URL),]
  newsList <- list()
  for( i in 1:nrow(UrlList)){
    newsList[[i]] <- getNewsUdn(UrlList[i,c("URL", "newsClick")])
    if ( (i %% sleepBetween) ==0) Sys.sleep(sleepTime)
    if ( printSts&(i%%100==0)) cat('聯合報 ', i, 'is ok', '\n')
  }
  UdnNews <- do.call(rbind, newsList)
  UdnNews
}

##### History
getAllNewsUdnHs <- function(dateWant, sleepBetween = 5,
                            sleepTime = 1, printSts = TRUE){
  PageUrl <- getPageUrlUdnHs(dateWant)
  UrlList <- lapply(PageUrl$URL, getUrlListUdnHs)
  UrlList <- do.call(rbind, UrlList)
  UrlList <- UrlList[!duplicated(UrlList$URL),]
  newsList <- list()
  for( i in 1:nrow(UrlList)){
    newsList[[i]] <- getNewsUdn(UrlList[i,c("URL", "newsClick")])
    if ( (i %% sleepBetween) ==0) Sys.sleep(sleepTime)
    if ( printSts&(i%%100==0)) cat('聯合報 ', i, 'is ok', '\n')
  }

  UdnNews <- rbindlist(newsList)
  UdnNews
}
