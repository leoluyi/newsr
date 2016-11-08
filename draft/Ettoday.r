getPageUrlEttoday <- function(dateWant){
  wantURL <- sprintf('http://www.ettoday.net/news/news-list-%s-%s-%s-0-%s.htm', substr(dateWant,1,4), substr(dateWant,5,6), substr(dateWant,7,8), 1)
  res <- GET(wantURL, encoding='utf8')
  res2 <- content(res, encoding='utf8')
  maxPage<- xpathSApply(res2, '//div[@class="menu_page"]/a', xmlAttrs)
  maxPage <- str_replace(str_extract(maxPage[length(maxPage)],'\\-0-[0-9]+'),'\\-0-', '')
  wantPages<- sprintf('http://www.ettoday.net/news/news-list-%s-%s-%s-0-%s.htm', substr(dateWant,1,4), substr(dateWant,5,6), substr(dateWant,7,8), 1:maxPage)
  return(wantPages)
}



getUrlListEttoday <- function(URL, sleepTime = 0){
  Sys.sleep(sleepTime)
  res <- GET(URL, encoding='utf8')
  res2 <- content(res, encoding='utf8')
  UrlList <- xpathSApply(res2, '//div[@class="part_list_1"]/h3/a', xmlAttrs)
  UrlList <-  UrlList[rownames(UrlList)=='href',]
  UrlListCate <- xpathSApply(res2, '//div[@class="part_list_1"]/h3/em', xmlValue)
  newsTitle <- xpathSApply(res2, '//div[@class="part_list_1"]/h3/a', xmlValue)
  newsTitle <- str_replace_all(newsTitle, '(\r|\n| )+', '')
  return(data.frame(URL=UrlList, cate=UrlListCate, newsTitle=newsTitle, stringsAsFactors = FALSE))
}


# UrlList <- getUrlListcnEttoday(PageUrl[1])

# URL <- 'http://www.ettoday.net/news/20150601/514621.htm' 
# URL <- 'http://www.ettoday.net/news/20150601/514531.htm'
# URL <-"http://travel.ettoday.net/article/514810.htm"
#一般版
# URL <-"http://www.ettoday.net/news/20150601/514481.htm"
# URL <-"http://www.ettoday.net/news/20150626/525596.htm"


#GAME
# URL <-"http://www.ettoday.net/news/20150601/514763.htm"
# URL <-"http://www.ettoday.net/news/20150704/530214.htm"

# URL <- UrlList2$URL[1]
# dataVector <- UrlList[i,1:2]

getNewsEttoday <- function(dataVector, sleepTime = 0){
  Sys.sleep(sleepTime)
  URL = as.character(dataVector[1])
  cate = as.character(dataVector[2])
  res <- htmlParse(URL, encoding='utf8')
  
  newsTitle_GAME <- xpathSApply(res, '//div[@class="module_news"]/h2', xmlValue)
  newsTitle_travel <- xpathSApply(res, '//div[@class="subjcet_news"]/h2', xmlValue)
  newsTitle_ETLIFE <- xpathSApply(res, '//div[@class="subjcet-article"]/h2', xmlValue)
  newsTitle_star <- xpathSApply(res,'//div[@class="module_1"]/h2[@class="title"]', xmlValue)
  newsTitle_fashion <- xpathSApply(res,'//h1[@class="title_article"]', xmlValue)
  newsTitle_sports <- xpathSApply(res,'//h1[@class="title"]', xmlValue)
  newsTitle <- xpathSApply(res, '//article/header/h2[@class="title clearfix"]', xmlValue)
  newsTitle <- paste(newsTitle, newsTitle_GAME, newsTitle_travel, newsTitle_ETLIFE, newsTitle_star, newsTitle_fashion, newsTitle_sports, collapse = '')
  newsTitle <- str_trim(newsTitle)
  
  newsText <- xpathSApply(res, '//div[@class="story"]/sectione/p', xmlValue)
  newsText_spe <- xpathSApply(res, '//div[@class="story"]/p', xmlValue)
  newsText_game <- xpathSApply(res, '//div[@class="article-detail-con-wrap"]', xmlValue)
  newsText <- paste(newsText,newsText_spe,newsText_game, collapse = '\n')
  newsAuthor <- str_extract(newsText, "記者.+(／|/)")
  newsAuthor <- str_replace_all(newsAuthor, '記者|／|/', '')
  
  
  newsDate <- xpathSApply(res, '//span[@class="news-time"]', xmlValue)
  newsDate_game <- xpathSApply(res, '//div[@class="menu_bread_crumb clearfix"]/div[@class="txt_2"]', xmlValue)
  newsDate_sp <- xpathSApply(res, '//div[@class="subjcet_news"]/div[@class="date"]', xmlValue)
  newsDate_ETLIFE <- xpathSApply(res, '//div[@class="subjcet-article"]/div[@class="date"]', xmlValue)
  newsDate_star <- xpathSApply(res,'//*[@class="date"]', xmlValue)
  newsDate <- paste(newsDate, newsDate_sp, newsDate_game, newsDate_ETLIFE, newsDate_star)
  newsDate <- str_replace_all(newsDate, '(\r|\n| )+$', '')
  newsDate <- str_replace_all(newsDate, '^(\r|\n| )+', '')
  newsDate_fashion <- ifelse(str_detect(newsDate,'[0-9]{2}/[0-9]{2}/[0-9]{4}'),
                             paste0(str_sub(newsDate, 7, 11),str_sub(newsDate, 1, 2),str_sub(newsDate, 4, 5)),
                             '')
  newsTime <- str_replace_all(str_extract(newsDate, '([0-9]{2}:[0-9]{2}:[0-9]{2}$)|([0-9]{2}:[0-9]{2})$'), ':', '')
  newsDate_sports <- str_extract(newsDate,'[0-9]+-[0-9]+-[0-9]+')
  newsDate_sports <- str_replace_all(newsDate_sports, '-', '')
  newsDate_sports <- str_replace_na(newsDate_sports,'')
  newsDate <- str_replace_all(newsDate, '(\r|\n| )+', '')
  newsDate <- str_replace_all(newsDate, '[:space:]', '')
  newsDate <- str_replace(newsDate, '[0-9]{2}:?[0-9]{2}$', '')
  
  newsDate <- paste0(str_extract(newsDate, '[0-9]{4}'),
                     str_pad(str_replace(str_extract(newsDate,'[0-9]+月'), '月', ''),2, pad='0'),
                     str_pad(str_replace(str_extract(newsDate,'[0-9]+日'), '日', ''),2, pad='0'))
  newsDate <- ifelse(str_detect(newsDate_sports,'[0-9]{8}'), newsDate_sports, newsDate)
  newsDate <- ifelse(str_detect(newsDate_fashion,'[0-9]{8}'), newsDate_fashion, newsDate)
  URLQry <- str_replace_all(str_replace_all(URL, '/', '%2F'), ':' ,'%3A')
  URLnewsShare1 <- paste0('http://graph.facebook.com/fql?q=SELECT%20share_count,%20like_count,%20comment_count,%20total_count,%20comments_fbid,%20click_count%20FROM%20link_stat%20WHERE%20url=%22',URLQry,'%22','&callback=_ate.cbs.rcb_httpwwwettodaynet0')
  
  newsShare1Content <- str_extract(rawToChar(GET(URLnewsShare1)$content), '\\{.+\\}')
  newsShare1Content <- rjson::fromJSON(newsShare1Content)
  newsShare <- newsShare1Content$data[[1]]$share_count
  newsShare <- ifelse(is.null(newsShare),0,newsShare)
  result <- try(data.frame(newsWebsite='東森新聞', newsCate=cate, newsAuthor=newsAuthor, newsDate=newsDate, newsTime=newsTime, 
                           newsTitle=newsTitle, newsText=newsText,
                           newsClick=NA, newsShare=newsShare,
                           href=URL, createTime=format(Sys.time(), '%Y%m%d%H%M%S'),
                           stringsAsFactors=FALSE ), silent = TRUE)
  if( inherits(result, 'try-error')){
    result <- data.frame(newsWebsite='東森新聞', newsCate=NA, newsAuthor=NA, newsDate=NA, newsTime=NA, 
                         newsTitle=NA, newsText=NA,
                         newsClick=NA, newsShare=NA,
                         href=URL, createTime=format(Sys.time(), '%Y%m%d%H%M%S'),
                         stringsAsFactors=FALSE )
  }
  return(result)
}



getAllNewsEttoday <- function(dateWant, sleepBetween = 5, sleepTime = 1, printSts=TRUE){
  PageUrl<- getPageUrlEttoday(dateWant)
  UrlList <- lapply(PageUrl, getUrlListEttoday)
  UrlList2 <- do.call(rbind, UrlList)
  newsList <- list()
  for( i in 1:nrow(UrlList2)){
    newsList[[i]] <- getNewsEttoday(UrlList2[i,1:2])
    if ( (i %% sleepBetween) ==0) Sys.sleep(sleepTime)
    if ( printSts&(i%%100==0)) cat('東森新聞 ', i,'is ok', '\n')
  }
  Ettodaynews <- do.call(rbind, newsList)
}
