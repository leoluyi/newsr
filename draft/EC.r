#(1)經濟日報新聞分類URL
getCateUrlEC <- function(){
  res <- GET('http://money.udn.com/money/index', encoding='utf8')
  res2 <- htmlParse(content(res, "text", encoding = "utf8"), encoding = "utf8")
  wantPages <- cssApply(res2,"#m1",cssLink)
  cate <- cssApply(res2,"#m1",cssCharacter)
  wantPages <- sprintf('http://money.udn.com%s', wantPages)
  data.frame(cate=cate, href=wantPages, stringsAsFactors = FALSE)
}

# URL<- "http://money.udn.com/money/money/cate/5588"

#(2)經濟日報新聞LINK
getUrlListcnEC <- function(URL, sleepTime = 0){
  Sys.sleep(sleepTime)
  res <- GET(URL, encoding='utf8')
  res2 <- htmlParse(content(res, "text", encoding = "utf8"), encoding = "utf8")
  UrlList <- cssApply(res2,"div.category_box_list%20author%20 dt>a",cssLink)
  getNewUrL <- function(URL){
    #wantURL <- toUTF8(sprintf('http://money.udn.com%s',URL))
    wantURL <- sprintf('http://money.udn.com%s',URL)
  }
  UrlList2 <- unlist(lapply(UrlList, getNewUrL))
  UrlList2 <- toUTF8(UrlList2)
  getNewUrL2 <- function(URL){
    wantURL <- URLencode(URL)
  }
  UrlList2 <- unlist(lapply(UrlList2, getNewUrL2))
  return(data.frame(URL=UrlList2, stringsAsFactors = FALSE))
}

# URL <- "http://money.udn.com/money/story/5612/1034301"

getNewsEC <- function(URL, sleepTime = 0){
  Sys.sleep(sleepTime)
  res <- GET(URL, encoding='utf8')
  res2 <- htmlParse(content(res, "text", encoding = "utf8"), encoding = "utf8")
  newsTitle <- cssApply(res2,"h2#story_art_title",cssCharacter)
  #newsTitle <- str_replace_all(newsTitle, '(\r|\n| )+', '')
  newsCate <- cssApply(res2,"#nav > a",cssCharacter)
  newsCate <- newsCate[length(newsCate)]
  
  newsText <- cssApply(res2,"#story_body_content>p",cssCharacter)
  newsText <- paste(newsText, collapse = '')
  newsText <- str_replace_all(newsText, '(\r|\n| )+', '')
  newsAuthor <- cssApply(res2,"#story_bady_info>h3>span",cssCharacter)
  newsAuthor <- str_replace_all(newsAuthor, '經濟日報| |整理|記者|／.+', '')
  newsDate <- cssApply(res2,"#story_bady_info>h3",cssCharacter)
  #newsDate <- str_replace_all(newsDate, '[:space:]', '')
  newsTime <- str_replace_all(str_extract(newsDate, '[0-9]{2}:[0-9]{2}:[0-9]{2}'), ':', '')
  newsDate <- str_replace_all(str_extract(newsDate, '[0-9]{4}-[0-9]{2}-[0-9]{2}'), '-', '')
  #newsClick <- xpathSApply(res, '//div[@class="article_star clear-fix"]/div[@class="art_click clear-fix"]/span[@class="num"]', xmlValue)[1]
  
  #URLnewsShareFB <- cssApply(res2,"span.pluginCountTextDisconnected",cssCharacter) #failed
  # URLnewsDiscuss <- cssApply(res2,"li.discuss.only_web > div > b",cssCharacter)
  
  URLQry <- str_replace_all(str_replace_all(str_replace_all(URL, '/', '%2F'), ':' ,'%3A'), '-', '%2D')
  URLnewsShare <- paste0('http://graph.facebook.com/fql?q=SELECT%20share_count,%20like_count,%20comment_count,%20total_count,%20comments_fbid,%20click_count%20FROM%20link_stat%20WHERE%20url=%22',URLQry,'%22','&callback=_ate.cbs.rcb_httpwwwchinatimescom0')
  newsShareContent <- str_extract(rawToChar(GET(URLnewsShare)$content), '\\{.+\\}')
  newsShareContent <- jsonlite::fromJSON(newsShareContent)
  URLnewsShare <- newsShareContent$data$total_count
  URLnewsShare <- ifelse(is.null(URLnewsShare), NA, URLnewsShare)
  
  URLnewsShare2 <- paste0('http://api-public.addthis.com/url/shares.json?url=',URLQry, '&callback=_ate.cbs.rcb_httpwwwchinatimescom0')
  newsShare2Content <- rawToChar(GET(URLnewsShare2)$content)
  newsShare2Content <- str_extract(newsShare2Content, '\\{.+\\}') 
  newsShare2Content <- jsonlite::fromJSON(newsShare2Content)
  URLnewsShare2 <- newsShare2Content$shares
  URLnewsShare2 <- ifelse(is.null(URLnewsShare2), NA, URLnewsShare2)
  result <- try(data.frame(newsWebsite='經濟日報', newsCate=newsCate, newsAuthor=newsAuthor, newsDate=newsDate, newsTime=newsTime, 
                           newsTitle=newsTitle, newsText=newsText,
                           newsClick=NA, newsShare=URLnewsShare+URLnewsShare2,
                           href=URL, createTime=format(Sys.time(), '%Y%m%d%H%M%S'),
                           stringsAsFactors=FALSE ), silent = TRUE)
  if( inherits(result, 'try-error')){
    result <- data.frame(newsWebsite='經濟日報', newsCate=NA, newsAuthor=NA, newsDate=NA, newsTime=NA, 
                         newsTitle=NA, newsText=NA,
                         newsClick=NA, newsShare=NA,
                         href=URL, createTime=format(Sys.time(), '%Y%m%d%H%M%S'),
                         stringsAsFactors=FALSE )
  }
  return(result)
}


getAllNewsEC <- function(sleepBetween = 5, sleepTime = 1, printSts=TRUE){
  PageUrl<- getCateUrlEC()
  UrlList <- lapply(PageUrl$href[-1], getUrlListcnEC) # 去除熱點
  UrlList <- unname(unlist(UrlList))
  newsList <- list()
  for( i in 1:length(UrlList)){
    newsList[[i]] <- getNewsEC(UrlList[i])
    if ( (i %% sleepBetween) ==0) Sys.sleep(sleepTime)
    if ( printSts&(i%%100 ==0) ) cat('經濟日報 ', i, 'is ok', '\n')
  }
  ECnews <- do.call(rbind, newsList)
}