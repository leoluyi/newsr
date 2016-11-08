#' Search appledaily
#'
#' @param keyword Vector of search strings.
#' @param date_from Begin of date of news to get with format \code{"yyyy-mm-dd"}.
#' @param date_to End of date of news to get with format \code{"yyyy-mm-dd"}.
#'
#' @return
#' data.table object with columns:
#'
#' @examples
#' search_appledaily(c("老年", "保險"))
#'
#' @export
search_appledaily <- function(keyword,
                              date_from = "2003-05-02",
                              date_to = Sys.Date()) {
  # date_from = "2016-10-01"
  # keyword = c("基隆")

  message(">> Searching keywords: ", keyword, "...")
  keyword <- paste0(keyword, collapse = " AND ")
  date_from <- date_from %>%
    ymd(tz = "ROC") %>%
    strftime(tz = "ROC", "%Y%m%d")
  date_from <- date_from %>%
    lubridate::ymd(tz = "ROC") %>%
    strftime(tz = "ROC", "%Y%m%d")
  date_to <- date_to %>%
    lubridate::ymd(tz = "ROC") %>%
    strftime(tz = "ROC", "%Y%m%d")
  date_int <- sprintf("[%s TO %s]", date_from, date_to)

  ## Get max page
  max_page <- maxpage_appledaily(keyword, date_int)
  if (max_page == -Inf) {
    message("No result.")
    return(invisible(NULL))
  }
  message(">> Getting ", max_page, " pages...")

  search_result <- data.table(
    title = character(0L),
    description = character(0L),
    news_url = character(0L)
  )
  # for (i in 1:max_page) {
  #   cat(i, " ")
  #   res <- search_appledaily_(keyword, i)
  #   search_result <- rbindlist(list(search_result, res))
  # }
  if (.Platform$OS.type == "unix") {
    f <- function(keyword, i) {
      cat(i, " ")
      tryCatch({
        search_appledaily_(keyword, i)
      }, error = function(e) {
        print(e)
        data.table()
      })
    }
    res_list <- parallel::mclapply(X = seq_len(max_page),
                                   FUN = f,
                                   keyword = keyword,
                                   mc.cores = 1)
    # --- not paralleling => will get status code: 500
  } else {
    stop("Unix-like system required")
  }
  # print(c(list(search_result), res_list)) # for debug
  search_result <- rbindlist(c(list(search_result), res_list))
  search_result[, news_url := news_url %>%
                  str_extract("^.*(?=/applesearch)")] %>%  # normalize url
    .[, c("title", "description") := NULL] # drop unused columns

  message("\n\n>> ", search_result[, news_url] %>% length(),
          " news to be loaded...")

  ## Prevent inconsistent number of news, e.g., caused by connection error
  news_click <- search_result[, get_appledaily(news_url)]
  out <- merge(news_click,
               search_result, by = "news_url", all.x = TRUE)

  message("\nDone! Got ", nrow(out), " news")
  out
}

maxpage_appledaily <- function(keyword, date_int) {
  url <- "http://search.appledaily.com.tw/appledaily/search"
  res <- POST(url,
              body = list(
                searchMode = "",
                searchType = "text",
                ExtFilter = "",
                sorttype = "1",
                keyword = keyword,
                rangedate = date_int,
                totalpage = "467",
                page = 1
              ))
  res_xml <- res %>%
    content(as = "text", encoding = "UTF-8") %>%
    read_html()
  ## get max page number
  max_page <- res_xml %>%
    html_nodes("#pageNumber a") %>%
    html_text() %>%
    stringr::str_subset("^\\d+$") %>%
    as.numeric() %>%
    max
}

search_appledaily_ <- function(keyword, page = 1, date_int) {
  # page = 1
  # date_int = "[20030502 TO 20160830]"
  url <- "http://search.appledaily.com.tw/appledaily/search"
  res <- POST(url,
              body = list(
                searchMode = character(1L),
                searchType = "text",
                ExtFilter = character(1L),
                sorttype = "1",
                keyword = keyword,
                rangedate = "[20030502 TO 20160830]",
                totalpage = "467",
                page = page
              ))
  # cat(status_code(res), "<<", url, "\n")  # for debug
  if (identical(status_code(res), 200L)) {
    res_xml <- res %>% content(as = "parsed", encoding = "UTF-8")
  } else {
    warning("http error: ", url)
    return(data.table())
  }

  res_xml <- res %>%
    content(as = "text", encoding = "UTF-8") %>%
    read_html()
  ## get title, url, description, ...
  title <- res_xml %>%
    html_nodes("h2 a:not(.nextmedia)") %>%
    html_text() %>%
    str_trim()
  news_url <- res_xml %>%
    html_nodes("h2 a:not(.nextmedia)") %>%
    html_attr("href")
  description <- res_xml %>%
    html_nodes("#result p") %>%
    html_text() %>%
    str_trim()

  data.table(title, description, news_url)
}



get_appledaily_ <- function(url) {
  # url = "http://www.appledaily.com.tw/appledaily/article/finance/20160626/37284508/"
  # url = "http://www.appledaily.com.tw/realtimenews/article/international/20160830/938362/"
  # url = "http://www.appledaily.com.tw/realtimenews/article/international/20160830/938415"
  res <- GET(url)
  if (identical(status_code(res), 200L)) {
    res_xml <- res %>% content(as = "parsed", encoding = "UTF-8")
  } else {
    warning("http error: ", url)
    return(data.table())
  }
  ## Title
  title <- res_xml %>%
    html_nodes("#h1") %>%
    html_text() %>%
    str_trim
  subtitle <- res_xml %>%
    html_nodes("#h2") %>%
    html_text() %>%
    str_trim
  if (length(subtitle) == 0) {subtitle <- NA_character_}
  ## Get news content
  news_text <- res_xml %>%
    html_nodes("#summary") %>%
    html_text() %>%
    str_trim
  if (length(news_text) == 0) {
    content_nodes <- res_xml %>%
      html_nodes("div.articulum.trans")
    if (content_nodes %>% xml_find_all("//script") %>% length()) {
      content_nodes %>% xml_find_all("//script") %>% xml_remove()
    }
    news_text <- content_nodes %>%
      html_text() %>%
      str_c("\n") %>%
      str_trim
  }
  ## keywords
  keywords <- res_xml %>%
    html_nodes('head > meta[name="keywords"]') %>%
    html_attr("content") %>%
    str_trim
  ## author
  author <- news_text %>%
    str_match("(?<=[(（])([^／/)）]+)[／/].*報導") %>%
    .[,2] %>%
    str_trim
  ## other
  datetime_text <- res_xml %>%
    html_node(".gggs time") %>%
    html_text() %>%
    str_trim
  if (!str_detect(datetime_text, ":")) {
    datetime_text <- str_c(datetime_text, "00:00")
  }
  datetime <- datetime_text %>%
    strptime(tz = "ROC", "%Y年%m月%d日%H:%M") %>%
    as.POSIXct()

  data.table(news_source = "蘋果日報",
             title, subtitle, datetime, news_text, author, keywords)
}

get_appledaily <- function(urls) {
  # urls = c(
  #   "http://www.appledaily.com.tw/appledaily/article/finance/20160626/37284508/",
  #   "http://www.appledaily.com.tw/realtimenews/article/international/20160830/938362/"
  # )
  index <- setNames(seq_along(urls), urls)
  f <- function(x) {
    cat(index[x], " ")
    tryCatch({
      get_appledaily_(x)
    }, error = function(e) {
      data.table()
    })
  }
  if (.Platform$OS.type == "unix") {
    out_list <- parallel::mcmapply(FUN = f, urls,
                                   SIMPLIFY = FALSE, USE.NAMES = TRUE,
                                   mc.cores = parallel::detectCores()-1)
  } else {
    stop("Only support Unix-like")
  }
  tryCatch({
    out <- rbindlist(out_list, idcol = "news_url")
  }, error = function(e) {
    out <- out_list
  })
  out
}

get_url_appledaily <- function(date_from, date_to) {

}

