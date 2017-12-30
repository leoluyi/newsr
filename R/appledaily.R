#' Search AppleDaily
#'
#' @param keyword Vector of search strings.
#' @param date_from Begin of date of news to get with format \code{"yyyy-mm-dd"}.
#' @param date_to End of date of news to get with format \code{"yyyy-mm-dd"}.
#'
#' @return
#' data.table object with columns:
#'
#' @examples
#' search_appledaily(c("老年", "保險"), date_from = Sys.Date())
#'
#' @import pbapply
#' @export
search_appledaily <- function(keyword,
                              date_from = "2003-05-02",
                              date_to = Sys.Date()) {
  # date_from = "2016-10-01"
  # date_to = Sys.Date()
  # keyword = c("老年")

  keyword <- paste0(keyword, collapse = " AND ")
  message(">> Searching keywords: ", keyword, "...")

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
    message("No news result found.")
    return(invisible(NULL))
  }
  message(">> Getting ", max_page, " pages...")

  # for (i in 1:max_page) {
  #   cat(i, " ")
  #   res <- search_appledaily_(keyword, i)
  #   search_result <- rbindlist(list(search_result, res))
  # }
  if (.Platform$OS.type == "unix") {
    f <- function(keyword, i, date_int) {
      cat(i, " ") # to show page num in mclapply
      tryCatch({
        search_appledaily_(keyword, i, date_int)
      }, error = function(e) {
        print(e)
        data.table()
      })
    }
    res_list <- parallel::mclapply(X = seq_len(max_page),
                                   FUN = f,
                                   keyword = keyword,
                                   date_int = date_int,
                                   mc.cores = 1)
    # --- not paralleling => will get status code: 500
  } else {
    stop("Unix-like system required")
  }

  search_result <- data.table(
    title = character(0L),
    description = character(0L),
    news_url = character(0L)
  )
  # print(c(list(search_result), res_list)) # for debug
  search_result <- rbindlist(c(list(search_result), res_list))
  search_result[, news_url := news_url %>%
                  str_extract("^.*(?=/applesearch)")] #%>%  # normalize url
    # .[, c("title", "description") := NULL] # drop unused columns

  message("\n\n>> ", search_result %>% nrow, " news to load...")

  out <- get_appledaily(search_result[, news_url], nb_core = "auto")

  # Prevent inconsistent number of news, e.g., caused by connection error
  # news_click <- search_result[res[, news_url], on = .(news_url)]
  # out <- merge(news_click,
  #              search_result, by = "news_url", all.x = TRUE)

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
                rangedate = date_int,
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
  # url = "http://www.appledaily.com.tw/realtimenews/article/entertainment/20170322/1081808"
  ua <- user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.110 Safari/537.36")

  res <- GET(url, ua)
  if (identical(status_code(res), 200L)) {
    res_xml <- res %>% content(as = "parsed", encoding = "UTF-8")
  } else {
    warning("http error: ", url)
    return(NULL)
  }

  location_301 <- res %>% content(encoding = "UTF-8") %>%
    html_nodes("script:contains('window.location.href')")
  if (length(location_301) == 1) {
    url_base <- res$all_headers[[1]]$headers$location %>% parse_url() %>% .$hostname
    new_url <- paste0("http://", url_base,
                      location_301 %>%
                        as.character() %>%
                        str_extract('(?<=href=\").*(?=\"</script>)'))
    res_xml <- GET(new_url, ua) %>% content()
  }

  tryCatch({
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
    if (nchar(news_text) == 0) {
      news_text <- res_xml %>% html_nodes("#introid, #bcontent") %>%
        html_text() %>%
        str_trim %>% paste(collapse = "\n")
    }
    ## keywords
    keywords <- res_xml %>%
      html_nodes('head > meta[name="keywords"]') %>%
      html_attr("content") %>%
      str_trim
    ## author
    author <- news_text %>%
      str_match("(?<=[【(（])([^】╱／/)）]+)[╱／/].*報導") %>%
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

    out <- data.table(news_source = "蘋果日報",
                      title, subtitle, datetime, news_text, author, keywords)
  }, error = function(e) {
    out <<- NULL
  })

  out
}

get_appledaily <- function(urls, nb_core = "auto") {
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
      message(e)
      NULL
    })
  }

  if (nb_core == "auto") {
    nb_core <- parallel::detectCores() - 1
  }
  if (nb_core == 1) {
    out_list <- sapply(urls, f,
                       simplify  = FALSE, USE.NAMES = TRUE)
  } else {
    if (.Platform$OS.type == "unix") {
      cl <- parallel::makeCluster(nb_core)
      on.exit(parallel::stopCluster(cl))
      parallel::clusterExport(cl, list("index", "get_appledaily_"), envir = environment())
      parallel::clusterEvalQ(cl, {
        library(httr)
        library(rvest)
        library(stringr)
        library(data.table)
      })

      out_list <- pbapply::pbsapply(cl = cl,
                                    urls, f,
                                    simplify  = FALSE, USE.NAMES = TRUE)
    } else {
      stop("Only support Unix-like")
    }
  }

  tryCatch({
    out <- rbindlist(out_list, idcol = "news_url")
  }, error = function(e) {
    out <<- out_list
    warning("Something went wrong with rbindlist(out_list)")
  })
  out
}

get_url_appledaily <- function(date_from, date_to) {

}

