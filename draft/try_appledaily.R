library(httr)
library(rvest)
library(stringr)
library(data.table)

url <- "https://tw.appledaily.com/search"
ua = user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.84 Safari/537.36")
res <- POST(url,
            body = list(
              # searchMode = "Adv",
              searchType = "text",
              searchMode = "Sim",
              querystrS = "老年 保險",
              sdate = "2003-05-02",
              edate = "2017-12-30"
            ))


res_xml <- res %>%
  content(as = "text", encoding = "UTF-8") %>%
  read_html()

## get page numbers
max_page <- res_xml %>%
  html_nodes("#pageNumber a") %>%
  html_text() %>%
  stringr::str_subset("^\\d+$") %>%
  as.numeric() %>%
  max


## get title
news_title <-res_xml %>%
  html_nodes("h2 a:not(.nextmedia)") %>%
  html_text()
news_url <- res_xml %>%
  html_nodes("h2 a:not(.nextmedia)") %>%
  html_attr("href")


# pagination --------------------------------------------------------------

url <- "https://tw.appledaily.com/search"
res <- POST(url,
            ua,
            body = list(
              # searchType = "text",
              # searchMode = "Sim",
              # querystrS = "老年 保險",
              searchMode = "",
              searchType = "text",
              ExtFilter = "",
              sorttype = "1",
              keyword = "老年 AND 保險",
              rangedate = "[20030502 TO 20160829999:99]",
              totalpage = "467",
              page = 1
            ),
            encode = "form")
res %>%
  content(as = "text", "UTF-8") %>%
  read_html() %>%
  html_nodes("h2 a:not(.nextmedia)") %>%
  html_text()


# news content ------------------------------------------------------------

res <- GET(str_extract(news_url[10], "^.*(?=/applesearch)"))
res_xml <- res %>%
  content()
## time
time <- res_xml %>%
  html_nodes(".gggs time") %>%
  html_attr("datetime") %>%
  as.POSIXct(tz = "CST")

## Get news content
news_content <- res_xml %>%
  html_nodes("#summary") %>%
  html_text()

if (length(news_content) == 0) {
  nodes <- res_xml %>%
    html_nodes("div.articulum.trans")
  if (nodes %>% xml_find_all("//script") %>% length()) {
    nodes %>% xml_find_all("//script") %>% xml_remove()
  }
  news_content <- nodes %>%
    html_text() %>%
    str_c("\n") %>%
    str_trim
}


