library(httr)
library(rvest)
library(stringr)
library(data.table)
library(lubridate)

search_url <- "http://news.ltn.com.tw/search"
parse_url(search_url) %>% build_url()
res <- GET(search_url,
           query = list(
             keyword = "老人 保險",
             conditions = "and",
             SYear = "2016",
             SMonth = "5",
             SDay = "31",
             EYear = "2016",
             EMonth = "8",
             EDay = "31"
           ))

res_xml <- res %>% content(as = "parsed")
news_url <- res_xml %>% 
  html_nodes("#newslistul a") %>% 
  html_attr("href") %>% 
  sprintf("http://news.ltn.com.tw%s", .)
title <- res_xml %>% 
  html_nodes("#newslistul a") %>% 
  html_text %>% 
  str_trim

res_xml %>% 
  html_nodes("#newslistul .search") %>% 
  html_text %>% 
  str_trim

max_page <- res_xml %>% 
  html_nodes(".p_num") %>% 
  html_text() %>% 
  as.integer() %>% 
  max(na.rm = TRUE)


