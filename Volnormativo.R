library(rvest)
library(magrittr)
library(tidyverse)
library(lubridate)

periods <- c("2000-2005","2005-2010","2010-2015","2015-2020")
normtype <- c("leyes", "decretos")
baseurl <- "https://www.presidencia.gub.uy"

urlpertype <- paste(baseurl, "normativa", periods, sep = "/") %>%
  sapply(function(x) {
    url <- paste(x, normtype, sep = "/")
    urlfix <- ifelse(str_extract(url,"[0-9]+-[0-9]+")=="2005-2010",
                     paste(url,"inicio",sep="/"), url) %>%
      list()
  }) %>%
  unlist()

ext <- sapply(urlpertype, function (x) {
  read_html(x) %>%
    html_nodes("#desarrollo a") %>%
    html_attr("href") %>% trimws()
}) %>%
  unlist()
ext <- ext[grep("[a-z0-9-]{12,16}",ext)]

norm <- sapply(ext, function(x) {tryCatch({
  url <- paste0(baseurl, x)
  type <- str_extract(url,"/[a-z]{5,8}/") %>% {gsub("/","",.)} %>% str_to_title()
  web <- read_html(url)
  text <- html_nodes(web, "#desarrollo a") %>% html_text() %>% trimws() %>% {gsub("\n          "," ",.)}
  cant <- length(text) %>% as.numeric()
  date <- str_extract(url,"-[0-9]+-[0-9]{4}") %>% substring(2)
  list(url,date,type,cant,text)},
  error=function(e) c(url,date,type,NA,NA))
})
df<-cbind(norm[1,],norm[2,],norm[3,],norm[4,]) %>% data.frame()
colnames(df) <- c("URL","Date","Type","Count")
df$Date <- parse_date_time(df$Date,orders=c("mY","my")) %>% as.Date()
df$Count <- as.numeric(df$Count)
df$URL <- as.character(df$URL)
df$Type <- as.character(df$Type)
