library(rvest)
library(magrittr)
library(tidyverse)
library(lubridate)

periodos <- c("2015-2020")
tipo <- c("leyes", "decretos")
urlbase <- "https://www.presidencia.gub.uy"

urlpertipo <- paste(urlbase, "normativa", periodos, sep = "/") %>%
  sapply(function(x) {
    url <- paste(x, tipo, sep = "/")
    urlfix <- ifelse(str_extract(url,"[0-9]+-[0-9]+")=="2005-2010",
                     paste(url,"inicio",sep="/"), url) %>%
    list()
  }) %>%
unlist()

ext <- sapply(urlpertipo, function (x) {
  read_html(x) %>%
  html_nodes("#desarrollo a") %>%
  html_attr("href") %>% trimws()
}) %>%
unlist()
ext <- ext[grep("[a-z0-9-]{12,16}",ext)]

norm <- sapply(ext, function(x) {tryCatch({
  url <- paste0(urlbase, x)
  type <- str_extract(url,"/[a-z]{5,8}/") %>% {gsub("/","",.)} %>% str_to_title()
  web <- read_html(url)
  text <- html_nodes(web, "#desarrollo a") %>% html_text() %>% trimws() %>% {gsub("\n          "," ",.)}
  cant <- length(text) %>% as.numeric()
  date <- str_extract(url,"-[0-9]+-[0-9]+") %>% substring(2)
  list(url,date,type,cant,text)},
  error=function(e) c(url,date,type,NA,NA))
  })
datalist <- list(norm[1,],norm[2,],norm[3,],norm[4,],norm[5,])
