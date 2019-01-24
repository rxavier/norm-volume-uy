library(rvest)
library(magrittr)
library(tidyverse)
library(lubridate)

periodos <- c("2000-2005","2005-2010","2010-2015","2015-2020")
tipo <- c("leyes", "decretos")
urlbase <- "https://www.presidencia.gub.uy"

urlpertipo <- paste(urlbase, "normativa", periodos, sep = "/") %>%
  sapply(function(x) {
    url <- paste(x, tipo, sep = "/")
    list(url)
  }) %>%
unlist()

ext <- sapply(urlpertipo, function (x) {
  read_html(x) %>%
  html_nodes("#desarrollo a") %>%
  html_attr("href") %>% trimws()
}) %>%
unlist()

norm <- sapply(ext, function(x) {tryCatch({
  url <- paste0(urlbase, x)
  web <- read_html(url)
  text <- html_nodes(web, "#desarrollo a") %>% html_text() %>% trimws()
  cant <- length(text) %>% as.numeric()
  date <- str_extract(url,"-[0-9]+-[0-9]+") %>% substring(2) %>% parse_date_time(orders="mY") %>% as.Date()
  list(date,url,cant,text)},
  error=function(e) c(date,url,NA,NA))
  })
datalist <- list(norm[1,],norm[2,],norm[3,])