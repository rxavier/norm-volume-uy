library(rvest)
library(magrittr)

periodos <- c("2000-2005", "2005-2010", "2010-2015", "2015-2020")
tipo <- c("leyes", "decretos")
meses <- sprintf("%02d", 1:12)
anos <- 2000:2020
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

norm <- sapply(ext, function(x) {
  url <- paste0(urlbase, x)
  web <- read_html(url)
  nodes <- html_nodes(web, "#desarrollo a")
  text <- html_text(nodes)
  
  })
