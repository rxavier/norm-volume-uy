library(rvest)
library(magrittr)

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
  nodes <- html_nodes(web, "#desarrollo a")
  text <- html_text(nodes) %>% trimws()
  cant <- as.numeric(length(nodes))
  list(url,cant,text)},
  error=function(e) c(url,NA,NA))
  })
datalist <- list(norm[1,],norm[2,],norm[3,])