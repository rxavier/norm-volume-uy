packages <-c("rvest","magrittr","tidyverse","lubridate","beepr")
invisible(lapply(packages, library, character.only = TRUE))

# Set legislature periods, norm type (law or decree) and the base url
periods <- c("2000-2005","2005-2010","2010-2015","2015-2020")
normtype <- c("leyes", "decretos")
baseurl <- "https://www.presidencia.gub.uy"

# Loop to create the 8 URLs (4 periods, 2 type of norm)
urlpertype <- paste(baseurl, "normativa", periods, sep = "/") %>%
  sapply(function(x) {
    url <- paste(x, normtype, sep = "/")
    # 2005-2010 URLs have a different format than the rest so I take that into consideration
    urlfix <- ifelse(str_extract(url,"[0-9]+-[0-9]+")=="2005-2010",
                     paste(url,"inicio",sep="/"), url) %>%
      list()
  }) %>%
  unlist()

# Scrape the above URLs to get the URL extention to every month for both laws and decrees
ext <- sapply(urlpertype, function (x) {
  read_html(x) %>%
    html_nodes("#desarrollo a") %>%
    html_attr("href") %>% trimws()
}) %>%
  unlist()

# Exclude URLs that don't correspond to months
ext <- ext[grep("[a-z0-9-]{12,16}",ext)]

# Loop to build final monthly URLs and scrape each of them for the data
norm <- sapply(ext, function(x) {tryCatch({
  url <- paste0(baseurl, x)
  web <- read_html(url)
  text <- html_nodes(web, "#desarrollo a") %>% html_text() %>% trimws() %>% str_replace_all("\\s+"," ")
  cant <- length(text) %>% as.numeric()
  list(url,cant,text)},
  error=function(e) c(url,NA,NA))
})

# Build the dataframe, parse dates and set classes
df<-cbind(norm[1,],norm[2,]) %>% data.frame()
colnames(df) <- c("URL","Count")
df$Date <- str_extract(df$URL,"-[0-9]+-[0-9]{2,4}") %>% substring(2) %>% parse_date_time(orders=c("mY","my")) %>% as.Date()
df$Type <- str_extract(df$URL,"/[a-z]{5,8}/") %>% {gsub("/","",.)} %>% str_to_title()
df$Count <- as.numeric(df$Count)
df$URL <- as.character(df$URL)
beep()

plot1=ggplot(df, aes(x=Date,y=Count,colour=Type)) +     geom_line() +     xlab("")

## Manually pick words which will be excluded from the norm count
exclude <- c("SUBGRUPO","GRUPO","CONVENIO","ACUERDO","COLECTIVO","UNIDAD REAJUSTABLE",
             "unidad reajustable","U.R.","UR","U.R.A.","URA","Se fija","Se actualiza","SUSCRITO",
             "ANEXO","DESIGNA", "DESIGNACIÓN","ESCUELA","PARTIDAS","COMISIÓN", "MERCOSUR",
             "MERCADO COMÚN","EMISIÓN","SALARIO MÍNIMO NACIONAL",
             "MONTO MÍNIMO DE LAS JUBILACIONES","INTERÉS NACIONAL",
             "COMPLEMENTACIÓN","COOPERACIÓN") %>% {paste0("\\b",.,"\\b",collapse="|")}

prune <- sapply(norm[3,],function(x) {
  text <- unlist(x) %>% {.[!str_detect(.,exclude)]}
  cant <- length(text) %>% as.numeric()
  list(cant,text)})
df$CountPrune <- prune[1,] %>% as.numeric()

plot2=ggplot(df, aes(x=Date,y=CountPrune,colour=Type)) +     geom_line() +     xlab("")
