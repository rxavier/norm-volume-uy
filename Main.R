packages <-c("rvest","magrittr","ggplot2","lubridate","beepr","tidyr","seasonal","stringr")
invisible(lapply(packages, library, character.only = TRUE))

# Set legislature periods, norm type (law or decree) and the base url
periods <- c("2000-2005", "2005-2010", "2010-2015", "2015-2020")
norm_type <- c("leyes", "decretos")
base_url <- "https://www.presidencia.gub.uy"

# Loop to create the 8 URLs (4 periods, 2 type of norm)
url_per_type <- paste(base_url, "normativa", periods, sep = "/") %>%
  sapply(function(x) {
    url <- paste(x,norm_type,sep = "/")
    # 2005-2010 URLs have a different format than the rest so I take that into consideration
    url_fix <- ifelse(str_extract(url, "[0-9]+-[0-9]+")=="2005-2010",
                      paste(url, "inicio", sep="/"), url) %>% list()
  }) %>%
  unlist()

# Scrape the above URLs to get the URL extension for every month for both laws and decrees
ext <- sapply(url_per_type, function (x) {
  read_html(x) %>% html_nodes("#desarrollo a") %>%
    html_attr("href") %>% trimws()
}) %>%
  unlist()

# Exclude URLs that don't correspond to months
ext <- str_subset(ext,"[a-z0-9-]{12,16}")

# Loop to build final monthly URLs and scrape each of them for the data
norm <- sapply(ext, function(x) {tryCatch({
  url <- paste0(base_url, x)
  web <- read_html(url)
  text <- html_nodes(web, "#desarrollo a") %>% html_text() %>%
    trimws() %>% str_replace_all("\\s+", " ")
  count <- length(text) %>% as.numeric()
  list(url, count, text)},
  error=function(e) c(url, NA, NA))
})

# Build the dataframe, parse dates and set classes
df<-cbind(norm[1,], norm[2,]) %>% data.frame()
colnames(df) <- c("URL", "Count")
df$Date <- str_extract(df$URL, "-[0-9]+-[0-9]{2,4}") %>% substring(2) %>%
  parse_date_time(orders=c("mY", "my")) %>% as.Date()
df$Type <- str_extract(df$URL, "/[a-z]{5,8}/") %>%
  {gsub("/", "", .)} %>% str_to_title()
df$Count <- as.numeric(df$Count)
df$URL <- as.character(df$URL)
beep()

## Manually pick words which will be excluded from the norm count
exclude <- c("SUBGRUPO","GRUPO","CONVENIO","ACUERDO","COLECTIVO","UNIDAD REAJUSTABLE",
             "U.R.","UR","U.R.A.","URA","Se fija","Se actualiza","SUSCRITO",
             "ANEXO","DESIGNA", "DESIGNACIÓN","ESCUELA","PARTIDAS","COMISIÓN", "MERCOSUR",
             "MERCADO COMÚN","EMISIÓN","SALARIO MÍNIMO NACIONAL",
             "MONTO MÍNIMO DE LAS JUBILACIONES","INTERÉS NACIONAL",
             "COMPLEMENTACIÓN","COOPERACIÓN") %>% {paste0("\\b",.,"\\b",collapse="|")}

# Remove norms that contain words in 'exclude'
prune <- sapply(norm[3,], function(x) {
  text <- unlist(x) %>% {.[!str_detect(., fixed(exclude,ignore_case = TRUE))]}
  count_prune <- length(text) %>% as.numeric()
  list(count_prune, text)})
df$Count_Prune <- prune[1,] %>% as.numeric()
# Set count_prune to NA if count has an NA
df$Count_Prune[is.na(df$Count)] <- NA

# Decompose time series. Output seasonally adjusted ts and trend
decomp <- sapply(c("Leyes", "Decretos"), function(x)
  sapply(c("Count", "Count_Prune"), function(y) 
  {decomp_proc <- subset(df,Type %in% x,select=c("Date", y)) %>%
    {.[order(.$Date),]} %>% {.[,!names(.) %in% c("Type", "URL", "Date")]} %>% 
    ts(start=c(2000, 3),frequency=12) %>% seas(x11="", na.action=na.x13)
  decomp_seas <- final(decomp_proc) %>% ifelse(.<0, 0, .)
  decomp_trend <- trend(decomp_proc) %>% ifelse(.<0, 0, .)
  list(decomp_seas,decomp_trend)}
  ))

# Rebuild dataframe with seasonally adjusted and trend series
df_decomp <- rbind.data.frame(decomp[5:8],decomp[1:4]) %>% 
  `colnames<-` (c("Count_Seas","Count_Trend","Count_Prune_Seas","Count_Prune_Trend"))
df_full <- df[with(df, order(df$Type,df$Date)),] %>% cbind.data.frame(.,df_decomp) %>%
  .[c("URL", "Date", "Type", "Count", "Count_Seas", "Count_Trend", "Count_Prune",
      "Count_Prune_Seas", "Count_Prune_Trend")]

# Define legislatures
df_full$Legislature <- str_extract(df_full$URL,"[0-9]{4}-[0-9]{4}")
df_full$Time <- sequence(rle(df_full$Legislature)$lengths)

# Transform dataframe to long format and plot
df_full_melt <- gather(df_full,Key,Count,Count:Count_Prune_Trend)

ggplot(df_full_melt, aes(x=Date, y=Count, colour=Type)) +
  geom_line() + labs(title="Testeo", y="Conteo", x="") +
  facet_grid(~Key) + theme(axis.text.x=element_text(angle=90, hjust=1), legend.position="bottom") +
  scale_color_discrete(name = "Tipo", labels=c("Decretos", "Leyes"))

ggplot(df_full_melt[which(df_full_melt$Key==
                            c("Count", "Count_Trend")),],
       aes(x=Date, y=Count, colour=Key)) + geom_line() +
  labs(title="Decretos y leyes en Uruguay, 2000-2019", subtitle="Series simples y tendenciales", y="Conteo", x="") +
  scale_color_discrete(name = "Tipo", labels=c("Simple", "Tendencia")) +
  theme(legend.position="bottom") + facet_wrap(~Type,scales="free_y")

ggplot(df_full_melt[which(df_full_melt$Key==c("Count_Prune_Trend")),],
       aes(x=Date, y=Count)) + geom_line() +
  labs(title="Decretos y leyes en Uruguay, 2000-2019", subtitle="Series tendenciales depuradas", y="Conteo", x="") +
  facet_wrap(~Type,scales="free_y")

ggplot(df_full_melt[which(df_full_melt$Key==c("Count_Prune_Trend")),],
       aes(x=Time, y=Count, colour=Legislature)) + geom_line() + 
  labs(title="Decretos y leyes en Uruguay, 2000-2019", subtitle="Series tendenciales depuradas por legislatura",
       y="Conteo", x="Meses desde el comienzo de la legislatura") +
  facet_wrap(~Type,scales="free_y") +
  scale_color_discrete(name = "Legislatura") + theme(legend.position="bottom")
