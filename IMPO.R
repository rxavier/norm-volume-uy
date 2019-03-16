library(httr)
library(rvest)
library(stringr)
library(zoo)

# Set defaults and initial values
impo_url <- "https://www.impo.com.uy"
suffix0 <- "/cgi-bin/bases/consultaBasesBS.cgi?tipoServicio=3&realizarconsulta=SI&nuevaconsulta=SI&parlistabases=&nrodocdesdehasta=0-"
type_norm_vec <- c("Leyes"=5, "Decretos"=6, "Resoluciones"=7, "Reglamentos"=11)
suffix1 <- "&numeros=&articulos=&textolibre=&texto1=&campotexto1=TODOS&optexto1=Y&texto2=&campotexto2=TODOS&optexto2=Y&texto3=&campotexto3=TODOS"
suffix2 <- "&fechapro1=&fechapro2=&comboBaseTematica=&indexcombobasetematica=0&tema=&nvocabulario=&refinar="
refresh_msg <- "Haga clic aqui para ingresar nuevamente al sistema."
nodoc_msg <- "Su búsqueda no produjo ningún documento."
headers <- c("Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8",
             "Accept-Encoding"="gzip, deflate, br",
             "Accept-Language"="es-UY,es;q=0.9,en-US;q=0.8,en;q=0.7,es-419;q=0.6",
             "Cache-Control"="max-age=0",
             "Connection"="keep-alive",
             "Cookie"="__utma=175831610.746919901.1511621124.1511621124.1511621124.1; usrts_3=anonimo; idsesionanonimo=z7lLlTGP",
             "DNT"="1",
             "Host"="www.impo.com.uy",
             "Upgrade-Insecure-Requests"="1",
             "User-Agent"="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/72.0.3626.121 Safari/537.36")

# Create start and end dates to be used in each function call
date_pub_start <- "01-03-1985" %>% as.Date("%d-%m-%Y")
date_pub_end <- "01-02-2019" %>% as.Date("%d-%m-%Y")
date_df <- as.data.frame(as.Date(seq(as.yearmon(date_pub_start), as.yearmon(date_pub_end), by=1/12), frac=1)) %>%
  `colnames<-` ("End")
date_df$Start <- date_df$End-as.POSIXlt(date_df$End)$mday + 1
dates_url <- paste0("&fechadiar1=", str_replace_all(format(date_df$Start, "%d-%m-%Y"),"-","%2F"),
                    "&fechadiar2=", str_replace_all(format(date_df$End, "%d-%m-%Y"),"-","%2F"))

type_norm <- type_norm_vec["Leyes"] # THIS IS TEMPORARY UNTIL LOOP IS IMPLEMENTED
date <- dates_url[length(dates_url)-2] # THIS IS TEMPORARY UNTIL LOOP IS IMPLEMENTED

# Build URL to be queried with only one document
number_docs <- 1
url <- paste0(impo_url, suffix0, number_docs, "&combo1=", type_norm, suffix1, date, suffix2)

request_html <- GET(url, add_headers(headers)) %>% read_html()
check1 <- html_nodes(request_html, ".contenido a") %>% html_text() %>% trimws() %>% toString()
check2 <- (html_nodes(request_html, "p") %>% html_text() %>% trimws())[2] %>% toString()

# If request text has error message asking for refresh, ping the refresh link and request again
if (check1 == refresh_msg) {
  html_nodes(request_html, ".contenido a") %>% html_attr("href") %>%
  {GET(paste0(impo_url,.), add_headers(headers))}
  request_html <- GET(url, add_headers(headers)) %>% read_html()
} else {
  print("No refresh necessary")
  if (check2 == nodoc_msg) print("No documents found for the selected dates")
}

# Get available documents for month and type of document selected and rerun
number_docs <- (html_nodes(request_html, "#divMsg b") %>% html_text() %>% trimws())[1]
url <- paste0(impo_url, suffix0, number_docs, "&combo1=", type_norm, suffix1, date, suffix2)
request_html <- GET(url, add_headers(headers)) %>% read_html()

norm_number <- html_nodes(request_html, "strong") %>% html_text() %>% trimws()
norm_text <- html_nodes(request_html, "font") %>% html_text() %>% trimws()
norm_type <- norm_text[str_detect(norm_text, "\\(Documento|\\(Texto")]
norm_content <- norm_text[!str_detect(norm_text, "\\(Documento|\\(Texto")]
month <- str_extract_all(date, "(?<=fechadiar2=)[0-9%F]+") %>% str_replace_all("%2F", "-") %>% as.Date("%d-%m-%Y")
norm_raw_df <- cbind.data.frame(month, norm_number, norm_type, norm_content)
norm_df <- norm_raw_df[!duplicated(norm_raw_df$norm_number), ]
                                            