library(httr)
library(rvest)
library(zoo)

# Set defaults and initial values ------------
impo_url <- "https://www.impo.com.uy"
suffix0 <- "/cgi-bin/bases/consultaBasesBS.cgi?tipoServicio=3&realizarconsulta=SI&nuevaconsulta=SI&parlistabases=&nrodocdesdehasta=0-"
suffix1 <- "&numeros=&articulos=&textolibre=&texto1=&campotexto1=TODOS&optexto1=Y&texto2=&campotexto2=TODOS&optexto2=Y&texto3=&campotexto3=TODOS"
suffix2 <- "&fechadiar1=&fechadiar2=&comboBaseTematica=&indexcombobasetematica=0&tema=&nvocabulario=&refinar="

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

type_norm_vec <- c("law"=5, "decree"=6, "resolution"=7, "rule"=11)


# Dates vector function ------------
dates_url <- function(start_date, end_date, date_format) {
  
  date_pub_start <- start_date %>% as.Date(date_format)
  date_pub_end <- end_date %>% as.Date(date_format)
  
  date_df <- as.data.frame(as.Date(seq(as.yearmon(date_pub_start), as.yearmon(date_pub_end), by=1/12), frac=1)) %>%
    `colnames<-` ("End")
  date_df$Start <- date_df$End-as.POSIXlt(date_df$End)$mday + 1
  
  return(paste0("&fechapro1=", str_replace_all(format(date_df$Start, "%d-%m-%Y"),"-","%2F"),
                "&fechapro2=", str_replace_all(format(date_df$End, "%d-%m-%Y"),"-","%2F")))
}


# Base function ------------
request_norm_dates <- function(type, dates) {
  
  type_num <- type_norm_vec[type]
  
  # Define how many documents to fetch by default
  if (type_num==5) {
    number_docs <- 60}
  else if (type_num==6) {
    number_docs <- 150}
  else if (type_num==7) {
    number_docs <- 150}
  else {number_docs <- 10}
  
  # Build URLs, make initial requests and compare error messages
  data <- sapply(dates, function(x) {
    
    month <- str_extract_all(x, "(?<=fechapro2=)[0-9%F]+") %>% str_replace_all("%2F", "-") %>% as.Date("%d-%m-%Y")
    url <- paste0(impo_url, suffix0, number_docs, "&combo1=", type_num, suffix1, x, suffix2)
    request_html <- GET(url, add_headers(headers)) %>% read_html()
    
    check1 <- html_nodes(request_html, ".contenido a") %>% html_text() %>% trimws() %>% toString()
    check2 <- (html_nodes(request_html, "p") %>% html_text() %>% trimws())[2] %>% toString()
    
    # Hit refresh URL if needed and set all values to ".." if no documents are found for a given date
    if (check1 == refresh_msg) {
      html_nodes(request_html, ".contenido a") %>% html_attr("href") %>% {GET(paste0(impo_url,.), add_headers(headers))}
      request_html <- GET(url, add_headers(headers)) %>% read_html()
      print(paste0(format(month, "%m-%Y"), ": URL refreshed"))}
    
    else if (check2 == nodoc_msg) {
      print(paste0(format(month, "%m-%Y"), ": no ",
                   names(type_num), " found"))
      return(list("..", "..", "..", ".."))}
    
    # Get effective number of docs found for selected dates and request again if needed
    tryCatch({
      number_docs_effective <- (html_nodes(request_html, "#divMsg b") %>% html_text() %>% trimws())[1]
      print(paste0(format(month, "%m-%Y"),": ", number_docs_effective, " ",
                   names(type_num), " found"))
      
      if (number_docs_effective>=number_docs) {
        url <- paste0(impo_url, suffix0, number_docs_effective, "&combo1=", type_num, suffix1, x, suffix2)
        request_html <- GET(url, add_headers(headers)) %>% read_html()}
      
      # Parse the data
      raw_table <- html_table(request_html, fill=TRUE) %>%
      {str_split_fixed(.[[1]][["X2"]], "\t|\n", 2)} %>% trimws()
      norm_number <- raw_table[, 1] %>% unlist() %>% trimws()
      norm_title <- str_remove_all(raw_table[, 2], "^(\\(.+?\\))") %>%
        unlist() %>% trimws()
      norm_update <- str_extract_all(raw_table[, 2], "^(\\(.+?\\))") %>%
        unlist() %>% trimws()
      norm_link <- html_nodes(request_html, "a.visitado") %>% html_attr("href") %>% paste0(impo_url, .)
      
      list(norm_number, norm_update, norm_title, norm_link)},
      error=function(e) list(NA, NA, NA, NA))
  })
  
  return(cbind.data.frame(unlist(data[1, ]), unlist(data[2, ]), unlist(data[3, ]), 
                          unlist(data[4, ]), str_to_title(names(type_num)), stringsAsFactors=F) %>%
           `colnames<-` (c("Number", "Type2", "Title", "URL", "Type")))
}


# Retry function for dates that failed for unforeseen reasons ------------
retry_request <- function(df, type, missing) {
  
  complete <- request_norm_dates(type, missing)
  
  if (sum(is.na(complete)) == 0) {
    rbind.data.frame(df[!is.na(df$Number), ], complete, stringsAsFactors=F)}
  else {print("Failed to request every missing date, try again")}
}
