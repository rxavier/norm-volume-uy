request_norm_dates <- function(type, dates) {
  if (type==5) {
    number_docs <- 60}
  else if (type==6) {
    number_docs <- 150}
  else if (type==7) {
    number_docs <- 150}
  else {number_docs <- 10}
  
  sapply(dates, function(x) {
    month <- str_extract_all(x, "(?<=fechapro2=)[0-9%F]+") %>% str_replace_all("%2F", "-") %>% as.Date("%d-%m-%Y")
    url <- paste0(impo_url, suffix0, number_docs, "&combo1=", type, suffix1, x, suffix2)
    request_html <- GET(url, add_headers(headers)) %>% read_html()
    check1 <- html_nodes(request_html, ".contenido a") %>% html_text() %>% trimws() %>% toString()
    check2 <- (html_nodes(request_html, "p") %>% html_text() %>% trimws())[2] %>% toString()
    
    if (check1 == refresh_msg) {
      html_nodes(request_html, ".contenido a") %>% html_attr("href") %>% {GET(paste0(impo_url,.), add_headers(headers))}
      request_html <- GET(url, add_headers(headers)) %>% read_html()
      print(paste0(format(month, "%m-%Y"), ": URL refreshed"))}
    else if (check2 == nodoc_msg) {
      print(paste0(format(month, "%m-%Y"), ": no ",
                   tolower(names(type_norm)[which(type_norm==type)]), " found"))
      return(list("No norms found", "No norms found", "No norms found", "No norms found"))}
    
    tryCatch({
      number_docs_effective <- (html_nodes(request_html, "#divMsg b") %>% html_text() %>% trimws())[1]
      print(paste0(format(month, "%m-%Y"),": ", number_docs_effective, " ",
                   tolower(names(type_norm)[which(type_norm==type)]), " found"))
      
      if (number_docs_effective>=number_docs) {
        url <- paste0(impo_url, suffix0, number_docs_effective, "&combo1=", type, suffix1, x, suffix2)
        request_html <- GET(url, add_headers(headers)) %>% read_html()}
      
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
}

retry_request <- function(df, type, missing) {
  missing_dates <- missing %>% {row.names(df[.,])}
  request_norm_dates(type, missing_dates)
}