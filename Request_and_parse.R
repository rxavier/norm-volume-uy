request_norm_dates <- function(type_norm, date_list) {
  if (type_norm==5) {
    number_docs <- 20}
  else if (type_norm==6) {
    number_docs <- 50}
  else {
    number_docs <- 100}
  
  sapply(date_list, function(x) {
    month <- str_extract_all(x, "(?<=fechadiar2=)[0-9%F]+") %>% str_replace_all("%2F", "-") %>% as.Date("%d-%m-%Y")
    url <- paste0(impo_url, suffix0, number_docs, "&combo1=", type_norm, suffix1, x, suffix2)
    request_html <- GET(url, add_headers(headers)) %>% read_html()
    check1 <- html_nodes(request_html, ".contenido a") %>% html_text() %>% trimws() %>% toString()
    check2 <- (html_nodes(request_html, "p") %>% html_text() %>% trimws())[2] %>% toString()
    
    if (check1 == refresh_msg) {
      html_nodes(request_html, ".contenido a") %>% html_attr("href") %>% {GET(paste0(impo_url,.), add_headers(headers))}
      request_html <- GET(url, add_headers(headers)) %>% read_html()
      print(paste0(format(month, "%m-%Y"), ": URL refreshed"))}
    else if (check2 == nodoc_msg) {
        print(paste0(format(month, "%m-%Y"), ": no ", tolower(names(type_norm)), " found"))
        return(list(NA, NA, NA))}
    
    number_docs_effective <- (html_nodes(request_html, "#divMsg b") %>% html_text() %>% trimws())[1]
    print(paste0(format(month, "%m-%Y"),": ", number_docs_effective, " ", tolower(names(type_norm)), " found"))
    
    if (number_docs_effective>=number_docs) {
      url <- paste0(impo_url, suffix0, number_docs_effective, "&combo1=", type_norm, suffix1, x, suffix2)
      request_html <- GET(url, add_headers(headers)) %>% read_html()}
    
    norm_number <- html_nodes(request_html, "strong") %>% html_text() %>% trimws()
    norm_text <- html_nodes(request_html, "font") %>% html_text() %>% trimws()
    norm_type <- norm_text[str_detect(norm_text, "\\(Documento|\\(Texto")]
    norm_content <- norm_text[!str_detect(norm_text, "\\(Documento|\\(Texto")]
    list(norm_number, norm_type, norm_content)
  })
}