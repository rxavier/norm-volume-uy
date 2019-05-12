library(httr)
library(rvest)
library(stringr)
library(zoo)

# Load function
source('Request_and_parse.R')

# Set defaults and initial values
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

type_norm_vec <- c("Law"=5, "Decree"=6, "Resolution"=7, "Rule"=11)

# Create start and end dates to be used in each function call
dates_url <- function(start_date, end_date) {
  
  date_pub_start <- start_date %>% as.Date("%d-%m-%Y")
  date_pub_end <- end_date %>% as.Date("%d-%m-%Y")
  
  date_df <- as.data.frame(as.Date(seq(as.yearmon(date_pub_start), as.yearmon(date_pub_end), by=1/12), frac=1)) %>%
    `colnames<-` ("End")
  date_df$Start <- date_df$End-as.POSIXlt(date_df$End)$mday + 1
  
  paste0("&fechapro1=", str_replace_all(format(date_df$Start, "%d-%m-%Y"),"-","%2F"),
         "&fechapro2=", str_replace_all(format(date_df$End, "%d-%m-%Y"),"-","%2F"))
}

# Function for producting dataframes per norm type
df_norms <- function(type, dates_url) {
  df_aux <- request_norm_dates(type, dates_url)
  missing <- which(is.na(df_aux$Number), arr.ind=TRUE)
  if (length(missing_laws) > 0) {
    df_norms_comp <- retry_request(df_aux, type, missing)
  } else {df_norms_comp <- df_aux}
}

# Bind laws and decrees dataframes, add dates, drop duplicates and export to .csv
df_comp <- rbind.data.frame(df_laws_comp, df_decrees_comp, stringsAsFactors=F) %>% 
  {.[!(.$Title==""), ]}
df_comp["Month"] <- rownames(df_comp) %>% str_extract_all("(?<=fechapro2=)[0-9%F]+") %>%
  str_replace_all("%2F", "-") %>% as.Date("%d-%m-%Y")
df_comp_nodupl <- df_comp[!duplicated(df_comp[, 1]) | !duplicated(df_comp[, 6]), ] %>% 
{.[order(.$Type, .$Month), ]} %>% 
  {.[, c("Month", "Type", "Number", "Title", "Type2", "URL")]} %>%`rownames<-` (NULL)

write.csv(df_comp_nodupl, "Data/Data_1985_2018.csv", row.names=FALSE)
