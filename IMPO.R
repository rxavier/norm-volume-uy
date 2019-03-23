library(httr)
library(rvest)
library(stringr)
library(zoo)

# Load function
source('Request_and_parse.R')

# Set defaults and initial values
impo_url <- "https://www.impo.com.uy"
suffix0 <- "/cgi-bin/bases/consultaBasesBS.cgi?tipoServicio=3&realizarconsulta=SI&nuevaconsulta=SI&parlistabases=&nrodocdesdehasta=0-"
type_norm_vec <- c("Laws"=5, "Decrees"=6, "Resolutions"=7, "Rules"=11)
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

# Create start and end dates to be used in each function call
date_pub_start <- "01-12-2018" %>% as.Date("%d-%m-%Y")
date_pub_end <- "01-01-2019" %>% as.Date("%d-%m-%Y")
date_df <- as.data.frame(as.Date(seq(as.yearmon(date_pub_start), as.yearmon(date_pub_end), by=1/12), frac=1)) %>%
  `colnames<-` ("End")
date_df$Start <- date_df$End-as.POSIXlt(date_df$End)$mday + 1
dates_url <- paste0("&fechapro1=", str_replace_all(format(date_df$Start, "%d-%m-%Y"),"-","%2F"),
                    "&fechapro2=", str_replace_all(format(date_df$End, "%d-%m-%Y"),"-","%2F"))

# Run function for laws and decrees
type_norm <- type_norm_vec["Laws"]
flat_df_laws <- request_norm_dates(type_norm, dates_url)
missing_laws <- which(is.na(flat_df_laws$Number), arr.ind=TRUE)
if (length(missing_laws) > 0) {
  complete_laws <- retry_request(flat_df_laws, type_norm, missing_laws)
  flat_df_laws <- rbind.data.frame(flat_df_laws[!is.na(flat_df_laws$Number), ], complete_laws)
}
  
type_norm <- type_norm_vec["Decrees"]
flat_df_decrees <- request_norm_dates(type_norm, dates_url)
missing_decrees <- which(is.na(flat_df_decrees$Number), arr.ind=TRUE)
if (length(missing_decrees) > 0) {
  complete_decrees <- retry_request(flat_df_decrees, type_norm, missing_decrees)
  flat_df_decrees <- rbind.data.frame(flat_df_decrees[!is.na(flat_df_decrees$Number), ], complete_decrees)
}

flat_df <- rbind.data.frame(flat_df_laws, flat_df_decrees)
flat_df["month"] <- rownames(flat_df) %>% str_extract_all("(?<=fechapro2=)[0-9%F]+") %>%
  str_replace_all("%2F", "-") %>% as.Date("%d-%m-%Y")
flat_df_nodup <- flat_df[!duplicated(flat_df[, 1]), ] %>% `rownames<-` (NULL)
