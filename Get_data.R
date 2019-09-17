library(stringr)

# Load functions
source('Request_and_parse.R')

# Produce dataframe for specified norm between dates
df_norms <- function(type, start, end, date_format="%d-%m-%Y", write=FALSE) {
  
  dates <- dates_url(start_date=start, end_date=end, date_format=date_format)
  
  df_aux <- request_norm_dates(type, dates)
  
  missing <- which(is.na(df_aux$Number), arr.ind=TRUE)
  missing_dates <- missing %>% {row.names(df_aux[. , ])}
  
  if (length(missing) > 0) {
    df_norms_comp <- retry_request(df_aux, type, missing_dates)
  } else {df_norms_comp <- df_aux}
  
  df_norms_comp["Month"] <- rownames(df_norms_comp) %>% str_extract_all("(?<=fechapro2=)[0-9%F]+") %>%
    str_replace_all("%2F", "-") %>% as.Date("%d-%m-%Y")
  df_comp_nodupl <- df_norms_comp[!duplicated(df_norms_comp[, 1]) | !duplicated(df_norms_comp[, 6]), ] %>% 
  {.[order(.$Type, .$Month), ]} %>% 
  {.[, c("Month", "Type", "Number", "Title", "Type2", "URL")]} %>%
  {.[!(.$Title==""), ]} %>% `rownames<-` (NULL)
  
  if (write==TRUE) {
    write.csv(df_comp_nodupl, paste0("Data/", type, "_", df_comp_nodupl$Month[1], "_",
                                     df_comp_nodupl$Month[nrow(df_comp_nodupl)], ".csv"), row.names=FALSE)}
  
  return(df_comp_nodupl)
}


# Update function
update_request <- function(df, type, end, date_format="%d-%m-%Y", write=FALSE) {
  
  start <- df[order(df$Month), ] %>% .[[nrow(.), 1]] + 1
  
  df_update <- df_norms(type, start, end, date_format=date_format, write=FALSE)
  df_comp_nodupl <- rbind.data.frame(df, df_update, stringsAsFactors=F)
  
  if (write==TRUE) {
    write.csv(df_comp_nodupl, paste0("Data/", type, "_", df_comp_nodupl$Month[1], "_",
                                     df_comp_nodupl$Month[nrow(df_comp_nodupl)], ".csv"), row.names=FALSE)}
  
  return(df_comp_nodupl)
}

# Bind laws and decrees dataframes, add dates, drop duplicates and export to .csv
df_comp <- rbind.data.frame(df_laws_comp, df_decrees_comp, stringsAsFactors=F) 
