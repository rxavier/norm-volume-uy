load("./Data.RData")

## Separate every string into words, remove some punctuation and years, calculate frequency
wordfreq <- str_split(unlist(norm[3,])," ") %>% 
  {str_remove_all(unlist(.),"\\.(?![A-Z])|[\",]|[0-9]{4}")} %>%
  {table(unlist(.))}

## Create dataframe based on words and frequencies and order according to frequency
wdf <- names(wordfreq) %>% str_remove_all("\\s+") %>%
  cbind.data.frame(as.integer(wordfreq),stringsAsFactors=F) 
colnames(wdf) <- c("A","B")
wdf <- subset(wdf,str_length(as.character(wdf$A))>3)
wdf <- wdf[order(-wdf$B),]

## Get the words that are repeated more than 100 times and sample 10 norms that match each
samples <- sum(wdf$B>99) %>% {unlist(wdf[1:.,1])} %>% paste0("\\b",.,"\\b") %>% 
  sapply(function(x) str_subset(unlist(norm[3,]),x) %>% sample(10))
colnames(samples) <- wdf[1:ncol(samples),1]

## Manually pick words which will be excluded from the norm count
exclude <- c("SUBGRUPO","GRUPO","CONVENIO","ACUERDO","COLECTIVO","UNIDAD REAJUSTABLE",
             "unidad reajustable","U.R.","UR","U.R.A.","URA","Se fija","Se actualiza","SUSCRITO",
             "ANEXO","DESIGNA", "DESIGNACIÓN","ESCUELA","PARTIDAS","COMISIÓN", "MERCOSUR",
             "MERCADO COMÚN","EMISIÓN","SALARIO MÍNIMO NACIONAL",
             "MONTO MÍNIMO DE LAS JUBILACIONES","INTERÉS NACIONAL", "COMPLEMENTACIÓN","COOPERACIÓN")

save.image("./Data.RData")
