load("./Data.RData")

wordfreq <- str_split(unlist(norm[3,])," ") %>% {table(unlist(.))}
wdf <- names(wordfreq) %>% str_remove_all("\\s+") %>%
  cbind.data.frame(as.integer(wordfreq),stringsAsFactors=F) 
colnames(wdf) <- c("A","B")
wdf <- subset(wdf,str_length(as.character(wdf$A))>3)
wdf <- wdf[order(-wdf$B),]

samples <- unlist(wdf[1:20,1]) %>% paste0("\\b",.,"\\b") %>% 
  sapply(function(x) str_subset(unlist(norm[3,]),x) %>% sample(5))
