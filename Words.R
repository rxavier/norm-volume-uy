load("./Data.RData")

wordfreq <- str_split(unlist(norm[3,])," ") %>% {table(unlist(.))}
wdf <- names(wordfreq) %>% str_remove_all("\\s+") %>%
  cbind.data.frame(as.integer(wordfreq)) 
colnames(wdf) <- c("A","B")
wdf <- subset(wdf,str_length(as.character(wdf$A))>3)
wdf <- wdf[order(-wdf$B),]

#sapply(as.character(wdf[1:2,1]), function(x) sample(str_subset(unlist(norm[3,]),"\bx\b"),10))
