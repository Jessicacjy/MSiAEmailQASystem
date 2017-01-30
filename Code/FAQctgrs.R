faqctgrs<-function(df){
  df$transcript <- ifelse(grepl("transcript", df$finaltext, ignore.case=T), "transcript", NA)
  df$deadline <- ifelse(grepl("deadline", df$finaltext, ignore.case=T), "deadline", NA)
  df$experience<- ifelse(grepl("experi", df$finaltext, ignore.case=T), "experience", NA)
  df$gregmat <- ifelse(grepl("gre |gmat", df$finaltext, ignore.case=T), "gregmat", NA)
  df$recommendation <- ifelse(grepl("recommend|letter", df$finaltext, ignore.case=T), "recommendation", NA)
  df$international <- ifelse(grepl("internat|toefl|ietls", df$finaltext, ignore.case=T), "international", NA)
  df$interest<-ifelse(grepl("interest|inform", df$finaltext, ignore.case=T), "interest", NA)
  df
}



