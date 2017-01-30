# Extract answers that have been labeled to a particular category
extractAnswersForCategory <- function(df, category) {
  categorizedAnswers <- data.frame()
  colIndex <- match(category, colnames(df))
  bodyIndex <- match("finaltext", colnames(df))
  acolIndex <- match("acol", colnames(df))
  
  categorizedAnswers <- rbind(categorizedAnswers,
                              df[!is.na(df[,colIndex]), c(acolIndex,bodyIndex)])
  
  return(categorizedAnswers)
}

library(xlsx)

# Extract most frequent answer sentences labeled for a particular category
extractAnswerSentences <- function(df, category) {
  categoryAnswers <- extractAnswersForCategory(a, category)
  categorySentences <- sentences(categoryAnswers)
  categorySentences<-categorySentences[(categorySentences$sent %in% freqansrsentcs$sent),]
  # write.csv(categorySentences, paste("ansSentences/", category, "Sentences.csv", sep = ""))
  categoryFreq <- count(categorySentences, var = "sent")
  categoryFreq <- categoryFreq[order(-categoryFreq$freq),]
  write.xlsx(categoryFreq, paste("ansSentences/", category, "Freq.xlsx", sep = ""))
}