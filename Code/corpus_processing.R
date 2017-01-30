#this function takes in a data frame and a custom list of words to remove and outputs a cleaned corpus
library(SnowballC)
library(tm)

#The library function removeWords does not work, so this is a custom remove words function 
removeWordsCustom <- function(x, words) {
  gsub(sprintf("\\b(%s)\\b", paste(sort(words, decreasing = TRUE),
                                   collapse = "|")), "", x)
}

rmWords <- c(stopwords('english'), 'victoria', 'northwestern', 'engin', 'school', 'northwestern', 'scienc',
             'master', 'analyt','univers','graduat','program','student','lindsay', 'evanston','mccormick',
             'msia','richmond','india')

# creates corpus into a converts to a document term matrix 
# which is weighted or unweighted depending on your argument paramater
create_corptodtm<-function(inputvector, weighting){
  
  corp<-Corpus(VectorSource(inputvector))
  corp <- tm_map(corp, content_transformer(tolower))
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, removeNumbers)
  corp = tm_map(corp, stemDocument)
  corp = tm_map(corp, stripWhitespace)
  corp <- tm_map(corp, content_transformer(removeWordsCustom), rmWords)
  
  if(weighting==F){
    dtm = DocumentTermMatrix(corp)
  }
  if(weighting==T){
    dtm = DocumentTermMatrix(corp,control = list(weighting = function(x) weightTfIdf(x, normalize = F)))
    dtm<-removeSparseTerms(dtm, .95)
  }
  dtm.m=as.matrix(dtm)
  dtm.m
}
  







  
  
