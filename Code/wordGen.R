
library(tm)
library(SnowballC)
library(wordcloud)

#The library function removeWords does not work, so this is a custom remove words function 
removeWordsCustom <- function(x, words) {
  gsub(sprintf("\\b(%s)\\b", paste(sort(words, decreasing = TRUE),
                                   collapse = "|")), "", x)
}

#Jessica's word cloud function, takes in dataframe, maximum number of words to consider, 
#numer of words to display in the word cloud and option to display the wordCloud
#This function  will generate a histogram and optional wordcloud
wordGen <- function(df, scale){
  # , main='histogram'
  df<- as.factor(df)
  df <- Corpus(VectorSource(df))
  # df <- tm_map(df, PlainTextDocument)
  df <- tm_map(df, content_transformer(tolower))
  df <- tm_map(df, removePunctuation)
  df <- tm_map(df, removeNumbers)
  df = tm_map(df, stemDocument)
  df = tm_map(df, stripWhitespace)
  
  rmWords <-   c('the','this','From','Hi','Hello','Dear','xxxxxxxxx','email','master','msia',
                 'fwd','northwestern','rn','sciencernrn2145','8474918005','program','analyt',
                 'science','northwestern','univers','question','^applicat','\\S+[0-9]+\\S+', 
                 'regard','thank','appli','applic','scienc','group','master','administrative',
                 'analyticsonuexchouexchang',stopwords('english'),'administr',
                 'fydibohfspdltcnrecipientscnomaeexnnnnnnnyellow','xxxxxxxxxxxxxxxxxx',
                 'montanari','mccormick','director','manag','lindsay','sheridan','please',
                 'xxxxxxxxxxxxxxxxxxxxxxxxxxx','please','victoria','xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')
  df <- tm_map(df, content_transformer(removeWordsCustom), rmWords)

  wordcloud(df, max.words = 100, random.order = FALSE,colors=brewer.pal(6, "Dark2"),scale=c(scale,.7))
  }  

