# read in input files, dedupe lines, and assign them as data frame variables 

qdoc<- readLines('anonymized_questions_final.csv')
adoc<- readLines('anonymized_answers_final.csv')

qdoc<-unique(qdoc)
adoc<-unique(adoc)

q<- data.frame(qdoc)
a<-data.frame(adoc)

########################################################################

# preprocess question and answer emails

source("preprocess.R", echo = FALSE)

q <- initLoad2(q, "qcol")
a <- initLoad(a, "acol")

source("preprocessQuestions.R", echo = FALSE)

q <- extractElementsFromQuestions(q)

source("preprocessAnswers.R", echo = FALSE)

a <- extractElementsFromAnswers(a)

# Number of emails that don't have any names and subjects identifedidentified

# 102 missing names from question emails
sum(is.na(q$nameFinal))
# 5 missing subjects from quesiton emails
sum(is.na(q$subject))

# 13 missing names from answer emails
sum(is.na(a$nameFinal))
# 4 missing subjects from answer emails
sum(is.na(a$subject))

########################################################################

# Merge and match the questions to the answers

source("mergeAndMatch.R", echo = FALSE)

mergetable <- mergeAndMatch(q, a)

# Collapse each group to map to question indices and answers indices

library(NLP)
mergetablered <- ddply(mergetable, .(name_sub_tscount), summarize,
                       qcol = strsplit(as.String(unique(qcol)), "\n"), 
                       acol = strsplit(as.String(unique(acol)), "\n"))

mergetablered$qcol[mergetablered$qcol=="NA"]<-NA
mergetablered$acol[mergetablered$acol=="NA"]<-NA

########################################################################

# final text for body of email
# only keep body after subject with token ^ and before xxx@xxx, which starts previous email chains

source("finaltextprocessing.R",echo = FALSE)

q<-q_final_text(q)
a<-a_final_text(a)

########################################################################

# make word clouds for top 100 substantive words in subjects and body
# of questions and answer emails

source("wordGen.R", echo = FALSE)

wordGen(q$finaltext, 2.3)
wordGen(q$subject, 3)

wordGen(a$finaltext,2.3)
wordGen(a$subject,3.3)

########################################################################

# corpus -> dtm 

source("corpus_processing.R",echo = FALSE)

qdtmnoweight<-create_corptodtm(q$finaltext, F)
adtmnoweight<-create_corptodtm(a$finaltext, F)

qdtmweight<-create_corptodtm(q$finaltext,T)

# create histogram of number of processed words (terms) versus email frequency (documents) from the Document Term Matrices

hist(rowSums(qdtmnoweight), breaks=40, xlab="Number of Words", main="Length of Question Emails")
hist(rowSums(adtmnoweight), breaks=30, xlab="Number of Words", main="Length of Answer Emails", ylim=c(0,300))

########################################################################

# clustering to get the top 10 keywords from 20 clusters and the size of each cluster

nclust=20

clust <- kmeans(qdtmweight, nclust, iter.max = 20)

# save clusters for later
clusters <- clust$cluster

# make dataframe of cluster centers
centers <- as.data.frame(clust$centers)

# first set the number of clusters
n = max(clusters)

# write a function that inputs cluser number k (e.g. k = 2) outputs the top 10 words
top10words<- function(k){
  theta.k <- centers[k,] # define theta-k, i.e.  row k of cluster centers dataframe
  theta.notk <- colSums(centers[-(k),])/(n-1) # define theta-not-k, i.e. rows not-k of cluster centers divided by number of clusters - 1)
  diffk <- as.data.frame(theta.k - theta.notk) # define difference diffk
  return(colnames(diffk[,order(diffk,decreasing=TRUE)][1:10])) # order decreasing, take top 10
}

# set up a matrix to contain data
keywords<- matrix(NA, nrow=10, ncol=n)

# fill it up
for (i in 1:nclust){
  keywords[,i] <- top10words(i)
}

# set up a matrix to contain data
keywords<- matrix(NA, nrow=10, ncol=n)

# fill it up
for (i in 1:nclust){
  keywords[,i] <- top10words(i)
}

keywords <- data.frame((keywords))

keywords
table(clusters)

write.csv(keywords, "keywords.csv")

########################################################################

# Create FAQ categories 

source("FAQctgrs.R",echo = FALSE)

q<-faqctgrs(q)
a<-faqctgrs(a)

faq<-colSums(!is.na(q[,c("transcript", "deadline", "experience", 
                         "gregmat", "recommendation", 
                         "international","interest")]))

# 786 67% have at least FAQ

atlstonenonna<-function(){
  countNA=0
  for(i in 1:nrow(q)){
    if(all(is.na(q[i,c("transcript", "deadline", "experience", 
                "gregmat", "recommendation", 
                "international","interest")]))){
    countNA=countNA+1
    }
  }
  nrow(q)-countNA
}
  

atlstonenonna()

faq<-colSums(!is.na(q[,c("transcript", "deadline", "experience", 
                          "gregmat", "recommendation", 
                          "international","interest")]))

x<-barplot(faq, main="Count of FAQs",ylab="Number of Emails", ylim=c(0,350), axisnames = FALSE)
text(x, par("usr")[3], labels = names(faq), srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)

########################################################################

# Extract all the sentences from answers

source("answersent.R",echo = FALSE)

# answer index and sentence

answsentcs<-sentences(a)

# frequency of sentence and sentence itself (no index)

freqansrsentcs <- count(answsentcs, c('sent'))

# take out non sensical answer sentences with high frequency and doesn't answer any questions

freqansrsentcs <-freqansrsentcs [-c(7,8,9,27,28,6,4,16,305,55,113,266,132, 141, 434 ),]

# all answers here. the most frequent are template answers

write.csv(freqansrsentcs,"freqansrsentcs.csv")

source("ctgryrspns.R",echo = FALSE)

# create csvs for candidate answers

extractAnswerSentences(a, "transcript")
extractAnswerSentences(a, "deadline")
extractAnswerSentences(a, "experience")
extractAnswerSentences(a, "gregmat")
extractAnswerSentences(a, "recommendation")
extractAnswerSentences(a, "international")
extractAnswerSentences(a, "interest")







