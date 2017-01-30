
q_final_text<-function(df){
  mydata<-data.frame(df$bodystripped)
  colnames(mydata) <- c('body')
  mydata$question <- NA
  #extract subject and put the rest of the email in the body
  question <- strsplit(as.character(mydata$body), '\\^')
  
  for (i in 1:length(question)){
    j = 2
    ques= question[[i]][j]
    while (is.null(ques)){
      j = j+ 1
      ques= question[[i]][j]
    }
    mydata$question[i]<-ques
  }
  
  question <- strsplit(as.character(mydata$question), 'xxx@xxx')
  
  for (i in 1:length(question)){
    j = 1
    ques= question[[i]][j]
    while (is.null(ques)){
      j = j+ 1
      ques= question[[i]][j]
    }
    mydata$question[i]<-ques
  }
  
  mydata$question<-gsub("\"", "", mydata$question)
  mydata$question<-gsub("[\\]", "", mydata$question)
  df$finaltext<-mydata$question
  df
}

a_final_text<-function(df){
  mydata <- data.frame(df$body)
  colnames(mydata) <- c('body')
  mydata$answer <- NA
  #extract subject and put the rest of the email in the body
  answer <- strsplit(as.character(mydata$body), '\\^')

  for (i in 1:length(answer)){
    j = 2
    ans= answer[[i]][j]
    while (is.null(ans)){
      j = j+ 1
      ans= answer[[i]][j]
    }
    mydata$answer[i]<-ans
  }

  answer <- strsplit(as.character(mydata$answer), 'xxx@xxx')

  for (i in 1:length(answer)){
    j = 1
    ans= answer[[i]][j]
    while (is.null(ans)){
      j = j+ 1
      ans= answer[[i]][j]
    }
    mydata$answer[i]<-ans
  }

  mydata$answer<-paste(mydata$answer, ".")
  mydata$answer<-gsub("\"", "", mydata$answer)
  mydata$answer<-gsub("[\\]", "", mydata$answer)
  mydata$answer<-gsub("[[:space:]]+", " ", mydata$answer)
  df$finaltext<-mydata$answer
  df
}