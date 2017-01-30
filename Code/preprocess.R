# Script containing helper functions to massage the data in data frames

# Perform preliminary steps just after loading
initLoad <- function(df, colname) {
  
  # Attach an index
  dfcol <- seq(1:nrow(df))
  df$dfcol <- dfcol
  colnames(df)[length(colnames(df))] <- colname
  
  # Create names and character data type for 1st column as they are processed
  # initially as factors
  colnames(df)[1]<-"body"
  df$body<-as.character(df$body)
  df$body<- gsub('\\\\r|\\\\n|\\\\t',' ',df$body)
  
  df
}

initLoad2 <- function(df, colname) {
  
  # Attach an index
  dfcol <- seq(1:nrow(df))
  df$dfcol <- dfcol
  colnames(df)[length(colnames(df))] <- colname
  
  # Create names and character data type for 1st column as they are processed
  # initially as factors
  colnames(df)[1]<-"body"
  df$body<-as.character(df$body)
  df$bodystripped<- gsub('\\\\r|\\\\n|\\\\t',' ',df$body)
  
  df
}

extractElements <- function(df) {
  
  # string split at all occurences of the ^ symbol
  dfsplit <- strsplit(df$body, "\\^")
  
  # assign list to listelements, separate out sub-elements into data frames,
  # and append them to the original dataset
  listelements <- splitloop(dfsplit)
  
  listelements
}

extractSubject <- function(df, listelements) {
  
  # Extract subject
  subject <- data.frame(listelements[[1]])
  
  # if subject begins with a RE: or FW: remove it from the subject vector with a blank string,
  # also get rid of any remaining leading spaces, add to the answers dataframe
  subject[,1] <- sub("^(.*)?RE:|FW:(\\s)+", "", subject[,1], ignore.case = T)
  subject[,1] <- sub("^\\s+", "", subject[,1])
  df$subject <- subject[,1]
  df$subject[df$subject==""]<-NA
  
  df
}

splitloop <- function(inputlist) {
  subj <- rep(NA, times=length(inputlist))
  bod <- rep(NA, times=length(inputlist))
  nam <- rep(NA, times=length(inputlist))
  
  for(i in 1:length(inputlist)) {
    subj[i] <- inputlist[[i]][1]
    bod[i] <- inputlist[[i]][2]
    for(j in 1:length(inputlist[[i]])) {
      if(inputlist[[i]][j]=="EX") {
        nam[i] <- inputlist[[i]][j+1]
        break
      }
    }
  }
  
  return(list(subject=subj, body=bod, nameEX=nam))
}

splitloop2 <- function(inputlist) {
  temp <- rep(0:length(inputlist))
  
  for(i in 1:length(inputlist)) {
    temp[i] <- inputlist[[i]][1]
  }
  
  return(list(nameEx=temp))
}

parseL <- function(splitL) {
  temp <- rep(0,times = length(splitL))
  temp2 <- rep(0,times = length(splitL))
  
  for (i in 1:length(splitL)) {
    temp[i] <- splitL[[i]][1]
    temp2[i] <- splitL[[i]][2]
    print(temp[i])
    print(temp2[i])
  }
  
  return(list(greeting = temp, nongreeting = temp2))
}

nameFromSignature <- function(pattern, colname, df) {
  
  xtrct <- str_extract(df[,1], paste(pattern, "[a-zA-Z]+", sep=""))
  xtrct <- sub(pattern, "", xtrct)
  df$newXtrct <- toupper(xtrct)
  df$newXtrct[df$newXtrct=="MASTER"|df$newXtrct=="PROSPECTIVE"|df$newXtrct=="LINDSAY"|df$newXtrct=="VICTORIA"|df$newXtrct=="ON"|df$newXtrct=="T"|df$newXtrct=="R"]<-NA
  colnames(df) [length(colnames(df))] <- colname
  
  missing <- is.na(df$nameFinal) & !is.na(df[colname])
  df$nameFinal[missing] <- df[missing, colname]
  
  df
}

nameFromSignatureNI <- function(pattern, pattern2, colname, df) {
  
  xtrct <- str_extract(df[,1], paste(pattern, "[a-zA-Z]+", sep=""))
  nonindex <- grep(pattern2, xtrct)
  xtrct[nonindex] <- NA
  xtrct <- sub(pattern, "", xtrct)
  df$newXtrct <- toupper(xtrct)
  df$newXtrct[df$newXtrct=="MASTER"|df$newXtrct=="PROSPECTIVE"|df$newXtrct=="LINDSAY"|df$newXtrct=="VICTORIA"|df$newXtrct=="ON"|df$newXtrct=="T"|df$newXtrct=="R"]<-NA
  colnames(df) [length(colnames(df))] <- colname
  
  missing <- is.na(df$nameFinal) & !is.na(df[colname])
  df$nameFinal[missing] <- df[missing, colname]
  
  df
}

nameFromBody <- function(pattern, colname, df) {
  
  xtrct <- str_extract(df[,1], paste(pattern, "[[:upper:]][a-zA-Z]+", sep=""))
  xtrct <- sub(pattern, "", xtrct)
  df$newXtrct <- toupper(xtrct)
  df$newXtrct[df$newXtrct=="MASTER"|df$newXtrct=="PROSPECTIVE"|df$newXtrct=="LINDSAY"|df$newXtrct=="VICTORIA"|df$newXtrct=="ON"|df$newXtrct=="T"|df$newXtrct=="R"]<-NA
  colnames(df) [length(colnames(df))] <- colname
  
  missing <- is.na(df$nameFinal) & !is.na(df[colname])
  df$nameFinal[missing] <- df[missing, colname]
  
  df
}