source("preprocess.R")

# Convert raw body into meaningful constituents
extractElementsFromAnswers <- function(df) {
  
  # Extract the various elements
  listelements <- extractElements(df)
  
  # Extract subject from the 
  df <- extractSubject(df, listelements)
  
  # Extract name following EX
  h <- strsplit(listelements[[3]],"\\s")
  h2 <- splitloop2(h)
  nameEx <- data.frame(h2[[1]])
  
  # remove any non alnumeric characters and @xxx@xxx.xxx string
  nameEx[,1]<-sub("@xxx@xxx.xxx", "", nameEx[,1])
  nameEx[,1]<-gsub("[0-9]", NA, nameEx[,1])
  nameEx[,1]<-gsub("[[:punct:]]", "", toupper(nameEx[,1]))
  df$nameEx<-nameEx[-(nrow(df)),1]
  df$nameEx<-gsub("XXX(.*)","",df$nameEx)
  df$nameEx<-gsub("XXX(.*)","",df$nameEx)
  df$nameEx[df$nameEx=="PROSPECTIVE"|df$nameEx=="HELLO"]<-NA
  
  # extracting other set of names from the initial greetings
  g <- data.frame(listelements[[2]])
  g2 <- parseL(strsplit(listelements[[2]], '[,|;|\\\r|\\\n|:|.]'))
  g3 <- data.frame(g2[[1]])
  
  g3[,1] <- apply(g3, 2, function(x) {
    ifelse(
      grepl("^(Dear|Hi|Hello|Good [Mm]orning|Dera)", x, ignore.case = T) == 1,
      x <- sub("^(Dear|Hi|Hello|Good [Mm]orning|Dera) ", replacement="", x, ignore.case = T),
      x <- NA)
  })
  
  df$nameGreet <- toupper(g3[,1])
  df$nameGreet <- gsub(" (.*)","",df$nameGreet)
  df$nameGreet[df$nameGreet==""|df$nameGreet=="HELLO"]<-NA
  
  df$nameFinal<-df$nameEx
  exnamemissing<-!is.na(df$nameGreet)&is.na(df$nameEx)
  df$nameFinal[exnamemissing]<-df$nameGreet[exnamemissing]
  
  sum(is.na(df$nameFinal) & !is.na(df$nameEx))
  
  df$nameFinal <- sub("^ ", "", df$nameFinal)
  df$nameFinal <- sub(" (.)*", "", df$nameFinal)
  df$nameFinal[df$nameFinal=="HI!"] <- NA
  
  df
}