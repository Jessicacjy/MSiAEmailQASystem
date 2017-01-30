sentences<-function(df){
  newdf<-data.frame(index=numeric(0), 
                    acol=numeric(0), 
                    sent=character(0))
  newdfindex<-1
  for (i in 1:nrow(df)){
    sentencelist<-strsplit(df$finaltext[i],split="\\. |\\? |\\! ",perl = T)[[1]]
    sentencelist<-sentencelist[sentencelist != ""]
    for(j in 1:length(sentencelist)){
      newdf<-rbind(newdf, 
                   data.frame(index=newdfindex, 
                              acol=df$acol[i], 
                              sent=sentencelist[j]))
      newdfindex<-newdfindex+1
    }
  }
  return(newdf)
}