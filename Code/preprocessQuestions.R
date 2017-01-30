source("preprocess.R")

extractElementsFromQuestions <- function(df) {
  
  library(stringr)
  
  # extracting names for questions file from the xxx$xxx.xxx string
  qxxxmatch <- str_extract(df[,1], "(\\^)[a-zA-z]{1,20} xxx@xxx.xxx of Science in Analytics")
  qxxxmatch <- sub(" xxx@xxx.xxx of Science in Analytics", "", qxxxmatch)
  qxxxmatch <- sub("\\^", "", qxxxmatch)
  df$nameXXX <- toupper(qxxxmatch)
  df$nameXXX[df$nameXXX=="MASTER"|df$nameXXX=="PROSPECTIVE"|df$nameXXX=="LINDSAY"|df$nameXXX=="VICTORIA"|df$nameXXX=="ON"|df$nameXXX=="T"|df$nameXXX=="R"]<-NA
  
  # Extract the various elements
  listelements <- extractElements(df)
  
  # Extract subject from the 
  df <- extractSubject(df, listelements)
  
  # returning to the names, we assign nameXXX to nameFinal
  df$nameFinal <- df$nameXXX
  
  # Extract meaningful names from the body
  # We do this in various ways: Signature, greetings & sentences within the body
  # that tend to have names
  df <- nameFromSignature("[Rr]egard(s)?[^[:alnum:]]*(\\\\r\\\\n[ -]*)+[^[:alnum:]]*",
                          "nameREGARD", df)
  
  df <- nameFromSignature("[Bb]est[^[:alnum:]]*(\\\\r\\\\n[ -]*)+[^[:alnum:]]*",
                          "nameBEST", df)
  
  df <- nameFromSignature("[Ss]incerely[^[:alnum:]]*(\\\\r\\\\n[ -]*)+[^[:alnum:]]*",
                          "nameSINCE", df)
  
  df <- nameFromSignature("name is ", "nameNAMEIS", df)
  
  df <- nameFromSignatureNI("[Tt]hank(s)?( )?(you)?[^[:alnum:]]*(\\\\r\\\\n[ -]*)+[^[:alnum:]]*",
                            "[Oo]n$|[Bb]est$|[Rr]egard(s)?$|[Ss]incerely$|[Ww]arm$|[Ss]ent$|[Ff]eb$|[Nn]ame$|[Ss]ee$|[Tt]hank$|[Kk]indest$|[Aa]nother$|[Hh]ave$",
                            "nameTHANK", df)
  
  df <- nameFromSignature("[Ww]ishes?[^[:alnum:]]*(\\\\r\\\\n[ -]*)+[^[:alnum:]]*",
                          "nameWISH", df)
  
  df <- nameFromSignatureNI("[Aa]gain[^[:alnum:]]*(\\\\r\\\\n[ -]*)+[^[:alnum:]]*",
                            "[Oo]n$|[Bb]est$|[Rr]egard(s)?$|[Ss]incerely$|[Ww]arm$|[Ss]ent$|[Ff]eb$|[Nn]ame$|[Ss]ee$|[Tt]hank(s)?$|[Kk]indest$|[Aa]nother$|[Hh]ave$|The$|Respectfully$",
                            "nameAGAIN", df)
  
  df <- nameFromSignature("[Ff]rom: ", "nameFROM", df)
  df$nameFROM[df$nameFROM == "XXX"] <- NA
  df$nameFROM[df$nameFROM=="MASTER"|df$nameFROM=="PROSPECTIVE"|df$nameFROM=="LINDSAY"|df$nameFROM=="VICTORIA"|df$nameFROM=="ON"|df$nameFROM=="T"|df$nameFROM=="R"]<-NA
  
  missing <- is.na(df$nameFinal) & !is.na(df$nameFROM)
  df$nameFinal[missing] <- df$nameFROM[missing]
  
  df <- nameFromBody("This is ", "nameTHISIS", df)
  
  qnamedash <- str_extract(df[,3], "- [[:upper:]][a-zA-Z]* [[:upper:]][a-zA-Z]*$")
  qnamedash[qnamedash == "- Application Process" | qnamedash == "- Jinan University" | 
              qnamedash == "- Prospective Applicant" | qnamedash == "- Northwestern University" |
              qnamedash == "- Analytics Application" | qnamedash == "- Online Application" |
              qnamedash == "- Online Application" | qnamedash == "- MS Analytics"] <- NA
  qnamedash <- sub("- ", "", qnamedash)
  qnamedash <- sub(" (.)*", "", qnamedash)
  df$nameDASH <- toupper(qnamedash)
  df$nameDASH[df$nameDASH=="MASTER"|df$nameDASH=="PROSPECTIVE"|df$nameDASH=="LINDSAY"|df$nameDASH=="VICTORIA"|df$nameDASH=="ON"|df$nameDASH=="T"|df$nameDASH=="R"]<-NA
  missing <- is.na(df$nameFinal) & !is.na(df$nameDASH)
  df$nameFinal[missing] <- df$nameDASH[missing]
  
  df <- nameFromBody("I am ", "nameIAM", df)
  
  qnamerich <- str_extract(df[,1], "(\\\\r\\\\n)(\\^)(.)* xxx@xxx.xxx L Richmond")
  qnamerich <- sub("(\\\\r\\\\n)(\\^)","", qnamerich)
  qnamerich <- sub(" (.)*", "", qnamerich)
  qnamerich <- sub("[^[:alnum:]]","", qnamerich)
  df$nameRICH <- toupper(qnamerich)
  df$nameRICH[df$nameRICH=="MASTER"|df$nameRICH=="PROSPECTIVE"|df$nameRICH=="LINDSAY"|df$nameRICH=="VICTORIA"|df$nameRICH=="ON"|df$nameRICH=="T"|df$nameRICH=="R"]<-NA
  missing <- is.na(df$nameFinal) & !is.na(df$nameRICH)
  df$nameFinal[missing] <- df$nameRICH[missing]
  
  qtranscript <- str_extract(df[,3], "^Transcripts- [a-zA-Z]+")
  qtranscript <- sub("^Transcripts- ", "",qtranscript)
  df$nameTRANSCRIPT  <-toupper(qtranscript)
  missing <- is.na(df$nameFinal) & !is.na(df$nameTRANSCRIPT)
  df$nameFinal[missing] <- df$nameRICH[missing]  
  
  df$nameFinal <- sub("[^a-zA-Z]", "", df$nameFinal)
  
  df
}