library(plyr)

# Extract timestamps
extract_timestamp_count <- function(text) {
  extraction_result <- 
    gregexpr("On [A-Z][a-z][a-z], [A-Z][a-z][a-z] [0-9]+, 20[0-9][0-9] at [0-9]+:[0-9]+ [AP]M", text)
  
  ifelse(extraction_result[[1]][1] == -1, 0, length(extraction_result[[1]]))
}

mergeAndMatch <- function(q_df, a_df) {

  # Add count of timetamps for questions and answers
  q_df$tscount <- sapply(q_df$body, extract_timestamp_count)
  a_df$tscount <- sapply(a_df$body, extract_timestamp_count)
  
  # Do a full outer join of the questions and answers
  mergetable <- merge(q_df, a_df, by = c("nameFinal", "subject", "tscount"), all.x = T, all.y = T)

  # Create the key column - its a composite key (name, subject, ts_count)
  mergetable$name_sub_tscount <- paste(mergetable$nameFinal, mergetable$subject, mergetable$tscount, sep = ' - ')

  mergetable  
}
