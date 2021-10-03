# Read in all txt files from a directory as a list of files
# Returns a list of dataframes

LoadFiles <- function(directory, pattern = "*.txt", n = -1L){
  filenames <- list.files(directory, pattern = pattern, full.names = TRUE)
  shortnames <- list.files(directory, pattern = pattern, full.names = FALSE)
  
  document_list <- list()
  
  for(index in 1:length(filenames)){
    
    document <- readLines(filenames[index],
                          n,
                          skipNul = TRUE,
                          encoding = "UTF-8")
    
    df_ <- tibble(line = 1:length(document),
                  text = document,
                  document = shortnames[index])
    
    document_list[[index]] <- df_
    
  }
  
  document_list
}

# takes dataframe input with col.names "line", "text", "document"
# returns a clean dataframe of tokens
CleanTokens <- function(unclean_df, n = 1, filter_df = NULL){
  
  if (n == 1){
      df_ <- unnest_tokens(tbl = unclean_df,
                           output = word,
                           input = text,
                           token = "words",
                           to_lower = TRUE)
    } else {
      df_ <- unnest_tokens(tbl = unclean_df,
                           output = word,
                           input = text,
                           token = "ngrams",
                           n = n,
                           to_lower = TRUE)
    }
    
    
    # apply filterwords if provided
    if(!(is.null(filter_df))){
      df_ <- anti_join(df_, filter_df, by = "word")
    }
    
    # remove entries with numbers present
    df_ <- filter(df_, !grepl(".*\\d+.*", word))
    df_

}

# takes a list of dataframes from CleanTokens
# returns a combined tidy dataframe with frequencies
MergeDTM <- function(document_list){
  
  df_ <- document_list[[1]]
  
  for (index in 2:length(document_list)){
    df_ <- df_ %>% rbind(document_list[[index]])
    
  }
  
  df_ <- df_ %>% count(document, word, sort=TRUE)
  
}