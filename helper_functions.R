# Read in all txt files from a directory as a list of files
# Returns a list of dataframes
LoadFiles <- function(directory, pattern = "*.txt", n = -1L){
  filenames <- list.files(directory, pattern = pattern, full.names = TRUE)
  shortnames <- list.files(directory, pattern = pattern, full.names = FALSE)
  
  document_list <- list()
  
  for(index in 1:length(filenames)){
    
    #reading
    document <- readLines(filenames[index],
                          n,
                          skipNul = TRUE,
                          encoding = "UTF-8")
    
    #stemming
    document <- stemDocument(document)
    
    #dataframe
    df_ <- tibble(line = 1:length(document),
                  text = document,
                  document = shortnames[index])
    
    # list of dataframes
    document_list[[index]] <- df_
    
  }
  
  document_list
}

# takes dataframe input with col.names "line", "text", "document"
# returns a clean dataframe of tokens
# n should equal 1, 2, or 3. 4-grams and higher not yet desired
CleanTokens <- function(unclean_df, n = 1, filter_df = NULL){
  
  if (n == 1){
      df_ <- unnest_tokens(tbl = unclean_df,
                           output = word,
                           input = text,
                           token = "words",
                           to_lower = TRUE)
      
      # apply filterwords if provided
      if(!(is.null(filter_df))){
        df_ <- df_ %>% anti_join(filter_df, by = "word")
      }
      
    } else {
      df_ <- unnest_tokens(tbl = unclean_df,
                           output = word,
                           input = text,
                           token = "ngrams",
                           n = n,
                           to_lower = TRUE)
      
      # apply filterwords if provided
      if(!(is.null(filter_df))){
        if (n == 2){
          
          df_ <- df_ %>% separate(word,
                                  c("word1", "word2"),
                                  sep = " ")
          
          df_ <- df_ %>%
            anti_join(filter_df, by = c("word1" = "word")) %>%
            anti_join(filter_df, by = c("word2" = "word"))
          
          df_ <- df_ %>%
            unite(word, word1, word2, sep = " ")
          
        } else if (n == 3){
          
          df_ <- df_ %>% separate(word,
                                  c("word1", "word2", "word3"),
                                  sep = " ")
          
          df_ <- df_ %>%
            anti_join(filter_df, by = c("word1" = "word")) %>%
            anti_join(filter_df, by = c("word2" = "word")) %>%
            anti_join(filter_df, by = c("word3" = "word")) 
          
          df_ <- df_ %>%
            unite(word, word1, word2, word3, sep = " ")
          
          }
        }
      
    }

    # remove entries with numbers present
    df_ <- filter(df_, !grepl(".*\\d+.*", word))
    
    # remove a common error
    df_ <- filter(df_, !grepl("^NA$|^NA NA$|^NA NA NA$", word))
    
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