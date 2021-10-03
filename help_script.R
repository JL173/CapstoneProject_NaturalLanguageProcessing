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
      df_ <- anti_join(df_, filter_df, by = "word")
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
          filter(!word1 %in% filter_df$word) %>%
          filter(!word2 %in% filter_df$word)
        
        df_ <- df_ %>%
          unite(word, word1, word2, sep = " ")
        
      } else if (n == 3){
        
        df_ <- df_ %>% separate(word,
                                c("word1", "word2", "word3"),
                                sep = " ")
        
        df_ <- df_ %>%
          filter(!word1 %in% filter_df$word) %>%
          filter(!word2 %in% filter_df$word) %>%
          filter(!word3 %in% filter_df$word)
        
        df_ <- df_ %>%
          unite(word, word1, word2, word3, sep = " ")
        
      }
    }
    
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


setwd("C:/Users/JL/Desktop/Study/Coursera/Johns Hopkins Data Science/10 capstone/CapstoneProject_NaturalLanguageProcessing")

library(tidyverse)
library(tidytext)
library(tm)

data(stop_words)
filter_words <- read.table("text_filter.txt", header=FALSE, sep="", col.names = "word")
filter_words <- full_join(filter_words, stop_words)


#base_files <- "data/en_US/" %>%
#  LoadFiles()

###
# 1
###

full_tidy_dtm_1 <- "data/en_US/" %>%
  LoadFiles() %>%
  lapply(CleanTokens,
         n = 1,
         filter_df = filter_words) %>%
  MergeDTM()



base_files <- LoadFiles("data/en_US")

a1 <- slice_head(base_files[[1]], n = 449644)
a2 <- slice_tail(base_files[[1]], n = 449644)
b <- base_files[[2]]
c1 <- slice_head(base_files[[3]], n = 1180074)
c2 <- slice_tail(base_files[[3]], n = 1180074)

###
# 2 long
###

clean2_a1 <- CleanTokens(a1,
                      n = 2,
                      filter_df = filter_words)

clean2_a2 <- CleanTokens(a2,
                        n = 2,
                        filter_df = filter_words)

clean2_b <- CleanTokens(b,
                        n = 2,
                        filter_df = filter_words)

clean2_c1 <- CleanTokens(c1,
                        n = 2,
                        filter_df = filter_words)

clean2_c2 <- CleanTokens(c2,
                        n = 2,
                        filter_df = filter_words)

full_tidy_dtm_2 <- MergeDTM(
  list(clean2_a1, clean2_a2, clean2_b, clean2_c1, clean2_c2)
)



###
# 3 long
###

clean3_a1 <- CleanTokens(a1,
                      n = 3,
                      filter_df = filter_words)

clean3_a2 <- CleanTokens(a2,
                        n = 3,
                        filter_df = filter_words)

clean3_b <- CleanTokens(b,
                        n = 3,
                        filter_df = filter_words)

clean3_c1 <- CleanTokens(c1,
                        n = 3,
                        filter_df = filter_words)

clean3_c2 <- CleanTokens(c2,
                        n = 3,
                        filter_df = filter_words)

full_tidy_dtm_3 <- MergeDTM(
  list(clean3_a1, clean3_a2, clean3_b, clean3_c1, clean3_c2)
)