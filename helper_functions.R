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


# split dataframe for efficiency
SplitDF <- function(df, size = 5e6){
  #5e6 = 50mb
  chunks <- object.size(df) %>%
    as.numeric()/size
  chunks <- as.integer(chunks +1)
  
  N <- as.integer(nrow(df) / chunks) + 1
  
  df_chunks <- split(df, (as.numeric(rownames(df)) - 1) %/% N)
  df_chunks
}


# takes list of lists of dataframes
# turns into one list of dataframes
# more efficient memory tasking for large dataframes
UnlistDF <- function(df_list, size = 5e6){
  list <- lapply(df_list, SplitDF, size) %>%
    unlist(recursive = F)
  list
}



## Parts Of Speech annotator for a string
POSstring <- function(string){
  initial_result = string %>% 
    annotate(list(Maxent_Sent_Token_Annotator(),
                  Maxent_Word_Token_Annotator())) %>% 
    annotate(string, Maxent_POS_Tag_Annotator(), .) %>% 
    subset(type=='word') 
  
  sapply(initial_result$features , '[[', "POS") %>%
    paste(collapse = " ")
}

## POS summary
## Takes a dataframe, calculates POS tags and summarises

POSsumDF <- function(df){
  options(java.parameters = "-Xmx8000m")
  
  df["text"] <- lapply(df["text"], POSstring)
  
  df_pos <- CleanTokens(df, n = 1) %>%
    group_by(document, word) %>%
    summarise(total = n())
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


# creates summary data of the corpus
# total word count
# total unique word count
# total line count
# min, mean, max, range, sd number of words per line
SummaryDoc <- function(doc_list, plot = FALSE){
  
  df_ <- doc_list[[1]]
  for (index in 2:length(doc_list)){
    df_ <- rbind(df_, doc_list[[index]])
  }
  
  df_ <- df_ %>% mutate(n_words = sapply(strsplit(text, " "), length))
  
  df_sum1 <- df_ %>%
    group_by(document) %>%
    summarise(lines = n(),
              total_words = sum(n_words),
              mean_words = mean(n_words),
              sd_words = sd(n_words),
              min_words = min(n_words),
              max_words = max(n_words))
  
  if (plot == TRUE){
    
    df_ %>%
      ggplot(aes(x = n_words, fill = document)) + 
      geom_histogram(bins = 100) + 
      xlim(0, 200) +
      facet_wrap(~document, scales = 'free') + 
      labs(title = "Distribution of words per line",
           y = "Frequency",
           x = "Number of words")
    
  } else {
    
    df_sum1
  }
  
}


# takes a list of dataframes
# returns a combined tidy dataframe
MergeDTM <- function(document_list){
  
  df_ <- document_list[[1]]
  
  for (index in 2:length(document_list)){
    df_ <- df_ %>% rbind(document_list[[index]])
    
  }
  
  df_
  
}

# Find k most frequent 'words' and-if plot them
MostFreqTerms <- function(tidy_dtm, k = 10, plot = FALSE){
  
  df_ <- tidy_dtm %>%
    group_by(document) %>%
    slice_max(n, n = k) %>%
    ungroup() %>%
    arrange(n)
  
  if (plot == TRUE){
    
    df_ %>%
      ggplot(aes(x = n, y = fct_reorder(word, n), fill = document)) + 
      geom_col() + 
      facet_wrap(~document, scales = 'free') + 
      labs(title = "Most frequent words per document",
           y = NULL,
           x = "Frequency")
  } else {
    df_
  }
}


# Find k most frequent 'words' by tf-idf and-if plot them
MostFreqTFIDF <- function(tidy_dtm, k = 10, plot = FALSE){
  
  df_tf_idf <- tidy_dtm %>% 
    bind_tf_idf(word, document, n) %>%
    arrange(desc(tf_idf))
  
  if (plot == TRUE){
    df_tf_idf %>%
      group_by(document) %>%
      slice_max(tf_idf, n = k) %>%
      ungroup() %>%
      ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = document)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~document, scales = "free") +
      labs(title = "Most frequent words by TF-IDF",
           x = "tf-idf",
           y = NULL)
  } else {
    df_tf_idf
  }
}


# Percentage of 'words' to cover document
# percent given as a decimal
PercentLexicon <- function(tidy_dtm, percent = NULL){
  
  df_tf_idf <- tidy_dtm %>% 
    bind_tf_idf(word, document, n) %>%
    arrange(desc(tf))%>% 
    group_by(document) %>% 
    mutate(cumulative_sum = cumsum(tf))
  
  if (is.null(percent)){
    df_tf_idf %>% group_by(document) %>%
      summarise('n 25%' = sum(cumulative_sum <= 0.25),
                'n 50%' = sum(cumulative_sum <= 0.5),
                'n 75%' = sum(cumulative_sum <= 0.75),
                'n 90%' = sum(cumulative_sum <= 0.9),
                'n 100%'= n())
  } else {
    df_tf_idf %>% group_by(document) %>%
      summarise(percent = sum(cumulative_sum <= percent))
  }
}


### Building an n-gram model
### Read 'Language Modeling' slides
### need P(single word chosen) x

WordFreqProb <- function(tidy_dtm, n = NULL){
  
  words <- tidy_dtm %>%
    group_by(word) %>%
    summarise(freq = sum(n)) %>%
    arrange(desc(freq)) %>%
    mutate(p = freq / sum(freq))
  
  if (is.null(n) == FALSE){
    words <- slice_head(words, n = n)
  }
  words
}

### need P(word, given one previous)
### need P(word, given two previous)
### >>> check notebook in book re: probs

### consider when one word of a unigram/bigram isn't in corpus
### 1. choose most likely wihtin corpus?
### (? > max(P), or a, ? > max(P))
### 2. use previous word as indicator for next
### (a, ? > max(P|a), or (?, a > max(P|a) [bigram model])) 
###
### consider efficiency methods of cutting corpus to PercentLexicon cover
