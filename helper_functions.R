# Read in all txt files from a directory as a list of files
# Returns a list of dataframes
LoadFiles <- function(directory, pattern = "*.txt", stem = TRUE, n = -1L){
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
    if (stem == TRUE){
      document <- stemDocument(document)
    }
    
    #dataframe
    df_ <- tibble(line = 1:length(document),
                  text = document,
                  document = shortnames[index])
    
    #data.table format
    setDF(df_)
    
    # list of dataframes
    document_list[[index]] <- df_
    
  }
  
  document_list
}



# LoadFiles with sampling
SampleLoadFiles <- function(directory, pattern = "*.txt", stem = TRUE, n = 1000, seed = NULL){
  filenames <- list.files(directory, pattern = pattern, full.names = TRUE)
  shortnames <- list.files(directory, pattern = pattern, full.names = FALSE)
  
  document_list <- list()
  
  for(index in 1:length(filenames)){
    
    #find length of document
    nlines <- countLines(filenames[index])
    
    
    #generate sample indicies
    if (!is.null(seed)){
      set.seed(seed)
    }
    sample <- as.integer(runif(n, min = 1, max = nlines))
    
    document <- c()
    
    #sample reading
    for (jndex in sample){
      
      #reading
      document_line <- read_lines(filenames[index],
                                  skip = jndex-1,
                                  skip_empty_rows = TRUE,
                                  n_max = 1,
                                  locale = default_locale())
      
      #stemming
      if (stem == TRUE){
        document_line <- stemDocument(document_line)
      }
      
      document <- c(document, document_line)
    }
    
    #dataframe
    df_ <- tibble(line = 1:length(document),
                  text = document,
                  document = shortnames[index])
    
    #data.table format
    setDF(df_)
    
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



# Function to create POS_summary dataframe from file_folder
# Memory constraints and time need consideration with size and chunks
POSsummaryDF <- function(filepath, size = 1e5, chunks = 150){
  
  #create chunks to optimise memory usage
  base_chunks <- UnlistDF(LoadFiles(filepath), size = 1e5)
  
  #create further chunks to optimise RAM for java.parameters
  chunk_number = chunks
  base_chunks_chunks <- split(base_chunks,
                              cut(seq_along(base_chunks),
                                  chunk_number,
                                  labels = FALSE))
  #remove uneeded temp files
  rm(base_chunks)
  
  #initialise list
  POS_summary_list <- list()
  for (index in 1:length(base_chunks_chunks)){
    gc()
    options(java.parameters = "-Xmx16000m")
    
    temp_list_ <- lapply(base_chunks_chunks[[index]], POSsumDF)
    
    POS_summary_list <- c(POS_summary_list, MergeDTM(temp_list_))
    rm(temp_list_)
    print(index)
  }
  
  length <- length(POS_summary_list)/3 - 1
  
  #initialise dataframe
  df_ <- data.table(document = POS_summary_list[[1]],
                    POS = POS_summary_list[[2]],
                    total = POS_summary_list[[3]])
  
  for (index in 1:length){
    temp_df <- data.table(document = POS_summary_list[[3*index+1]],
                          POS = POS_summary_list[[3*index+2]],
                          total = POS_summary_list[[3*index+3]])
    
    df_ <- rbind(df_, temp_df)
  }
  df_
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
PercentDocLexicon <- function(tidy_dtm, percent = NULL){
  
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
WordFreqProb <- function(tidy_dtm, k = NULL){
  
  words <- tidy_dtm %>%
    group_by(word) %>%
    summarise(freq = sum(n())) %>%
    arrange(desc(freq)) %>%
    mutate(P = freq / sum(freq))
  
  if (is.null(k) == FALSE){
    words <- slice_head(words, n = k)
  }
  words
}



# Significant memory storage save by collecting only top n entries
TopNProbFreq <- function(df, k = 3){
  
  if ("word4" %in% colnames(df)){
    df <- df %>% group_by(word1, word2, word3) %>%
      slice_max(order_by = P, n = k) %>%
      arrange(desc(P))
    
  } else if ("word3" %in% colnames(df)){
    df <- df %>% group_by(word1, word2) %>%
      slice_max(order_by = P, n = k) %>%
      arrange(desc(P))
    
  } else if ("word2" %in% colnames(df)){
    df <- df %>% group_by(word1) %>%
      slice_max(order_by = P, n = k) %>%
      arrange(desc(P))
  }
  
  df
}



# Create probability table
CreateProbTable <- function(unigram, bigram, trigram, quagram){
  
  # split bigram word into two words
  # join on the first word with unigram
  # add freq columns of first word and second
  # calculate probability
  # sort
  bigram_probs <- bigram %>%
    separate(word, c("word1", "word2"), sep = " ") %>%
    left_join(unigram, by = c("word1" = "word"), suffix = c(".x", ".y")) %>% 
    mutate(P = freq.x / freq.y) %>%
    drop_na() %>%
    select(-c("P.x", "P.y")) %>% 
    arrange(word1, desc(freq.y))
  
  # split trigram word into three words
  # unite the first two words to match bigram
  # join on the two words with bigram
  # add freq columns of first word and second
  # calculate probability
  # sort
  trigram_probs <- trigram %>%
    separate(word, c("word1", "word2", "word3"), sep = " ") %>% 
    unite(col = word, word1, word2, sep = " ", na.rm = TRUE) %>%
    left_join(bigram, by = c("word" = "word"), suffix = c(".x", ".y")) %>%
    drop_na() %>%
    mutate(P = freq.x / freq.y) %>%
    select(-c("P.x", "P.y")) %>%
    arrange(word, desc(freq.y))
  
  # split quagram word into four words
  # unite the first two words to match trigram
  # join on the three words with trigram
  # add freq columns of first word and second
  # calculate probability
  # sort
  quagram_probs <- quagram %>%
    separate(word, c("word1", "word2", "word3", "word4"), sep = " ") %>% 
    unite(col = word, word1, word2, word3, sep = " ", na.rm = TRUE) %>%
    left_join(trigram, by = c("word" = "word"), suffix = c(".x", ".y")) %>%
    drop_na() %>%
    mutate(P = freq.x / freq.y) %>%
    select(-c("P.x", "P.y")) %>%
    arrange(word, desc(freq.y))
  
  # formatting
  unigram_probs <- rename(unigram, word1 = word)
  unigram_probs <- unigram_probs %>%
    select(c("word1", "P")) %>%
    drop_na()
  unigram_probs[, "word2"] <- NA
  unigram_probs[, "word3"] <- NA
  unigram_probs[, "word4"] <- NA
  
  # formatting
  bigram_probs <- bigram_probs %>%
    select(c("word1", "word2", "P")) %>%
    drop_na()
  bigram_probs[, "word3"] <- NA
  bigram_probs[, "word4"] <- NA
  
  # formatting
  trigram_probs <- trigram_probs %>%
    separate(word, c("word1", "word2")) %>%
    select(c("word1", "word2", "word3", "P")) %>%
    drop_na()
  trigram_probs[, "word4"] <- NA
  
  # formatting
  quagram_probs <- quagram_probs %>%
    separate(word, c("word1", "word2", "word3")) %>%
    select(c("word1", "word2", "word3", "word4", "P")) %>%
    drop_na()
  
  # store only top '3' results of each
  bigram_probs <- bigram_probs %>% TopNProbFreq(k = 3)
  trigram_probs <- trigram_probs %>% TopNProbFreq(k = 3)
  quagram_probs <- quagram_probs %>% TopNProbFreq(k = 3)
  
  # combine prob tables of all three
  prob_table <- rbind(unigram_probs,
                      bigram_probs,
                      trigram_probs,
                      quagram_probs) %>%
    select(order(colnames(.)))
  
  prob_table
}



# Random 'k' word1 from unigrams
RandomUnigram <- function(unigram_ptable, k = 3){
  indices <- sample(seq_len(nrow(unigram_ptable)),
                   size = k,
                   prob = unigram_ptable$P)
  unigram_ptable[indices, "word1", order("P")]
}



# Take a string and find last 3,2,1 words for searching
StringTailngram <- function(string,
                            ngram = 3,
                            stem = FALSE,
                            filter = NULL){
  
  str <- removePunctuation(string)
  
  if (stem == TRUE){
    str <- stemDocument(string)
  }
  
  if (is.null(filter) == FALSE){
    
    strdf <- data.table(text = string) %>%
      unnest_tokens(output = word,
                    input = text,
                    token = "words",
                    to_lower = TRUE) %>%
      anti_join(filter, by = "word")
    
    str <- strdf$word
    
  } else {
    
    #split string into separate tokens
    str <- as.list(strsplit(str, '\\s+')[[1]])
  } 
  
  str_len <- length(string)
  
  if (ngram == 3 | str_len >= 3){
    search_list <- tail(str, 3)
    
  } else if (ngram == 2 | str_len == 2){
    search_list <- tail(str, 2)
    
  } else if(ngram == 1 | str_len == 1){
    search_list <- tail(str, 1)
    
  }
  search_list
}



# Match on a string and return a predicted word from a probability table
MatchStringPredict <- function(string,
                               ptable,
                               ngram = 3,
                               preds = 3,
                               stem = FALSE,
                               filter = NULL,
                               ReturnString = FALSE){

  
  # obtain unigram objects from ptable
  uniptable <- ptable %>%
    filter(is.na(word2)) %>%
    filter(is.na(word3)) %>%
    filter(is.na(word4))
  
  # get string search parameters
  search <- tail(StringTailngram(string, ngram, stem, filter), 3)
  
  str_len <- length(search)
  
  # filter ptable for prediction
  if (str_len == 3){
    pred_table <- ptable %>%
      filter(word1 == search[1],
             word2 == search[2],
             word3 == search[3]) %>%
      drop_na() %>%
      head(3)
    
    prediction <- pred_table$word4
    
    if (nrow(pred_table) == 0){
      pred_table2 <- ptable %>%
        filter(word1 == search[2],
               word2 == search[3]) %>%
        drop_na() %>%
        head(3)
      
      prediction <- pred_table2$word3
      
      if (nrow(pred_table2) == 0){
        pred_table3 <- ptable %>%
          filter(word1 == search[3]) %>%
          drop_na() %>%
          head(3)
        
        prediction <- pred_table3$word2
        
        if (nrow(pred_table3) == 0){
          prediction <- RandomUnigram(uniptable, k = preds)
        }
      }
    }
    
  } else if (str_len == 2){
    pred_table <- ptable %>%
      filter(word1 == search[1],
             word2 == search[2]) %>%
      drop_na() %>%
      head(3)
    
    prediction <- pred_table$word3
    
    if (nrow(pred_table) == 0){
      pred_table2 <- ptable %>%
        filter(word1 == search[2]) %>%
        drop_na() %>%
        head(3)
      
      prediction <- pred_table2$word2
      
      if (nrow(pred_table2) == 0){
        prediction <- RandomUnigram(uniptable, k = preds)
      }
    }
    
  } else if (str_len == 1){
    pred_table <- ptable %>%
      filter(word1 == search[1]) %>%
      drop_na() %>%
      head(3)
    
    predictions <- pred_table$word2
    
    if (nrow(pred_table) == 0){
      prediction <- RandomUnigram(uniptable, k = preds)
    }
    
  } else if (str_len == 0){
    prediction <- RandomUnigram(uniptable, k = preds)
  }
  
  # return original string with prediction appended
  if (ReturnString == TRUE){
    prediction = paste(string, prediction[1], sep = " ")
  }
  prediction
}



# This script is for one function to call all other functions to create
# the final model. This should form the basis for an object class
CallAll <- function(filepath,
                    size,
                    sample = FALSE,
                    seed = 17373,
                    stem = TRUE,
                    coverage = 0.5,
                    filter = NULL){
  
  if (sample == TRUE){
    base_ <- SampleLoadFiles(filepath,
                             n = size,
                             seed = seed,
                             stem = stem)
  } else {
    base_ <- LoadFiles(filepath,
                       n = size,
                       stem = stem)
  }
  
  tidy_1_ <- lapply(base_, CleanTokens, n = 1, filter_df = filter) %>% MergeDTM()
  tidy_2_ <- lapply(base_, CleanTokens, n = 2, filter_df = filter) %>% MergeDTM()
  tidy_3_ <- lapply(base_, CleanTokens, n = 3, filter_df = filter) %>% MergeDTM()
  tidy_4_ <- lapply(base_, CleanTokens, n = 4, filter_df = filter) %>% MergeDTM()
  
  unigram_ <- WordFreqProb(tidy_1_)
  bigram_ <- WordFreqProb(tidy_2_)
  trigram_ <- WordFreqProb(tidy_3_)
  quagram_ <- WordFreqProb(tidy_4_)
  
  vocab_n_ <- unigram_ %>%
    mutate(cumsum = cumsum(P)) %>%
    summarise(vocab = sum(cumsum <= coverage))%>%
    as.integer()
  
  unigram_ <- unigram_ %>% slice_head(n = vocab_n_)
  
  prob_table_ <- CreateProbTable(unigram_,
                                 bigram_,
                                 trigram_,
                                 quagram_)
  
  prob_table_
}