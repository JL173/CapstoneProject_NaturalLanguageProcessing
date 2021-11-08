# Read in all txt files from a directory as a list of files
#
# diectory = filepath from working directory eg. "data/en_US/"
# pattern = pattern to match all documents
# stem = stem words found in files
# n = number of lines to read, -1L is all lines
#
# Returns a list of data.tables with observations of a line, with variables
# line = line number, text = <char> contents of line, document = document name
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


# Function to add prefix and suffix to a string
#
# string = input string
# SOS/EOS = adding 'sos' <start of sentence> etc.
#
# Returns string with sos/eos appended
SOSEOS <- function(string, SOS = TRUE, EOS = TRUE){
  
  if (SOS == TRUE){
    string <- paste("SOS", string, sep = " ")
  }
  if (EOS == TRUE){
    string <- paste(string, "EOS", sep = " ")
  }
  string
}




# Loads a single document
# Analagous to LoadFiles, except for single file
#
# filepath = filepath to document (include file extension)
# stem = stems words in each line
# n = number of lines to read, -1L is all lines
# SOS/EOS = function call to SOSEOS
#
# Returns a data.table with observations on each line of document
# with variables line = line number, text = <chr> contents of line
# document = name of document
LoadSingleFile <- function(filepath,
                           stem = TRUE,
                           n = -1L,
                           SOS = FALSE,
                           EOS = FALSE){
  
  shortname <- shortPathName(filepath)
  
  # reading
  document <- readLines(filepath, n,
                        skipNul = TRUE, encoding = "UTF-8")
  
  # stemming
  if (stem == TRUE){
    document <- stemDocument(document)
    
    #dataframe
    df_ <- tibble(line = 1:length(document),
                  text = document,
                  document = shortname)
    
    #data.table format
    setDF(df_)
    
    if (SOS == TRUE){
      df_[, "text"] <- lapply(df_[, "text"], SOSEOS, SOS = TRUE, EOS = FALSE)
    }
    
    if (EOS == TRUE){
      df_[, "text"] <- lapply(df_[, "text"], SOSEOS, SOS = FALSE, EOS = TRUE)
    }
  }
  df_
}
  





# LoadFiles with sampling
# see 'LoadFiles'
#
# n = number of lines to sample
# seed = random seed to use for sampling
#
# returns list of data.tables with observations on each line with variables
# line = line number, text = <chr> content of line, document = document name
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



# splits dataframe for memory efficiency
#
# df = dataframe to be split
# size = max size of each dataframe to be returned
#
# Returns a list of dataframes
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
#
# df_list = list of dataframes
# size = parameter to be passed to SplitDF
#
# Returns a list of dataframes
UnlistDF <- function(df_list, size = 5e6){
  list <- lapply(df_list, SplitDF, size) %>%
    unlist(recursive = F)
  list
}



# Parts Of Speech annotator for a string
#
# string = string to be annotated
#
# Returns a Maxent_POS_Tag_Annotator object feature
# a list of POS tags for said string
POSstring <- function(string){
  initial_result = string %>% 
    annotate(list(Maxent_Sent_Token_Annotator(),
                  Maxent_Word_Token_Annotator())) %>% 
    annotate(string, Maxent_POS_Tag_Annotator(), .) %>% 
    subset(type=='word') 
  
  sapply(initial_result$features , '[[', "POS") %>%
    paste(collapse = " ")
}



# Takes a dataframe, calculates POS tags and summarises
#
# df = dataframe input, with variables line, text, document and
# row observations on each line of document, see 'LoadFiles' or equivalent
#
# Returns dataframe with POS tag summaries
POSsumDF <- function(df){
  options(java.parameters = "-Xmx8000m")
  
  df["text"] <- lapply(df["text"], POSstring)
  
  df_pos <- CleanTokens(df, n = 1) %>%
    group_by(document, word) %>%
    summarise(total = n())
}



# Wrapper function to create POS_summary dataframe from file_folder
# Memory constraints and time need consideration with size and chunks
#
# filepath = directory location for files
# size = maximum memory allocation to each dataframe. see 'UnlistDF'
# chunks = number of further splits to be made for java memory optimisation
#
# Returns a dataframe of POS tag sumamries for documents
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
# n should equal 1, 2, 3, 4
CleanTokens <- function(unclean_df, n = 1, filter_df = NULL){
  
  # remove puntuation
  unclean_df$text <- lapply(unclean_df$text,
                                str_replace_all, "[[:punct:]]", "")
    
  if (n == 1){
    df_ <- unnest_tokens(tbl = unclean_df,
                         output = word,
                         input = text,
                         token = "words",
                         to_lower = TRUE)
    # remove entries with numbers present
    df_ <- filter(df_, !grepl(".*\\d+.*", word))
    
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
    
    # remove entries with numbers present
    df_ <- filter(df_, !grepl(".*\\d+.*", word))
    
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
        
      } else if (n == 4){
        
        df_ <- df_ %>% separate(word,
                                c("word1", "word2", "word3", "word4"),
                                sep = " ")
        
        df_ <- df_ %>%
          anti_join(filter_df, by = c("word1" = "word")) %>%
          anti_join(filter_df, by = c("word2" = "word")) %>%
          anti_join(filter_df, by = c("word3" = "word")) %>%
          anti_join(filter_df, by = c("word4" = "word")) 
        
        df_ <- df_ %>%
          unite(word, word1, word2, word3, word4, sep = " ")
      }
      
    }
    
    # remove a common error
    df_ <- filter(df_, !grepl("^NA$|^NA NA$|^NA NA NA$|^NA NA NA NA$", word))
    
  }
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



### Tidy Freqeuncy of each token
WordFreq <- function(tidy_dtm, k = NULL){
  
  words <- tidy_dtm %>%
    group_by(word) %>%
    summarise(freq = sum(n())) %>%
    arrange(desc(freq))
  
  # filter tokens by frequency
  if (is.null(k) == FALSE){
    words <- words %>% filter(freq > k)
  }
  words
}



# Create Freq Table
#
# *gram = dataframe of ngrams spit into separate word/token variables
# with frequency column. see 'CleanTokens' function
#
# Returns data.table of combined *grams, frequency sorted with
# observations on each word combination with variables
# word1, word2, word3, word4, freq
CreateFreqTable <- function(unigram, bigram, trigram, quagram){
  
  # formatting
  unigram_ <- rename(unigram, word1 = word)
  unigram_ <- unigram_ %>%
    select(c("word1", "freq"))
  unigram_[, "word2"] <- NA
  unigram_[, "word3"] <- NA
  unigram_[, "word4"] <- NA
  
  # formatting
  bigram_ <- bigram %>%
    separate(word, c("word1", "word2"), sep = " ") %>%
    select(c("word1", "word2", "freq"))
  bigram_[, "word3"] <- NA
  bigram_[, "word4"] <- NA
  
  # formatting
  trigram_ <- trigram %>%
    separate(word, c("word1", "word2", "word3"), sep = " ") %>%
    select(c("word1", "word2", "word3", "freq"))
  trigram_[, "word4"] <- NA
  
  # formatting
  quagram_ <- quagram %>%
    separate(word, c("word1", "word2", "word3", "word4"), sep = " ") %>%
    select(c("word1", "word2", "word3", "word4", "freq"))
  
  # combine prob tables of all three
  freq_table <- rbind(unigram_,
                      bigram_,
                      trigram_,
                      quagram_) %>%
    select(order(colnames(.)))
  
  freq_table
}
 

 
# Take a string and find last 3,2,1 words for searching
#
# string = string used for predicting next word
# ngram = prediction level desired
# stem = stem string
# filter = apply a filter to words in string
# SOS = apply SOSEOS function
#
# Returns a list of <chr> strings
StringTailngram <- function(string,
                            ngram = 3,
                            stem = FALSE,
                            filter = NULL,
                            SOS = TRUE){
  
  # remove punctuation
  str <- str_replace_all(string, "[[:punct:]]", "")
  
  # add SOS token
  str <- SOSEOS(str, SOS = TRUE, EOS = FALSE)
  
  if (stem == TRUE){
    str <- stemDocument(str)
  }
  
  if (is.null(filter) == FALSE){
    
    strdf <- data.table(text = str) %>%
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
  
  str_len <- length(str)
  
  if (ngram == 3){
    search_list <- tail(str, 3)
    
  } else if (ngram == 2){
    search_list <- tail(str, 2)
    
  } else if(ngram == 1){
    search_list <- tail(str, 1)
    
  }
  search_list <- lapply(search_list, tolower)
  search_list
}



# bigram prediction with back-off
#
# all ngrams have the standard column labels
# freq, word1, word2, word3, word4 from model
#
# string = string to predict next word
# *grams_ = ngrams to be passed to predictor
# gamma = discount variable to weigh larger ngram predictions less
# stem/filter/SOS = paramters to pass to StringTailngram
#
# Returns a dataframe of predictions, observations on prediction with variables
# P = probability, word = prediction, word* = prediction basis from model
PredictBigram <- function(string, unigrams, bigrams, gamma,
                          stem = FALSE, filter = NULL, SOS = TRUE){
  
  str <- StringTailngram(string, ngram = 1,
                         stem = stem, filter = filter,
                         SOS = SOS)
  
  known_bigrams <- bigrams %>%
    filter(word1 == str[1], !is.na(word2), is.na(word3))
  
  unigram_sum <- unigrams %>%
    filter(word1 == str[1])
  unigram_sum <- unigram_sum$freq
  
  if (sum(unigram_sum) == 0){
    unigram_sum <- sum(unigrams$freq)}
  
  # P for known bigrams
  known_bigrams$P <- (known_bigrams$freq - gamma) / unigram_sum
  
  # p-density for unobserved bigrams
  alpha <- 1 - sum(known_bigrams$P)
  
  # all unobserved bigram tails
  unob_bigrams <- anti_join(unigrams, known_bigrams,
                            by = c("word1" = "word2"))
  
  ### Back-off implementation
  
  # formatting
  unob_bigrams$word2 <- unob_bigrams$word1
  unob_bigrams$word1 <- str[1]
  
  # probability of unobserved bigrams
  unob_sum <- sum(unob_bigrams$freq)
  unob_bigrams$P <- alpha * (unob_bigrams$freq / unob_sum)
  
  ###
  
  # collect into one data.table
  predictions <- rbind(known_bigrams, unob_bigrams) %>%
    arrange(desc(P))
  
  predictions
}



# trigram prediction with back-off
#
# all ngrams have the standard column labels
# freq, word1, word2, word3, word4 from model
#
# string = string to predict next word
# *grams_ = ngrams to be passed to predictor
# gamma = discount variable to weigh larger ngram predictions less
# stem/filter/SOS = paramters to pass to StringTailngram
#
# Returns a dataframe of predictions, observations on prediction with variables
# P = probability, word = prediction, word* = prediction basis from model
PredictTrigram <- function(string, unigrams, bigrams, trigrams, gamma,
                           stem = FALSE, filter = NULL, SOS = TRUE){
  
  str <- StringTailngram(string, ngram = 2,
                         stem = stem, filter = filter,
                         SOS = SOS)
  
  known_trigrams <- trigrams %>%
    filter(word1 == str[1], word2 == str[2], !is.na(word3), is.na(word4))
  
  bigram_sum <- bigrams %>%
    filter(word1 == str[1], word2 == str[2])
  bigram_sum <- bigram_sum$freq
  
  if (sum(bigram_sum) == 0){
    bigram_sum <- sum(bigrams$freq)}
  
  # P for known trigrams
  known_trigrams$P <- (known_trigrams$freq - gamma) / bigram_sum
  
  # P-density for unobserved trigrams
  alpha1 <- 1 - sum(known_trigrams$P)
  
  # all unobserved trigram tails
  unob_trigrams <- anti_join(bigrams, known_trigrams,
                             by = c("word2" = "word3"))
  
  ### Back-off implementation
  
  known_bigrams <- unob_trigrams %>%
    filter(word1 == str[2], !is.na(word2), is.na(word3))
  
  unigram_sum <- unigrams %>%
    filter(word1 == str[2])
  unigram_sum <- unigram_sum$freq
  
  if (sum(unigram_sum) == 0){
    unigram_sum <- sum(unigrams$freq)}
  
  # P for known bigrams
  known_bigrams$P <- (known_bigrams$freq - gamma) / unigram_sum
  
  # p-density for unobserved bigrams
  alpha2 <- 1 - sum(known_bigrams$P)
  
  # all unobserved bigram tails
  unob_bigrams <- anti_join(unigrams, known_bigrams,
                            by = c("word1" = "word2")) %>%
    anti_join(known_trigrams, by = c("word1" = "word3"))
  
  ### ### Back-off implementation
  
  # formatting
  unob_bigrams$word2 <- unob_bigrams$word1
  unob_bigrams$word1 <- str[2]
  
  # probability of unobserved bigrams
  unob_sum <- sum(unob_bigrams$freq)
  unob_bigrams$P <- alpha2 * (unob_bigrams$freq / unob_sum)
  
  ### ###
  
  # collect into one data.table
  bigram_predictions <- rbind(known_bigrams, unob_bigrams) %>%
    arrange(desc(P))
  
  bigram_predictions$P <- alpha1 * bigram_predictions$P
  
  ###
  
  predictions <- rbind(known_trigrams, bigram_predictions) %>%
    arrange(desc(P))
  
  predictions
}



# quagram prediction with back-off
#
# all ngrams have the standard column labels
# freq, word1, word2, word3, word4 from model
#
# string = string to predict next word
# *grams_ = ngrams to be passed to predictor
# gamma = discount variable to weigh larger ngram predictions less
# stem/filter/SOS = paramters to pass to StringTailngram
#
# Returns a dataframe of predictions, observations on prediction with variables
# P = probability, word = prediction, word* = prediction basis from model
PredictQuagram <- function(string, unigrams, bigrams, trigrams, quagrams, gamma,
                           stem = FALSE, filter = NULL, SOS = TRUE){
  
  str <- StringTailngram(string, ngram = 3,
                         stem = stem, filter = filter,
                         SOS = SOS)
  
  known_quagrams <- quagrams %>%
    filter(word1 == str[1], word2 == str[2], word3 == str[3], !is.na(word4))
  
  trigram_sum <- trigrams %>%
    filter(word1 == str[1], word2 == str[2], word3 == str[3])
  trigram_sum <- trigram_sum$freq
  
  if (sum(trigram_sum) == 0){
    trigram_sum <- sum(trigrams$freq)}
  
  # P for known quagrams
  known_quagrams$P <- (known_quagrams$freq - gamma) / trigram_sum
  
  # P-density for unobserved quagrams
  alpha1 <- 1 - sum(known_quagrams$P)
  
  # all unobserved quagram tails
  unob_quagrams <- anti_join(trigrams, known_quagrams,
                             by = c("word3" = "word4"))
  
  ### Back-off implementation
  
  known_trigrams <- unob_quagrams %>%
    filter(word1 == str[2], word2 == str[3], !is.na(word3), is.na(word4))
  
  bigram_sum <- bigrams %>%
    filter(word1 == str[2], word2 == str[3])
  bigram_sum <- bigram_sum$freq
  
  if (sum(bigram_sum) == 0){
    bigram_sum <- sum(bigrams$freq)}
  
  # P for known trigrams
  known_trigrams$P <- (known_trigrams$freq - gamma) / bigram_sum
  
  # P-density for unobserved trigrams
  alpha2 <- 1 - sum(known_trigrams$P)
  
  # all unobserved trigram tails
  unob_trigrams <- anti_join(bigrams, known_trigrams,
                             by = c("word2" = "word3")) %>%
    anti_join(known_quagrams, by = c("word2" = "word4"))
  
  ### ### Back-off implementation
  
  known_bigrams <- unob_trigrams %>%
    filter(word1 == str[3], !is.na(word2), is.na(word3))
  
  unigram_sum <- unigrams %>%
    filter(word1 == str[3])
  unigram_sum <- unigram_sum$freq
  
  if (sum(unigram_sum) == 0){
    unigram_sum <- sum(unigrams$freq)}
  
  # P for known bigrams
  known_bigrams$P <- (known_bigrams$freq - gamma) / unigram_sum
  
  # p-density for unobserved bigrams
  alpha3 <- 1 - sum(known_bigrams$P)
  
  # all unobserved bigram tails
  unob_bigrams <- anti_join(unigrams, known_bigrams,
                            by = c("word1" = "word2")) %>%
    anti_join(known_trigrams, by = c("word1" = "word3")) %>%
    anti_join(known_quagrams, by = c("word1" = "word4"))
  
  ### ### ### Back-off implementation
  
  # formatting
  unob_bigrams$word2 <- unob_bigrams$word1
  unob_bigrams$word1 <- str[3]
  
  # probability of unobserved bigrams
  unob_sum <- sum(unob_bigrams$freq)
  unob_bigrams$P <- alpha3 * (unob_bigrams$freq / unob_sum)
  
  ### ### ###
  
  # collect into one data.table
  bigram_predictions <- rbind(known_bigrams, unob_bigrams) %>%
    arrange(desc(P))
  
  bigram_predictions$P <- alpha2 * bigram_predictions$P
  
  ### ###
  
  trigram_predictions <- rbind(known_trigrams, bigram_predictions) %>%
    arrange(desc(P))
  
  trigram_predictions$P <- alpha1 * trigram_predictions$P
  
  ###
  
  predictions <- rbind(known_quagrams, trigram_predictions) %>%
    arrange(desc(P))
  
  predictions
}


# Wrapper for separate prediction functions
Predict <- function(string, model, ngram, gamma,
                    stem = FALSE, filter = NULL, SOS = TRUE){
  
  unigrams <- model %>%
    filter(!is.na(word1), is.na(word2))
  
  bigrams <- model %>%
    filter(!is.na(word2), is.na(word3))
  
  trigrams <- model %>%
    filter(!is.na(word3), is.na(word4))
  
  quagrams <- model %>%
    filter(!is.na(word4))
  
  if (ngram == 3){
    
    predictions <- PredictQuagram(string, unigrams, bigrams, trigrams, quagrams,
                                  gamma, stem = stem, filter = filter, SOS = SOS)
    
  } else if (ngram == 2){
    
    predictions <- PredictTrigram(string, unigrams, bigrams, trigrams,
                                  gamma, stem = stem, filter = filter, SOS = SOS)
    
  } else if (ngram == 1){
    
    predictions <- PredictBigram(string, unigrams, bigrams,
                                 gamma, stem = stem, filter = filter, SOS = SOS)
    
  }
  
  predictions <- predictions %>%
    unite(col = sent, word1, word2, word3, word4, sep = " ",
          na.rm = TRUE, remove = TRUE)
  
  predictions$word <- as.character(lapply(predictions$sent, word, -1))
  
  predictions
}



# Load CSV lyrics
LoadCSV <- function(directory){
  filenames <- list.files(directory, pattern = "*.csv", full.names = TRUE)
  shortnames <- list.files(directory, pattern = "*.csv", full.names = FALSE)
  
  for (index in length(filenames)){
    
    df <- read_csv(filenames[index], na = "NA")
    df <- select(df, Lyric)
    write.table(df, file = paste(filenames[index], ".txt", sep = ""),
                sep = "\n",
                row.names = FALSE,
                col.names = FALSE)
    rm(df)
  }
  
}