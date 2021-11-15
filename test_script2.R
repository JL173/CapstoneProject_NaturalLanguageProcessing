base <- LoadSingleFile(
  "data/en_US/25/en_US.news.25.txt",
  SOS = TRUE, EOS = TRUE)

news_unigrams <- CleanTokens(base, n = 1) %>%
  WordFreq() %>%
  filter(freq > 2) 

news_bigrams <- CleanTokens(base, n = 2) %>%
  WordFreq() %>%
  filter(freq > 1)

news_trigrams <- CleanTokens(base, n = 3) %>%
  WordFreq() %>%
  filter(freq > 1)

news_quagrams <- CleanTokens(base, n = 4) %>%
  WordFreq() %>%
  filter(freq > 1)



TrainTestSplit <- function(df, percent = 0.95, seed = 17373){
  smp_size <- floor(percent * nrow(df))
  set.seed(seed)
  train_ind <- sample(seq_len(nrow(df)), size = smp_size)
  
  train <- df[train_ind, ]
  test <- df[-train_ind, ]
  
  results <- list(train = train, test = test)
}



trigrams_ <-TrainTestSplit(news_trigrams, 0.95)

test_news_trigrams <- trigrams_$test
train_news_trigrams <- trigrams_$train
rm(trigrams_)

quagrams_ <-TrainTestSplit(news_quagrams, 0.95)

test_news_quagrams <- quagrams_$test
train_news_quagrams <- quagrams_$train
rm(quagrams_)


news_train_model <- CreateFreqTable(news_unigrams,
                                       news_bigrams,
                                       train_news_trigrams,
                                       train_news_quagrams)

#save(news_train_model, file = "models/news_train_model.RDa")



RemoveTestDuplicates <- function(df, ngram){
  
  if (ngram == 3){
    
    df <- df %>%
      separate(word, c("word1", "word2", "word3"), sep = " ") %>%
      select(c("word1", "word2", "word3", "freq"))
    df[, "word4"] <- NA
    
    df <- distinct(df, word1, word2, .keep_all = TRUE)
  } else if (ngram == 4){
    
    df <- df %>%
      separate(word, c("word1", "word2", "word3", "word4"), sep = " ") %>%
      select(c("word1", "word2", "word3", "word4", "freq"))
    
    df <- distinct(df, word1, word2, word3, .keep_all = TRUE)
  }
  
  df
}



test_news_trigrams <- test_news_trigrams %>%
  RemoveTestDuplicates(ngram = 3) %>%
  filter(!is.na(word1))

test_news_quagrams <- test_news_quagrams %>%
  RemoveTestDuplicates(ngram = 4) %>% 
  filter(!is.na(word1))



CreatePredictionString <- function(df, ngram){
  
  if (ngram == 3){
    
    df <- df %>%
      unite(col = string, word1, word2, sep = " ",
            na.rm = TRUE, remove = FALSE)
    
  } else if (ngram == 4){
    
    df <- df %>%
      unite(col = string, word1, word2, word3, sep = " ",
            na.rm = TRUE, remove = FALSE)
  }
  
}



test_news_trigrams <- test_news_trigrams %>%
  CreatePredictionString(ngram = 3)
test_news_quagrams <- test_news_quagrams %>%
  CreatePredictionString(ngram = 4)

t <- Sys.time()
test_copy <- test_news_trigrams

test_strings <- test_copy[["string"]]

result_words <- c()
result_P <- c()
index = 0

for (string in test_strings){
  if (index == 0){
    start_time <- Sys.time()
  }
  index =  index + 1
  if ((index %% 50) == 0){
    percent_complete <- index / length(test_strings)
    print(index / length(test_strings))
    total_time <- (Sys.time() - start_time) / percent_complete
    time_remaining <- total_time * (1 - percent_complete)
    print(time_remaining)
  }
  
  result <- Predict(string,
                    model = news_train_model,
                    ngram = 2,
                    gamma = 0.3) %>%
    head(1)
  
  result_words <- c(result_words, result[1, "word"])
  result_P <- c(result_P, result[1, "P"])
}

test_news_trigrams$predictions <- result_words
test_news_trigrams$probability <- result_P

n <- Sys.time()
t <- n - t
print(t)

save(test_news_trigrams, file = "output/test_news_trigrams_5.RDa")



t <- Sys.time()
test_copy <- test_news_quagrams

test_strings <- test_copy[["string"]]

result_words <- c()
result_P <- c()
index = 0

for (string in test_strings){
  if (index == 0){
    start_time <- Sys.time()
  }
  index =  index + 1
  if ((index %% 50) == 0){
    percent_complete <- index / length(test_strings)
    print(index / length(test_strings))
    total_time <- (Sys.time() - start_time) / percent_complete
    time_remaining <- total_time * (1 - percent_complete)
    print(time_remaining)
  }
  result <- Predict(string,
                    model = news_train_model,
                    ngram = 3,
                    gamma = 0.3) %>%
    head(1)
  
  result_words <- c(result_words, result[1, "word"])
  result_P <- c(result_P, result[1, "P"])
}

test_news_quagrams$predictions <- result_words
test_news_quagrams$probability <- result_P

n <- Sys.time()
t <- n - t
print(t)



save(test_news_quagrams, file = "output/test_news_quagrams_5.RDa")










base <- LoadSingleFile(
  "data/en_US/25/en_US.blogs.25.txt",
  SOS = TRUE, EOS = TRUE)

blogs_unigrams <- CleanTokens(base, n = 1) %>%
  WordFreq() %>%
  filter(freq > 2) 

blogs_bigrams <- CleanTokens(base, n = 2) %>%
  WordFreq() %>%
  filter(freq > 1)

blogs_trigrams <- CleanTokens(base, n = 3) %>%
  WordFreq() %>%
  filter(freq > 1)

blogs_quagrams <- CleanTokens(base, n = 4) %>%
  WordFreq() %>%
  filter(freq > 1)



TrainTestSplit <- function(df, percent = 0.95, seed = 17373){
  smp_size <- floor(percent * nrow(df))
  set.seed(seed)
  train_ind <- sample(seq_len(nrow(df)), size = smp_size)
  
  train <- df[train_ind, ]
  test <- df[-train_ind, ]
  
  results <- list(train = train, test = test)
}



trigrams_ <-TrainTestSplit(blogs_trigrams, 0.95)

test_blogs_trigrams <- trigrams_$test
train_blogs_trigrams <- trigrams_$train
rm(trigrams_)

quagrams_ <-TrainTestSplit(blogs_quagrams, 0.95)

test_blogs_quagrams <- quagrams_$test
train_blogs_quagrams <- quagrams_$train
rm(quagrams_)


blogs_train_model <- CreateFreqTable(blogs_unigrams,
                                       blogs_bigrams,
                                       train_blogs_trigrams,
                                       train_blogs_quagrams)

#save(blogs_train_model, file = "models/blogs_train_model.RDa")



RemoveTestDuplicates <- function(df, ngram){
  
  if (ngram == 3){
    
    df <- df %>%
      separate(word, c("word1", "word2", "word3"), sep = " ") %>%
      select(c("word1", "word2", "word3", "freq"))
    df[, "word4"] <- NA
    
    df <- distinct(df, word1, word2, .keep_all = TRUE)
  } else if (ngram == 4){
    
    df <- df %>%
      separate(word, c("word1", "word2", "word3", "word4"), sep = " ") %>%
      select(c("word1", "word2", "word3", "word4", "freq"))
    
    df <- distinct(df, word1, word2, word3, .keep_all = TRUE)
  }
  
  df
}



test_blogs_trigrams <- test_blogs_trigrams %>%
  RemoveTestDuplicates(ngram = 3) %>%
  filter(!is.na(word1))

test_blogs_quagrams <- test_blogs_quagrams %>%
  RemoveTestDuplicates(ngram = 4) %>% 
  filter(!is.na(word1))



CreatePredictionString <- function(df, ngram){
  
  if (ngram == 3){
    
    df <- df %>%
      unite(col = string, word1, word2, sep = " ",
            na.rm = TRUE, remove = FALSE)
    
  } else if (ngram == 4){
    
    df <- df %>%
      unite(col = string, word1, word2, word3, sep = " ",
            na.rm = TRUE, remove = FALSE)
  }
  
}



test_blogs_trigrams <- test_blogs_trigrams %>%
  CreatePredictionString(ngram = 3)
test_blogs_quagrams <- test_blogs_quagrams %>%
  CreatePredictionString(ngram = 4)

t <- Sys.time()
test_copy <- test_blogs_trigrams

test_strings <- test_copy[["string"]]

result_words <- c()
result_P <- c()
index = 0

for (string in test_strings){
  if (index == 0){
    start_time <- Sys.time()
  }
  index =  index + 1
  if ((index %% 50) == 0){
    percent_complete <- index / length(test_strings)
    print(index / length(test_strings))
    total_time <- (Sys.time() - start_time) / percent_complete
    time_remaining <- total_time * (1 - percent_complete)
    print(time_remaining)
  }
  
  result <- Predict(string,
                    model = blogs_train_model,
                    ngram = 2,
                    gamma = 0.3) %>%
    head(1)
  
  result_words <- c(result_words, result[1, "word"])
  result_P <- c(result_P, result[1, "P"])
}

test_blogs_trigrams$predictions <- result_words
test_blogs_trigrams$probability <- result_P

n <- Sys.time()
t <- n - t
print(t)

save(test_blogs_trigrams, file = "output/test_blogs_trigrams_5.RDa")



t <- Sys.time()
test_copy <- test_blogs_quagrams

test_strings <- test_copy[["string"]]

result_words <- c()
result_P <- c()
index = 0

for (string in test_strings){
  if (index == 0){
    start_time <- Sys.time()
  }
  index =  index + 1
  if ((index %% 50) == 0){
    percent_complete <- index / length(test_strings)
    print(index / length(test_strings))
    total_time <- (Sys.time() - start_time) / percent_complete
    time_remaining <- total_time * (1 - percent_complete)
    print(time_remaining)
  }
  result <- Predict(string,
                    model = blogs_train_model,
                    ngram = 3,
                    gamma = 0.3) %>%
    head(1)
  
  result_words <- c(result_words, result[1, "word"])
  result_P <- c(result_P, result[1, "P"])
}

test_blogs_quagrams$predictions <- result_words
test_blogs_quagrams$probability <- result_P

n <- Sys.time()
t <- n - t
print(t)



save(test_blogs_quagrams, file = "output/test_blogs_quagrams_5.RDa")








base <- LoadSingleFile(
  "data/IMDB/neg_merged.txt",
  SOS = TRUE, EOS = TRUE)

imdb_neg_unigrams <- CleanTokens(base, n = 1) %>%
  WordFreq() %>%
  filter(freq > 2) 

imdb_neg_bigrams <- CleanTokens(base, n = 2) %>%
  WordFreq() %>%
  filter(freq > 1)

imdb_neg_trigrams <- CleanTokens(base, n = 3) %>%
  WordFreq() %>%
  filter(freq > 1)

imdb_neg_quagrams <- CleanTokens(base, n = 4) %>%
  WordFreq() %>%
  filter(freq > 1)



TrainTestSplit <- function(df, percent = 0.95, seed = 17373){
  smp_size <- floor(percent * nrow(df))
  set.seed(seed)
  train_ind <- sample(seq_len(nrow(df)), size = smp_size)
  
  train <- df[train_ind, ]
  test <- df[-train_ind, ]
  
  results <- list(train = train, test = test)
}



trigrams_ <-TrainTestSplit(imdb_neg_trigrams, 0.95)

test_imdb_neg_trigrams <- trigrams_$test
train_imdb_neg_trigrams <- trigrams_$train
rm(trigrams_)

quagrams_ <-TrainTestSplit(imdb_neg_quagrams, 0.95)

test_imdb_neg_quagrams <- quagrams_$test
train_imdb_neg_quagrams <- quagrams_$train
rm(quagrams_)


imdb_neg_train_model <- CreateFreqTable(imdb_neg_unigrams,
                                       imdb_neg_bigrams,
                                       train_imdb_neg_trigrams,
                                       train_imdb_neg_quagrams)

#save(imdb_neg_train_model, file = "models/imdb_neg_train_model.RDa")



RemoveTestDuplicates <- function(df, ngram){
  
  if (ngram == 3){
    
    df <- df %>%
      separate(word, c("word1", "word2", "word3"), sep = " ") %>%
      select(c("word1", "word2", "word3", "freq"))
    df[, "word4"] <- NA
    
    df <- distinct(df, word1, word2, .keep_all = TRUE)
  } else if (ngram == 4){
    
    df <- df %>%
      separate(word, c("word1", "word2", "word3", "word4"), sep = " ") %>%
      select(c("word1", "word2", "word3", "word4", "freq"))
    
    df <- distinct(df, word1, word2, word3, .keep_all = TRUE)
  }
  
  df
}



test_imdb_neg_trigrams <- test_imdb_neg_trigrams %>%
  RemoveTestDuplicates(ngram = 3) %>%
  filter(!is.na(word1))

test_imdb_neg_quagrams <- test_imdb_neg_quagrams %>%
  RemoveTestDuplicates(ngram = 4) %>% 
  filter(!is.na(word1))



CreatePredictionString <- function(df, ngram){
  
  if (ngram == 3){
    
    df <- df %>%
      unite(col = string, word1, word2, sep = " ",
            na.rm = TRUE, remove = FALSE)
    
  } else if (ngram == 4){
    
    df <- df %>%
      unite(col = string, word1, word2, word3, sep = " ",
            na.rm = TRUE, remove = FALSE)
  }
  
}



test_imdb_neg_trigrams <- test_imdb_neg_trigrams %>%
  CreatePredictionString(ngram = 3)
test_imdb_neg_quagrams <- test_imdb_neg_quagrams %>%
  CreatePredictionString(ngram = 4)

t <- Sys.time()
test_copy <- test_imdb_neg_trigrams

test_strings <- test_copy[["string"]]

result_words <- c()
result_P <- c()
index = 0

for (string in test_strings){
  if (index == 0){
    start_time <- Sys.time()
  }
  index =  index + 1
  if ((index %% 50) == 0){
    percent_complete <- index / length(test_strings)
    print(index / length(test_strings))
    total_time <- (Sys.time() - start_time) / percent_complete
    time_remaining <- total_time * (1 - percent_complete)
    print(time_remaining)
  }
  
  result <- Predict(string,
                    model = imdb_neg_train_model,
                    ngram = 2,
                    gamma = 0.3) %>%
    head(1)
  
  result_words <- c(result_words, result[1, "word"])
  result_P <- c(result_P, result[1, "P"])
}

test_imdb_neg_trigrams$predictions <- result_words
test_imdb_neg_trigrams$probability <- result_P

n <- Sys.time()
t <- n - t
print(t)

save(test_imdb_neg_trigrams, file = "output/test_imdb_neg_trigrams_5.RDa")



t <- Sys.time()
test_copy <- test_imdb_neg_quagrams

test_strings <- test_copy[["string"]]

result_words <- c()
result_P <- c()
index = 0

for (string in test_strings){
  if (index == 0){
    start_time <- Sys.time()
  }
  index =  index + 1
  if ((index %% 50) == 0){
    percent_complete <- index / length(test_strings)
    print(index / length(test_strings))
    total_time <- (Sys.time() - start_time) / percent_complete
    time_remaining <- total_time * (1 - percent_complete)
    print(time_remaining)
  }
  result <- Predict(string,
                    model = imdb_neg_train_model,
                    ngram = 3,
                    gamma = 0.3) %>%
    head(1)
  
  result_words <- c(result_words, result[1, "word"])
  result_P <- c(result_P, result[1, "P"])
}

test_imdb_neg_quagrams$predictions <- result_words
test_imdb_neg_quagrams$probability <- result_P

n <- Sys.time()
t <- n - t
print(t)



save(test_imdb_neg_quagrams, file = "output/test_imdb_neg_quagrams_5.RDa")







base <- LoadSingleFile(
  "data/IMDB/pos_merged.txt",
  SOS = TRUE, EOS = TRUE)

imdb_pos_unigrams <- CleanTokens(base, n = 1) %>%
  WordFreq() %>%
  filter(freq > 2) 

imdb_pos_bigrams <- CleanTokens(base, n = 2) %>%
  WordFreq() %>%
  filter(freq > 1)

imdb_pos_trigrams <- CleanTokens(base, n = 3) %>%
  WordFreq() %>%
  filter(freq > 1)

imdb_pos_quagrams <- CleanTokens(base, n = 4) %>%
  WordFreq() %>%
  filter(freq > 1)



TrainTestSplit <- function(df, percent = 0.95, seed = 17373){
  smp_size <- floor(percent * nrow(df))
  set.seed(seed)
  train_ind <- sample(seq_len(nrow(df)), size = smp_size)
  
  train <- df[train_ind, ]
  test <- df[-train_ind, ]
  
  results <- list(train = train, test = test)
}



trigrams_ <-TrainTestSplit(imdb_pos_trigrams, 0.95)

test_imdb_pos_trigrams <- trigrams_$test
train_imdb_pos_trigrams <- trigrams_$train
rm(trigrams_)

quagrams_ <-TrainTestSplit(imdb_pos_quagrams, 0.95)

test_imdb_pos_quagrams <- quagrams_$test
train_imdb_pos_quagrams <- quagrams_$train
rm(quagrams_)


imdb_pos_train_model <- CreateFreqTable(imdb_pos_unigrams,
                                       imdb_pos_bigrams,
                                       train_imdb_pos_trigrams,
                                       train_imdb_pos_quagrams)

#save(imdb_pos_train_model, file = "models/imdb_pos_train_model.RDa")



RemoveTestDuplicates <- function(df, ngram){
  
  if (ngram == 3){
    
    df <- df %>%
      separate(word, c("word1", "word2", "word3"), sep = " ") %>%
      select(c("word1", "word2", "word3", "freq"))
    df[, "word4"] <- NA
    
    df <- distinct(df, word1, word2, .keep_all = TRUE)
  } else if (ngram == 4){
    
    df <- df %>%
      separate(word, c("word1", "word2", "word3", "word4"), sep = " ") %>%
      select(c("word1", "word2", "word3", "word4", "freq"))
    
    df <- distinct(df, word1, word2, word3, .keep_all = TRUE)
  }
  
  df
}



test_imdb_pos_trigrams <- test_imdb_pos_trigrams %>%
  RemoveTestDuplicates(ngram = 3) %>%
  filter(!is.na(word1))

test_imdb_pos_quagrams <- test_imdb_pos_quagrams %>%
  RemoveTestDuplicates(ngram = 4) %>% 
  filter(!is.na(word1))



CreatePredictionString <- function(df, ngram){
  
  if (ngram == 3){
    
    df <- df %>%
      unite(col = string, word1, word2, sep = " ",
            na.rm = TRUE, remove = FALSE)
    
  } else if (ngram == 4){
    
    df <- df %>%
      unite(col = string, word1, word2, word3, sep = " ",
            na.rm = TRUE, remove = FALSE)
  }
  
}



test_imdb_pos_trigrams <- test_imdb_pos_trigrams %>%
  CreatePredictionString(ngram = 3)
test_imdb_pos_quagrams <- test_imdb_pos_quagrams %>%
  CreatePredictionString(ngram = 4)

t <- Sys.time()
test_copy <- test_imdb_pos_trigrams

test_strings <- test_copy[["string"]]

result_words <- c()
result_P <- c()
index = 0

for (string in test_strings){
  if (index == 0){
    start_time <- Sys.time()
  }
  index =  index + 1
  if ((index %% 50) == 0){
    percent_complete <- index / length(test_strings)
    print(index / length(test_strings))
    total_time <- (Sys.time() - start_time) / percent_complete
    time_remaining <- total_time * (1 - percent_complete)
    print(time_remaining)
  }
  
  result <- Predict(string,
                    model = imdb_pos_train_model,
                    ngram = 2,
                    gamma = 0.3) %>%
    head(1)
  
  result_words <- c(result_words, result[1, "word"])
  result_P <- c(result_P, result[1, "P"])
}

test_imdb_pos_trigrams$predictions <- result_words
test_imdb_pos_trigrams$probability <- result_P

n <- Sys.time()
t <- n - t
print(t)

save(test_imdb_pos_trigrams, file = "output/test_imdb_pos_trigrams_5.RDa")



t <- Sys.time()
test_copy <- test_imdb_pos_quagrams

test_strings <- test_copy[["string"]]

result_words <- c()
result_P <- c()
index = 0

for (string in test_strings){
  if (index == 0){
    start_time <- Sys.time()
  }
  index =  index + 1
  if ((index %% 50) == 0){
    percent_complete <- index / length(test_strings)
    print(index / length(test_strings))
    total_time <- (Sys.time() - start_time) / percent_complete
    time_remaining <- total_time * (1 - percent_complete)
    print(time_remaining)
  }
  result <- Predict(string,
                    model = imdb_pos_train_model,
                    ngram = 3,
                    gamma = 0.3) %>%
    head(1)
  
  result_words <- c(result_words, result[1, "word"])
  result_P <- c(result_P, result[1, "P"])
}

test_imdb_pos_quagrams$predictions <- result_words
test_imdb_pos_quagrams$probability <- result_P

n <- Sys.time()
t <- n - t
print(t)



save(test_imdb_pos_quagrams, file = "output/test_imdb_pos_quagrams_5.RDa")









base <- LoadSingleFile(
  "data/ARTISTS/lyrics.txt",
  SOS = TRUE, EOS = TRUE)

lyrics_unigrams <- CleanTokens(base, n = 1) %>%
  WordFreq() %>%
  filter(freq > 2) 

lyrics_bigrams <- CleanTokens(base, n = 2) %>%
  WordFreq() %>%
  filter(freq > 1)

lyrics_trigrams <- CleanTokens(base, n = 3) %>%
  WordFreq() %>%
  filter(freq > 1)

lyrics_quagrams <- CleanTokens(base, n = 4) %>%
  WordFreq() %>%
  filter(freq > 1)



TrainTestSplit <- function(df, percent = 0.95, seed = 17373){
  smp_size <- floor(percent * nrow(df))
  set.seed(seed)
  train_ind <- sample(seq_len(nrow(df)), size = smp_size)
  
  train <- df[train_ind, ]
  test <- df[-train_ind, ]
  
  results <- list(train = train, test = test)
}



trigrams_ <-TrainTestSplit(lyrics_trigrams, 0.95)

test_lyrics_trigrams <- trigrams_$test
train_lyrics_trigrams <- trigrams_$train
rm(trigrams_)

quagrams_ <-TrainTestSplit(lyrics_quagrams, 0.95)

test_lyrics_quagrams <- quagrams_$test
train_lyrics_quagrams <- quagrams_$train
rm(quagrams_)


lyrics_train_model <- CreateFreqTable(lyrics_unigrams,
                                       lyrics_bigrams,
                                       train_lyrics_trigrams,
                                       train_lyrics_quagrams)

#save(lyrics_train_model, file = "models/lyrics_train_model.RDa")



RemoveTestDuplicates <- function(df, ngram){
  
  if (ngram == 3){
    
    df <- df %>%
      separate(word, c("word1", "word2", "word3"), sep = " ") %>%
      select(c("word1", "word2", "word3", "freq"))
    df[, "word4"] <- NA
    
    df <- distinct(df, word1, word2, .keep_all = TRUE)
  } else if (ngram == 4){
    
    df <- df %>%
      separate(word, c("word1", "word2", "word3", "word4"), sep = " ") %>%
      select(c("word1", "word2", "word3", "word4", "freq"))
    
    df <- distinct(df, word1, word2, word3, .keep_all = TRUE)
  }
  
  df
}



test_lyrics_trigrams <- test_lyrics_trigrams %>%
  RemoveTestDuplicates(ngram = 3) %>%
  filter(!is.na(word1))

test_lyrics_quagrams <- test_lyrics_quagrams %>%
  RemoveTestDuplicates(ngram = 4) %>% 
  filter(!is.na(word1))



CreatePredictionString <- function(df, ngram){
  
  if (ngram == 3){
    
    df <- df %>%
      unite(col = string, word1, word2, sep = " ",
            na.rm = TRUE, remove = FALSE)
    
  } else if (ngram == 4){
    
    df <- df %>%
      unite(col = string, word1, word2, word3, sep = " ",
            na.rm = TRUE, remove = FALSE)
  }
  
}



test_lyrics_trigrams <- test_lyrics_trigrams %>%
  CreatePredictionString(ngram = 3)
test_lyrics_quagrams <- test_lyrics_quagrams %>%
  CreatePredictionString(ngram = 4)

t <- Sys.time()
test_copy <- test_lyrics_trigrams

test_strings <- test_copy[["string"]]

result_words <- c()
result_P <- c()
index = 0

for (string in test_strings){
  if (index == 0){
    start_time <- Sys.time()
  }
  index =  index + 1
  if ((index %% 50) == 0){
    percent_complete <- index / length(test_strings)
    print(index / length(test_strings))
    total_time <- (Sys.time() - start_time) / percent_complete
    time_remaining <- total_time * (1 - percent_complete)
    print(time_remaining)
  }
  
  result <- Predict(string,
                    model = lyrics_train_model,
                    ngram = 2,
                    gamma = 0.3) %>%
    head(1)
  
  result_words <- c(result_words, result[1, "word"])
  result_P <- c(result_P, result[1, "P"])
}

test_lyrics_trigrams$predictions <- result_words
test_lyrics_trigrams$probability <- result_P

n <- Sys.time()
t <- n - t
print(t)

save(test_lyrics_trigrams, file = "output/test_lyrics_trigrams_5.RDa")



t <- Sys.time()
test_copy <- test_lyrics_quagrams

test_strings <- test_copy[["string"]]

result_words <- c()
result_P <- c()
index = 0

for (string in test_strings){
  if (index == 0){
    start_time <- Sys.time()
  }
  index =  index + 1
  if ((index %% 50) == 0){
    percent_complete <- index / length(test_strings)
    print(index / length(test_strings))
    total_time <- (Sys.time() - start_time) / percent_complete
    time_remaining <- total_time * (1 - percent_complete)
    print(time_remaining)
  }
  result <- Predict(string,
                    model = lyrics_train_model,
                    ngram = 3,
                    gamma = 0.3) %>%
    head(1)
  
  result_words <- c(result_words, result[1, "word"])
  result_P <- c(result_P, result[1, "P"])
}

test_lyrics_quagrams$predictions <- result_words
test_lyrics_quagrams$probability <- result_P

n <- Sys.time()
t <- n - t
print(t)



save(test_lyrics_quagrams, file = "output/test_lyrics_quagrams_5.RDa")