
# use findand replace to swap out 'twitter'/'blogs' etc.

base <- LoadSingleFile(
  "data/en_US/25/en_US.twitter.25.txt",
  SOS = TRUE, EOS = TRUE)

twitter_unigrams <- CleanTokens(base, n = 1) %>%
  WordFreq() %>%
  filter(freq > 2) 

twitter_bigrams <- CleanTokens(base, n = 2) %>%
  WordFreq() %>%
  filter(freq > 1)

twitter_trigrams <- CleanTokens(base, n = 3) %>%
  WordFreq() %>%
  filter(freq > 1)

twitter_quagrams <- CleanTokens(base, n = 4) %>%
  WordFreq() %>%
  filter(freq > 1)



TrainTestSplit <- function(df, percent = 0.5, seed = 17373){
  smp_size <- floor(percent * nrow(df))
  set.seed(seed)
  train_ind <- sample(seq_len(nrow(df)), size = smp_size)
  
  train <- df[train_ind, ]
  test <- df[-train_ind, ]
  
  results <- list(train = train, test = test)
}



trigrams_ <-TrainTestSplit(twitter_trigrams, 0.5)

test_twitter_trigrams <- trigrams_$test
train_twitter_trigrams <- trigrams_$train
rm(trigrams_)

quagrams_ <-TrainTestSplit(twitter_quagrams, 0.5)

test_twitter_quagrams <- quagrams_$test
train_twitter_quagrams <- quagrams_$train
rm(quagrams_)


twitter_train_model <- CreateFreqTable(twitter_unigrams,
                                    twitter_bigrams,
                                    train_twitter_trigrams,
                                    train_twitter_quagrams)

#save(twitter_train_model, file = "models/twitter_train_model.RDa")



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



test_twitter_trigrams <- test_twitter_trigrams %>%
  RemoveTestDuplicates(ngram = 3) %>%
  filter(!is.na(word1))

test_twitter_quagrams <- test_twitter_quagrams %>%
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



test_twitter_trigrams <- test_twitter_trigrams %>%
  CreatePredictionString(ngram = 3)
test_twitter_quagrams <- test_twitter_quagrams %>%
  CreatePredictionString(ngram = 4)

t <- Sys.time()
test_copy <- test_twitter_trigrams

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
                    model = twitter_train_model,
                    ngram = 2,
                    gamma = 0.3) %>%
    head(1)

  result_words <- c(result_words, result[1, "word"])
  result_P <- c(result_P, result[1, "P"])
}

test_twitter_trigrams$predictions <- result_words
test_twitter_trigrams$probability <- result_P

n <- Sys.time()
t <- n - t
print(t)

save(test_twitter_trigrams, file = "output/test_twitter_trigrams_50.RDa")



t <- Sys.time()
test_copy <- test_twitter_quagrams

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
                    model = twitter_train_model,
                    ngram = 3,
                    gamma = 0.3) %>%
    head(1)
  
  result_words <- c(result_words, result[1, "word"])
  result_P <- c(result_P, result[1, "P"])
}

test_twitter_quagrams$predictions <- result_words
test_twitter_quagrams$probability <- result_P

n <- Sys.time()
t <- n - t
print(t)



save(test_twitter_quagrams, file = "output/test_twitter_quagrams_50.RDa")


load("output/test_twitter_trigrams_50.RDa")
CostCheck(test_twitter_trigrams, "word3")
load("output/test_twitter_trigrams_25.RDa")
CostCheck(test_twitter_trigrams, "word3")
load("output/test_twitter_trigrams_10.RDa") 
CostCheck(test_twitter_trigrams, "word3")
load("output/test_twitter_trigrams_5.RDa")
CostCheck(test_twitter_trigrams, "word3") # using 95/5 split

load("output/test_twitter_quagrams_50.RDa")
CostCheck(test_twitter_quagrams, "word4")
load("output/test_twitter_quagrams_25.RDa")
CostCheck(test_twitter_quagrams, "word4")
load("output/test_twitter_quagrams_10.RDa") 
CostCheck(test_twitter_quagrams, "word4")
load("output/test_twitter_quagrams_5.RDa") # using 95/5 split
CostCheck(test_twitter_quagrams, "word4")




