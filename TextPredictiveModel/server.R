#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tidytext)
library(tm)
library(ggthemes)
library(openNLP)
library(R.utils)
library(data.table)

#
#
#
#
# Helper Functions

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


#
#
#
#
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    

})
