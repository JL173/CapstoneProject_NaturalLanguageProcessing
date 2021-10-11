# working directory
setwd("C:/Users/JL/Desktop/Study/Coursera/Johns Hopkins Data Science/10 capstone/CapstoneProject_NaturalLanguageProcessing")

# modules
library(tidyverse)
library(tidytext)
library(tm)
library(openNLP)
library(data.table)
library(parallel)
library(doParallel)

data(stop_words)
filter_words <- read.table("text_filter.txt", header=FALSE, sep="", col.names = "word")
filter_words <- full_join(filter_words, stop_words)

###
# 1
###

full_tidy_dtm_1 <- "data/en_US/" %>%
  LoadFiles() %>%
  lapply(CleanTokens,
         n = 1,
         filter_df = filter_words) %>%
  MergeDTM()

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

