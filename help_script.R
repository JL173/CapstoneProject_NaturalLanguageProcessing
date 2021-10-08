setwd("C:/Users/JL/Desktop/Study/Coursera/Johns Hopkins Data Science/10 capstone/CapstoneProject_NaturalLanguageProcessing")

library(tidyverse)
library(tidytext)
library(tm)
library(openNLP)

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



# Get PArt of Speech tags and sum of each
base_ch <- UnlistDF(base_1000, size = 5e4) %>%
  lapply(POSsumDF) %>%
  MergeDTM()
