# Read in all txt files from a directory as a list of files
LoadFiles <- function(directory, pattern = "*.txt", n = -1L){
  filenames <- list.files(directory, pattern = pattern, full.names = TRUE)
  shortnames <- list.files(directory, pattern = pattern, full.names = FALSE)
  
  document_list <- list()
  
  for(index in length(filenames)){
    
    document <- readLines(filenames[index],
                          n,
                          skipNul = TRUE,
                          encoding = "UTF-8")
    
    df_ <- tibble(line = 1:length(document),
                  text = document,
                  book = shortnames[index])
    
    document_list <- c(document_list, df_)
    
  }
  
  document_tibble <- tibble(index = 1:length(filenames),
                            data = document_list)
}