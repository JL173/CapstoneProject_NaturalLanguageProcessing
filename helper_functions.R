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
                  book = shortnames[index])
    
    document_list[[index]] <- df_
    
  }
  
  document_list
}