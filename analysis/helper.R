library(tidyverse)
library(vroom)

combine_csv_data <- function(path_to_files, column_names){
  # list of csv files
  csv_files <- dir(path = path_to_files, 
                   pattern=".*[.]csv", full.names = TRUE)
  # use vroom library to combine files
  combined_data <- vroom(csv_files, skip = 1, col_names = column_names)
  return(combined_data)
}

replace_character_in_column_names <- function(dataframe, old, new){
  # replace "old" with "new" character in all columns of supplied dataframe
  colnames(dataframe) <- gsub(old, new, colnames(dataframe), fixed = TRUE)
  
  # merge any duplicate columns
  
  return(dataframe)
}