source("helpers/load_libs.R")
library(reshape2)
library(VIM)

combine_csv_data <- function(path_to_files, column_names){
  # list of csv files
  csv_files <- dir(path = path_to_files, 
                   pattern=".*[.]csv", full.names = TRUE)
  # use vroom library to combine files into single tibble
  combined_data <- vroom(csv_files, skip = 1, 
                         col_names = column_names)
  return(combined_data)
}

convert_dates <- function(data){
  # parse_date_time() function from lubridate package
  data$Archive_Date <- parse_date_time(x = data$Archive_Date,
                                       orders = c("Y-m-d", "d/m/Y"))
  # convert dates to quarterly figures
  data$Archive_Date <- lubridate::quarter(data$Archive_Date, 
                                          with_year = TRUE,
                                          fiscal_start = 1)
  return(data)
}