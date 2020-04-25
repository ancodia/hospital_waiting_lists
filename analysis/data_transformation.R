source("analysis/helper.R")
#######################################
# Data transformation 
# - load and process source data, 
#   then output to new csv files
#######################################

# column names vectors to avoid duplicates on load of data, 
# some source files have a space in column names, while others use "_".
# Using underscore for all

#######################################
# inpatient day case data columns
#######################################
# names:
columns_ipdc <- c("Archive_Date",
                  "Hospital_Group",
                  "Hospital_HIPE",
                  "Hospital_Name",
                  "Specialty_HIPE",
                  "Specialty_Name",
                  "Case_Type",
                  "Adult_Child",
                  "Age_Profile",
                  "Time_Bands",
                  "Total")
# types
# https://www.rdocumentation.org/packages/vroom/versions/1.0.2/topics/vroom
# c = character, i = integer, n = number, d = double, l = logical, f = factor, 
# D = date, T = date time, t = time
column_types_ipdc <- "cfffffffffi"

#######################################
# outpatient data columns
#######################################
# names:
# no case type in outpatients waiting list data
columns_op <- columns_ipdc[columns_ipdc != "Case_Type"]

# types:
column_types_op <- "cffffffffi"

# load data from csv files
# inpatient/day case waiting lists
ipdc_data <- combine_csv_data("datasets/ipdc", 
                              column_names = columns_ipdc,
                              column_types = column_types_ipdc)
nrow(ipdc_data)
# inpatient/day case waiting lists for GI Endoscopy patients
ipdc_gi_data <- combine_csv_data("datasets/ipdc_gi", 
                                 column_names = columns_ipdc,
                                 column_types = column_types_ipdc)
nrow(ipdc_gi_data)
# out patient waiting lists
op_data <- combine_csv_data("datasets/op", 
                            column_names = columns_op,
                            column_types = column_types_op)
nrow(op_data)

# inspect tibble structures
str(ipdc_data)
str(ipdc_gi_data)
str(op_data)

# dates were imported as characters because there is a mixture of Y-m-d and d/m/Y formats
# so they need to be converted to a single date type
ipdc_data <- convert_dates(ipdc_data)
ipdc_gi_data <- convert_dates(ipdc_gi_data)
op_data <- convert_dates(op_data)

# check that Archive_Date is now date type
str(ipdc_data)
str(ipdc_gi_data)
str(op_data)

# removing Speciality_HIPE column because it doesn't provide any useful information
# dplyr select
ipdc_data <- select(ipdc_data, -c(Specialty_HIPE))
ipdc_gi_data <- select(ipdc_gi_data, -c(Specialty_HIPE))
op_data <- select(op_data, -c(Specialty_HIPE))

# Hospital_HIPE code is useful in the case where a hospital's name has changed
# for example Letterkenny General Hospital has been renamed to 
# Letterkenny University Hospital. There is an issue with these Hospital_HIPE though
# where some have a proceeding 0 such as Our Lady's Childeren's Hospital Crumlin
# having 0941 as HIPE in some records while it is 941 in other.
