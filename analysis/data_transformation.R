source("analysis/helper.R")

# column names to avoid duplicates, 
# some source files have a space in column names, while others use "_".
# Using underscore for all
columns1 <- c("Archive_Date",
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
# no case type in outpatients waiting list data
columns2 <- columns1[columns1 != "Case_Type"]

# load data from csv files
# inpatient/day case waiting lists
ipdc_data <- combine_csv_data("datasets/ipdc", column_names = columns1)
nrow(ipdc_data)
# inpatient/day case waiting lists for GI Endoscopy patients
ipdc_gi_data <- combine_csv_data("datasets/ipdc_gi", column_names = columns1)
nrow(ipdc_gi_data)
# out patient waiting lists
op_data <- combine_csv_data("datasets/op", column_names = columns2)
nrow(op_data)

# inspect dataframes
str(ipdc_data)
str(ipdc_gi_data)
str(op_data)

# there is differences in column naming conventions in each of the frames, 
# "." vs "_" for spaces. This needs to be updated so that all use an underscore
ipdc_data <- replace_character_in_column_names(ipdc_data, ".", "_")

