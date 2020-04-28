source("data_prep/helper.R")
library(reshape2)
##############################################################################
# Data transformation 
# - load and process source data, 
#   then output to new csv files
##############################################################################

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

#######################################
# outpatient data columns
#######################################
# names:
# no case type in outpatients waiting list data
columns_op <- columns_ipdc[columns_ipdc != "Case_Type"]

# load data from csv files
# inpatient/day case waiting lists
ipdc_data <- combine_csv_data("datasets/ipdc", 
                              column_names = columns_ipdc)
nrow(ipdc_data)
# inpatient/day case waiting lists for GI Endoscopy patients
ipdc_gi_data <- combine_csv_data("datasets/ipdc_gi", 
                                 column_names = columns_ipdc)
nrow(ipdc_gi_data)
# out patient waiting lists
op_data <- combine_csv_data("datasets/op", 
                            column_names = columns_op)
# add Case_Type which will be NA for all out patient records
op_data$Case_Type <- NA
nrow(op_data)

# combine all waiting list data
all_waiting_lists <- bind_rows(list(ipdc_data, ipdc_gi_data, op_data))
# sort by descending archive data
all_waiting_lists <- all_waiting_lists[
  order(all_waiting_lists$Archive_Date, decreasing = TRUE),]
all_waiting_lists
# inspect tibble structure
str(all_waiting_lists)

# dates are a mixture of Y-m-d and d/m/Y formats
# so they need to be converted to a single date type, the day part will be excluded 
# because it is not necessary for the analysis that follows
all_waiting_lists <- convert_dates(all_waiting_lists)

# other column type conversions
attach(all_waiting_lists)
all_waiting_lists$Hospital_Group <- as.factor(Hospital_Group)
all_waiting_lists$Hospital_Name <- as.factor(Hospital_Name)
all_waiting_lists$Hospital_HIPE <- as.numeric(Hospital_HIPE)
all_waiting_lists$Specialty_Name <- as.factor(Specialty_Name)
all_waiting_lists$Specialty_HIPE <- as.numeric(Specialty_HIPE)
all_waiting_lists$Case_Type <- as.factor(Case_Type)
all_waiting_lists$Adult_Child <- as.factor(Adult_Child)
all_waiting_lists$Age_Profile <- as.factor(Age_Profile)
all_waiting_lists$Time_Bands <- as.factor(Time_Bands)
detach(all_waiting_lists)

# check the structure again
str(all_waiting_lists)

# drop rows that have no associated hospital name
all_waiting_lists <- drop_records_with_no_hospital(all_waiting_lists)

##############################################################################
# Create hospital details lookup file
##############################################################################
str(all_waiting_lists)
# hipe has 48 variations, while hospital names have 62 variations

# get unique combinations of 'Hospital_Group', 'Hospital_HIPE' and 'Hospital_Name'
unique_hospital_values <- unique(all_waiting_lists
                                 [, c('Hospital_HIPE',
                                      'Hospital_Group',
                                      'Hospital_Name')])
# take first occurance of each HIPE code to avoid duplication
unique_hospital_values <- unique_hospital_values[!duplicated(
  unique_hospital_values$Hospital_HIPE), ]
# check for NA values in HIPE column
na_hospital_hipe <- is.na(unique_hospital_values$Hospital_HIPE)
na_hospital_hipe
# no NAs present!

# write these to a csv file
write_csv(unique_hospital_values, "data_prep/hospital_lookup.csv")
# now the full strings for hospital group and name can be removed from the dataset
all_waiting_lists <- select(all_waiting_lists, -c(Hospital_Group, Hospital_Name))
##############################################################################

##############################################################################
# Create Speciality name lookup file
##############################################################################
unique_speciality_values <- unique(all_waiting_lists
                                   [, c('Specialty_HIPE',
                                        'Specialty_Name')])
unique_speciality_values <- unique_speciality_values[!duplicated(
  unique_speciality_values$Specialty_HIPE), ]
# check for NA values in HIPE column
na_speciality_hipe <- is.na(unique_speciality_values$Specialty_HIPE)
na_speciality_hipe # 1 returned
# get the NA row
na_speciality_hipe <- unique_speciality_values[
  is.na(unique_speciality_values$Specialty_HIPE),]
na_speciality_hipe # "Other" speciality type
# update it to 1111
other_hipe <- 1111
attach(unique_speciality_values)
unique_speciality_values$Specialty_HIPE[Specialty_Name == "Other"] <- other_hipe
detach(unique_speciality_values)

# take first occurance of each HIPE code to avoid any duplication
unique_speciality_values <- unique_speciality_values[!duplicated(
  unique_speciality_values$Specialty_HIPE), ]

# write specialities to a csv file
write_csv(unique_speciality_values, "data_prep/speciality_lookup.csv")

# update the other hipe code and remove the speciality name field from the main dataset
attach(all_waiting_lists)
all_waiting_lists$Specialty_HIPE[Specialty_Name == "Other"] <- other_hipe
detach(all_waiting_lists)
all_waiting_lists <- select(all_waiting_lists, -c(Specialty_Name))
##############################################################################

# new structure of dataset
str(all_waiting_lists)

# make date a factor for use when splitting the data
all_waiting_lists$Archive_Date <- as.factor(all_waiting_lists$Archive_Date)
str(all_waiting_lists)

# output initial cleaned data to csv
write_csv(all_waiting_lists, "data_prep/all_waiting_lists.csv")

##############################################################################
# Create new dataset with monthly totals per hospital as columns
##############################################################################
# reshape data so that there is a column for each month
# and rows with for each time band per hospital
hospitals_with_date_columns <- dcast(all_waiting_lists, 
                                     Hospital_HIPE + Time_Bands ~ Archive_Date,
                                     value.var = "Total")
str(with_date_columns)

# write to a csv file
write_csv(with_date_columns, "data_prep/monthly_figures_by_hospital.csv")

##############################################################################
# Create new dataset with monthly totals per speciality as columns
##############################################################################
# reshape data so that there is a column for each month
# and rows with totals per speciality
specialities_with_date_columns <- dcast(all_waiting_lists, 
                                        Specialty_HIPE ~ Archive_Date,
                                        value.var = "Total")
str(specialities_with_date_columns)

# write to a csv file
write_csv(specialities_with_date_columns, "data_prep/monthly_figures_by_speciality.csv")
