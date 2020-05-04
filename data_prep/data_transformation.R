source("helpers/data_prep_helper.R")
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
                  "Speciality_HIPE",
                  "Speciality_Name",
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

# inspect tibble structure
str(all_waiting_lists)

# dates are a mixture of Y-m-d and d/m/Y formats
# so they need to be converted to a single date type
all_waiting_lists <- convert_dates(all_waiting_lists)
# check the structure again
str(all_waiting_lists)

missing_values <- aggr(all_waiting_lists, prop = FALSE, numbers = TRUE)
missing_values
# Missing case types are expected for all outpatient records so 
# assigning those a value of "Outpatient"
all_waiting_lists$Case_Type[is.na(all_waiting_lists$Case_Type)] <- "Outpatient" 

# drop rows that have no associated hospital name
all_waiting_lists <- subset(all_waiting_lists, 
                            !is.na(all_waiting_lists$Hospital_Name))

# The 1 row with a missing Time_Bands value will be removed
# all other missing values are deemed irrelevant for the current 
# project, no action will be taken
nrow(all_waiting_lists)
all_waiting_lists <- all_waiting_lists[!is.na(all_waiting_lists$Time_Bands),]
nrow(all_waiting_lists)



# get all possible time bands to check for any duplication
unique(all_waiting_lists[, c("Time_Bands")])
# some duplicates with added white space/different format
# update so there are no duplicates
all_waiting_lists$Time_Bands <- as.character(all_waiting_lists$Time_Bands)
all_waiting_lists$Time_Bands <- trimws(all_waiting_lists$Time_Bands)
all_waiting_lists$Time_Bands[
  all_waiting_lists$Time_Bands == "18 Months +"] <- "18+ Months"

# looks good now
unique(all_waiting_lists[, c("Time_Bands")])

# categorise time bands as either waiting for < 1 year or > 1 year 
attach(all_waiting_lists)
# < 1 year
all_waiting_lists$Time_Bands[
  Time_Bands == "0-3 Months" | 
    Time_Bands == "3-6 Months" |
    Time_Bands == "6-9 Months" |
    Time_Bands == "9-12 Months"] <- "< 1 Year"
# > 1 year
all_waiting_lists$Time_Bands[
  Time_Bands == "12-15 Months" | 
    Time_Bands == "15-18 Months" |
    Time_Bands == "18+ Months"] <- "> 1 Year"
detach(all_waiting_lists)
all_waiting_lists$Time_Bands <- as.factor(all_waiting_lists$Time_Bands)
str(all_waiting_lists)

# removing Hospital_HIPE, Hospital_Name, Speciality_HIPE, 
# Case_Type, Age_Profile and Adult_Child columns. The focus of 
all_waiting_lists <- select(all_waiting_lists, -c(Hospital_HIPE,
                                                  Hospital_Name,
                                                  Speciality_HIPE,
                                                  Case_Type,
                                                  Adult_Child,
                                                  Age_Profile))

# combining the Children's Hospital Group and Children's Health Ireland, 
# all fall under CHI since 2018
attach(all_waiting_lists)
all_waiting_lists$Hospital_Group[
  Hospital_Group == "Children's Hospital Group"] <- "Children's Health Ireland"
detach(all_waiting_lists)

str(all_waiting_lists)
# aggregate totals based on length on waiting list
all_waiting_lists <- aggregate(cbind(Total) ~ 
                                 Time_Bands + 
                                 Speciality_Name + 
                                 Hospital_Group + 
                                Archive_Date, 
                               data = all_waiting_lists, sum)
# remove row where Archive_Date = 2020.1 
# as this only includes January figures for that year
all_waiting_lists <- all_waiting_lists[
  (year(all_waiting_lists$Archive_Date) < 2020), ]

write_csv(all_waiting_lists, "data_prep/combined_waiting_lists.csv")