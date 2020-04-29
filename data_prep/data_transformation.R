source("helpers/data_prep_helper.R")
library(reshape2)
library(VIM)
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
# so they need to be converted to a single date type, the day part will be excluded 
# because it is not necessary for the analysis that follows
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

# column type conversions
attach(all_waiting_lists)
all_waiting_lists$Hospital_Group <- as.factor(Hospital_Group)
all_waiting_lists$Hospital_Name <- as.factor(Hospital_Name)
all_waiting_lists$Hospital_HIPE <- as.numeric(Hospital_HIPE)
all_waiting_lists$Speciality_Name <- as.factor(Speciality_Name)
all_waiting_lists$Speciality_HIPE <- as.numeric(Speciality_HIPE)
all_waiting_lists$Case_Type <- as.factor(Case_Type)
all_waiting_lists$Adult_Child <- as.factor(Adult_Child)
all_waiting_lists$Age_Profile <- as.factor(Age_Profile)
all_waiting_lists$Time_Bands <- as.factor(Time_Bands)
detach(all_waiting_lists)

##############################################################################
# Handle missing Speciality HIPE
# - assign specialities an id and then remove the HIPE and name columns
##############################################################################
# get unique speciality name values
unique_specialities <- unique(all_waiting_lists[, c("Speciality_Name")])
str(unique_specialities)

# convert the Speciality_Name factor to integer values
# then add prefix to each row and make a factor again
unique_specialities$Speciality_ID <- as.integer(unique_specialities$Speciality_Name)
unique_specialities$Speciality_ID <- paste("spec", 
                                           unique_specialities$Speciality_ID, 
                                           sep = "_")
unique_specialities$Speciality_ID <- as.factor(unique_specialities$Speciality_ID)
head(unique_specialities)

# write this data to a csv for later lookup purposes
write_csv(unique_specialities, "data_prep/speciality_lookup.csv")

# add a speciality id column to main dataset and remove speciality hipe/name 
# inner join unique_specialities on all_waiting_lists by "Speciality_Name"
all_waiting_lists <- all_waiting_lists %>% inner_join(unique_specialities)
all_waiting_lists <- select(all_waiting_lists, -c(Speciality_Name, Speciality_HIPE))

# check number of categories of hospitals and hospital group
count(unique(all_waiting_lists[, c("Hospital_Name")]))
count(unique(all_waiting_lists[, c("Hospital_Group")]))
# hospital groups will be used for analysis rather than individual hospitals.
# The reasoning for this is that patients in regions are generally shared among hospitals
# so looking at combined figures is probably more accurate. 
# In this case 8 variations are considered rather than 62 
# if each individual hospital is used

# get unique hospital group name values
unique_groups <- unique(all_waiting_lists[, c("Hospital_Group")])

# convert the Hospital_Group factor to integer values
unique_groups$Group_ID <- as.integer(unique_groups$Hospital_Group)
unique_groups$Group_ID <- paste("group", 
                                unique_groups$Group_ID, 
                                sep = "_")
unique_groups$Group_ID <- as.factor(unique_groups$Group_ID)
head(unique_groups)

# write this data to a csv for later lookup purposes
write_csv(unique_groups, "data_prep/hospital_group_lookup.csv")

# now merge these IDs into the main dataset and remove 
# Hospital_Group, Hospital_Name & HIPE
all_waiting_lists <- all_waiting_lists %>% inner_join(unique_groups)
all_waiting_lists <- select(all_waiting_lists, -c(Hospital_Group, 
                                                  Hospital_Name, 
                                                  Hospital_HIPE))

# make date a factor for use when splitting the data
all_waiting_lists$Archive_Date <- as.factor(all_waiting_lists$Archive_Date)

# new structure of dataset
str(all_waiting_lists)

# output initial cleaned data to csv
write_csv(all_waiting_lists, "data_prep/all_waiting_lists.csv")

##############################################################################
# Create new dataset with monthly totals of each speciality 
# per hospital group as columns
##############################################################################
# reshape data so that there is a column for each speciality
# and rows for each month per hospital group
speciality_columns_added <- reshape2::dcast(all_waiting_lists, 
                                  Archive_Date + 
                                    Time_Bands + 
                                    Group_ID 
                                  ~ Speciality_ID,
                                  value.var = "Total",
                                  )
str(speciality_columns_added)

# get all possible time bands
unique(speciality_columns_added[, c("Time_Bands")])
# some duplicates with added white space/different format
# update so there are no duplicates
speciality_columns_added$Time_Bands <- as.character(speciality_columns_added$Time_Bands)
speciality_columns_added$Time_Bands <- trimws(speciality_columns_added$Time_Bands)
speciality_columns_added$Time_Bands[
  speciality_columns_added$Time_Bands == "18 Months +"] <- "18+ Months"


# # replace spaces and dashes with underscore
# speciality_columns_added$Time_Bands <- gsub(speciality_columns_added$Time_Bands, 
#                                             pattern = "-", replacement = "_") 
# speciality_columns_added$Time_Bands <- gsub(speciality_columns_added$Time_Bands, 
#                                             pattern = "\\+", replacement = "plus") 
# speciality_columns_added$Time_Bands <- gsub(speciality_columns_added$Time_Bands, 
#                                             pattern = " Months", replacement = "") 
# speciality_columns_added$Time_Bands <- paste("waiting", 
#                                              speciality_columns_added$Time_Bands, 
#                                              sep = "_")
# speciality_columns_added$Time_Bands <- as.factor(speciality_columns_added$Time_Bands)

# check again
unique(speciality_columns_added[, c("Time_Bands")])
# looks good now

# categorise time bands as either waitining for < 1 year or > 1 year 
attach(speciality_columns_added)
# < 1 year
speciality_columns_added$Time_Bands[
  Time_Bands == "0-3 Months" | 
    Time_Bands == "3-6 Months" |
    Time_Bands == "6-9 Months" |
    Time_Bands == "9-12 Months"] <- "<1Yr"
# > 1 year
speciality_columns_added$Time_Bands[
  Time_Bands == "12-15 Months" | 
    Time_Bands == "15-18 Months" |
    Time_Bands == "18+ Months"] <- ">1Yr"
detach(speciality_columns_added)

# add all speciality figures to get monthly total for each timeband of all hospital groups
monthly_totals <- speciality_columns_added %>% 
  select(starts_with("spec_")) %>% 
  rowSums()
monthly_totals

# apply percentage (of monthly total) calculations to all speciality columns
speciality_columns_added <- speciality_columns_added %>%
  mutate_at(vars(starts_with("spec_")), funs(. / monthly_totals))

# write to a csv file
write_csv(speciality_columns_added, "data_prep/modified_waiting_list_data.csv")

####################





# # get sum of each speciality per month for each hospital group
# sum_of_speciality_figures <- speciality_columns_added %>% 
#   group_by(Archive_Date, Group_ID) %>% 
#   summarise_at(vars(starts_with("spec_")), funs(sum))
# 
# # create column for each hospital group's timeband total per month
# time_bands_summed <- reshape2::dcast(total_per_time_band,
#                                     Archive_Date +
#                                       Group_ID 
#                                     ~ Time_Bands,
#                                     value.var = "total", sum)
# 
# # merge time band totals data with main dataset 
# modified_waiting_lists_data <- time_bands_summed %>% 
#   inner_join(sum_of_speciality_figures, 
#              by = c("Archive_Date", "Group_ID"))
# 
# # check that totals match
# spec_totals <- modified_waiting_lists_data %>% 
#   select(starts_with("spec_")) %>% rowSums()
# time_band_totals <- modified_waiting_lists_data %>% 
#   select(starts_with("waiting_")) %>% rowSums()
# all.equal(spec_totals, time_band_totals) # returns TRUE so totals are the same!
# 
# str(modified_waiting_lists_data)
# 
# # calculate percentage makeup of time bands
# columns_to_sum <- c("waiting_0_3", 
#                     "waiting_12_15",
#                     "waiting_15_18",
#                     "waiting_18plus",
#                     "waiting_3_6",
#                     "waiting_6_9",
#                     "waiting_9_12")
# total <- rowSums(modified_waiting_lists_data[, columns_to_sum])
# 
# modified_waiting_lists_data$`waiting_0_3` <- 
#   modified_waiting_lists_data$`waiting_0_3` / total
# modified_waiting_lists_data$`waiting_3_6` <- 
#   modified_waiting_lists_data$`waiting_3_6` / total
# modified_waiting_lists_data$`waiting_6_9` <- 
#   modified_waiting_lists_data$`waiting_6_9` / total
# modified_waiting_lists_data$`waiting_9_12` <- 
#   modified_waiting_lists_data$`waiting_9_12` / total
# modified_waiting_lists_data$`waiting_12_15` <- 
#   modified_waiting_lists_data$`waiting_12_15` / total
# modified_waiting_lists_data$`waiting_15_18` <- 
#   modified_waiting_lists_data$`waiting_15_18` / total
# modified_waiting_lists_data$waiting_18plus <- 
#   modified_waiting_lists_data$waiting_18plus / total
# 
# # write to a csv file
# write_csv(modified_waiting_lists_data, "data_prep/modified_waiting_list_data.csv")
# 
# ##############################################################################
# # Create new dataset with monthly totals per speciality as columns
# ##############################################################################
# # reshape data so that there is a column for each month
# # and rows with totals per speciality
# specialities_with_date_columns <- dcast(all_waiting_lists, 
#                                         Speciality_HIPE ~ Archive_Date,
#                                         value.var = "Total")
# str(specialities_with_date_columns)
# 
# # write to a csv file
# write_csv(specialities_with_date_columns, "data_prep/monthly_figures_by_speciality.csv")
