source("helpers/descriptive_statistics_helper.R")

# load the prepared dataset
waiting_lists <- read_csv("data_prep/combined_waiting_lists.csv")
str(waiting_lists)
# set time bands to be an ordered factor
waiting_lists$Time_Bands <- factor(waiting_lists$Time_Bands, 
                                   ordered = TRUE, 
                                   levels = c("< 1 Year", "> 1 Year"))
# hospital groups should 
# also be factor variable (unordered)
waiting_lists$Hospital_Group <- as.factor(waiting_lists$Hospital_Group)
unique(waiting_lists$Hospital_Group)
str(waiting_lists)

# grouped by year
waiting_lists_per_year <- aggregate(cbind(Total) ~ 
                                   Time_Bands + 
                                   Speciality_Name + 
                                   Hospital_Group + 
                                   year(Archive_Date), 
                                 data = waiting_lists, sum)
names(waiting_lists_per_year)[names(waiting_lists_per_year) == "year(Archive_Date)"] <- "Year"
# grouped bar plot of numbers on waiting lists per year
bar_chart <- ggplot(waiting_lists_per_year, 
       aes(fill=Time_Bands, y=Total, x=Year)) + 
  geom_bar(position="dodge", stat="identity")


ggpubr::ggpar(bar_chart,
              title = "Number of Patients on Waiting Lists",
              subtitle = "Irish Hospitals (2014-19)",
              legend.title = "Waiting Time", legend.position = "top",
              ggtheme = theme_gray(), palette = "jco", xticks.by = 1)

# exclude day from archive date
waiting_lists$Archive_Date <- format(as.Date(waiting_lists$Archive_Date, format="%Y-%m-%d"), "%Y-%m")

# grouped by month
waiting_lists_per_month <- aggregate(cbind(Total) ~ 
                             Time_Bands + 
                             Hospital_Group + 
                             Archive_Date, 
                           data = waiting_lists, sum)

# plot a density chart for each hospital group
density <- ggplot(waiting_lists, aes(x = Total, color = Time_Bands)) +
  geom_density(adjust = 15) +
  facet_wrap(~Hospital_Group, nrow = 2)

ggpubr::ggpar(density,
              title = "Densities of Total Waiting List Numbers",
              subtitle = "Irish Hospital Groups (2014-19)",
              legend.title = "Waiting Time", legend.position = "top",
              ggtheme = theme_grey(), palette = "Set1")

# densities are skewed so will need to use non-parametric methods for analysis

# create a tibble for each hospital group
attach(waiting_lists_per_month)
unique(Hospital_Group)
# combining children's hospital and children's health groups
childrens_hospital <- waiting_lists_per_month[(Hospital_Group == "CHI"), ]
dublin_midlands <- waiting_lists_per_month[(Hospital_Group == "Dublin Midlands"), ]
ireland_east <- waiting_lists_per_month[(Hospital_Group == "Ireland East"), ]
rcsi <- waiting_lists_per_month[(Hospital_Group == "RCSI"), ]
saolta <- waiting_lists_per_month[(Hospital_Group == "Saolta"), ]
university_of_limerick <- waiting_lists_per_month[(Hospital_Group == "UL"), ]
south_south_west <- waiting_lists_per_month[(Hospital_Group == "South/South West"), ]
detach(waiting_lists_per_month)

#############################################
# Trend Analysis
#############################################

#########################################
# Overall
#########################################
attach(waiting_lists_per_month)
less_than_1yr_overall <- waiting_lists_per_month[(Time_Bands == "< 1 Year"), ]
greater_than_1yr_overall <- waiting_lists_per_month[(Time_Bands == "> 1 Year"), ]
detach(waiting_lists_per_month)

# Less than a year waiting
title1 <- "Overall (< 1 Year)"
ts1 <- time_series_analysis(less_than_1yr_overall, title = title1)
plot_timeseries_data(ts1, title1)

# More than a year waiting
title2 <- "Overall (> 1 Year)"
ts2 <- time_series_analysis(greater_than_1yr_overall, title = title2)
plot_timeseries_data(ts2, title2)

#########################################
# Children's Health Ireland
#########################################
attach(childrens_hospital)
less_than_1yr_childrens_hospital <- childrens_hospital[(Time_Bands == "< 1 Year"), ]
greater_than_1yr_childrens_hospital <- childrens_hospital[(Time_Bands == "> 1 Year"), ]
detach(childrens_hospital)

# Less than a year waiting
title1 <- "Children's Health Ireland (< 1 Year)"
ts1 <- time_series_analysis(less_than_1yr_childrens_hospital, title = title1)
plot_timeseries_data(ts1, title1)

# More than a year waiting
title2 <- "Children's Health Ireland (> 1 Year)"
ts2 <- time_series_analysis(greater_than_1yr_childrens_hospital, title = title2)
plot_timeseries_data(ts2, title2)

#########################################
# Dublin Midlands Hospital Group
#########################################
attach(dublin_midlands)
less_than_1yr_dublin_midlands <- dublin_midlands[(Time_Bands == "< 1 Year"), ]
greater_than_1yr_dublin_midlands <- dublin_midlands[(Time_Bands == "> 1 Year"), ]
detach(dublin_midlands)

# Less than a year waiting
title1 <- "Dublin Midlands Hospital Group (< 1 Year)"
ts1 <- time_series_analysis(less_than_1yr_dublin_midlands, title = title1)
plot_timeseries_data(ts1, title1)

# More than a year waiting
title2 <- "Dublin Midlands Hospital Group (> 1 Year)"
ts2 <- time_series_analysis(greater_than_1yr_dublin_midlands, title = title2)
plot_timeseries_data(ts2, title2)


#########################################
# Ireland East Hospital Group
#########################################
attach(ireland_east)
less_than_1yr_ireland_east <- ireland_east[(Time_Bands == "< 1 Year"), ]
greater_than_1yr_ireland_east <- ireland_east[(Time_Bands == "> 1 Year"), ]
detach(ireland_east)

# Less than a year waiting
title1 <- "Ireland East Hospital Group (< 1 Year)"
ts1 <- time_series_analysis(less_than_1yr_ireland_east, title = title1)
plot_timeseries_data(ts1, title1)

# More than a year waiting
title2 <- "Ireland East Hospital Group (> 1 Year)"
ts2 <- time_series_analysis(greater_than_1yr_ireland_east, title = title2)
plot_timeseries_data(ts2, title2)

#########################################
# RCSI Hospitals Group
#########################################
attach(rcsi)
less_than_1yr_rcsi <- rcsi[(Time_Bands == "< 1 Year"), ]
greater_than_1yr_rcsi <- rcsi[(Time_Bands == "> 1 Year"), ]
detach(rcsi)

# Less than a year waiting
title1 <- "RCSI Hospitals Group (< 1 Year)"
ts1 <- time_series_analysis(less_than_1yr_rcsi, title = title1)
plot_timeseries_data(ts1, title1)

# More than a year waiting
title2 <- "RCSI Hospitals Group (> 1 Year)"
ts2 <- time_series_analysis(greater_than_1yr_rcsi, title = title2)
plot_timeseries_data(ts2, title2)

#########################################
# Saolta University Health Care Group
#########################################
attach(saolta)
less_than_1yr_saolta <- saolta[(Time_Bands == "< 1 Year"), ]
greater_than_1yr_saolta <- saolta[(Time_Bands == "> 1 Year"), ]
detach(saolta)

# Less than a year waiting
title1 <- "Saolta University Health Care Group (< 1 Year)"
ts1 <- time_series_analysis(less_than_1yr_saolta, title = title1)
plot_timeseries_data(ts1, title1)

# More than a year waiting
title2 <- "Saolta University Health Care Group (> 1 Year)"
ts2 <- time_series_analysis(greater_than_1yr_saolta, title = title2)
plot_timeseries_data(ts2, title2)

#########################################
# University of Limerick Hospital Group
#########################################
attach(university_of_limerick)
less_than_1yr_ul <- university_of_limerick[(Time_Bands == "< 1 Year"), ]
greater_than_1yr_ul <- university_of_limerick[(Time_Bands == "> 1 Year"), ]
detach(university_of_limerick)

# Less than a year waiting
title1 <- "UL Hospital Group (< 1 Year)"
ts1 <- time_series_analysis(less_than_1yr_ul, title = title1)
plot_timeseries_data(ts1, title1)

# More than a year waiting
title2 <- "UL Hospital Group (> 1 Year)"
ts2 <- time_series_analysis(greater_than_1yr_ul, title = title2)
plot_timeseries_data(ts2, title2)

#########################################
# South/South West Hospital Group
#########################################
attach(south_south_west)
less_than_1yr_ssw <- south_south_west[(Time_Bands == "< 1 Year"), ]
greater_than_1yr_ssw <- south_south_west[(Time_Bands == "> 1 Year"), ]
detach(south_south_west)

# Less than a year waiting
title1 <- "South/South West Hospital Group (< 1 Year)"
ts1 <- time_series_analysis(less_than_1yr_ssw, title = title1)
plot_timeseries_data(ts1, title1)

# More than a year waiting
title2 <- "South/South West Hospital Group (> 1 Year)"
ts2 <- time_series_analysis(greater_than_1yr_ssw, title = title2)
plot_timeseries_data(ts2, title2)

