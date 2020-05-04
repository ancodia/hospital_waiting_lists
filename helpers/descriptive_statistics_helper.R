source("helpers/load_libs.R")

time_series_analysis <- function(data, title = ""){
  # create time series
  ts <- ts(data$Total,
           frequency = 12,
           start = c(2014, 1), 
           end = c(2019, 12))
  cat("\n\n------------------------------------------------------------------------\n")
  cat(paste("Time-series analysis for waiting lists in", title))
  
  cat("\n\n------------------\nTime Series:\n------------------\n")
  print(ts)
  cat("\n")
  print(summary(ts))
  
  # # plot the seasonal and trend decomposition using Loess
  # plot(stl(ts, s.window="periodic"), main = paste("Time Series Decomposition (STL) \n", title))
  # 
  # Mann-Kendall Test - to check if a trend exists
  cat("\n\n------------------------------------\n")
  cat("Mann-Kendall Test:\n")
  
  # from the Kendall package
  mk <- Kendall::MannKendall(ts)
  print(mk)
  cat(paste("S:", mk["S"]))
  cat(paste("\nVariance of S:", mk["varS"]))
  
  # Sen's Slope Test - to check the magnitude of trend, if present
  cat("\n\n------------------------------------\n")
  # from trend package
  print(trend::sens.slope(ts))
  
  # Augmented Dickey-Fuller Test - check if series is stationary
  cat("\n\n------------------------------------\n")
  # from tseries package, k=12 for 12 month cycle
  print(tseries::adf.test(ts, k = 12))
  
  cat("\n\n------------------------------------------------------------------------\n")
  
  return(ts)
}

plot_timeseries_data <- function(ts, title){
  # plot the seasonal and trend decomposition using Loess
  plot(stl(ts, s.window="periodic"), 
       main = paste("Time Series Decomposition (STL) \n", title))
}