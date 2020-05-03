source("helpers/load_libs.R")

custom_plot.decomposed.ts = function(x, title = "", ...) {
  xx <- x$x
  if (is.null(xx)) 
    xx <- with(x, if (type == "additive") 
      random + trend + seasonal
      else random * trend * seasonal)
  plot(cbind(observed = xx, trend = x$trend, seasonal = x$seasonal, random = x$random), 
       main = paste("Time Series Decomposition -", title), yax.flip = TRUE, ...)
}

time_series_analysis <- function(data, title = ""){
  # create time series
  ts <- ts(data$Total,
           frequency = 12,
           start = c(2014, 1), 
           end = c(2019, 12))
  cat("\n\n------------------\nTime Series:\n------------------\n")
  print(ts)
  # plot the decomposed time series using moving averages
  custom_plot.decomposed.ts(decompose(ts), title = title)
  # and the seasonal and trend decomposition using Loess
  plot(stl(ts, s.window="periodic"), main = paste("STL Time Series Decomp. -", title))
  
  # trend package
  # https://rdrr.io/cran/trend/f/inst/doc/trend.pdf
  cat("\n\n------------------\nMann-Kendall Test:\n------------------\n")
  print(mk.test(ts))

  cat("\n\n------------------\nCox-Stuart Test:\n------------------\n")
  print(cs.test(ts))

  cat("\n\n------------------\nSen's Slope Test:\n------------------\n")
  print(sens.slope(ts))
}