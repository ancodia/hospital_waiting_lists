source("helpers/predictions_helper.R")

# load the prepared dataset
waiting_lists <- read_csv("data_prep/combined_waiting_lists.csv")

# the saolta group is the one of interest for this task so extracting it from the dataset
saolta <- subset(waiting_lists, Hospital_Group == "Saolta")

# exclude day from archive date for monthly values
saolta$Archive_Date <- format(as.Date(saolta$Archive_Date, format="%Y-%m-%d"), "%Y-%m")

# combine monthly time band figures so only one per time period exists
saolta <- aggregate(cbind(Total) ~ 
                      Archive_Date, 
                    data = saolta, sum)
nrow(saolta)
# 72 rows as expected, 12(months) x 6(years)

# convert data to a time series, frequency 12 for monthly, starting from Jan. 2014
saolta_ts <- ts(saolta$Total, frequency = 12, start = c(2014, 1))
saolta_ts

# for ARIMA the following values are required, ARIMA(p, d, q):
# p: the number of autoregressive terms (AR order)
# d: the number of nonseasonal differences needed for stationarity
# q: the number of lagged forecast errors in the prediction equation (MA order)

######################################################
# Stationarity
######################################################
# plot the decomposed time series
saolta_ts_decomposed <- plot_timeseries_data(saolta_ts, title = "Saolta")
# time series is additive with a clear upward trend, i.e not stationary

# decomposed time series plot looks like there is seasonality, 
# checking variance of decomposed series with original
apply(saolta_ts_decomposed$time.series, 2, var) / var(saolta_ts)
# the trend explains almost all the variance in the time series

# confirm with isSeasonal test
seastests::isSeasonal(saolta_ts)

# confirm non-stationary with acf/pacf
astsa::acf2(saolta_ts, main = "Saolta Waiting Lists Time Series (No diff) ACF/PACF")
# slow drop off in acf due to non-stationarity

#### Stationarity
# check number of differences required
ndiffs(saolta_ts) # = 1
# apply differencing to the time series to make it stationary
saolta_ts_diff <- diff(saolta_ts, differences = 1)
# check ndiffs again to confirm no trend left
ndiffs(saolta_ts_diff) # = 0
plot(saolta_ts_diff, type = "l", main = "Differenced Saolta Time Series")

astsa::acf2(saolta_ts_diff, main = "Saolta Waiting Lists Time Series ACF/PACF")
# now the acf/pacf drops below the dotted line 
# indicating that the majority of values are not significantly different from 0

# Test for stationarity
# Augmented Dickey-Fuller test, using from urca package rather than tseries version 
# as it can be run with including drift/trend (already removed with differencing)
# H0 = time series is not stationary
summary(urca::ur.df(saolta_ts_diff))
# p-value = 0.00027 => time series is stationary 

# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test 
# H0 = time series is stationary
tseries::kpss.test(saolta_ts_diff)
# p-value > 0.1 => time series is stationary

######################################################
# Model Specification
######################################################
# 1 difference required so d = 1
# parameters obtained from acf/pacf:
# both cut off after lag 1 so parameters for ARMA(p,q) = (1,0), (0,1) or (1,1)
# => 3 ARIMA models can be proposed: ARIMA(1, 1, 0), ARIMA(0, 1, 1) and ARIMA(1, 1, 1)

# split time series into train and test sets (80:20 split) 
test_size <- round(length(saolta_ts) * 0.2)
split_saolta_ts <- ts_split(saolta_ts, sample.out = test_size)
train <- split_saolta_ts$train
test <- split_saolta_ts$test

# build the 3 models
arima_model1 = forecast::Arima(train, order = c(1, 1, 0))
arima_model1
accuracy(arima_model1)

arima_model2 = forecast::Arima(train, order = c(0, 1, 1))
arima_model2
accuracy(arima_model2)

arima_model3 = forecast::Arima(train, order = c(1, 1, 1))
arima_model3
accuracy(arima_model3)

# AIC:
#   ARIMA(1, 1, 0) = 999.51
#   ARIMA(0, 1, 1) = 1003.37
#   ARIMA(1, 1, 1) = 1001.01

# MAPE:
#   ARIMA(1, 1, 0) = 1.115
#   ARIMA(0, 1, 1) = 1.166
#   ARIMA(1, 1, 1) = 1.09

# check quantile-quantile plots for residuals of each model to determine if they are normally distributed
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 3))

qqnorm(arima_model1$residuals, main = "Q-Q Plot: ARIMA(1,1,0)")
qqline(arima_model1$residuals)

qqnorm(arima_model2$residuals, main = "Q-Q Plot: ARIMA(0,1,1)")
qqline(arima_model2$residuals)

qqnorm(arima_model3$residuals, main = "Q-Q Plot: ARIMA(1,1,1)")
qqline(arima_model3$residuals)

hist(arima_model1$residuals, main = "Histogram: ARIMA(1,1,0)", 
     xlab = "Residuals", 
     sub = paste("Shapiro p-value:", 
                 round(shapiro.test(arima_model1$residuals)$p.value, 6)))
hist(arima_model2$residuals, main = "Histogram: ARIMA(0,1,1)", 
     xlab = "Residuals",
     sub = paste("Shapiro p-value:", 
                 round(shapiro.test(arima_model2$residuals)$p.value, 6)))
hist(arima_model3$residuals, main = "Histogram: ARIMA(1,1,1)", 
     xlab = "Residuals",
     sub = paste("Shapiro p-value:", 
                 round(shapiro.test(arima_model3$residuals)$p.value, 6)))

par(opar)

# Use Ljung-Box test
# H0 = the autocorrelations are all zero
Box.test(arima_model1$residuals, type = "Ljung-Box")
Box.test(arima_model2$residuals, type = "Ljung-Box")
Box.test(arima_model3$residuals, type = "Ljung-Box")

# Box-Ljung test p-values:
#   ARIMA(1, 1, 0) = 0.1811
#   ARIMA(0, 1, 1) = 0.3488
#   ARIMA(1, 1, 1) = 0.4944

# from evaluating the models, model3 appears to be the best fit

# plot forecasts for each model
opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 3))

forecast_model1 <- forecast(arima_model1, test_size)
plot(forecast_model1)

forecast_model2 <- forecast(arima_model2, test_size)
plot(forecast_model2)

forecast_model3 <- forecast(arima_model3, test_size)
plot(forecast_model3)

par(opar)

# check what model is suggested with auto arima
fit <- auto.arima(saolta_ts)
fit
######################


