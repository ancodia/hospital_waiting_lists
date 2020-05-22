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

# check if any na records exist in time series
anyNA(saolta_ts)

# Show time series data
plot(saolta_ts,
     xlab="Year", 
     ylab = "Patients waiting",
     main="Saolta University Hospital Group Waiting Lists 2014-19")
# linear relationship between number of patients waiting and time
abline(reg = lm(saolta_ts ~ time(saolta_ts)))

# use boxplot to check seasonality
boxplot(saolta_ts ~ cycle(saolta_ts),
        xlab="Month", 
        ylab = "Patients waiting",
        main ="Saolta University Hospital Group Waiting Lists 2014-19")
# doesn't appear to be seasonal, mean is fairly stable over the course of years


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
# the trend explains almost all (97.5%) the variance in the time series

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
# now the acf/pacf quickly drops below the dotted line 
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

# split time series into train and test
# training from 2014-18 data
train <- window(x = saolta_ts, start = c(2014, 1), end = c(2018, 12))
# test with 2019 data
test <- window(x = saolta_ts, start = c(2019, 1), end = c(2019, 12))

### BUILD
# build the 3 models and an auto arima model
arima_model1 = forecast::Arima(train, order = c(1, 1, 0))
arima_model2 = forecast::Arima(train, order = c(0, 1, 1))
arima_model3 = forecast::Arima(train, order = c(1, 1, 1))
auto_arima_model <- auto.arima(train) # gives ARIMA(1,1,0) with drift 
# drift is the amount of change over time - uses the average change seen in historical data

### EVALUATE
arima_model1
# AIC: 1032.69
model1_accuracy <- accuracy(arima_model1)
str(model1_accuracy)
# MAPE: 1.093352

arima_model2
# AIC: 1036.51
model2_accuracy <- accuracy(arima_model2)
# MAPE: 1.135171

arima_model3
# AIC: 1034.2
model3_accuracy <- accuracy(arima_model3)
# MAPE: 1.0715

auto_arima_model
# AIC: 1031.2
model_auto_accuracy <- accuracy(auto_arima_model)
# MAPE: 1.070919


# check quantile-quantile plots and histogram for residuals of each model to determine 
# if they are normally distributed and then  verify with shapiro-wilk test
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))

qqnorm(arima_model1$residuals, main = "Q-Q Plot: ARIMA(1,1,0)")
qqline(arima_model1$residuals)

qqnorm(arima_model2$residuals, main = "Q-Q Plot: ARIMA(0,1,1)")
qqline(arima_model2$residuals)

qqnorm(arima_model3$residuals, main = "Q-Q Plot: ARIMA(1,1,1)")
qqline(arima_model3$residuals)

qqnorm(auto_arima_model$residuals, main = "Q-Q Plot: (Auto) ARIMA(1,1,0) with drift")
qqline(auto_arima_model$residuals)
par(opar)

opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
hist(arima_model1$residuals, main = "Histogram: ARIMA(1,1,0)", 
     xlab = "Residuals")
model1_shapiro <- shapiro.test(arima_model1$residuals)

hist(arima_model2$residuals, main = "Histogram: ARIMA(0,1,1)", 
     xlab = "Residuals")
model2_shapiro <- shapiro.test(arima_model2$residuals)

hist(arima_model3$residuals, main = "Histogram: ARIMA(1,1,1)", 
     xlab = "Residuals")
model3_shapiro <- shapiro.test(arima_model3$residuals)

hist(auto_arima_model$residuals, main = "Histogram: (Auto) ARIMA(1,1,0) with drift", 
     xlab = "Residuals")
model_auto_shapiro <- shapiro.test(auto_arima_model$residuals)
par(opar)

# Use Ljung-Box test
# H0 = the autocorrelations are all zero
model1_ljung <- Box.test(arima_model1$residuals, type = "Ljung-Box")
model2_ljung <- Box.test(arima_model2$residuals, type = "Ljung-Box")
model3_ljung <- Box.test(arima_model3$residuals, type = "Ljung-Box")
model_auto_ljung <- Box.test(auto_arima_model$residuals, type = "Ljung-Box")


# create dataframe with evaluation metrics
model_ids <- c("ARIMA(1,1,0)", "ARIMA(0,1,1)", "ARIMA(1,1,1)", "(Auto) ARIMA(1,1,0) w/ drift")
aic_valules <- c(arima_model1$aic, arima_model2$aic, arima_model3$aic, auto_arima_model$aic)
mape_values <- c(model1_accuracy[, "MAPE"],
                 model2_accuracy[, "MAPE"],
                 model3_accuracy[, "MAPE"],
                 model_auto_accuracy[, "MAPE"])
shapiro_pvalues <- c(round(model1_shapiro$p.value, 5), 
                     round(model2_shapiro$p.value, 5), 
                     round(model3_shapiro$p.value, 5), 
                     round(model_auto_shapiro$p.value, 5))
ljung_pvalues <- c(round(model1_ljung$p.value, 5), 
                   round(model2_ljung$p.value, 5), 
                   round(model3_ljung$p.value, 5), 
                   round(model_auto_ljung$p.value, 5))

evaluation_df <- data.frame(model_ids, aic_valules, mape_values, shapiro_pvalues, ljung_pvalues)
colnames(evaluation_df) <- c("Model", "AIC", "MAPE", "Shapiro-Wilk", "Ljung-box")
evaluation_df

### VALIDATION
# check model predictions
# plot forecasts for each model

forecast_model1 <- forecast(arima_model1, h = 12)
forecast_model1
plot_arima_model(forecast_model1, train, test)


forecast_model2 <- forecast(arima_model2, h = 12)
forecast_model2
plot_arima_model(forecast_model2, train, test)

forecast_model3 <- forecast(arima_model3, h = 12)
forecast_model3
plot_arima_model(forecast_model3, train, test)

auto_forecast <- forecast(auto_arima_model, h = 12)
auto_forecast
plot_arima_model(auto_forecast, train, test)

test
# make actuals_predicted dataframe
# for each arima model
actuals_predictions <- data.frame(cbind(cycle(test), 
                                        test, 
                                        forecast_model1$mean, 
                                        forecast_model2$mean, 
                                        forecast_model3$mean, 
                                        auto_forecast$mean))
colnames(actuals_predictions) <- c("2019", 
                                  "Actual", 
                                  "ARIMA(1,1,0)", 
                                  "ARIMA(0,1,1)", 
                                  "ARIMA(0,1,1)", 
                                  "(Auto) ARIMA(1,1,0) w/ drift")
actuals_predictions


correlation_accuracy <- cor(actuals_predictions[-1])
correlation_accuracy

######################
### FORECASTING
# build model using parameters suggested by auto arima
model <- auto.arima(saolta_ts)
model

forecast <- forecast(model, h = 12)
forecast

plot_arima_model(forecast)

# check forecasted increase compared to 2019
actuals_predictions_2020 <- data.frame(test, forecast$mean)

#actuals_predictions_2020 <- data.frame(as.vector(test), as.vector(forecast$mean))


percent_change <- ((as.vector(actuals_predictions_2020[,2]) - 
                      as.vector(actuals_predictions_2020[,1])) / 
                     as.vector(actuals_predictions_2020[,1])) * 100 

actuals_predictions_2020$pct_change <- percent_change
colnames(actuals_predictions_2020) <- c("2019 (Actual)", "2020 (Forecasted)", "Percentage Change")
actuals_predictions_2020

mean(actuals_predictions_2020[,3])

str(actuals_predictions_2020)

# plot.ts(cbind(actuals_predictions_2020[,1],
#               actuals_predictions_2020[,2]), plot.type = "single")
