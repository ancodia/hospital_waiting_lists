source("helpers/load_libs.R")
source("helpers/descriptive_statistics_helper.R")

plot_arima_model <- function(model_forecast, train_data = NULL, test_data = NULL){
  clrs <- c("black", "blue", "red", "purple")
  
  # conditionally add Fitted/Train/Test lines in test and train data is provided
  model_forecast %>% autoplot(xlab = "Year", 
                              ylab = "Patients waiting") +
    autolayer(model_forecast$mean, series="Forecast") +
    {if (!is.null(train_data) && (!is.null(test_data)))
      autolayer(fitted(model_forecast), series='Fitted')} + 
    {if (!is.null(train_data)) autolayer(train_data, series = 'Train')} +
    {if (!is.null(test_data)) autolayer(test_data, series='Test')} +
    guides(colour=guide_legend(title="Data series")) +
    scale_color_manual(values=clrs)
}