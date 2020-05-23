source("helpers/load_libs.R")
source("helpers/descriptive_statistics_helper.R")

plot_arima_model_forecast <- function(model_forecast, test_data){
  # create dataframe for plotting
  for_plot <- fortify(model_forecast, ts.connect = TRUE)
  
  plot <- ggplot(data = for_plot) + 
    geom_line(aes(x = Index, y = Data, color = "raw")) +
    geom_line(aes(x = Index, y = `Point Forecast`, color = "forecast")) +
    geom_ribbon(aes(x = Index, ymin = `Lo 80`, ymax = `Hi 80`,  fill = "80"),  alpha = 0.2) +
    geom_ribbon(aes(x = Index, ymin = `Lo 95`, ymax = `Hi 95`,  fill = "95"),  alpha = 0.2) +
    scale_fill_manual("Level", values = c("blue", "dodgerblue"))+
    scale_color_manual("Data", values = c("red", "blue")) +
    xlab("Year") + ylab("Patients waiting") + 
    ggtitle(paste("Forecast for", model_forecast$method)) +
    theme(
      plot.title = element_text(size=11),
      axis.title.x = element_text(size=9),
      axis.title.y = element_text(size=9),
      axis.text = element_text(size=9),
      legend.title = element_text(size=8),
      legend.text = element_text(size=8)
    )
}