# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(forecast)
library(ggcorrplot)
library(corrplot)

# Set file path
file_path <- "~/Downloads/updated_merged_2018_2022.csv"

# Read dataset
data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)

# Display structure and summary
str(data)
summary(data)

# Extract numeric columns
numeric_columns <- select_if(data, is.numeric)

# Compute Descriptive Statistics
descriptive_stats <- data.frame(
  Variable = names(numeric_columns),
  Mean = sapply(numeric_columns, mean, na.rm = TRUE),
  Median = sapply(numeric_columns, median, na.rm = TRUE),
  SD = sapply(numeric_columns, sd, na.rm = TRUE)
)

print(descriptive_stats)

# ---- Improved Histogram ----
ggplot(data, aes(x = numeric_columns[[1]])) + 
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) + 
  scale_y_log10() +  # Log scale for better differentiation
  labs(title = "Histogram with Adjusted Bins", x = "Value", y = "Frequency") + 
  theme_minimal()

# ---- Improved Boxplot ----
ggplot(data, aes(x = "", y = numeric_columns[[1]])) + 
  geom_boxplot(fill = "red", outlier.color = "black") + 
  scale_y_continuous(limits = c(min(numeric_columns[[1]], na.rm = TRUE) - 5, 
                                max(numeric_columns[[1]], na.rm = TRUE) + 5)) + 
  labs(title = "Boxplot with Adjusted Y-Axis", y = "Value") + 
  theme_minimal()

# ---- Scatterplot ----
ggplot(data, aes(x = numeric_columns[[1]], y = numeric_columns[[2]])) +
  geom_jitter(color = "blue", size = 2, alpha = 0.5) +  
  scale_x_continuous(limits = c(min(numeric_columns[[1]], na.rm = TRUE) - 5, 
                                max(numeric_columns[[1]], na.rm = TRUE) + 5)) +
  scale_y_continuous(limits = c(min(numeric_columns[[2]], na.rm = TRUE) - 5, 
                                max(numeric_columns[[2]], na.rm = TRUE) + 5)) +
  labs(title = "Scatterplot with Adjusted Axes", x = "Var1", y = "Var2") +
  theme_minimal()

# ---- Time Series Analysis ----
if ("DateColumn" %in% colnames(data)) {
  data$DateColumn <- as.Date(data$DateColumn, format = "%Y-%m-%d")
  
  # Aggregate Data by Month
  time_series_data <- data %>%
    group_by(month = floor_date(DateColumn, "month")) %>%
    summarise(Value = sum(numeric_columns[[1]], na.rm = TRUE))
  
  # Convert to Time Series
  ts_data <- ts(time_series_data$Value, start = c(2018, 1), frequency = 12)
  
  # ---- Time Series Plot ----
  ggplot(time_series_data, aes(x = month, y = Value)) +
    geom_line(color = "blue", size = 1) +
    geom_smooth(method = "loess", color = "red", linetype = "dashed") +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
    scale_y_continuous(limits = c(min(time_series_data$Value) * 0.9, 
                                  max(time_series_data$Value) * 1.1)) +
    labs(title = "Time Series with Trend Line", x = "Time", y = "Value") +
    theme_minimal()
  
  # ---- ARIMA Forecast ----
  model <- auto.arima(ts_data)
  forecasted_values <- forecast(model, h = 12)
  
  # ---- Forecast Plot ----
  plot(forecasted_values, main = "Forecast for Next 12 Months", col = "red", 
       ylim = c(min(forecasted_values$lower), max(forecasted_values$upper)))  
} else {
  print("DateColumn is missing in the dataset.")
}

# ---- Improved Correlation Analysis ----
if (ncol(numeric_columns) > 1) {
  cor_matrix <- cor(numeric_columns, use = "complete.obs")
  
  # Heatmap-style correlation plot
  ggcorrplot(cor_matrix, method = "square", type = "full", lab = TRUE, 
             lab_size = 3, colors = c("blue", "white", "red"),
             title = "Overall Correlation Heatmap", outline.col = "black")
  
  # Traditional correlation plot
  corrplot(cor_matrix, method = "color", type = "lower", 
           col = colorRampPalette(c("blue", "white", "red"))(200),
           tl.cex = 0.8, tl.col = "black", number.cex = 0.7, 
           addCoef.col = "black", title = "Overall Correlation Matrix")
} else {
  print("Not enough numeric columns for correlation analysis.")
}

# ---- Year-wise Correlation Analysis ----
if ("DateColumn" %in% colnames(data)) {
  data$Year <- year(data$DateColumn)
  
  yearly_correlation <- data %>%
    group_by(Year) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE))
  
  cor_matrix_yearly <- cor(yearly_correlation[,-1], use = "complete.obs")
  
  # Heatmap for year-wise correlation
  ggcorrplot(cor_matrix_yearly, method = "square", type = "full", lab = TRUE, 
             lab_size = 3, colors = c("blue", "white", "red"),
             title = "Year-wise Correlation Heatmap", outline.col = "black")
  
  # Traditional correlation matrix for year-wise trends
  corrplot(cor_matrix_yearly, method = "color", type = "lower", 
           col = colorRampPalette(c("blue", "white", "red"))(200),
           tl.cex = 0.8, tl.col = "black", number.cex = 0.7, 
           addCoef.col = "black", title = "Year-wise Correlation Matrix")
} else {
  print("DateColumn is missing.")
}
