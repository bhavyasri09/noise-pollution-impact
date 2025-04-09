# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load datasets
noise_data <- read.csv("~/Downloads/updated_merged_2018_2022.csv")
mental_health_data <- read.csv("~/Downloads/Mental_Health_Citywise_India.csv")
# Aggregate noise levels by year
noise_summary <- noise_data %>%
  group_by(Year) %>%
  summarise(
    Avg_Noise_Level = mean(Day, na.rm = TRUE),
    Max_Noise_Level = max(Day, na.rm = TRUE)
  )

# Estimate affected population percentage (scaled between 20-30%)
min_noise <- min(noise_summary$Avg_Noise_Level)
max_noise <- max(noise_summary$Avg_Noise_Level)
noise_summary$Affected_Population <- 20 + 
  (noise_summary$Avg_Noise_Level - min_noise) / (max_noise - min_noise) * 10

# Compute Pearson correlation between noise levels and affected population
correlation <- cor(noise_summary$Avg_Noise_Level, noise_summary$Affected_Population)

# Print results
print(noise_summary)
print(paste("Pearson Correlation: ", round(correlation, 2)))

# Plot Noise Levels vs. Affected Population
ggplot(noise_summary, aes(x = Year)) +
  geom_line(aes(y = Avg_Noise_Level, color = "Avg Noise Level"), size = 1) +
  geom_line(aes(y = Affected_Population, color = "Affected Population"), size = 1) +
  labs(title = "Noise Levels vs. Affected Population", x = "Year", y = "Value") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"))

# Simulated ML Performance Data
ml_performance <- data.frame(
  Model = c("Random Forest", "SVM", "LSTM"),
  Accuracy = c(88.7, 83.9, 90.8),
  Precision = c(87.5, 82.8, 90.2),
  Recall = c(89.3, 84.5, 91.7),
  F1_Score = c(88.4, 83.6, 90.9),
  AUC_Score = c(0.90, 0.86, 0.93)
)

# Plot Model Accuracy
ggplot(ml_performance, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Model Accuracy Comparison", x = "Model", y = "Accuracy (%)") +
  theme_minimal()

# Plot ROC Curve Comparison
ggplot(ml_performance, aes(x = Model, y = AUC_Score, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "ROC Curve Comparison", x = "Model", y = "AUC Score") +
  theme_minimal()
