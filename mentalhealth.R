# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(ggcorrplot)
library(reshape2)
library(ggridges)

# Load the dataset
file_path <- "~/Downloads/Mental_Health_Citywise_India.csv"
df <- read_csv(file_path)

# View first few rows
head(df)

# Summary statistics
summary(df)

# Check data structure
str(df)

# Correlation analysis
cor_data <- cor(df %>% select(where(is.numeric)))
print(cor_data)

# Heatmap of correlations
ggcorrplot(cor_data, method = "circle", lab = TRUE)

# Top 10 cities with highest current mental morbidity
df %>%
  arrange(desc(`Current mental morbidity (%)`)) %>%
  head(10)

# Scatter Plot: Stress vs Anxiety Levels
ggplot(df, aes(x = `Stress Level (1-10)`, y = `Anxiety Levels (1-10)`, color = City)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Stress vs Anxiety Levels",
       x = "Stress Level (1-10)",
       y = "Anxiety Levels (1-10)") +
  theme_minimal()

# Scatter Plot: Tobacco use vs Alcohol use
ggplot(df, aes(x = `Tobacco use (%)`, y = `Alcohol use (%)`, color = City)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Tobacco vs Alcohol Use",
       x = "Tobacco Use (%)",
       y = "Alcohol Use (%)") +
  theme_minimal()

# Scatter Plot: Suicidal Risk vs Mental Morbidity
ggplot(df, aes(x = `Suicidal risk (%)`, y = `Current mental morbidity (%)`, color = City)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Suicidal Risk vs Current Mental Morbidity",
       x = "Suicidal Risk (%)",
       y = "Current Mental Morbidity (%)") +
  theme_minimal()

# Bar Chart: Sleep Disorders by City
ggplot(df, aes(x = reorder(City, -`Sleep Disorders (%)`), y = `Sleep Disorders (%)`, fill = City)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Sleep Disorders Percentage by City",
       x = "City",
       y = "Sleep Disorders (%)") +
  theme_minimal()

# Bar Chart: Top 10 Cities with Highest Anxiety Levels
top_anxiety <- df %>%
  arrange(desc(`Anxiety Levels (1-10)`)) %>%
  head(10)

ggplot(top_anxiety, aes(x = reorder(City, -`Anxiety Levels (1-10)`), y = `Anxiety Levels (1-10)`, fill = City)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Cities with Highest Anxiety Levels",
       x = "City",
       y = "Anxiety Levels (1-10)") +
  theme_minimal()

# Heatmap of Mental Health Indicators
melted_data <- melt(df, id.vars = c("City", "State"))
ggplot(melted_data, aes(x = variable, y = City, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Heatmap of Mental Health Indicators by City",
       x = "Mental Health Factors",
       y = "City") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Alternative 1: Ridgeline Plot for Stress Level Distribution
ggplot(df, aes(x = `Stress Level (1-10)`, y = City, fill = City)) +
  geom_density_ridges(stat = "binline", bins = 10, scale = 1.2, alpha = 0.7) +
  labs(title = "Distribution of Stress Levels Across Cities",
       x = "Stress Level (1-10)",
       y = "City") +
  theme_minimal()



# Alternative 2: Stacked Bar Chart of Mental Health Indicators by City
df_long <- df %>%
  select(City, `Sleep Disorders (%)`, `Suicidal risk (%)`, `Current mental morbidity (%)`, `Alcohol use (%)`) %>%
  melt(id.vars = "City")

ggplot(df_long, aes(x = City, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  labs(title = "Mental Health Indicators by City",
       x = "City",
       y = "Percentage",
       fill = "Indicator") +
  theme_minimal()

# Alternative 3: Bubble Chart - Suicidal Risk vs Mental Morbidity
ggplot(df, aes(x = `Suicidal risk (%)`, y = `Current mental morbidity (%)`, size = `Stress Level (1-10)`, color = City)) +
  geom_point(alpha = 0.6) +
  scale_size(range = c(3, 10)) +
  labs(title = "Suicidal Risk vs Mental Morbidity (Bubble Chart)",
       x = "Suicidal Risk (%)",
       y = "Current Mental Morbidity (%)",
       size = "Stress Level (1-10)") +
  theme_minimal()
