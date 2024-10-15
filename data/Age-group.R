# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read the data
data <- read.csv("Pyramid.csv")

# Create age intervals
data$AgeGroup <- cut(data$Age, breaks = seq(0, 100, by = 10), right = FALSE, include.lowest = TRUE)

# Calculate relative frequency positivity in percentage
age_group_summary <- data %>%
  group_by(AgeGroup) %>%
  summarize(Frequency = n()) %>%
  mutate(RelativeFrequency = Frequency / sum(Frequency) * 100)

# Calculate mean age and standard deviation
mean_age <- mean(data$Age, na.rm = TRUE)
sd_age <- sd(data$Age, na.rm = TRUE)

# Linear model to calculate R-squared value
model <- lm(Age ~ Sexo, data = data)
r_squared <- summary(model)$r.squared

# Plotting the graph
plot <- ggplot(age_group_summary, aes(x = AgeGroup, y = RelativeFrequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Relative Frequency Positivity by Age Group",
       x = "Age Group (years)",
       y = "Relative Frequency (%)") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste("Mean Age:", round(mean_age, 2), "\n",
                                                   "SD:", round(sd_age, 2), "\n",
                                                   "R-squared:", round(r_squared, 4)),
           hjust = 1.1, vjust = 1.1, size = 3, color = "black")

# Print the plot
print(plot)

# Print the results
cat("Mean Age:", mean_age, "\n")
cat("Standard Deviation of Age:", sd_age, "\n")
cat("R-squared value:", r_squared, "\n")
