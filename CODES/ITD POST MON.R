# Load required libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

# Define the file path (adjust if needed)
file_path <- "C:/Users/Amit Mallick/Desktop/7th sem MINOR/POST MONSOON (OCT-NOV).xlsx"

# Read the data from Sheet1
data <- read_excel(file_path, sheet = "Sheet1")

# Reshape to long format
data_long <- data %>%
  pivot_longer(cols = -c(`Sl. No.`, District), names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.numeric(Year))

# Plot trends for each district
ggplot(data_long, aes(x = Year, y = Value, color = District)) +
  geom_line(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Linear Trend Analysis of Post-Monsoon Precipitation (1995–2023)",
    x = "Year",
    y = "Precipitation (mm)",
    color = "District"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Perform linear regression for each district
results <- data_long %>%
  group_by(District) %>%
  summarise(
    Slope = coef(lm(Value ~ Year))[2],
    Intercept = coef(lm(Value ~ Year))[1],
    .groups = 'drop'
  )

# Save regression results to CSV
write.csv(results, "C:/Users/Amit Mallick/Desktop/trend_analysis_results.csv", row.names = FALSE)

# Display results
print(results)
