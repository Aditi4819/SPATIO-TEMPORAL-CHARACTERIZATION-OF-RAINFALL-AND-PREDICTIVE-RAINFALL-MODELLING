# Load required libraries
library(readxl)  # For reading Excel files
library(ggplot2) # For visualization
library(dplyr)   # For data manipulation

# Define the file path
file_path <- "C:/Users/Amit Mallick/Desktop/7th sem MINOR/ODISHA_1995-2023.xlsx"

# Read the data
data <- read_excel(file_path, sheet = "Sheet1")

# Convert the year column to numeric (if necessary)
data$DIST <- as.numeric(data$DIST)

# Melt data for easier plotting (if necessary)
library(tidyr)
data_long <- data %>% pivot_longer(-DIST, names_to = "District", values_to = "Value")

# Plot and analyze trends for each district
ggplot(data_long, aes(x = DIST, y = Value, color = District)) +
  geom_line() +  # Line plot for trends
  geom_smooth(method = "lm", se = FALSE) +  # Add trend lines (linear regression)
  labs(
    title = "Innovative Trend Analysis for Odisha Districts",
    x = "Year",
    y = "Metric Value",
    color = "District"
  ) +
  theme_minimal()

# Perform linear regression for each district (example for one district)
results <- data_long %>%
  group_by(District) %>%
  summarize(
    Slope = lm(Value ~ DIST)$coefficients[2],  # Extract slope
    Intercept = lm(Value ~ DIST)$coefficients[1]  # Extract intercept
  )

# Save the results to a CSV (optional)
write.csv(results, "trend_analysis_results.csv", row.names = FALSE)

# Display results
print(results)
