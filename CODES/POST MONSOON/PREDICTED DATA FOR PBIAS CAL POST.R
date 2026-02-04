# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

# File path (update if needed)
file_path <- "C:/Users/Amit Mallick/Desktop/7th sem MINOR/POST MONSOON (OCT-NOV).xlsx"

# Read data from the Excel file
data <- read_excel(file_path, sheet = "Sheet1")

# Convert wide format to long format
data_long <- data %>%
  pivot_longer(cols = -c(`Sl. No.`, District), names_to = "Year", values_to = "Precipitation") %>%
  mutate(Year = as.numeric(Year))

# Add predicted values for each district using linear regression
predicted_data <- data_long %>%
  group_by(District) %>%
  mutate(
    Predicted = predict(lm(Precipitation ~ Year, data = cur_data_all()))
  ) %>%
  ungroup()

# Save to CSV
output_path <- "C:/Users/Amit Mallick/Desktop/OCT_NOV_Predicted_Values_1995_2023.csv"
write.csv(predicted_data, output_path, row.names = FALSE)

# Display confirmation
cat("Predicted values saved to:", output_path, "\n")
