# Load necessary library
library(readxl)
library(dplyr)

# Load the Excel file
file_path <- "C:/Users/Amit Mallick/Desktop/7th sem MINOR/POST MONSOON (OCT-NOV).xlsx"
data <- read_excel(file_path, sheet = "Sheet1")

# Define the year columns
year_cols <- as.character(1995:2023)

# Initialize a result dataframe
autocorr_results <- data.frame(District = character(), Lag1_Autocorrelation = numeric(), stringsAsFactors = FALSE)

# Loop over each district
for (i in 1:nrow(data)) {
  district_name <- data$District[i]
  
  # Extract rainfall data for the district from 1995 to 2023
  district_data <- as.numeric(data[i, year_cols])
  district_data_clean <- na.omit(district_data)
  
  # Ensure enough data points for correlation
  if (length(district_data_clean) > 1) {
    lag_1_ac <- cor(district_data_clean[-length(district_data_clean)], district_data_clean[-1])
  } else {
    lag_1_ac <- NA  # Not enough data
  }
  
  # Append to results
  autocorr_results <- rbind(autocorr_results, data.frame(District = district_name, Lag1_Autocorrelation = lag_1_ac))
}

# Save the result to CSV
write.csv(autocorr_results, "Districts_Lag1_Autocorrelation_1995_2023.csv", row.names = FALSE)
