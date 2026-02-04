# Load necessary library
library(readxl)  # For reading Excel files

# Load the Excel file
file_path <- "C:/Users/Amit Mallick/Desktop/7th sem MINOR/PBIAS.xlsx"
data <- read_excel(file_path, sheet = "sun")

# Rename columns for easier access if needed
colnames(data) <- c("Year", "Observed", "Predicted")

# Define the PBIAS function
PBIAS <- function(observed, predicted) {
  bias <- sum(observed - predicted)
  percent_bias <- 100 * bias / sum(observed)
  return(percent_bias)
}

# Calculate PBIAS
pbias_value <- PBIAS(data$Observed, data$Predicted)

# Print the result
print(paste("PBIAS:", pbias_value, "%"))