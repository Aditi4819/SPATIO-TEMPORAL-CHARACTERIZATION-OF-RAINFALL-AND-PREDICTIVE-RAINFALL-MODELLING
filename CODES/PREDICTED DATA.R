# Install required packages if not already installed
install.packages("readxl", dependencies = TRUE)
install.packages("forecast", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)

# Load libraries
library(readxl)
library(forecast)
library(ggplot2)

# Load the Excel file
ODISHA_1995_2023 <- "C:/Users/Amit Mallick/Desktop/7th sem MINOR/ODISHA_1995-2023.xlsx"

# Read the first sheet from the Excel file
data <- read_xlsx(ODISHA_1995_2023, sheet = "Sheet1")

# Check the first few rows of the dataset and the column names
head(data)
print(names(data))

# Select the column for analysis (e.g., 'Sun' district)
if ("Sun" %in% names(data)) {
  # Remove missing values from the 'Sun' column if necessary
  data <- data[!is.na(data$Sun), ]
  
  # Convert the 'Sun' column to a time series object (1995–2023)
  time_series_data <- ts(data$Sun, start = 1995, frequency = 1)  # Adjust start year if necessary
  
  # Build the ARIMA model automatically
  model <- auto.arima(time_series_data)
  
  # Display the model summary
  summary(model)
  
  # Predict (fitted) values for the years 1995–2023
  fitted_values <- fitted(model)
  
  # Create a data frame to show predicted values along with actual data
  prediction_results <- data.frame(
    Year = 1995:2023,
    Actual_Rainfall = data$Sun,
    Predicted_Rainfall = fitted_values
  )
  
  # Print predicted values
  print(prediction_results)
  
  # Plot the actual vs predicted rainfall data
  ggplot(prediction_results, aes(x = Year)) +
    geom_line(aes(y = Actual_Rainfall, color = "Actual"), size = 1) +
    geom_line(aes(y = Predicted_Rainfall, color = "Predicted"), size = 1, linetype = "dashed") +
    labs(title = "Actual vs Predicted Rainfall for Sun District (1995–2023)",
         x = "Year", y = "Rainfall (mm)") +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
    theme_minimal()
  
  # Save the results to a CSV
  write.csv(prediction_results, "Prediction_Results.csv", row.names = FALSE)
  
} else {
  stop("Column 'Sun' not found in the dataset. Please check column names.")
}

