library(readxl)
library(e1071)
library(dplyr)
library(ggplot2)
library(tidyr)
library(openxlsx)

# Define file path correctly
file_path <- "C:/Users/Amit Mallick/Desktop/7th sem MINOR/ODISHA_1995-2023.xlsx"

# Load dataset
if (!file.exists(file_path)) {
  stop("File not found. Check the file path.")
}

data <- read_excel(file_path, sheet = "Sheet1", col_names = TRUE)

# Rename first column to 'Year'
colnames(data)[1] <- "Year"

# Train SVR Model for each district
future_years <- data.frame(Year = 2023:2034)
predictions <- list()

for (district in colnames(data)[-1]) {
  svr_model <- svm(as.formula(paste(district, "~ Year")), data = data, kernel = "radial", cost = 100, gamma = 0.1, epsilon = 0.1)
  predictions[[district]] <- predict(svr_model, newdata = future_years)
}

# Convert predictions to data frame
predicted_values <- as.data.frame(predictions)
predicted_values$Year <- future_years$Year

# Save predictions to an Excel file
write.xlsx(predicted_values, "C:/Users/Amit Mallick/Desktop/7th sem MINOR/SVR_2024-2034.xlsx")

# Plot rainfall trends for a sample district
sample_district <- "Ang"
ggplot() +
  geom_line(data = data, aes(x = Year, y = data[[sample_district]], color = "Actual"), size = 1) +
  geom_line(data = predicted_values, aes(x = Year, y = predicted_values[[sample_district]], color = "Predicted"), size = 1, linetype = "dashed") +
  labs(title = paste("Rainfall Prediction for", sample_district, "using Decision Tree"),
       x = "Year", y = "Rainfall (mm)") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()
