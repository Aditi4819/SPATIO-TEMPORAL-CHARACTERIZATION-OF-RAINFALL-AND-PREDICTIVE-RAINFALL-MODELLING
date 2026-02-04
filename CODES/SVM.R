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

# Normalize the data (excluding 'Year')
scaler <- function(x) (x - min(x)) / (max(x) - min(x))
data_scaled <- as.data.frame(lapply(data[,-1], scaler))
data_scaled$Year <- data$Year

# Train SVM model for each district
future_years <- data.frame(Year = 2023:2034)
predictions <- list()

for (district in colnames(data)[-1]) {
  model <- svm(data_scaled[[district]] ~ Year, data = data_scaled, kernel = "radial", cost = 100, gamma = 0.1, epsilon = 0.1)
  pred_scaled <- predict(model, newdata = future_years)
  predictions[[district]] <- pred_scaled
}

# Convert predictions back to original scale
district_names <- colnames(data)[-1]
predicted_values <- as.data.frame(do.call(cbind, predictions))
predicted_values <- as.data.frame(lapply(1:ncol(predicted_values), function(i) {
  min_val <- min(data[[district_names[i]]])
  max_val <- max(data[[district_names[i]]])
  pred_scaled <- predicted_values[[i]]
  pred_scaled * (max_val - min_val) + min_val
}))
colnames(predicted_values) <- district_names
predicted_values$Year <- future_years$Year

# Save predictions to an Excel file
write.xlsx(predicted_values, "C:/Users/Amit Mallick/Desktop/7th sem MINOR/LSTM_2024-2034.xlsx")

# Plot rainfall trends for a sample district
sample_district <- "Ang"
ggplot() +
  geom_line(data = data, aes(x = Year, y = data[[sample_district]], color = "Actual"), size = 1) +
  geom_line(data = predicted_values, aes(x = Year, y = predicted_values[[sample_district]], color = "Predicted"), size = 1, linetype = "dashed") +
  labs(title = paste("Rainfall Prediction for", sample_district, "using SVM"),
       x = "Year", y = "Rainfall (mm)") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()
