# Load necessary library
library(readxl)

# Load the Excel file
file_path <- "C:/Users/Amit Mallick/Desktop/7th sem MINOR/POST MONSOON (OCT-NOV).xlsx"

data <- read_excel("C:/Users/Amit Mallick/Desktop/7th sem MINOR/POST MONSOON (OCT-NOV).xlsx"
, sheet = "Sheet1")

# Extract the "Angul" column
angul_data <- na.omit(data$Angul) # Remove any missing values

# Calculate the lag-1 autocorrelation
lag_1_ac <- cor(Angul_data[-length(Angul_data)], Angul_data[-1])

# Print the result
print(paste("Lag-1 Autocorrelation:", lag_1_ac))

plot(Angul_data, type = "l", main = "Rainfall inAngul Over Time", xlab = "Time", ylab = "Rainfall")
