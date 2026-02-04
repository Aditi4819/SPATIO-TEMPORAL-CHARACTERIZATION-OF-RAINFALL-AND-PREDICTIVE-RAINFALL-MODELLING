# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)

# Set file path
file_path <- "ODISHA_1995-2023.xlsx"

# Load data 
data <- read_excel("C:/Users/Amit Mallick/Desktop/7th sem MINOR/ODISHA_1995-2023.xlsx", sheet = "Sheet1")

# Reshape data from wide to long format
long_data <- data %>%
  pivot_longer(cols = -DIST, names_to = "District", values_to = "Rainfall") %>%
  rename(Year = DIST)

# Calculate mean annual rainfall for each district
district_means <- long_data %>%
  group_by(District) %>%
  summarise(Mean_Rainfall = mean(Rainfall, na.rm = TRUE))

# Merge mean rainfall back to the main data
long_data <- long_data %>%
  left_join(district_means, by = "District")

# Calculate rainfall anomaly (deviation from the mean)
long_data <- long_data %>%
  mutate(Anomaly = Rainfall - Mean_Rainfall)

# Separate positive and negative anomalies
long_data <- long_data %>%
  group_by(District) %>%
  mutate(
    Pos_Anomaly = ifelse(Anomaly > 0, Anomaly, NA),
    Neg_Anomaly = ifelse(Anomaly < 0, Anomaly, NA)
  )

# Calculate RAI
long_data <- long_data %>%
  group_by(District) %>%
  mutate(
    RAI = case_when(
      Anomaly > 0 ~ (3 * Pos_Anomaly / max(Pos_Anomaly, na.rm = TRUE)),
      Anomaly < 0 ~ (-3 * Neg_Anomaly / min(Neg_Anomaly, na.rm = TRUE)),
      TRUE ~ 0
    )
  )

# View results
print(long_data)

# Save results to a CSV
write.csv(long_data, "Rainfall_Anomaly.csv", row.names = FALSE)
