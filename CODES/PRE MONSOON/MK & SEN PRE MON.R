# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(trend)    # for mk.test (returns Z-value)
library(zyp)      # for Sen's slope
library(writexl)

# Read the Excel file
data <- read_xlsx("C:/Users/Amit Mallick/Desktop/7th sem MINOR/PRE MONSOON (MAR, APR, MAY).xlsx", sheet = "Sheet1")

# Clean column names (just in case)
colnames(data)[1:2] <- c("Sl_No", "District")

# Reshape to long format (excluding Sl_No and District)
long_data <- data %>%
  pivot_longer(cols = -c(Sl_No, District), names_to = "Year", values_to = "Precipitation") %>%
  mutate(
    Year = as.numeric(Year),
    Precipitation = as.numeric(Precipitation)
  ) %>%
  filter(!is.na(Precipitation), !is.na(Year))  # Remove NA rows

# Initialize results dataframe
results <- data.frame()

# Loop over each district
districts <- unique(long_data$District)

for (dist in districts) {
  dist_data <- long_data %>% filter(District == dist)
  
  # Mann-Kendall Test
  mk_test <- mk.test(dist_data$Precipitation)
  
  # Sen's slope
  sen <- zyp.sen(Precipitation ~ Year, data = dist_data)
  
  # Append results
  results <- rbind(results, data.frame(
    District = dist,
    MK_Tau = mk_test$estimates[1],
    MK_Z = mk_test$statistic,
    MK_p_value = mk_test$p.value,
    Sens_Slope = sen$coefficients[2],
    Intercept = sen$coefficients[1]
  ))
}

# Save to CSV
write.csv(results, "C:/Users/Amit Mallick/Desktop/All_Districts_Trend_Analysis_With_Z.csv", row.names = FALSE)
