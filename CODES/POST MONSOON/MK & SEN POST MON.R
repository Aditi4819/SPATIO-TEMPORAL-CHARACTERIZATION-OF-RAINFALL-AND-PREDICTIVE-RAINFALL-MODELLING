# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(trend)    # for mk.test (returns Z-value)
library(zyp)      # for Sen's slope
library(writexl)

# Read the Excel file
data <- read_xlsx("C:/Users/Amit Mallick/Desktop/7th sem MINOR/POST MONSOON (OCT-NOV).xlsx", sheet = "Sheet1")

# Reshape to long format
long_data <- data %>%
  pivot_longer(cols = -c(`Sl. No.`, District), names_to = "Year", values_to = "Precipitation") %>%
  mutate(Year = as.numeric(Year))

# Initialize results dataframe
results <- data.frame()

# Loop over each district
districts <- unique(long_data$District)

for (dist in districts) {
  dist_data <- long_data %>% filter(District == dist)
  
  # Mann-Kendall Test (get Z-value, p-value, tau)
  mk_test <- mk.test(dist_data$Precipitation)
  
  # Sen's slope
  sen <- zyp.sen(Precipitation ~ Year, data = dist_data)
  
  # Collect results
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
