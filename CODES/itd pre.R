# Load libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

# Load Excel data
data <- read_xlsx("C:/Users/Amit Mallick/Desktop/7th sem MINOR/PRE MONSOON (MAR, APR, MAY).xlsx", sheet = "Sheet1")

# Fix column names: make sure first column is Year
colnames(data)[1] <- "Year"

# Convert Year to numeric
data$Year <- as.numeric(data$Year)

# Ensure all column names are characters (to prevent type-mixing)
colnames(data) <- as.character(colnames(data))

# Pivot data to long format
data_long <- data %>%
  pivot_longer(cols = -Year, names_to = "District", values_to = "Value") %>%
  mutate(Value = as.numeric(Value)) %>%
  filter(!is.na(Value), !is.na(Year))  # Clean NAs

# Plot trends
ggplot(data_long, aes(x = Year, y = Value, color = District)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Innovative Trend Analysis for Odisha Districts (1995–2023)",
    x = "Year",
    y = "Precipitation",
    color = "District"
  ) +
  theme_minimal()

# Perform linear regression per district
results <- data_long %>%
  group_by(District) %>%
  summarise(
    Intercept = coef(lm(Value ~ Year))[1],
    Slope = coef(lm(Value ~ Year))[2],
    .groups = "drop"
  )

# Save results
write.csv(results, "C:/Users/Amit Mallick/Desktop/ITD.csv", row.names = FALSE)

# Print results
print(results)
