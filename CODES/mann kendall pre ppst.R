# =====================================
# 📦 Load Required Libraries
# =====================================
# Uncomment below to install if not already installed
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("ggplot2")
# install.packages("trend")

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(trend)

# =====================================
# 📂 Read and Prepare Data
# =====================================
file_path <- "C:/Users/Amit Mallick/Desktop/7th sem MINOR/POST MONSOON (OCT-NOV).xlsx"

# Read Excel data (sheet name assumed as "Sheet1")
data <- read_excel(file_path, sheet = "Sheet1")

# Rename first column to 'District'
colnames(data)[1] <- "District"

# Convert to long format: District | Year | Rainfall
data_long <- data %>%
  pivot_longer(cols = -District, names_to = "Year", values_to = "Rainfall") %>%
  mutate(Year = as.numeric(Year))

# =====================================
# 📈 Visualization: Rainfall Trends
# =====================================
ggplot(data_long, aes(x = Year, y = Rainfall, color = District)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Pre-Monsoon Rainfall Trends in Odisha (1995–2023)",
    x = "Year",
    y = "Rainfall (mm)",
    color = "District"
  ) +
  theme_minimal()

# =====================================
# 📊 Linear Regression Trend Analysis
# =====================================
linear_results <- data_long %>%
  group_by(District) %>%
  filter(!is.na(Rainfall)) %>%
  summarize(
    Slope = coef(lm(Rainfall ~ Year))[2],
    Intercept = coef(lm(Rainfall ~ Year))[1],
    Linear_Trend = case_when(
      Slope > 0 ~ "Increasing",
      Slope < 0 ~ "Decreasing",
      TRUE ~ "Stable"
    ),
    .groups = 'drop'
  )

# =====================================
# 📈 Mann-Kendall Trend Test
# =====================================
mk_results <- data_long %>%
  group_by(District) %>%
  summarize(
    p_value = ifelse(sum(!is.na(Rainfall)) >= 10,
                     mk.test(na.omit(Rainfall))$p.value, NA),
    tau = ifelse(sum(!is.na(Rainfall)) >= 10,
                 mk.test(na.omit(Rainfall))$estimates[1], NA),
    MK_Trend = case_when(
      !is.na(p_value) & p_value < 0.05 & tau > 0 ~ "Increasing",
      !is.na(p_value) & p_value < 0.05 & tau < 0 ~ "Decreasing",
      !is.na(p_value) ~ "No Significant Trend",
      TRUE ~ NA_character_
    ),
    .groups = 'drop'
  )

# =====================================
# 📁 Merge Both Results
# =====================================
final_results <- left_join(linear_results, mk_results, by = "District")

# =====================================
# 💾 Save Results to CSV
# =====================================
write.csv(final_results, "C:/Users/Amit Mallick/Desktop/7th sem MINOR/post_rainfall_trend_analysis.csv", row.names = FALSE)

# =====================================
# 🖨️ View Final Table
# =====================================
print(final_results)
