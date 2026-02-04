# Load required libraries
library(trend)
library(readxl)
library(ggplot2)
library(dplyr)
library(Kendall)  # Load Kendall package for Mann-Kendall test
library(zyp)      # Load zyp package for Sens slope

# Correct path to your Excel file
ODISHA_1995_2023 <- read_xlsx("C:/Users/Amit Mallick/Desktop/7th sem MINOR/ODISHA_1995-2023.xlsx")

# Perform Mann-Kendall Test on Angul data
mk_result <- MannKendall(ODISHA_1995_2023$`Ang`)
print(mk_result)

# Perform Sens slope on Angul data
sens_result <- sens.slope(ODISHA_1995_2023$`Ang`)
print(sens_result)

# Ensure DIST is a factor
ODISHA_1995_2023$DIST <- as.factor(ODISHA_1995_2023$DIST)

ggplot(ODISHA_1995_2023, aes(x = DIST, y = `Ang`, group = DIST)) +
  geom_line(aes(color = DIST)) +  # Differentiate districts by color
  geom_point() +
  geom_smooth(method = lm, se = FALSE, col = 'red', size = 1) +
  xlab("DISTRICT") +
  ylab("YEAR") +
  labs(title = "Precipitation Data for 1995") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.1))  # Rotate x-axis labels if needed
