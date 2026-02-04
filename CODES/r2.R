# Install required packages (uncomment if you haven't installed them yet)
install.packages("trend")
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("xlsx")
install.packages("mk.test")
install.packages("sens.slope")

# Load required libraries
library("trend")
library("readxl")
library("ggplot2")
library("dplyr")
# library("sens.slope") 

# Read the Excel file (make sure the path is correct)
ODISHA_1995_2023 <- read_xlsx("C:/path/to/your/file/ODISHA_1995_2023.xlsx")

# Perform Mann-Kendall Test on January data
mk.test(ODISHA_1995_2023$JAN)

# Perform Sens slope on January data
sens.slope(ODISHA_1995_2023$JAN)

# Plotting the graph for February data
ggplot(ODISHA_1995_2023, aes(YEAR, JAN)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, col = 'red', size = 1) +
  xlab("TIME") +
  ylab("PRECIPITATION") +
  labs(title = "Demo")
