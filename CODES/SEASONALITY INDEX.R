install.packages("tidyr")
install.packages("viridis")
# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

# Set the file path (update as necessary)
file_path <- "C:/Users/Amit Mallick/Desktop/7th sem MINOR/ODISHA_1995-2023.xlsx"

# Load the Excel file
rainfall_data <- read_excel(file_path, sheet = "Sheet1")

# Step 2: Reshape the Data
# Convert from wide to long format
rainfall_long <- rainfall_data %>%
  pivot_longer(cols = -DIST, names_to = "District", values_to = "Rainfall") %>%
  rename(Year = DIST)  # Rename the first column to 'Year'

# Step 3: Calculate District Averages
district_avg <- rainfall_long %>%
  group_by(District) %>%
  summarise(Average_Rainfall = mean(Rainfall, na.rm = TRUE))

# Step 4: Calculate the Overall Average
overall_avg <- mean(district_avg$Average_Rainfall, na.rm = TRUE)

# Step 5: Calculate Seasonality Index
district_avg <- district_avg %>%
  mutate(Seasonality_Index = Average_Rainfall / overall_avg)

# Step 6: Save Results to CSV
write.csv(district_avg, "ODISHA_Seasonality_Index.csv", row.names = FALSE)

# Step 7: Visualize Seasonality Index
ggplot(district_avg, aes(x = reorder(District, Seasonality_Index), y = Seasonality_Index, fill = District)) +
  geom_bar(stat = "identity") +
  labs(title = "Seasonality Index of Rainfall by District (1995-2023)",
       x = "District",
       y = "Seasonality Index") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()

# Print completion message
cat("Seasonality index calculated and saved to 'ODISHA_Seasonality_Index.csv'.\n")
