#install.packages('dplyr','tidyr','ggplot2','lubridate')
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(zoo)

################################## Load Inputs ##################################
##'rapid_transit_and_bus_prediction_accuracy_data.csv'
mbta_data <- read.table(file.choose(), sep=",",header=TRUE, stringsAsFactors = FALSE)
##'MBTA_Ratings_%26_Seasons.csv'
season_data<- read.table(file.choose(), sep=",",header=TRUE, stringsAsFactors = FALSE)
# Load ridership data
ridership <- read.table(file.choose(), sep=",",header=TRUE, stringsAsFactors = FALSE)
################################## Load Inputs ##################################

################################## Merge Seasons and Ridership ##################################
# Convert date fields to Date type
mbta_data$weekly <- as.Date(mbta_data$weekly)
season_data$date_start <- as.Date(season_data$date_start)
season_data$date_end <- as.Date(season_data$date_end)

# Merge datasets based on weekly date falling within season date ranges
merged_data <- mbta_data %>% 
  mutate(route_id = gsub("^\\s*$", "bus", route_id), # Replace empty spaces with 'bus'
         route_id = ifelse(grepl("Green", route_id), "Green", route_id),
         route_id = ifelse(grepl("Orange", route_id), "Orange", route_id),
         route_id = ifelse(grepl("Blue", route_id), "Blue", route_id),
         route_id = ifelse(grepl("Red", route_id), "Red", route_id)) %>%  
inner_join(season_data, by = character()) %>%
  filter(weekly >= date_start & weekly <= date_end)
# Select only relevant columns
merged_data <- merged_data %>%
  select(-date_start, -date_end, -ObjectId) 
# Add a month column (convert weekly dates to YYYY-MM format)
merged_data <- merged_data %>%
  mutate(month =  format(weekly, "%Y-%m")) # Converts to "YYYY-MM" format

# Create a mapping table for ridership routes to prediction accuracy routes
route_mapping <- tibble::tibble(
  ridership_routes = c("Bus", "Commuter Rail", "Green Line", "Orange Line", "Red Line", 
                       "Silver Line", "The RIDE", "Blue Line", "Boat-F1", "Boat-F3", 
                       "Boat-F4", "Ferry"),
  prediction_routes = c("bus", NA, "Green", "Orange", "Red", 
                        NA, NA, "Blue", "Ferry", "Ferry", 
                        "Ferry", "Ferry") # Map to comparable names or NA for no equivalent
)

# Join ridership with route_mapping
standardized_ridership <- ridership %>%
  inner_join(route_mapping, by = c("route_or_line" = "ridership_routes"))
# Ridership By Month and Routes
ridership_group <- standardized_ridership %>%
  mutate(
    service_date = as.Date(service_date), # Convert to Date format if not already
    yyyy_month = format(service_date, "%Y-%m") # Extract year and month in "YYYY-MM" format
  ) %>%
  group_by(yyyy_month, prediction_routes) %>%
  summarize(
    total_ridership = sum(average_monthly_ridership, na.rm = TRUE)
  )

# Step 3: Merge ridership_group with merged_data
final_data_merged_ridership <- merged_data %>%
  inner_join(ridership_group, by = c("month" = "yyyy_month","route_id"="prediction_routes"))

final_data_merged_ridership_grouped <- final_data_merged_ridership %>%
  group_by(route_id,month,bin) %>%
  summarize(
    total_predictions = sum(num_predictions, na.rm = TRUE),
    total_accurate = sum(num_accurate_predictions, na.rm = TRUE),
    total_ridership= min(total_ridership),
    bin=min(bin)
  ) %>%
  mutate(accuracy_rate = (total_accurate / total_predictions) * 100)

# 4. Bar Plot for `total_ridership` by `route_id`
ggplot(final_data_merged_ridership_grouped, aes(x = reorder(route_id,desc(total_ridership)), y = total_ridership/1000000, fill = route_id)) + 
  geom_bar(stat = "identity") +
  labs(title = "Total Ridership by Route From 2020-08 to 2024-05 ", x = "Route ID", y = "Total Ridership(in Mils)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10)
  )


# Rescaling ridership for consistent scaling
final_data_merged_ridership_grouped <- final_data_merged_ridership_grouped %>%
  mutate(scaled_ridership = total_ridership / max(total_ridership) * 100)

# Plot : Prediction and Ridership follows almost similar trends
ggplot(final_data_merged_ridership_grouped, aes(x = as.Date(paste0(month, "-01")))) +
  geom_line(aes(y = accuracy_rate, color = "Accuracy Rate"), size = 1) +
  geom_line(aes(y = scaled_ridership, color = "Ridership Volume"), size = 1)+ #linetype = "dashed") +
  scale_y_continuous(
    name = "Accuracy Rate (%)",
    sec.axis = sec_axis(~ . * max(final_data_merged_ridership_grouped$total_ridership) / 1000000, name = "Total Ridership (in Million)")
  ) +
  labs(
    title = "Accuracy Rate and Ridership Trends Over Time by Route",
    x = "Month",
    color = "Metric"
  ) +
  facet_wrap(~ route_id, scales = "free_y") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )


# Check for missing values
colSums(is.na(final_data_merged_ridership_grouped))

# Function to create a glimpse table
create_glimpse_table <- function(df) {
  tibble(
    Column_Name = names(df),
    Data_Type = sapply(df, class),
    Example_Value = sapply(df, function(x) if (length(x) > 0) x[1] else NA)
  )
}

raw_data_glimpse<-create_glimpse_table(merged_data)


################################## Merge Seasons and Ridership ##################################

################################## EDA -1 ###############################################
# Data transformation: Calculate accuracy rate per route
data_summary <- mbta_data %>%
  mutate(route_id = gsub("^\\s*$", "bus", route_id)) %>%  # Replace empty spaces with 'bus'
  group_by(route_id) %>%
  summarize(
    total_predictions = sum(num_predictions, na.rm = TRUE),
    total_accurate = sum(num_accurate_predictions, na.rm = TRUE)
  ) %>%
  mutate(accuracy_rate = (total_accurate / total_predictions) * 100)

# Bar plot of prediction accuracy by route 
ggplot(data_summary, aes(x = reorder(route_id,desc(accuracy_rate)), y = accuracy_rate, fill = route_id)) + #
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Prediction Accuracy by Route",
       x = "Route ID",
       y = "Prediction Accuracy Rate (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10)
  )


################################## EDA ##################################  


################################## EDA -2 Not in Report ##################################  
# Calculate accuracy rate per late time bin
data_summary_1 <- mbta_data %>%
  group_by(bin) %>%
  summarize(total_predictions = sum(num_predictions, na.rm = TRUE),
            total_accurate = sum(num_accurate_predictions, na.rm = TRUE)) %>%
  mutate(accuracy_rate = total_accurate / total_predictions * 100)

# Bar plot of prediction accuracy by route and bin
ggplot(data_summary_1, aes(x = reorder(bin,desc(accuracy_rate)), y = accuracy_rate, fill = bin)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Prediction Accuracy Across Delay Time Intervals",
       x = "Delay Time Intervals (in min)",
       y = "Prediction Accuracy Rate (%)") +
  geom_text(aes(label = paste0(round(accuracy_rate,2), "%")), vjust = -0.5, size = 4) + 
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")+theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10)
  )
################################## EDA -2 Not in Report ##################################  


################################## EDA -3 ##################################  
# Calculate accuracy rate per Date
data_summary_2 <- mbta_data %>%
  mutate(year = year(weekly),  # Extract year
         quarter = quarter(weekly)) %>%  # Extract quarter (1-4)
  group_by(year, quarter) %>%
  summarize(total_predictions = sum(num_predictions, na.rm = TRUE),
            total_accurate = sum(num_accurate_predictions, na.rm = TRUE)) %>%
  mutate(accuracy_rate = total_accurate / total_predictions * 100)

# Combine year and quarter into a single 'quarter' label for plotting
data_summary_2 <- data_summary_2 %>%
  mutate(quarter_label = factor(paste(year, "Q", quarter, sep = "-"), 
                                levels = paste(rep(unique(data_summary_2$year), each = 4), 
                                               "Q", 1:4, sep = "-")))

# Plot a line chart for accuracy_rate over time (by year and quarter)
ggplot(data_summary_2, aes(x = quarter_label, y = accuracy_rate)) +  
  geom_line(color = "blue", group = 1) +  # Create a line plot, group = 1 ensures a single line
  geom_point(color = "red") +  # Optionally add points to the line
  theme_minimal() +  # Use a clean theme
  labs(title = "Prediction Accuracy Over Time (by Quarter)", x = "Quarter", y = "Prediction Accuracy Rate (%)") +
  scale_y_continuous(limits = c(0, NA)) +  # Set the y-axis to start at 0 
  theme(
  plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
  axis.text.y = element_text(size = 10)
)





################################## EDA -3 ##################################  

################################## EDA -4 ################################## 
# Box plot of prediction accuracy by route and bin
ggplot(final_data_merged_ridership_grouped, aes(x = bin, y = accuracy_rate, fill = route_id)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Prediction Accuracy Distribution Across Delay Time Intervals",
       x = "Delay Time Intervals (in min)",
       y = "Prediction Accuracy Rate (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10)
  ) +
  facet_wrap(~ route_id, scales = "free_y")  # Add route as a facet
################################## EDA -4 ################################## 

################################## Hypothesis -1 : Prediction Vs Ridership ################################################
# Perform Pearson correlation test
correlation_result <- cor.test(final_data_merged_ridership_grouped$accuracy_rate, final_data_merged_ridership_grouped$total_ridership)
correlation_result
################################## Hypothesis -1 : Prediction Accuracy Vs Ridership Volume ################################################

################################## Hypothesis -2 : Prediction Accuracy Vs Modes ################################################
# Perform one-way ANOVA to test the difference in prediction accuracy across modes
anova_result_modes <- aov(accuracy_rate ~ route_id, data = final_data_merged_ridership_grouped)

# Summary of the ANOVA result
summary(anova_result_modes)

# Perform Tukey's HSD post-hoc test to compare each pair of modes
tukey_result <- TukeyHSD(anova_result_modes)

# Summary of Tukey's results
print(tukey_result)
################################## Hypothesis -2 : Prediction Accuracy Vs Modes ################################################

