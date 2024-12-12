#install.packages('dplyr','tidyr','ggplot2','lubridate','zoo','glmnet','caret','car','MASS','knitr')
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(glmnet)
library(caret)
library(MASS)
library(knitr)
library(car)

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

final_data_merged_ridership_grouped_new <- final_data_merged_ridership %>%
  # Take out Month alone
  mutate(month_numeric = substr(month, nchar(month) - 1, nchar(month))) %>%
  group_by(route_id,month_numeric,season_name) %>%
  summarize(
    total_predictions = sum(num_predictions, na.rm = TRUE),
    total_accurate = sum(num_accurate_predictions, na.rm = TRUE),
    total_ridership= min(total_ridership),
    bin=min(bin)
  ) %>%
  mutate(accuracy_rate = (total_accurate / total_predictions) * 100)

# Check for missing values
colSums(is.na(final_data_merged_ridership_grouped_new))

final_data_merged_ridership_grouped <- final_data_merged_ridership %>%
  group_by(route_id,month,bin) %>%
  summarize(
    total_predictions = sum(num_predictions, na.rm = TRUE),
    total_accurate = sum(num_accurate_predictions, na.rm = TRUE),
    total_ridership= min(total_ridership),
    bin=min(bin)
  ) %>%
  mutate(accuracy_rate = (total_accurate / total_predictions) * 100)

# Check for missing values
colSums(is.na(final_data_merged_ridership_grouped))


################################## Merge Seasons and Ridership ##################################

################################## EDA ##################################
# Bar Plot for `total_ridership` by `route_id`
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
################################## EDA ##################################

################################## EDA ##################################

# Rescaling ridership for consistent scaling
final_data_merged_ridership_grouped <- final_data_merged_ridership_grouped %>%
  mutate(scaled_ridership = total_ridership / max(total_ridership) * 1000)


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

################################## EDA ##################################

################################## EDA ################################## 
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
################################## EDA ################################## 

################################## Hypothesis -1 : Prediction Accuracy Vs Ridership ################################################
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

################################## Hypothesis -3 : Prediction Accuracy Vs Time ################################################
# Data Cleaning
data_cleaned <- mbta_data %>%
  filter(!is.na(route_id)) %>%
  mutate(
    prediction_accuracy = num_accurate_predictions / num_predictions,
    weekly = as.Date(weekly)
  )

# Filter data for the last 6 months
latest_date <- max(data_cleaned$weekly, na.rm = TRUE)
six_months_ago <- latest_date - months(6)

data_last_6_months <- data_cleaned %>%
  filter(weekly >= six_months_ago)

# Categorize prediction accuracy into bins
data_last_6_months <- data_last_6_months %>%
  mutate(
    accuracy_category = cut(
      prediction_accuracy,
      breaks = c(0, 0.7, 0.9, 1),
      labels = c("Low", "Medium", "High"),
      include.lowest = TRUE
    )
  )

# Calculate weekly mean prediction accuracy
accuracy_trends <- data_last_6_months %>%
  group_by(weekly) %>%
  summarize(mean_accuracy = mean(prediction_accuracy, na.rm = TRUE))

summary(accuracy_trends)

# Plot the trends
ggplot(accuracy_trends, aes(x = weekly, y = mean_accuracy)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    title = "Prediction Accuracy Trends Over the Past 6 Months",
    x = "Week",
    y = "Mean Prediction Accuracy"
  ) +
  theme_minimal()

# Create a contingency table
contingency_table <- table(data_last_6_months$weekly, data_last_6_months$accuracy_category)

# Perform Chi-Square Test
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)

################################## Hypothesis -3 : Prediction Accuracy Vs Time ################################################

# Set seed for reproducibility
set.seed(123)

# Partition the data: 80% training and 20% testing
trainIndex <- createDataPartition(final_data_merged_ridership_grouped_new$accuracy_rate, p = 0.8, list = FALSE)
train_data <- final_data_merged_ridership_grouped_new[trainIndex, ]
test_data <- final_data_merged_ridership_grouped_new[-trainIndex, ]

# Model 1: General Linear Regression
lm_model <- lm(accuracy_rate ~ route_id + month_numeric + total_ridership, data = train_data)
lm_pred_train <- predict(lm_model, train_data)
lm_pred_test <- predict(lm_model, test_data)
summary(lm_model)

### Residual Plot
par(mfrow=c(2,2))
plot(lm_model)

## Component +Residual Plot for each predictor
### Residual Plot
par(mfrow=c(1,1))
crPlots(lm_model)

# Calculate RMSE for Linear Regression
lm_rmse_train <- sqrt(mean((lm_pred_train - train_data$accuracy_rate)^2))
lm_rmse_test <- sqrt(mean((lm_pred_test - test_data$accuracy_rate)^2))
cat(lm_rmse_train,lm_rmse_test)

# Model 2: Lasso Regression
x_train <- model.matrix(accuracy_rate ~ route_id + month_numeric + total_ridership, data = train_data)[, -1]
y_train <- train_data$accuracy_rate
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1)
lasso_pred_train <- predict(lasso_model, x_train, s = "lambda.min")
lasso_pred_test <- predict(lasso_model, model.matrix(accuracy_rate ~ route_id + month_numeric + total_ridership, data = test_data)[, -1], s = "lambda.min")

# Plots and Results
cat("Lasso Regression: lambda.min =", lasso_model$lambda.min, "lambda.1se =", lasso_model$lambda.1se, "\n")
abline(v=log(c(lasso_model$lambda.min,lasso_model$lambda.1se)),lty=2)
## PLOT COEFFICIENTS
print(coef(lasso_model))
plot(coef(lasso_model))
abline(h=0)

# Calculate RMSE for Lasso Regression
lasso_rmse_train <- sqrt(mean((lasso_pred_train - y_train)^2))
lasso_rmse_test <- sqrt(mean((lasso_pred_test - test_data$accuracy_rate)^2))
cat(lasso_rmse_train,lasso_rmse_test)

# Model 3: Stepwise Regression
step_model <- stepAIC(lm_model, direction = "both", trace = FALSE)
step_pred_train <- predict(step_model, train_data)
step_pred_test <- predict(step_model, test_data)

# Calculate RMSE for Stepwise Regression
step_rmse_train <- sqrt(mean((step_pred_train - train_data$accuracy_rate)^2))
step_rmse_test <- sqrt(mean((step_pred_test - test_data$accuracy_rate)^2))
cat(step_rmse_train,step_rmse_test)

# Collect results for comparison
coefficients_lm <- coef(lm_model)
coefficients_lasso <- coef(lasso_model, s = "lambda.min")
coefficients_step <- coef(step_model)
summary(step_model)

# Extract the anova table from the stepwise model, which contains the step information.
step_info <- step_model$anova

# Create a data frame to store step information for plotting
stepwise_df <- data.frame(
  Step = 1:nrow(step_info),
  AIC = step_info$AIC,
  Variable = step_info$Step
)

# Use kable to display the table in a report-friendly format
kable(stepwise_df, 
      caption = "Stepwise Regression: Steps, AIC, and Variables Added/Removed",
      col.names = c("Step", "AIC", "Variable Added/Removed"),
      format = "markdown") 

# Print the table for reporting
print(stepwise_df)

# Load necessary libraries
library(knitr)

# Linear Regression - Adjusted R-squared
adj_r2_linear <- summary(lm_model)$adj.r.squared

# Stepwise Regression - Adjusted R-squared
adj_r2_step <- summary(step_model)$adj.r.squared

# Create a data frame for comparison
model_comparison <- data.frame(
  Model = c("Linear Regression", "Stepwise Regression", "Lasso Regression"),
  `Adj. R-squared` = c(adj_r2_linear, adj_r2_step, 'NA'),
  `Train RMSE` = c(lm_rmse_train, step_rmse_train, lasso_rmse_train),
  `Test RMSE` = c(lm_rmse_test, step_rmse_test, lasso_rmse_test)
)

kable(model_comparison, caption = "Comparison of Models: Adjusted R-squared and RMSE")

# Create a data frame for RMSE comparison
rmse_comparison <- data.frame(
  Model = c("Linear Regression (Train)", "Stepwise Regression (Train)", "Lasso Regression (Train)",
            "Linear Regression (Test)", "Stepwise Regression (Test)", "Lasso Regression (Test)"),
  RMSE = c(step_rmse_train, step_rmse_train, lasso_rmse_train,
           step_rmse_test, step_rmse_test, lasso_rmse_test)
)

# Load the knitr library for kable
library(knitr)

# Create a table using kable to display RMSE comparison
kable(rmse_comparison, caption = "RMSE Comparison Across Models", digits = 4)

