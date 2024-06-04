library(tidyverse)
library(lightgbm)
library(Metrics)
library(caret)

# Loading data -----------------------------------------------------

load("housing_prices_data_clean.Rdata")

housing_prices_data_clean <- housing_prices_data_clean %>%
  mutate_if(is.factor, ~ as.numeric(as.character(.)))

housing_prices_data_clean %>%
  filter(across(everything(), ~ is.na(.)))


# Defining features and target --------------------------------------
features <- setdiff(names(housing_prices_data_clean), "totalPrice")
target <- "totalPrice"


# Splitting the data into training and testing sets -------------------------
set.seed(717)
trainIndex <- createDataPartition(housing_prices_data_clean$totalPrice, p = 0.85, list = FALSE)
train_data <- housing_prices_data_clean[trainIndex, ]
test_data <- housing_prices_data_clean[-trainIndex, ]


# Preparing data for LightGBM ------------------------------------------------
trainMatrix <- lgb.Dataset(data = as.matrix(train_data[, -which(names(train_data) == "totalPrice")]), 
                           label = train_data$totalPrice)
testMatrix <- lgb.Dataset(data = as.matrix(test_data[, -which(names(test_data) == "totalPrice")]), 
                          label = test_data$totalPrice)



# Defining LightGBM parameters based on the article -------------------------
params <- list(
  objective = "regression",
  metric = "rmse",
  min_child_weight = 2,
  num_leaves = 36,
  colsample_bytree = 0.8,
  reg_lambda = 0.40,
  learning_rate = 0.05,
  feature_fraction = 0.9
)


# Training the LightGBM model with the specified number of estimators --------------
model <- lgb.train(params,
                   trainMatrix,
                   nrounds = 1000,
                   valids = list(test = testMatrix),
                   early_stopping_rounds = 10)


# Prediction -----------------------------------------------------------------
test_predictions <- predict(model, as.matrix(test_data[, -which(names(test_data) == "totalPrice")]))
train_predictions <- predict(model, train_matrix_data)

# Evaluation metrics -------------------------------------------------------

## RMSLE and MAPE for test data
test_rmsle_score <- ModelMetrics::rmsle(test_data[[target]], test_predictions)
test_mape_score <- mape(test_data[[target]], test_predictions)

## RMSLE and MAPE for train data
train_rmsle_score <- rmsle(train_data[[target]], train_predictions)
train_mape_score <- mape(train_data[[target]], train_predictions)

cat("Test RMSLE: ", test_rmsle_score, "\n")
cat("Test MAPE: ", test_mape_score, "\n")
cat("Train RMSLE: ", train_rmsle_score, "\n")
cat("Train MAPE: ", train_mape_score, "\n")


# Plot --------------------------------------------------------------------

# Create a dataframe for plotting
plot_data <- data.frame(
  Real = test_data[[target]],
  Predicted = test_predictions
)

# Plot the data
ggplot(plot_data, aes(x = Predicted, y = Real)) +
  geom_point(alpha = 0.6) +  # Plot the actual vs. predicted values
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Perfect fit line
  labs(title = "Predicted vs. Real Values",
       x = "Predicted Values",
       y = "Real Values") +
  theme_minimal()
