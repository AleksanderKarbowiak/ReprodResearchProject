library(tidyverse)
library(lightgbm)
library(Metrics)
library(caret)
library(gridExtra)

# Loading data -----------------------------------------------------

load("housing_prices_data_clean.Rdata")

housing_prices_data_clean <- housing_prices_data_clean %>%
  mutate_if(is.factor, ~ as.numeric(as.character(.)))

housing_prices_data_clean %>%
  filter(if_any(everything(), is.na))


# Defining features and target --------------------------------------
features <- setdiff(names(housing_prices_data_clean), "totalPrice")
target <- "totalPrice"


# Splitting the data into training and testing sets -------------------------
set.seed(123456789)

data_which_train <- createDataPartition(housing_prices_data_clean$totalPrice ,
                                        p = 0.8, 
                                        list = FALSE) 

train_data <- housing_prices_data_clean[data_which_train,]
test_data <- housing_prices_data_clean[-data_which_train,]


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
  learning_rate = 0.15,
  feature_fraction = 0.9
)

# Training the LightGBM model with the specified number of estimators --------------
model_lgbm <- lgb.train(params,
                   trainMatrix,
                   nrounds = 1000,
                   valids = list(test = testMatrix),
                   early_stopping_rounds = 10)


# Prediction -----------------------------------------------------------------
test_predictions <- predict(model_lgbm, as.matrix(test_data[, -which(names(test_data) == "totalPrice")]))
train_predictions <- predict(model_lgbm, as.matrix(train_data[, -which(names(train_data) == "totalPrice")]))

## Excluding negative predicted values
test_predictions <- ifelse(test_predictions < 0, 0.01, test_predictions)
train_predictions <- ifelse(train_predictions < 0, 0.01, train_predictions)

# Evaluation metrics -------------------------------------------------------

## RMSLE and MAPE for test data
test_rmsle_score <- Metrics::rmsle(test_data[[target]], test_predictions)
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
plot_test <- data.frame(
  Real = test_data[[target]],
  Predicted = test_predictions
)

plot_train <- data.frame(
  Real = train_data[[target]],
  Predicted = train_predictions
)

# Plot the data
test_p <- ggplot(plot_test, aes(x = Predicted, y = Real)) +
  geom_point(alpha = 0.6, color = "lightblue") + 
  geom_abline(slope = 1, intercept = 0, color = "darkorange", lwd = 0.7) +  
  labs(
    x = "Light GBM Predictions",  
    y = "Actual Price"  ,
    subtitle = "Test Data"
  ) +
  scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +  # Format x-axis labels in millions
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +  # Format y-axis labels in millions
  theme_minimal() +
  theme(plot.title = element_blank()) 


train_p <- ggplot(plot_train, aes(x = Predicted, y = Real)) +
  geom_point(alpha = 0.6, color = "lightblue") + 
  geom_abline(slope = 1, intercept = 0, color = "darkorange", lwd = 0.7) +  
  labs(
    x = "Light GBM Predictions",  
    y = "Actual Price"  ,
    subtitle = "Train Data"
  ) +
  scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +  # Format x-axis labels in millions
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +  # Format y-axis labels in millions
  theme_minimal() +
  theme(plot.title = element_blank()) 

#grid.arrange(train_p, test_p, ncol = 2)
