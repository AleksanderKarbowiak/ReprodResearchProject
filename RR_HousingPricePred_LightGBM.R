library(tidyverse)
library(lightgbm)
library(Metrics)
library(ModelMetrics)

# Loading data -----------------------------------------------------

source("ReprodResearch_HousingPricePred.R")

# Defining features and target --------------------------------------
features <- setdiff(names(housing_prices_data_clean), "totalPrice")
target <- "totalPrice"


# Splitting the data into training and testing sets -------------------------
set.seed(717)
train_index <- sample(seq_len(nrow(housing_prices_data_clean)), 
                      size = 0.8 * nrow(housing_prices_data_clean))
train_data <- housing_prices_data_clean[train_index, ]
test_data <- housing_prices_data_clean[-train_index, ]


# Preparing data for LightGBM ------------------------------------------------
train_matrix <- lgb.Dataset(data = as.matrix(train_data[features]), 
                            label = train_data[[target]])
test_matrix <- as.matrix(test_data[features])


# Defining LightGBM parameters based on the article -------------------------
params <- list(
  objective = "regression",
  metric = "rmse",
  min_child_weight = 2,
  num_leaves = 36,
  colsample_bytree = 0.8,
  reg_lambda = 0.40
)


# Training the LightGBM model with the specified number of estimators --------------
model <- lgb.train(
  params = params,
  data = train_matrix,
  nrounds = 64,
  verbose = 1
)


# Prediction -----------------------------------------------------------------
predictions <- predict(model, test_matrix)


# Evaluation metrics -------------------------------------------------------

## Train data ---------------------------------------------------------------

## Test data -----------------------------------------------------------------
## RMSLE 
rmsle_score <- rmsle(log1p(test_data[[target]]), log1p(predictions))
## MAPE 
mape_score <- mape(test_data[[target]], predictions)

cat("RMSLE: ", rmsle_score, "\n")
cat("MAPE: ", mape_score, "\n")


# Plot --------------------------------------------------------------------

# Create a dataframe for plotting
plot_data <- data.frame(
  Real = test_data[[target]],
  Predicted = predictions
)

# Plot the data
ggplot(plot_data, aes(x = Predicted, y = Real)) +
  geom_point(alpha = 0.6) +  # Plot the actual vs. predicted values
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Perfect fit line
  labs(title = "Predicted vs. Real Values",
       x = "Predicted Values",
       y = "Real Values") +
  theme_minimal()