library(h2o)

# Combine predictions into a data frame
combined_predictions <- data.frame(pred_rf = data_rf, pred_lgb = pred_lgb, pred_xgb = data_xgboost)

# Split the combined predictions into train and test sets
set.seed(123)
train_indices <- sample(nrow(combined_predictions), 0.8 * nrow(combined_predictions))
train_data <- combined_predictions[train_indices, ]
test_data <- combined_predictions[-train_indices, ]

# Initialize H2O
h2o.init()

# Convert data to H2O frames
train_h2o <- as.h2o(train_data)
test_h2o <- as.h2o(test_data)

# Identify predictors and response
x <- setdiff(names(train_h2o), "totalPrice")
y <- "totalPrice"

# Define base learner models
base_learners <- list(
  rf = h2o.randomForest(x = x, y = y, training_frame = train_h2o),
  lgb = h2o.gbm(x = x, y = y, training_frame = train_h2o),
  xgb = h2o.xgboost(x = x, y = y, training_frame = train_h2o)
)

# Train level-one ensemble model using XGBoost as the metalearner
stacked_model <- h2o.stackedEnsemble(
  x = x,
  y = y,
  training_frame = train_h2o,
  base_models = base_learners,
  metalearner_algorithm = "xgboost",
  nfolds = 5
)

# Evaluate ensemble performance on the test set
perf <- h2o.performance(stacked_model, newdata = test_h2o)
print(perf)
