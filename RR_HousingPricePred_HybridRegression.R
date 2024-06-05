
data_rf <- readRDS("data_rf_model.rds")
data_xgboost <- readRDS("data_xgboost_model.rds")

source("RR_HousingPricePred_LightGBM.R")

load("housing_prices_data_clean.Rdata")
set.seed(123456789)
data_which_train <- createDataPartition(housing_prices_data_clean$totalPrice ,
                                        p = 0.8,
                                        list = FALSE) 

data_train <- housing_prices_data_clean[data_which_train,]
data_test <- housing_prices_data_clean[-data_which_train,]

pred_rf <- predict(data_rf, data_test)
pred_xgb <- predict(data_xgboost, data_test)
pred_lgb <- predict(model_lgbm, as.matrix(data_test[, -which(names(data_test) == "totalPrice")]))


# Combined Predictions
combined_predictions <- (pred_rf + pred_xgb + pred_lgb) / 3
combined_predictions <- ifelse(combined_predictions < 0, 0.01, combined_predictions)

# Evaluation of the combined model
combined_rmsle <- Metrics::rmsle(test_data$totalPrice, combined_predictions)
combined_mape <- Metrics::mape(test_data$totalPrice, combined_predictions)

cat("Combined RMSLE: ", combined_rmsle, "\n")
cat("Combined MAPE: ", combined_mape, "\n")

# Plot 
combined_plot <- tibble(
  pred = combined_predictions,
  actual = test_data$totalPrice
) %>% 
  ggplot(aes(pred, actual)) +
  geom_point(col = "lightblue") +
  geom_smooth(method = "lm", color = "darkorange") +
  labs(title = "Price Predictions using Hybrid Model", x = "Hybrid Model Prediction", y = "Total Price") +
  theme_minimal()

combined_plot
