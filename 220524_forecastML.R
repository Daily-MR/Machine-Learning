# 1. Load Data and Package
library(forecastML)
library(dplyr)
library(DT)
library(ggplot2)
library(glmnet)
library(randomForest)

data("data_seatbelts", package = "forecastML")
data = data_seatbelts
date_frequency = "1 month"
dates = seq(as.Date("1960-01-01"), as.Date("1984-12-01"), by = date_frequency)

data$PetrolPrice = round(data$PetrolPrice, 3)
data = data[, c("DriversKilled", "kms", "PetrolPrice", "law")]
DT::datatable(head(data, 5))

# 2. Train-Test Split
data_train = data[1:(nrow(data) - 12), ]
data_test = data[(nrow(data) - 12 + 1):nrow(data), ]

p = ggplot(data, aes(x = dates, y = DriversKilled))
p = p + geom_line()
p = p + geom_vline(xintercept = dates[nrow(data_train)], color = "red", size = 1.1)
p = p + theme_bw() + xlab("Dataset index")
p

# 3. Data Preparation
## 3.1 forecastML::create_lagged_df
outcome_col = 1
horizons = c(1, 3, 6, 12)
lookback = c(1:6, 9, 12, 15)
dynamic_features = "law"
data_list = forecastML::create_lagged_df(data_train,
                                         outcome_col = outcome_col,
                                         type = "train",
                                         horizons = horizons,
                                         lookback = lookback,
                                         date = dates[1:nrow(data_train)],
                                         frequency = date_frequency,
                                         dynamic_features = dynamic_features)
DT::datatable(head(data_list$horizon_6, 10), options = list(scrollX = TRUE))
plot(data_list)

## 3.2 forecastML::create_windows
windows = forecastML::create_windows(lagged_df = data_list, window_length = 24, skip = 0,
                                     window_start = NULL, window_stop = NULL, include_partial_window = TRUE)
windows
plot(windows, data_list, show_labels = TRUE)

# 4. Model Training
## 4.1 User-defined modeling function(사용자 정의 함수)
## example 1 - LASSO
model_function = function(data){
  constant_features = which(unlist(lapply(data[, -1], function(x) {!(length(unique(x)) > 1)})))
  
  if (length(constant_features) > 1){
    data = data[, -c(constant_features + 1)]
  }
  
  x = data[, -(1), drop = FALSE]
  y = data[, 1, drop = FALSE]
  x = as.matrix(x, ncol = ncol(x))
  y = as.matrix(y, ncol = ncol(y))
  
  model = glmnet::cv.glmnet(x, y, nfolds = 3)
  return(list("model" = model, "constant_features" = constant_features))
}

## example 2 - Random Forest
model_function_2 = function(data){
  outcome_names = names(data)[1]
  model_formula = formula(paste0(outcome_names, "~ ."))
  
  model = randomForest::randomForest(formula = model_formula, data = data, ntree = 200)
  return(model)
}

## 4.2 forecastML::train_model
model_results = forecastML::train_model(data_list, windows, model_name = "LASSO",
                                        model_function, use_future = FALSE)
model_results_2 = forecastML::train_model(data_list, windows, model_name = "RF",
                                          model_function_2, use_future = FALSE)

# 5. User-defined prediction function
## example 1 - LASSO
prediction_function = function(model, data_features) {
  if (length(model$constant_features) > 1) {  
    data = data[, -c(model$constant_features )]
  }
  
  x = as.matrix(data_features, ncol = ncol(data_features))
  
  data_pred = data.frame("y_pred" = predict(model$model, x, s = "lambda.min"))
  return(data_pred)
}

## example 2 - Random Forest
prediction_function_2 = function(model, data_features) {
  data_pred = data.frame("y_pred" = predict(model, data_features))
  return(data_pred)
}

# 6. Predict on historical data
data_results = predict(model_results, model_results_2,
                       prediction_function = list(prediction_function, prediction_function_2), data = data_list)
data_results$DriversKilled_pred = round(data_results$DriversKilled_pred, 0)
DT::datatable(head(data_results, 30), options = list(scrollX = TRUE))

plot(data_results, type = "prediction", horizons = c(1, 6, 12))
plot(data_results, type = "residual", horizons = c(1, 6, 12), windows = 5:7)
plot(data_results, type = "forecast_stability", windows = max(data_results$window_number))

data_error = forecastML::return_error(data_results, metrics = c("mae", "mape", "smape"))
data_error$error_global[, c("mae", "mape", "smape")] = lapply(data_error$error_global[, c("mae", "mape", "smape")], round, 1)

# 7. Model Performance
data_error = forecastML::return_error(data_results, metrics = c("mae", "mape", "smape"))
data_error$error_global[, c("mae", "mape", "smape")] = lapply(data_error$error_global[, c("mae", "mape", "smape")], round, 1)
DT::datatable(data_error$error_global, options = list(scrollX = TRUE))

plot(data_error, type = "window", facet = ~ horizon, horizons = c(1, 6, 12))
plot(data_error, type = "horizon", facet = ~ horizon, horizons = c(1, 6, 12))
plot(data_error, type = "global", facet = ~ horizon)

# 8. Hyperparameters
## 8.1 User-defined hyperparameter function
hyper_function <- function(model) {
  
  lambda_min <- model$model$lambda.min
  lambda_1se <- model$model$lambda.1se
  
  data_hyper <- data.frame("lambda_min" = lambda_min, "lambda_1se" = lambda_1se)
  return(data_hyper)
}

## 8.2 forecastML::return_hyper
data_hyper <- forecastML::return_hyper(model_results, hyper_function)

plot(data_hyper, data_results, data_error, type = "stability", horizons = c(1, 6, 12))
plot(data_hyper, data_results, data_error, type = "error", c(1, 6, 12))

# 9. Forecast with Multiple Models from Nested CV
## 9.1 forecastML::create_lagged_df
data_forecast_list <- forecastML::create_lagged_df(data_train,
                                                   outcome_col = outcome_col,
                                                   type = "forecast",
                                                   horizons = horizons,
                                                   lookback = lookback,
                                                   date = dates[1:nrow(data_train)],
                                                   frequency = date_frequency,
                                                   dynamic_features = dynamic_features
)

DT::datatable(head(data_forecast_list$horizon_6), options = list(scrollX = TRUE))

## 9.2 Dynamic features
for (i in seq_along(data_forecast_list)) {
  data_forecast_list[[i]]$law <- 1
}

## 9.3 Forecast results
data_forecast <- predict(model_results, model_results_2, 
                         prediction_function = list(prediction_function, prediction_function_2), 
                         data = data_forecast_list)

data_forecast$DriversKilled_pred <- round(data_forecast$DriversKilled_pred, 0)
DT::datatable(head(data_forecast, 10), options = list(scrollX = TRUE))

plot(data_forecast, data_actual = data[-(1:150), ], actual_indices = dates[-(1:150)], horizons = c(1, 6, 12))

## 9.4 Forecast Error
data_error = forecastML::return_error(data_forecast,
                                       data_test = data_test,
                                       test_indices = dates[(nrow(data_train) + 1):length(dates)],
                                       metrics = c("mae", "mape", "smape", "mdape"))

data_error$error_by_horizon[, c("mae", "mape", "smape", "mdape")] = lapply(data_error$error_by_horizon[, c("mae", "mape", "smape", "mdape")], round, 1)
DT::datatable(head(data_error$error_by_horizon, 10), options = list(scrollX = TRUE)) 


# 10. Model Selection and Re-training
data_list = forecastML::create_lagged_df(data_train,
                                          outcome_col = outcome_col,
                                          type = "train",
                                          horizons = horizons,
                                          lookback = lookback,
                                          date = dates[1:nrow(data_train)],
                                          frequency = date_frequency,
                                          dynamic_features = dynamic_features
)

windows = forecastML::create_windows(data_list, window_length = 0)
plot(windows, data_list, show_labels = TRUE) 

model_results = forecastML::train_model(data_list, windows,  model_name = "LASSO", model_function)
data_results = predict(model_results, prediction_function = list(prediction_function), data = data_list)
DT::datatable(head(data_results, 10), options = list(scrollX = TRUE))
plot(data_results, type = "prediction", horizons = c(1, 6, 12)) 

## 10.1 Training error with forecastML::return_error
data_error = forecastML::return_error(data_results, metrics = c("mae", "mape", "mdape", "smape"),
                                       models = NULL)

data_error$error_global[, c("mae", "mape", "mdape", "smape")] = lapply(data_error$error_global[, c("mae", "mape", "mdape", "smape")], round, 1)
DT::datatable(head(data_error$error_global), options = list(scrollX = TRUE))

## 11. Forecast with 1 Model Per Horizon
data_forecast_list = forecastML::create_lagged_df(data_train,
                                                   outcome_col = outcome_col,
                                                   type = "forecast",
                                                   horizons = horizons,
                                                   lookback = lookback,
                                                   date = dates[1:nrow(data_train)],
                                                   frequency = date_frequency,
                                                   dynamic_features = dynamic_features
)

for (i in seq_along(data_forecast_list)) {
  data_forecast_list[[i]]$law <- 1
}

data_forecast <- predict(model_results, prediction_function = list(prediction_function), data = data_forecast_list)

plot(data_forecast,
     data_actual = data[-(1:150), ],
     actual_indices = dates[-(1:150)])

## 11.1 Forecast error with forecastML::return_error
data_error = forecastML::return_error(data_forecast, data_test = data_test, 
                                       test_indices = dates[(nrow(data_train) + 1):nrow(data)],
                                       metrics = c("mae", "mape", "mdape", "smape"))

data_error$error_by_horizon[, c("mae", "mape", "mdape", "smape")] = lapply(data_error$error_by_horizon[, c("mae", "mape", "mdape", "smape")], round, 1)
data_error$error_global[, c("mae", "mape", "mdape", "smape")] = lapply(data_error$error_global[, c("mae", "mape", "mdape", "smape")], round, 1)
DT::datatable(data_error$error_global, options = list(scrollX = TRUE))

## 11.2 Forecast Combination with forecastML::combine_forecasts
data_combined = forecastML::combine_forecasts(data_forecast)
data_actual = data[dates >= as.Date("1980-01-01"), ]
actual_indices = dates[dates >= as.Date("1980-01-01")]

plot(data_combined, data_actual = data_actual, actual_indices = actual_indices)
