## EXAMPLE 1
library(MASS)
library(glmnet)

# Generate data
set.seed(19875)
n = 1000; p = 5000; real_p = 15
x = matrix(rnorm(n * p), nrow = n, ncol = p)
y = apply(x[, 1:real_p], 1, sum) + rnorm(n)

# Split data into train (2/3) and test(1/3) sets
train_rows = sample(1:n, .66*n)
x.train = x[train_rows, ]
x.test = x[-train_rows, ]
y.train = y[train_rows]
y.test = y[-train_rows]

# Fit models
fit.lasso = glmnet(x.train, y.train, family = "gaussian", alpha = 1)
fit.ridge = glmnet(x.train, y.train, family = "gaussian", alpha = 0)
fit.elnet = glmnet(x.train, y.train, family = "gaussian", alpha = .5)

# 10-fold cross validation
for (i in 0:10){
  assign(paste("fit", i, sep = ""), cv.glmnet(x.train, y.train, type.measure = "mse",
                                              alpha = i/10, family = "gaussian"))
}

# plot solution paths
par(mfrow = c(3, 2))
plot(fit.lasso, xvar = "lambda")
plot(fit10, main = "LASSO")
plot(fit.ridge, xvar = "lambda")
plot(fit0, main = "Ridge")
plot(fit.elnet, xvar = "lambda")
plot(fit5, main = "Elastic Net")

# MSE on test set
yhat0 = predict(fit0, s = fit0$lambda.1se, newx = x.test)
yhat1 = predict(fit1, s = fit1$lambda.1se, newx = x.test)
yhat2 = predict(fit2, s = fit2$lambda.1se, newx = x.test)
yhat3 = predict(fit3, s = fit3$lambda.1se, newx = x.test)
yhat4 = predict(fit4, s = fit4$lambda.1se, newx = x.test)
yhat5 = predict(fit5, s = fit5$lambda.1se, newx = x.test)
yhat6 = predict(fit6, s = fit6$lambda.1se, newx = x.test)
yhat7 = predict(fit7, s = fit7$lambda.1se, newx = x.test)
yhat8 = predict(fit8, s = fit8$lambda.1se, newx = x.test)
yhat9 = predict(fit9, s = fit9$lambda.1se, newx = x.test)
yhat10 = predict(fit10, s = fit10$lambda.1se, newx = x.test)

mse0 = mean((y.test - yhat0)^2)
mse1 = mean((y.test - yhat1)^2)
mse2 = mean((y.test - yhat2)^2)
mse3 = mean((y.test - yhat3)^2)
mse4 = mean((y.test - yhat4)^2)
mse5 = mean((y.test - yhat5)^2)
mse6 = mean((y.test - yhat6)^2)
mse7 = mean((y.test - yhat7)^2)
mse8 = mean((y.test - yhat8)^2)
mse9 = mean((y.test - yhat9)^2)
mse10 = mean((y.test - yhat10)^2)
## > Lasso(mse10; alpha = 1) is the winner

## EXAMPLE 2
library(MASS)
library(glmnet)

# Generate data
set.seed(19874)
n = 1000; p = 5000; real_p = 1500
x = matrix(rnorm(n * p), nrow = n, ncol = p)
y = apply(x[, 1:real_p], 1, sum) + rnorm(n)

# Split data into train (2/3) and test(1/3) sets
train_rows = sample(1:n, .66*n)
x.train = x[train_rows, ]
x.test = x[-train_rows, ]
y.train = y[train_rows]
y.test = y[-train_rows]

# Fit models
fit.lasso = glmnet(x.train, y.train, family = "gaussian", alpha = 1)
fit.ridge = glmnet(x.train, y.train, family = "gaussian", alpha = 0)
fit.elnet = glmnet(x.train, y.train, family = "gaussian", alpha = .5)

# 10-fold cross validation
fit.lasso.cv = cv.glmnet(x.train, y.train, type.measure = "mse", alphs = 1, family = "gaussian")
fit.ridge.cv = cv.glmnet(x.train, y.train, type.measure = "mse", alphs = 0, family = "gaussian")
fit.elnet.cv = cv.glmnet(x.train, y.train, type.measure = "mse", alphs = .5, family = "gaussian")

for (i in 0:10){
  assign(paste("fit", i, sep = ""), cv.glmnet(x.train, y.train, type.measure = "mse",
                                              alpha = i/10, family = "gaussian"))
}

# plot solution paths
par(mfrow = c(3, 2))
plot(fit.lasso, xvar = "lambda")
plot(fit10, main = "LASSO")
plot(fit.ridge, xvar = "lambda")
plot(fit0, main = "Ridge")
plot(fit.elnet, xvar = "lambda")
plot(fit5, main = "Elastic Net")

# MSE on test set
yhat0 = predict(fit0, s = fit0$lambda.1se, newx = x.test)
yhat1 = predict(fit1, s = fit1$lambda.1se, newx = x.test)
yhat2 = predict(fit2, s = fit2$lambda.1se, newx = x.test)
yhat3 = predict(fit3, s = fit3$lambda.1se, newx = x.test)
yhat4 = predict(fit4, s = fit4$lambda.1se, newx = x.test)
yhat5 = predict(fit5, s = fit5$lambda.1se, newx = x.test)
yhat6 = predict(fit6, s = fit6$lambda.1se, newx = x.test)
yhat7 = predict(fit7, s = fit7$lambda.1se, newx = x.test)
yhat8 = predict(fit8, s = fit8$lambda.1se, newx = x.test)
yhat9 = predict(fit9, s = fit9$lambda.1se, newx = x.test)
yhat10 = predict(fit10, s = fit10$lambda.1se, newx = x.test)

mse0 = mean((y.test - yhat0)^2)
mse1 = mean((y.test - yhat1)^2)
mse2 = mean((y.test - yhat2)^2)
mse3 = mean((y.test - yhat3)^2)
mse4 = mean((y.test - yhat4)^2)
mse5 = mean((y.test - yhat5)^2)
mse6 = mean((y.test - yhat6)^2)
mse7 = mean((y.test - yhat7)^2)
mse8 = mean((y.test - yhat8)^2)
mse9 = mean((y.test - yhat9)^2)
mse10 = mean((y.test - yhat10)^2)
## Ridge(mse0; alpha = 0) is the winner


## EXAMPLE 3
set.seed(19873)
n = 100; p = 50
CovMatrix = outer(1:0, 1:0, function(x, y){.7^abs(x-y)})
x = mvrnorm(n, rep(0, p), CovMatrix)  # error
y = 10 * apply(x[, 1:2], 1, sum) +
  5 * apply(x[, 3:4], 1, sum) +
  apply(x[, 5:14], 1, sum) +
  rnorm(n)

# Split data into train (2/3) and test(1/3) sets
train_rows = sample(1:n, .66*n)
x.train = x[train_rows, ]
x.test = x[-train_rows, ]
y.train = y[train_rows]
y.test = y[-train_rows]

# Fit models
fit.lasso = glmnet(x.train, y.train, family = "gaussian", alpha = 1)
fit.ridge = glmnet(x.train, y.train, family = "gaussian", alpha = 0)
fit.elnet = glmnet(x.train, y.train, family = "gaussian", alpha = .5)

# 10-fold cross validation
fit.lasso.cv = cv.glmnet(x.train, y.train, type.measure = "mse", alphs = 1, family = "gaussian")
fit.ridge.cv = cv.glmnet(x.train, y.train, type.measure = "mse", alphs = 0, family = "gaussian")
fit.elnet.cv = cv.glmnet(x.train, y.train, type.measure = "mse", alphs = .5, family = "gaussian")

for (i in 0:10){
  assign(paste("fit", i, sep = ""), cv.glmnet(x.train, y.train, type.measure = "mse",
                                              alpha = i/10, family = "gaussian"))
}

# plot solution paths
par(mfrow = c(3, 2))
plot(fit.lasso, xvar = "lambda")
plot(fit10, main = "LASSO")
plot(fit.ridge, xvar = "lambda")
plot(fit0, main = "Ridge")
plot(fit.elnet, xvar = "lambda")
plot(fit5, main = "Elastic Net")

# MSE on test set
yhat0 = predict(fit0, s = fit0$lambda.1se, newx = x.test)
yhat1 = predict(fit1, s = fit1$lambda.1se, newx = x.test)
yhat2 = predict(fit2, s = fit2$lambda.1se, newx = x.test)
yhat3 = predict(fit3, s = fit3$lambda.1se, newx = x.test)
yhat4 = predict(fit4, s = fit4$lambda.1se, newx = x.test)
yhat5 = predict(fit5, s = fit5$lambda.1se, newx = x.test)
yhat6 = predict(fit6, s = fit6$lambda.1se, newx = x.test)
yhat7 = predict(fit7, s = fit7$lambda.1se, newx = x.test)
yhat8 = predict(fit8, s = fit8$lambda.1se, newx = x.test)
yhat9 = predict(fit9, s = fit9$lambda.1se, newx = x.test)
yhat10 = predict(fit10, s = fit10$lambda.1se, newx = x.test)

mse0 = mean((y.test - yhat0)^2)
mse1 = mean((y.test - yhat1)^2)
mse2 = mean((y.test - yhat2)^2)
mse3 = mean((y.test - yhat3)^2)
mse4 = mean((y.test - yhat4)^2)
mse5 = mean((y.test - yhat5)^2)
mse6 = mean((y.test - yhat6)^2)
mse7 = mean((y.test - yhat7)^2)
mse8 = mean((y.test - yhat8)^2)
mse9 = mean((y.test - yhat9)^2)
mse10 = mean((y.test - yhat10)^2)
## --- is the winner

## ANOTHER example -- 과제(22.05.10)
library(tidyverse)
library(caret)
library(glmnet)
data("Boston", package = "MASS")
help(Boston)
set.seed(1212)

# split the data into training and test data
sample_size = floor(0.75 * nrow(Boston))
training_index = sample(seq_len(nrow(Boston)), size = sample_size)
train = Boston[training_index, ]
test = Boston[-training_index, ]

# predictor
x = model.matrix(medv ~ ., train)[, -1]
# response
y = train$medv

cor(x)
ols = lm(y ~ x)
summary(ols)

# Fit Ridge model, CV
model.ridge0 = glmnet(x, y, alpha = 0)
coef(model.ridge0)
plot(model.ridge0, xvar = "lambda")
plot(model.ridge0, main = "Ridge")

cv.r = cv.glmnet(x, y, alpha = 0)
cv.r$lambda.min
model.ridge = glmnet(x, y, alpha = 0, lambda = cv.r$lambda.min)
coef(model.ridge)

# test data
x.test.ridge = model.matrix(medv ~ ., test)[, -1]
predictions.ridge = model.ridge %>% predict(x.test.ridge) %>% as.vector()
data.frame(
  RMSE.r = RMSE(predictions.ridge, test$medv),
  Rsquare.r = R2(predictions.ridge, test$medv))

# Fit Lasso regression model, CV
model.lasso0 = glmnet(x, y, alpha = 1)
coef(model.lasso0)
plot(model.lasso0, xvar = "lambda")
plot(model.lasso0, main = "LASSO")

cv.l = cv.glmnet(x, y, alpha = 1)
cv.l$lambda.min
model.lasso = glmnet(x, y, alpha = 1, lambda = cv.l$lambda.min)
coef(model.lasso)

# test data
x.test.lasso = model.matrix(medv ~ ., test)[, -1]
predictions.lasso = model.lasso %>% predict(x.test.lasso) %>% as.vector()
data.frame(
  RMSE.l = RMSE(predictions.lasso, test$medv),
  Rsquare.l = R2(predictions.lasso, test$medv))

# Fit Elastic Net model, CV
model.net = train(
  medv ~ ., data = train, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10)
model.net$bestTune
coef(model.net$finalModel, model.net$bestTune$lambda)

# test data
x.test.net = model.matrix(medv ~ ., test)[, -1]
predictions.net = model.net %>% predict(x.test.net) 
data.frame(
  RMSE.net = RMSE(predictions.net, test$medv),
  Rsquare.net = R2(predictions.net, test$medv))
