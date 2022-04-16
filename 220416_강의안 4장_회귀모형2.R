## 1. 모집단 가정(원래는 알 수 없음)
library(caret)
library(ISLR)
library(gam)
data(Wage)
str(Wage)
attach(Wage)

## data
set.seed(1234)
x = round(runif(500, -3, 3), 3)
e = round(rnorm(500, 0, 2), 3)
y = 0.3 - 2 * x - 0.1 * x^2 + x^3 + e
data = data.frame(x, y)

## test data
test_x = round(runif(200, -3, 3), 3)
test_e = round(rnorm(200, 0, 2), 3)
test_y = 0.3 - 2 * test_x - 0.1 * test_x^2 + test_x^3 + test_e
test = data.frame(x = test_x, y = test_y)

## x, y plot
plot(x, y)

## index로 분할 & split training, validation data
ind_train = createDataPartition(y = data$y, p = 0.75, list = FALSE)
training = data[ind_train, ]   # training data
val = data[-ind_train, ]       # validation data

nrow(training)
nrow(val)

## 다항회귀 ##
fit1 = lm(y ~ poly(x, 3, raw = T), data = training)
summary(fit1)

fit2 = lm(y ~ x + I(x^2) + I(x^3), data = training)
summary(fit2)

## 표준화기저함수 ##
fit3 = lm(y ~ scale(x), data = training)
summary(fit3)

## 일반화 가법 모형 ##
gam.m3 = gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)
gam1 = gam(y ~ s(x, 4), data = training)
summary(gam1)

gam2 = lm(y ~ ns(x, 3), data = training)
summary(gam2)

## prediction
## 다항회귀 ##
pred1 = predict(fit1, newdata = val, se = T)
plot(val$x, val$y, cex = 0.5, col = "darkgrey")
list1 = data.frame(x = val$x, y = pred1$fit)
list1 = list1[order(list1$x), ]
lines(list1$x, list1$y, col = "black")

## 기저함수 ##
pred2 = predict(fit3, newdata = val, se = T)
list2 = data.frame(x = val$x, y = pred2$fit)
list2 = list2[order(list2$x), ]
lines(list2$x, list2$y, col = "red")

## 일반화 가법모형 ##
pred3 = predict(gam1, newdata = val)
list3 = data.frame(x = val$x, y = pred3)
list3 = list3[order(list3$x), ]
lines(list3$x, list3$y, col = "green")

pred4 = predict(gam2, newdata = val)
list4 = data.frame(x = val$x, y = pred4)
list4 = list4[order(list4$x), ]
lines(list4$x, list4$y, col = "blue")

## example data에 적용(Wage)
## training & validatioin data 분할
my.ind = createDataPartition(y = Wage$wage, p = 0.75, list = FALSE)
Wage.tr = Wage[my.ind, ]   # training data
Wage.val = Wage[-my.ind, ]       # validation data

## (age + age^2 + age^3 + age^4)
fit11 = lm(wage ~ poly(age, 4), data = Wage.tr)
summary(fit11)
fit12 = lm(wage ~ poly(age, 4, raw = T), data = Wage.tr)
summary(fit12)
fit13 = lm(wage ~ age + I(age^2) + I(age^3) + I(age^4),
           data = Wage.tr)
summary(fit13)

## predict & se 추정
agelims = range(Wage.tr$age)
age.grid = seq(from = agelims[1], to = agelims[2])
preds = predict(fit11, newdata = list(age = age.grid), se = T)
se.bands = cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

## plot
par(mfrow = c(1, 1), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(Wage.tr$age, Wage.tr$wage, xlim = agelims, cex = 0.5, col="darkgrey")
title("Degree-4 Polynomial", outer = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

## Wage data 일반화 가법모형 ##
gam.m = gam(wage ~ s(age, 5), data = Wage.tr)

## predict
agelims = range(Wage.tr$age)
age.grid = seq(from = agelims[1], to = agelims[2])
preds.gam = predict(gam.m, newdata = list(age = age.grid))
summary(gam.m)

## plot
par(mfrow = c(1, 1), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(Wage.tr$age, Wage.tr$wage, xlim = agelims, cex = 0.5, col="darkgrey")
title("GAM", outer = T)
lines(age.grid, preds.gam, lwd = 2, col = "blue")

## validation
library(forecast)
preds.poly = predict(fit11, newdata = Wage.val)
preds.gam = predict(gam.m, newdata = Wage.val)
accuracy(preds.poly, Wage.val$y)  # poly
accuracy(preds.gam, Wage.val$y)   # gam

## 로지스틱 회귀분석
## data
x = round(runif(500, -3, 3), 3)
exp_B = exp(0.3 - 2 * x - 0.1 * x^2 + x^3)
p = exp_B / (1 + exp_B)
y = rbinom(500, 1, prob = p)
y = as.factor(y)
data = data.frame(x, y)

## plot
plot(x, y, col = y)
true = data.frame(x, p)
true = true[order(x), ]
lines(true$x, true$p+1, col = y)

## 단순 logistic
glm.m = glm(y ~ x, family = binomial, data)
pred = predict(glm.m, data, type = "response")
preds = data.frame(x, pred)
preds = preds[order(x), ]
lines(preds$x, preds$pred + 1, col = "blue")

## 다항 logistic
glm.m = glm(y ~ poly(x, 3), family = binomial, data)
pred = predict(glm.m, data, type = "response")
preds = data.frame(x, pred)
preds = preds[order(x), ]
lines(preds$x, preds$pred + 1, col = "green")

## 범주형 데이터
## data
mydata = read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(mydata)
str(mydata)

## 변수 factor화
mydata$admit = as.factor(mydata$admit)
mydata$rank = as.factor(mydata$rank)
summary(mydata)

## training & validation data 분할
library(caret)
my.ind = createDataPartition(y = mydata$admit, p = 0.75, list = FALSE)
my.training = mydata[my.ind, ]
my.val = mydata[-my.ind, ]

## polynomial logistic regressioin 적합
glm.fit = glm(admit ~ poly(gre) + poly(gpa) + rank, data = my.training, family = "binomial")
summary(glm.fit)

## predict
glm.pred = predict(glm.fit, newdata = my.val, type = "response")
glm.prediction = as.factor(round(glm.pred))

## confustion matrix
confusionMatrix(my.val$admit, glm.prediction)
