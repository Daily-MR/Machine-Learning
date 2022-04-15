## 1. 모집단 가정(원래는 알 수 없음)
## 연속형 데이터
## 데이터 생성
set.seed(1234)
x = round(runif(500, -3, 3), 3)
e = round(rnorm(500, 0, 2), 3)
y = 0.3 - 2 * x - 0.1 * x ^ 2 + x ^ 3 + e
data = data.frame(x, y)

## test 데이터 생성
test_x = round(runif(200, -3, 3), 3)
test_e = round(rnorm(200, 0, 2), 3)
test_y = 0.3 - 2 * test_x - 0.1 * test_x^2 + test_x^3 + test_e
test = data.frame(x = test_x, y = test_y)

## plot
plot(x, y, main = 'Scatter Plot')

## training & validation data 분할
library(caret)
ind_train = createDataPartition(y = data$y, p = 0.75, list = FALSE)
training = data[ind_train, ]   # training data
val = data[-ind_train, ]       # validation data

nrow(training)  # training 376개
nrow(val)       # validation 124개

## model 적합
## 선형회귀
model1 = lm(y ~ x, data =training)
summary(model1)
beta1 = model1$coefficients  # model1의 회귀계수 beta1

## 2차항
model2 = lm(y ~ x + I(x^2), data = training)
summary(model2)
beta2 = model2$coefficients  # model2의 회귀계수 beta2

## 3차항
model3 = lm(y ~ x + I(x^2) + I(x^3), data = training)
summary(model3)
beta3 = model3$coefficients  # model3의 회귀계수 beta3

## 적합된 모형들의 plot
plot(training$x, training$y)
curve(0.3 - 2 * x - 0.1 * x^2 + x^3, from = -3, to = 3, add = T, col = "red", lwd = 2)
curve(beta1[1] + beta1[2] * x, from = -3, to = 3, add = T, col = "green3", lwd = 2)
curve(beta2[1] + beta2[2] * x + beta2[3] * x^2, from = -3, to = 3, add = T, col = 'blue', lwd = 1)
curve(beta3[1] + beta3[2] * x + beta3[3] * x^2 + beta3[4] * x^3, from = -3, to = 3, add = T, col = 'purple', lwd = 2)
legend("topleft", pch = 16, legend = c("가정", "1차", "2차", "3차"), col = c("red", "green3", "blue", "purple"))

## 예측
## model1
pred1 = predict(model1, newdata = val)
plot(val$y, pred1, xlim = c(-20, 20), ylim = c(-20, 20), ylab = "prediction", xlab = "validation", main = '1차 다항회귀')
abline(a = 0, b = 1, col = "red")
error1 = val$y - pred1
plot(val$y, error1, main = "model1's residual plot")
abline(a = 0, b = 0, col = "red")

## model2
pred2 = predict(model2, newdata = val)
plot(val$y, pred2, xlim = c(-20, 20), ylim = c(-20, 20), ylab = "prediction", xlab = "validation", main = '2차 다항회귀')
abline(a = 0, b = 1, col = "red")
error2 = val$y - pred2
plot(val$y, error2, main = "model2's residual plot")
abline(a = 0, b = 0, col = "red")

## model3
pred3 = predict(model3, newdata = val)
plot(val$y, pred3, xlim = c(-20, 20), ylim = c(-20, 20), ylab = "prediction", xlab = "validation", main = '3차 다항회귀')
abline(a = 0, b = 1, col = "red")
error3 = val$y - pred3
plot(val$y, error3, main = "model3's residual plot")
abline(a = 0, b = 0, col = "red")

## 평가기준
## accuracy{forecast}
library(forecast)
accuracy(pred1, val$y)
accuracy(pred2, val$y)
accuracy(pred3, val$y)  # 3차항의 모형으로 채택

## test data에 적용 + 결과확인
## 3차항 모형으로 training + val data에 적합
model = lm(y ~ x + I(x^2) + I(x^3), data = data)
summary(model)

## 최종 3차항 모델로 예측
pred = predict(model, newdata = test)
plot(test$y, pred, xlim = c(-20, 20), ylim = c(-20, 20), ylab = "prediction", xlab = "validation", main = "3차항모형")
abline(a = 0, b = 1, col = "red")
error = test$y - pred
plot(test$y, error)
accuracy(pred, test$y)

## 범주형 데이터
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
my.test = mydata[-my.ind, ]  

sum(table(mydata$admit))  
nrow(my.ind)
nrow(my.training)
nrow(my.test)

## logistic regression 적합
glm.fit = glm(admit ~., data = my.training, family = "binomial")
summary(glm.fit)

## 예측
glm.pred = predict(glm.fit, newdata = my.test, type = "response")
glm.predicton = as.factor(round(glm.pred))

## confusion matrix로 결과 비교
confusionMatrix(my.test$admit, glm.predicton)

## ROC curve
library(ROCR)
pr = prediction(glm.pred, my.test$admit)
prf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
