## 데이터 생성
set.seed(1234)
x1 = rnorm(400, 0, 1)
x2 = rnorm(400, 0, 1)
x3 = 2 * x1 + rnorm(400, 1, 1)
e = rnorm(400, 0, 1)

cor(x1, x2)
cor(x1, x3)
plot(x1, x2)
plot(x1, x3)

## RSS matrix function
RSS = function(model){
  b1 = model$coefficients[2]
  b1 = seq(b1 - 3, b1 + 3, length.out = 100)
  b2 = model$coefficients[3]
  b2 = seq(b2 - 3, b2+ 3, length.out = 100)
  
  rss = c()
  
  for (i in b1){
    for (j in b2){
      pred = model$coefficients[1] + i * model$model[, 2] + j * model$model[, 3]
      
      if (length(model$coefficients) == 4){
        pred = model$coefficients[1] + i * model$model[, 2] + j * model$model[, 3] +
          model$coefficients[4] * model$model[, 2] * model$model[, 3]
      }
      tmp.rss = sum((y - pred)^2) / 100
      rss = c(rss, tmp.rss)
    }
    rss = matrix(rss, ncol = 100, nrow = 100, byrow = T)
    return(rss)
  }
}

## 두 변수가 독립일 때 SSE[linear model1(x1, x2)]
## 모집단 가정
y = 2 * x1 + 4 * x2 + e
lm.12 = lm(y ~ x1 + x2)
summary(lm.12)

b1.12 = lm.12$coefficients[2]
b1.12 = seq(b1.12-3, b1.12+3, length.out = 100)
b2.12 = lm.12$coefficients[3]
b2.12 = seq(b2.12-3, b2.12+3, length.out = 100)

contour(b1.12, b2.12, RSS(lm.12))

## 두 변수가 독립이 아닐 때 SSE[linear model1(x1, x3)]
## 모집단 가정
y = 2 * x1 + 4 * x3 + e
lm.13 = lm(y ~ x1 + x3)
summary(lm.13)

b1.13 = lm.13$coefficients[2]
b1.13 = seq(b1.13-5, b1.13+5, length.out = 100)
b2.13 = lm.13$coefficients[3]
b2.13 = seq(b2.13-5, b2.13+5, length.out = 100)

contour(b1.13, b2.13, RSS(lm.13), levels = c(20, 40, 50, 60))

## 두 변수가 독립일 때 SSE[linear model1(x1, x2, 교호효과)]
## 모집단 가정(교호효과 o)
y = 2 * x1 + 4 * x2 + 3 * x1 * x2 + e
lm.1212 = lm(y ~ x1 + x2 + x1 * x2)
summary(lm.1212)

b1.1212 = lm.1212$coefficients[2]
b1.1212 = seq(b1.1212-3, b1.1212+3, length.out = 100)
b2.1212 = lm.1212$coefficients[3]
b2.1212 = seq(b2.1212-3, b2.1212+3, length.out = 100)

contour(b1.1212, b2.1212, RSS(lm.1212))

## 두 변수가 독립이 아닐 때 SSE[linear model1(x1, x3, 교호효과)]
## 모집단 가정(교호효과 o)
y = 2 * x1 + 4 * x3  + 3 * x1 * x3 + e
lm.1313 = lm(y ~ x1 + x3 + x1 * x3)
summary(lm.1313)

b1.1313 = lm.1313$coefficients[2]
b1.1313 = seq(b1.1313-5, b1.1313+5, length.out = 100)
b2.1313 = lm.1313$coefficients[3]
b2.1313 = seq(b2.1313-5, b2.1313+5, length.out = 100)

contour(b1.1313, b2.1313, RSS(lm.1313), levels = c(20, 40, 50, 60))

## 연속형 데이터
## 데이터 생성 [10개]
set.seed(1234)
x1 = rnorm(10, 0, 1)
x2 = rnorm(10, 0, 1)
x3 = 2 * x1 + rnorm(10, 1, 1)
e = rnorm(10, 0, 1)

cor(x1, x2)
cor(x1, x3)
plot(x1, x2)
plot(x1, x3)

## 두 변수가 독립일 때 SSE[linear model1(x1, x2)]
## 모집단 가정
y = 2 * x1 + 4 * x2 + e
lm.12 = lm(y ~ x1 + x2)
summary(lm.12)

b1.12 = lm.12$coefficients[2]
b1.12 = seq(b1.12-3, b1.12+3, length.out = 100)
b2.12 = lm.12$coefficients[3]
b2.12 = seq(b2.12-3, b2.12+3, length.out = 100)

contour(b1.12, b2.12, RSS(lm.12))

## 두 변수가 독립이 아닐 때 SSE[linear model1(x1, x3)]
## 모집단 가정
y = 2 * x1 + 4 * x3 + e
lm.13 = lm(y ~ x1 + x3)
summary(lm.13)

b1.13 = lm.13$coefficients[2]
b1.13 = seq(b1.13-5, b1.13+5, length.out = 100)
b2.13 = lm.13$coefficients[3]
b2.13 = seq(b2.13-3, b2.13+3, length.out = 100)

contour(b1.13, b2.13, RSS(lm.13))

## 두 변수가 독립이 아닐 때 SSE[linear model1(x1, x3, 교호효과)]
## 모집단 가정(교호효과 o)
y = 2 * x1 + 4 * x3  + 3 * x1 * x3 + e
lm.1313 = lm(y ~ x1 + x3 + x1 * x3)
summary(lm.1313)

b1.1313 = lm.1313$coefficients[2]
b1.1313 = seq(b1.1313-5, b1.1313+5, length.out = 100)
b2.1313 = lm.1313$coefficients[3]
b2.1313 = seq(b2.1313-3, b2.1313+3, length.out = 100)

contour(b1.1313, b2.1313, RSS(lm.1313))

## 범주형 데이터
## 데이터 가져오기
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

## logistic regression 적합
glm.fit = glm(admit ~ ., data = my.training, family = "binomial")
summary(glm.fit)

## 예측
glm.pred = predict(glm.fit, newdata = my.val, type = "response")
glm.prediction = as.factor(round(glm.pred))

## confusion matrix로 결과 비교
confusionMatrix(my.val$admit, glm.prediction)

## ROC curve
library(ROCR)
pr = prediction(glm.pred, my.val$admit)
prf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)