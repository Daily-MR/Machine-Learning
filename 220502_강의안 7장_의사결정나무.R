## 범주형 데이터
library(caret)
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

## tree 적합
library(rpart)
set.seed(123)
rpart.fit1 = rpart(admit ~ ., data = my.training, control = list(maxdepth = 1))
set.seed(123)
rpart.fit2 = rpart(admit ~ ., data = my.training, control = list(maxdepth = 2))
set.seed(123)
rpart.fit3 = rpart(admit ~ ., data = my.training, control = list(maxdepth = 3))
set.seed(123)
rpart.fit5 = rpart(admit ~ ., data = my.training, control = list(maxdepth = 5))

## tree 적합 그래프 생성
library(rattle)
fancyRpartPlot(rpart.fit2)
fancyRpartPlot(rpart.fit5)

## 그래프를 그리기 위한 데이터 정렬
train = my.training[order(my.training[, "admit"]), ]
rpart.fit5 = rpart(admit ~ gre + gpa, data = train, control = list(maxdepth = 5))
fancyRpartPlot(rpart.fit5)

## training 자료 그래프 생성
px = seq(200, 800, 5)
py = seq(2, 4, 0.05)
pgrid = expand.grid(px, py)
names(pgrid) = names(train)[c(2, 3)]
rpart.fit1 = rpart(admit ~ ., data = my.training, control = list(maxdepth = 1))

plot(c(), type = "n", xlim = c(200, 800), ylim = c(2, 4), xlab = "", ylab = "")
par(new = T)
plot(train[, c(2, 3)], col = c(rep("blue", 205), rep("red", 96)), xlim = c(200, 800),
     ylim = c(2, 4), pch = 19, cex = 1.5, axes = F)

## 의사결정나무 모형 적합 결과 추가
par(new = T)
contour(px, py, array(predict(rpart.fit5, newdata = pgrid), dim = c(length(px), length(py))),
        xlim = c(200, 800), ylim = c(2, 4), col = "purple", lwd = 5, drawlabels = T, levels = 0.5)

## 의사결정나무 적합 모형을 통한 예측
rpart.pred1 = as.factor(predict(rpart.fit1, newdata = my.val, type = "class"))
rpart.pred2 = as.factor(predict(rpart.fit2, newdata = my.val, type = "class"))
rpart.pred3 = as.factor(predict(rpart.fit3, newdata = my.val, type = "class"))
rpart.pred5 = as.factor(predict(rpart.fit5, newdata = my.val, type = "class"))

## confusion matrix를 통한 예측 결과 비교
confusionMatrix(my.val$admit, rpart.pred1)
confusionMatrix(my.val$admit, rpart.pred2)
confusionMatrix(my.val$admit, rpart.pred3)
confusionMatrix(my.val$admit, rpart.pred5)

## 연속형 데이터
## regression 분석을 위한 data 생성
reg.data = read.csv("https://stats.idre.ucla.edu/wp-content/uploads/2019/02/elemapi2v2.csv")
summary(reg.data)

regdata.ind = createDataPartition(y = reg.data$api00, p = 0.75, list = FALSE)
reg.training = reg.data[regdata.ind, ]
reg.val = reg.data[-regdata.ind, ]

## regression tree 적합
set.seed(123)
rpart.fit1 = rpart(api00 ~ enroll + full + col_grad + emer, data = reg.training, 
                   control = list(maxdepth = 1))
set.seed(123)
rpart.fit2 = rpart(api00 ~ enroll + full + col_grad + emer, data = reg.training, 
                   control = list(maxdepth = 2))
set.seed(123)
rpart.fit3 = rpart(api00 ~ enroll + full + col_grad + emer, data = reg.training, 
                   control = list(maxdepth = 3))
set.seed(123)
rpart.fit5 = rpart(api00 ~ enroll + full + col_grad + emer, data = reg.training, 
                   control = list(maxdepth = 5))

## 예측
rpart.pred1 = predict(rpart.fit1, newdata = reg.val)
rpart.pred2 = predict(rpart.fit2, newdata = reg.val)
rpart.pred3 = predict(rpart.fit3, newdata = reg.val)
rpart.pred5 = predict(rpart.fit5, newdata = reg.val)
actual = reg.val$api00

## 지표를 통한 결과 비교
library(forecast)
accuracy(ts(rpart.pred1), actual)
accuracy(ts(rpart.pred2), actual)
accuracy(ts(rpart.pred3), actual)
accuracy(ts(rpart.pred5), actual)
