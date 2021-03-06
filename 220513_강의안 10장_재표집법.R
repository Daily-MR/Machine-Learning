## ensemble 및 cross-validation
## 범주형 데이터
library(caret)
mydata = read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(mydata)
str(mydata)

## 변수 factor화
mydata$admit = as.factor(mydata$admit)
mydata$rank = as.factor(mydata$rank)
summary(mydata)

## 데이터 분할
set.seed(123)
my.ind = createDataPartition(y = mydata$admit,p = 0.75, list = FALSE)
my.training = mydata[my.ind, ]
my.test = mydata[-my.ind, ]

## bagging & cross-validation
cctrl1 = trainControl(method = "cv", number = 5)
set.seed(123)
model.bag = train(admit ~ ., data = my.training, trControl = cctrl1, method = "bag",
                  tuneGrid = data.frame(vars = seq(5, 20, by = 5)),
                  bagControl = bagControl(fit = ldaBag$fit, predict = ldaBag$pred, aggregate = ldaBag$aggregate))

## gradient boosting & cross-validation
cctrl1 = trainControl(method = "cv", number = 5)
set.seed(123)
model.gbm = train(admit ~ ., data = my.training, trControl = cctrl1, method = "gbm", tuneLength = 3)

## adaboost & cross-validation
cctrl1 = trainControl(method = "cv", number = 5)
set.seed(123)
model.ada = train(admit ~ ., data = my.training, trControl = cctrl1, method = "ada", tuneLength = 3)

## random forest & cross-validation
cctrl1 = trainControl(method = "cv", number = 5)
set.seed(123)
model.rf = train(admit ~ ., data = my.training, trControl = cctrl1, method = "rf", tuneLength = 3)

## 예측 및 평가 측도 계산
bag.pred = predict(model.bag, newdata = my.test)
gbm.pred = predict(model.gbm, newdata = my.test)
ada.pred = predict(model.ada, newdata = my.test)
rf.pred = predict(model.rf, newdata = my.test)

confusionMatrix(my.test$admit, bag.pred)
confusionMatrix(my.test$admit, gbm.pred)
confusionMatrix(my.test$admit, ada.pred)
confusionMatrix(my.test$admit, rf.pred)

## 연속형 데이터
## regression 분석을 위한 data 생성
reg.data = read.csv("https://stats.idre.ucla.edu/wp-content/uploads/2019/02/elemapi2v2.csv")
regdata.ind = createDataPartition(y = reg.data$api00, p = 0.75, list = FALSE)
reg.training = reg.data[regdata.ind, ]
reg.test = reg.data[-regdata.ind, ]
summary(reg.training)

## bagging & cross-validation
library(party)
cctrl1 = trainControl(method = "cv", number = 5)
model.bagr = train(api00 ~ enroll + full + col_grad + emer, data = reg.training, trControl = cctrl1, 
                   B = 100, method = "bag", tuneGrid = expand.grid(vars = c(10, 15, 20)),
                  bagControl = bagControl(fit = ctreeBag$fit, predict = ctreeBag$pred, aggregate = ctreeBag$aggregate))

## gradient boosting & cross-validation
cctrl1 = trainControl(method = "cv", number = 5)
set.seed(123)
model.gbmr = train(api00 ~ enroll + full + col_grad + emer, data = reg.training,
                   trControl = cctrl1, method = "gbm", tuneLength = 3)

## adaboost는 caret 패키지에서 regression 불가
## adaboost & cross-validation
#cctrl1 = trainControl(method = "cv", number = 5)
#set.seed(123)
#model.adar = train(api00 ~ enroll + full + col_grad + emer, data = reg.training,
#                   trControl = cctrl1, method = "ada", tuneLength = 3)

## random forest & cross-validation
cctrl1 = trainControl(method = "cv", number = 5)
set.seed(123)
model.rfr = train(api00 ~ enroll + full + col_grad + emer, data = reg.training,
                  trControl = cctrl1, method = "rf", tuneLength = 3)

## 예측 및 평가 측도 계산
bagr.pred = predict(model.bagr, newdata = reg.test)
gbmr.pred = predict(model.gbmr, newdata = reg.test)
# adar.pred = predict(model.adar, newdata = reg.test)
rfr.pred = predict(model.rfr, newdata = reg.test)

library(forecast)
accuracy(ts(bagr.pred), reg.test$api00)
accuracy(ts(gbmr.pred), reg.test$api00)
# accuracy(ts(adar.pred), reg.test$api00)
accuracy(ts(rfr.pred), reg.test$api00)
