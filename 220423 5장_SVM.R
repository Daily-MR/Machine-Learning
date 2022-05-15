## 데이터 준비
mydata = read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
mydata$admit = as.factor(mydata$admit)
mydata$rank = as.factor(mydata$rank)

set.seed(123)
library(caret)
my.ind = createDataPartition(y = mydata$admit, p = 0.75, list = FALSE)
my.training = mydata[my.ind, ]
my.val = mydata[-my.ind, ]

## SVM 적합
library(e1071)
set.seed(123)
svm.fit1 = svm(admit ~ ., data = my.training, cost = 1)
set.seed(123)
svm.fit5 = svm(admit ~ ., data = my.training, cost = 5)
set.seed(123)
svm.fit10 = svm(admit ~ ., data = my.training, cost = 10)
set.seed(123)
svm.fit100 = svm(admit ~ ., data = my.training, cost = 100)
plot(svm.fit1, my.training, gre ~ gpa, slice = list(gre = 3, gpa = 4))

## SVM 적합 모형을 통한 예측
svm.pred1 = predict(svm.fit1, newdata = my.val, type= "response")
svm.pred5 = predict(svm.fit5, newdata = my.val, type= "response")
svm.pred10 = predict(svm.fit10, newdata = my.val, type= "response")
svm.pred100 = predict(svm.fit100, newdata = my.val, type= "response")

## confustion matrix를 통한 예측 결과 비교
confusionMatrix(my.val$admit, svm.pred1)
confusionMatrix(my.val$admit, svm.pred5)
confusionMatrix(my.val$admit, svm.pred10)
confusionMatrix(my.val$admit, svm.pred100)
