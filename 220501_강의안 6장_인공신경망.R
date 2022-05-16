## 범주형 데이터 ##
# 데이터 가져오기
library(caret)
mydata = read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

# 변수 factor화
mydata$admit = as.factor(mydata$admit)
mydata$rank = as.factor(mydata$rank)

# nnet 성능 향상을 위한 데이터 정규화
nnet.data = data.frame(mydata[, c("admit", "rank")], scale(mydata[, c("gre", "gpa")]))
summary(nnet.data)

# 데이터 분할
set.seed(1234)
nnet.ind = createDataPartition(y = nnet.data$admit, p = 0.75, list = FALSE)
nnet.training = nnet.data[nnet.ind, ]
nnet.val = nnet.data[-nnet.ind, ]

# nnet 적합
library(nnet)
set.seed(123)
nnet.fit2 = nnet(admit ~ ., data = nnet.training, size = 2)
set.seed(123)
nnet.fit4 = nnet(admit ~ ., data = nnet.training, size = 4)
set.seed(123)
nnet.fit6 = nnet(admit ~ ., data = nnet.training, size = 6)
set.seed(123)
nnet.fit10 = nnet(admit ~ ., data = nnet.training, size = 10)

# nnet 적합 그래프 
library(devtools)
source_gist("5086859")
plot.nnet(nnet.fit2)
plot.nnet(nnet.fit10)
## weight 선의 굵기로 상대적 영향력 비교 가능

# ANN 적합 모형을 통한 예측
nnet.pred2 = as.factor(predict(nnet.fit2, newdata = nnet.val, type = "class"))
nnet.pred4 = as.factor(predict(nnet.fit4, newdata = nnet.val, type = "class"))
nnet.pred6 = as.factor(predict(nnet.fit6, newdata = nnet.val, type = "class"))
nnet.pred10 = as.factor(predict(nnet.fit10, newdata = nnet.val, type = "class"))
confusionMatrix(nnet.val$admit, nnet.pred2)
confusionMatrix(nnet.val$admit, nnet.pred4)
confusionMatrix(nnet.val$admit, nnet.pred6)
confusionMatrix(nnet.val$admit, nnet.pred10)

## 연속형 데이터 ##
# 데이터 가져오기
reg.data = read.csv("https://stats.idre.ucla.edu/wp-content/uploads/2019/02/elemapi2v2.csv")

# nnet 성능 향상을 위한 정규화
normalize = function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}
scale.data = data.frame(lapply(reg.data, normalize))

# 데이터 분할
scale.ind = createDataPartition(y = scale.data$api00, p = 0.75, list = FALSE)
scale.training = scale.data[scale.ind, ]
scale.val = scale.data[-scale.ind, ]

# nnet 적합
library(nnet)
set.seed(123)
nnet.fit2 = nnet(api00 ~ enroll + meals + full, data = scale.training, size = 2)
set.seed(123)
nnet.fit4 = nnet(api00 ~ enroll + meals + full, data = scale.training, size = 4)
set.seed(123)
nnet.fit6 = nnet(api00 ~ enroll + meals + full, data = scale.training, size = 6)
set.seed(123)
nnet.fit10 = nnet(api00 ~ enroll + meals + full, data = scale.training, size = 10)

# nnet 적합 그래프 생성
library(devtools)
source_gist("5086859")
plot.nnet(nnet.fit2)
plot.nnet(nnet.fit10)

# 예측 및 비교를 위한 역변환
nnet.pred2 = predict(nnet.fit2, newdata = scale.val) * (max(reg.data$api00) - min(reg.data$api00)) + min(reg.data$api00)
nnet.pred4 = predict(nnet.fit4, newdata = scale.val) * (max(reg.data$api00) - min(reg.data$api00)) + min(reg.data$api00)
nnet.pred6 = predict(nnet.fit6, newdata = scale.val) * (max(reg.data$api00) - min(reg.data$api00)) + min(reg.data$api00)
nnet.pred10 = predict(nnet.fit10, newdata = scale.val) * (max(reg.data$api00) - min(reg.data$api00)) + min(reg.data$api00)
actual = scale.val$api00 * (max(reg.data$api00) - min(reg.data$api00)) + min(reg.data$api00)

# 지표를 통해 결과 비교
library(MLmetrics)
library(forecast)
accuracy(ts(nnet.pred2), actual)
accuracy(ts(nnet.pred4), actual)
accuracy(ts(nnet.pred6), actual)
accuracy(ts(nnet.pred10), actual)
