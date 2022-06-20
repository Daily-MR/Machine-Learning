# 0. 환경설치
library(keras)
library(tidyverse)
library(tensorflow)
install_tensorflow()
library(ggplot2)
library(tibble)

# 1. 데이터 불러오기
mnist_lst = dataset_mnist()
str(mnist_lst)

# 2. 탐색적 데이터 분석
# 2.1 사람손으로 쓴 손글씨 숫자
mnist_lst$train$y %>% as_tibble() %>%
  ggplot(aes(x = as.factor(value), fill = as.factor(value))) +
  geom_bar(stat = "count") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw(base_family = "AppleGothic") +
  labs(title = "필기 숫자 훈련데이터", x = "숫자", y = "빈도수", fill = "") +
  theme(legend.position = "right")

# 2.2 사람손으로 쓴 손글씨 숫자 시각화
mnist_smpl = mnist_lst$train$x %>% tbl_df() %>%
  sample_n(50)
par(mfrow = c(5, 10), mar = c(0.1, 0.1, 0.1, 0.1))

display_digit = function(input){
  m = matrix(unlist(input), nrow = 28, byrow = FALSE)
  m = t(apply(m, 2, rev))
  image(m, col = grey.colors(255), axes = FALSE)
}

for (i in 1 : 50){
  display_digit(mnist_smpl[i, ])
}

# 3. 훈련/학습 데이터 분할
train_x = mnist_lst$train$x
train_y = mnist_lst$train$y

test_x = mnist_lst$test$x
test_y = mnist_lst$test$y

# 4. 데이터 전처리
## 데이터 정규화(20 배열 > 10 배열)
train_x = array(as.numeric(train_x), dim = c(dim(train_x)[[1]], 784))
test_x = array(as.numeric(test_x), dim = c(dim(test_x)[[1]], 784))

## RGB 값을 [0, 1] 범위로 변환
train_x = train_x / 255
test_x = test_x / 255

cat(dim(train_x)[[1]], '개: 훈련표본\n')
cat(dim(test_x)[[1]], "개: 검증표본 \n")

## 종속변수 가변수 변환
train_y = to_categorical(train_y, 10)
test_y = to_categorical(test_y, 10)

# 5. 딥러닝 모형
# 5.1 모형 초기화
model = keras_model_sequential()

# 5.2 모형 아키텍쳐 구성
model %>%
  layer_dense(units = 256, activation = "relu", input_shape = c(784)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 10, activation = "softmax")
summary(model)

model %>% 
  compile(loss = "categorical_crossentropy",
          optimizer = "adam",
          metrics = c("accuracy"))

# 5.3 학습
history = model %>%
  fit(train_x, train_y, 
      epochs = 10,
      batch_size = 128, 
      callbacks = callback_tensorboard(log_dir = "logs/run_b"),
      validation_split = 0.2)
history$metrics

# 5.4 딥러닝 모형평가
## 훈련데이터 모형성능 시각화
eval_df = tibble(train_acc = history$metrics$accuracy,
                 test_acc = history$metrics$val_accuracy)
eval_df %>%
  mutate(epoch = 1:10) %>%
  pivot_longer(-epoch, names_to = "metric", values_to = "accuracy") %>%
  ggplot(aes(x = epoch, y = accuracy, color = metric)) +
  geom_line() + geom_point() + labs(x = "한세대(이폭, Epochs)", y = "정확도",
                                    title = "모형 정확도(Accuracy) 개선 추이")+
  scale_y_continuous(labels = scales::percent) + theme_bw(base_family = "NanumGothic")

plot(history, labels = TRUE)

## 검증데이터 평가점수
score = model %>% evaluate(test_x, test_y)
cat("손실오차(loss): ", scales::percent(score[[1]]), "\n")
cat("정확도(Accuracy): ", scales::percent(score[[2]]), "\n")
