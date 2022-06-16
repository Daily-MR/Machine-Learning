# data : https://www.kaggle.com/datasets/mlg-ulb/creditcardfraud
# Highly Imbalanced Data
creditcard = read.csv("G:/내 드라이브/22-1 기계학습 실습/Data/creditcard.csv", header = T)
names(creditcard)
head(creditcard)
table(creditcard$Class)
prop.table(table(creditcard$Class))

# oversampling
library(ROSE)
n_legit = 284315
new_frac_legit = 0.50
new_n_total = n_legit / new_frac_legit # = 284315 / 0.50

oversampling_result = ovun.sample(Class ~ ., data = creditcard, method = "over", N = new_n_total, seed = 2018)
oversampled_credit = oversampling_result$data
table(oversampled_credit$Class)
class_lable_1_over = oversampled_credit[oversampled_credit$Class == 1, ]
class_lable_0_over = oversampled_credit[oversampled_credit$Class == 0, ]
plot(class_lable_1_over$V1, class_lable_1_over$V2, col = "red")
par(new = T)
plot(class_lable_0_over$V1, class_lable_0_over$V2, col = "blue")

# undersampling
n_fraud = 492
new_frac_fraud = 0.50
new_n_total = n_fraud / new_frac_fraud
undersampling_result = ovun.sample(Class ~ ., data = creditcard, method = "under", N = new_n_total, seed = 2018)
undersampled_credit = undersampling_result$data
table(undersampled_credit$Class)
class_lable_1_under = undersampled_credit[undersampled_credit$Class == 1, ]
class_lable_0_under = undersampled_credit[undersampled_credit$Class == 0, ]
plot(class_lable_1_under$V1, class_lable_1_under$V2, col = "red")
par(new = T)
plot(class_lable_0_under$V1, class_lable_0_under$V2, col = "blue")

# sampling(over & under both)
n_new = nrow(creditcard)
fraction_fraud_new = 0.50
sampling_result = ovun.sample(Class ~ ., data = creditcard, method = "both", N = n_new, p = fraction_fraud_new, seed = 2018)
sampled_credit = sampling_result$data
table(sampled_credit$Class)
class_lable_1_both = sampled_credit[sampled_credit$Class == 1, ]
class_lable_0_both = sampled_credit[sampled_credit$Class == 0, ]
plot(class_lable_1_both$V1, class_lable_1_both$V2, col = "red")
par(new = T)
plot(class_lable_0_both$V1, class_lable_0_both$V2, col = "blue")

# SMOTE
library(smotefamily)
SMOTE_Method = SMOTE(creditcard[, -31], creditcard[, 31])
SMOTE_Method$syn_data
Smote_data = SMOTE_Method$data
table(Smote_data$class)
class_lable_1_SMOTE = Smote_data[Smote_data$class == 1, ]
class_lable_0_SMOTE = Smote_data[Smote_data$class == 0, ]
plot(class_lable_1_SMOTE$V1, class_lable_1_SMOTE$V2, col = "red")
par(new = T)
plot(class_lable_0_SMOTE$V1, class_lable_0_SMOTE$V2, col = "blue")

# 방법론 별 결과 비교
par(mfrow = c(2, 2))
plot(class_lable_1_over$V1, class_lable_1_over$V2, col = "red")
plot(class_lable_1_under$V1, class_lable_1_under$V2, col = "red")
plot(class_lable_1_both$V1, class_lable_1_both$V2, col = "red")
plot(class_lable_1_SMOTE$V1, class_lable_1_SMOTE$V2, col = "red")

plot(class_lable_0_over$V1, class_lable_0_over$V2, col = "blue")
plot(class_lable_0_under$V1, class_lable_0_under$V2, col = "blue")
plot(class_lable_0_both$V1, class_lable_0_both$V2, col = "blue")
plot(class_lable_0_SMOTE$V1, class_lable_0_SMOTE$V2, col = "blue")