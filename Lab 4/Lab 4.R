# Exercise 1 part 1
bank <- read.csv("~/SSC442/bank.csv")

library(kernlab)
data("spam")
tibble::as.tibble(spam)

is.factor(spam$type)
levels(spam$type)

set.seed(42)
# spam_idx = sample(nrow(spam), round(nrow(spam) / 2))
spam_idx = sample(nrow(spam), 1000)
spam_trn = spam[spam_idx, ]
spam_tst = spam[-spam_idx, ]

fit_caps = glm(type ~ capitalTotal,
               data = spam_trn, family = binomial)
fit_selected = glm(type ~ edu + money + capitalTotal + charDollar,
                   data = spam_trn, family = binomial)
fit_additive = glm(type ~ .,
                   data = spam_trn, family = binomial)
fit_over = glm(type ~ capitalTotal * (.),
               data = spam_trn, family = binomial, maxit = 50)


mean(ifelse(predict(fit_caps) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_selected) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_additive) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_over) > 0, "spam", "nonspam") != spam_trn$type)

library(boot)
set.seed(1)
cv.glm(spam_trn, fit_caps, K = 5)$delta[1] #1
cv.glm(spam_trn, fit_selected, K = 5)$delta[1] #2
cv.glm(spam_trn, fit_additive, K = 5)$delta[1] #4
cv.glm(spam_trn, fit_over, K = 5)$delta[1] #3

set.seed(10)
cv.glm(spam_trn, fit_caps, K = 100)$delta[1] 
cv.glm(spam_trn, fit_selected, K = 100)$delta[1] 
cv.glm(spam_trn, fit_additive, K = 100)$delta[1] 
cv.glm(spam_trn, fit_over, K = 100)$delta[1] 



#1.(Most Undefit) cv.glm(spam_trn, fit_caps, K = 5)$delta[1] 
#2.cv.glm(spam_trn, fit_selected, K = 5)$delta[1]
#3.cv.glm(spam_trn, fit_over, K = 5)$delta[1]
#4.cv.glm(spam_trn, fit_additive, K = 5)$delta[1]

#2. When running the code above, my conclusion does not change.

# Exercise 1 part 2

make_conf_mat = function(predicted, actual) {
  table(predicted = predicted, actual = actual)
}

spam_tst_pred = ifelse(predict(fit_additive, spam_tst) > 0,
                       "spam",
                       "nonspam")
spam_tst_pred = ifelse(predict(fit_additive, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")
conf_mat_50 = make_conf_mat(predicted = spam_tst_pred, actual = spam_tst$type)

table(spam_tst$type) / nrow(spam_tst)

# first model
spam_tst_pred1 = ifelse(predict(fit_caps, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")
conf_mat_1 = make_conf_mat(predicted = spam_tst_pred1, actual = spam_tst$type)

table(spam_tst$type) / nrow(spam_tst)

# second model
spam_tst_pred2 = ifelse(predict(fit_selected, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")

conf_mat_2 = make_conf_mat(predicted = spam_tst_pred2, actual = spam_tst$type)


# third model
spam_tst_pred3 = ifelse(predict(fit_additive, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")

conf_mat_3 = make_conf_mat(predicted = spam_tst_pred3, actual = spam_tst$type)

# fourth model
spam_tst_pred4 = ifelse(predict(fit_over, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")

conf_mat_4 = make_conf_mat(predicted = spam_tst_pred4, actual = spam_tst$type)


# Exercise 2
install.packages("kernlab")
library(kernlab)

bank<- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/bank.csv",
                  header = TRUE,
                  sep = ",")

set.seed(42)
# spam_idx = sample(nrow(spam), round(nrow(spam) / 2))
bank_idx = sample(nrow(bank), 1000)
bank_trn = bank[bank_idx, ]
bank_tst = bank[-bank_idx, ]


fit_caps = glm(loan ~ balance,
               data = bank_trn, family = binomial)
fit_selected = glm(loan ~ balance + duration + marital + age,
                   data = bank_trn, family = binomial)
fit_additive = glm(loan ~ .,
                   data = bank_trn, family = binomial)
fit_over = glm(loan ~ balance * (.),
               data = bank_trn, family = binomial, maxit = 50)



# Balance which has a negative coeffiecent saying if you have balance in you account the less likely you are to have a loan
# Marital status which are positive for both single and married making it more likely you have a loan
# duration which is postive meaning the longer you have an account the more likely you have a loan
# Age which has a positive coeffiecent meaning the older you are the more likelyhood you would have a loan


# training misclassification rate
mean(ifelse(predict(fit_caps) > 0, "yes", "no") != bank_trn$loan)
mean(ifelse(predict(fit_selected) > 0, "yes", "no") != bank_trn$loan)
mean(ifelse(predict(fit_additive) > 0, "yes", "no") != bank_trn$loan)
mean(ifelse(predict(fit_over) > 0, "yes", "no") != bank_trn$loan)

library(boot)
set.seed(1)
cv.glm(bank_trn, fit_caps, K = 10)$delta[1]
cv.glm(bank_trn, fit_selected, K = 10)$delta[1]
cv.glm(bank_trn, fit_additive, K = 10)$delta[1]
cv.glm(bank_trn, fit_over, K = 10)$delta[1]


make_conf_mat = function(predicted, actual) {
  table(predicted = predicted, actual = actual)
}

bank_tst_pred = ifelse(predict(fit_additive, bank_tst) > 0,
                       "yes",
                       "no")
bank_tst_pred = ifelse(predict(fit_additive, bank_tst, type = "response") > 0.5,
                       "yes",
                       "no")
(conf_mat = make_conf_mat(predicted = bank_tst_pred, actual = bank_tst$loan))

table(bank_tst$loan) / nrow(bank_tst)