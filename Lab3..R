
library(ggplot2)
library(tidyverse)
library(dplyr)

# INTRO 1

rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

get_complexity = function(model) {
  length(coef(model)) - 1
}

# EXERCISE 1
## 1
Ames <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")

NewAmes1 = select(Ames,-18:-19)

## 2
fit1 = lm(SalePrice ~ LotArea, data = NewAmes1)
fit2 = lm(SalePrice ~ LotArea+ MSSubClass, data = NewAmes1)
fit3 = lm(SalePrice ~ LotArea + MSSubClass + YearBuilt, data = NewAmes1)
fit4 = lm(SalePrice ~ LotArea + MSSubClass + YearBuilt +YearRemodAdd, data = NewAmes1)
fit5 = lm(SalePrice ~ LotArea + MSSubClass + YearBuilt +YearRemodAdd +Fireplaces, data = NewAmes1)
fit6 = lm(SalePrice ~ LotArea + MSSubClass + YearBuilt +YearRemodAdd +Fireplaces+BsmtFinSF1, data = NewAmes1)
fit7 = lm(SalePrice ~ LotArea + MSSubClass + YearBuilt +YearRemodAdd +Fireplaces+BsmtFinSF1 +TotalBsmtSF, data = NewAmes1)
fit8 = lm(SalePrice ~ LotArea + MSSubClass + YearBuilt +YearRemodAdd +Fireplaces+BsmtFinSF1+TotalBsmtSF +GrLivArea, data = NewAmes1)
fit9 = lm(SalePrice ~ LotArea + MSSubClass + YearBuilt +YearRemodAdd +Fireplaces+BsmtFinSF1 +TotalBsmtSF +GrLivArea +FullBath, data = NewAmes1)
fit10 = lm(SalePrice ~ LotArea + MSSubClass + YearBuilt +YearRemodAdd +Fireplaces+BsmtFinSF1 +TotalBsmtSF +GrLivArea +FullBath +MoSold, data = NewAmes1)
fit11 = lm(SalePrice ~ LotArea + MSSubClass + YearBuilt +YearRemodAdd+Fireplaces+BsmtFinSF1 +TotalBsmtSF +GrLivArea +FullBath +MoSold+YrSold, data = NewAmes1)
fit12 = lm(SalePrice ~ LotArea + MSSubClass + YearBuilt +YearRemodAdd +Fireplaces+BsmtFinSF1 +TotalBsmtSF +GrLivArea +FullBath +MoSold+YrSold+MiscVal, data = NewAmes1)
fit13 = lm(SalePrice ~ LotArea + MSSubClass + YearBuilt +YearRemodAdd +Fireplaces+BsmtFinSF1 +TotalBsmtSF +GrLivArea +FullBath +MoSold+YrSold+MiscVal+PoolArea, data = NewAmes1)
fit14 = lm(SalePrice ~ LotArea + MSSubClass + YearBuilt +YearRemodAdd +Fireplaces+BsmtFinSF1 +TotalBsmtSF +GrLivArea +FullBath +MoSold+YrSold+MiscVal+PoolArea+ScreenPorch, data = NewAmes1)
fit15 = lm(SalePrice ~ LotArea + MSSubClass + YearBuilt +YearRemodAdd +Fireplaces+BsmtFinSF1 +TotalBsmtSF +GrLivArea +FullBath +MoSold+YrSold+MiscVal+PoolArea+ScreenPorch+GarageArea, data = NewAmes1)
# find complexity values
fitlist = list(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10, fit11, fit12, fit13, fit14, fit15)
fitcomplexity = sapply(fitlist, get_complexity)

# find rmse values
RSS <- c(crossprod(fit1$residuals)) #residual sum of squares
MSE <- RSS / length(fit1$residuals) #mean squared error
RMSE <- sqrt(MSE) #rmse
sqrt(mean(fit1$residuals^2)) #check

RSS2 <- c(crossprod(fit2$residuals)) #residual sum of squares
MSE2 <- RSS2 / length(fit2$residuals) #mean squared error
RMSE2 <- sqrt(MSE2) #rmse
sqrt(mean(fit2$residuals^2)) #check


RSS3 <- c(crossprod(fit3$residuals)) #residual sum of squares
MSE3 <- RSS3 / length(fit3$residuals) #mean squared error
RMSE3 <- sqrt(MSE3) #rmse
sqrt(mean(fit3$residuals^2)) #check

RSS4 <- c(crossprod(fit4$residuals)) #residual sum of squares
MSE4 <- RSS4 / length(fit4$residuals) #mean squared error
RMSE4 <- sqrt(MSE4) #rmse
sqrt(mean(fit4$residuals^2)) #check

RSS5 <- c(crossprod(fit5$residuals)) #residual sum of squares
MSE5 <- RSS5 / length(fit5$residuals) #mean squared error
RMSE5 <- sqrt(MSE5) #rmse
sqrt(mean(fit5$residuals^2)) 

RSS6 <- c(crossprod(fit6$residuals)) #residual sum of squares
MSE6 <- RSS6 / length(fit6$residuals) #mean squared error
RMSE6 <- sqrt(MSE6) #rmse
sqrt(mean(fit6$residuals^2)) 

RSS7 <- c(crossprod(fit7$residuals)) #residual sum of squares
MSE7 <- RSS7 / length(fit7$residuals) #mean squared error
RMSE7 <- sqrt(MSE7) #rmse
sqrt(mean(fit7$residuals^2)) 

RSS8 <- c(crossprod(fit8$residuals)) #residual sum of squares
MSE8 <- RSS8 / length(fit8$residuals) #mean squared error
RMSE8 <- sqrt(MSE8) #rmse
sqrt(mean(fit8$residuals^2)) 

RSS9 <- c(crossprod(fit9$residuals)) #residual sum of squares
MSE9 <- RSS9 / length(fit9$residuals) #mean squared error
RMSE9 <- sqrt(MSE9) #rmse
sqrt(mean(fit9$residuals^2)) 

RSS10 <- c(crossprod(fit10$residuals)) #residual sum of squares
MSE10 <- RSS10 / length(fit10$residuals) #mean squared error
RMSE10 <- sqrt(MSE10) #rmse
sqrt(mean(fit10$residuals^2)) 

RSS11 <- c(crossprod(fit11$residuals)) #residual sum of squares
MSE11 <- RSS11 / length(fit11$residuals) #mean squared error
RMSE11 <- sqrt(MSE11) #rmse
sqrt(mean(fit11$residuals^2)) 

RSS12 <- c(crossprod(fit12$residuals)) #residual sum of squares
MSE12 <- RSS12 / length(fit12$residuals) #mean squared error
RMSE12 <- sqrt(MSE12) #rmse
sqrt(mean(fit12$residuals^2)) 

RSS13 <- c(crossprod(fit13$residuals)) #residual sum of squares
MSE13 <- RSS13 / length(fit13$residuals) #mean squared error
RMSE13 <- sqrt(MSE13) #rmse
sqrt(mean(fit13$residuals^2)) 

RSS14 <- c(crossprod(fit14$residuals)) #residual sum of squares
MSE14 <- RSS14 / length(fit14$residuals) #mean squared error
RMSE14 <- sqrt(MSE14) #rmse
sqrt(mean(fit14$residuals^2)) 

RSS15 <- c(crossprod(fit15$residuals)) #residual sum of squares
MSE15 <- RSS15 / length(fit15$residuals) #mean squared error
RMSE15 <- sqrt(MSE15) #rmse
sqrt(mean(fit15$residuals^2)) 

y <- c(RMSE, RMSE2, RMSE3,RMSE4,RMSE5,RMSE6,RMSE7,RMSE8,RMSE9,RMSE10,RMSE11,RMSE12,RMSE13,RMSE14,RMSE15)

## 3 
chart <- data.frame("Complexity" = fitcomplexity, "RMSE" = y)

plot(chart$Complexity, chart$RMSE, main="Complexity vs. RMSE Correlation", 
     xlab="Model Complexity ", ylab="Model RMSE",col="red", pch=19)

#I noticed that as the complexity of a model increases, the RMSE decreases.I think a full
#model should be used so you are able to see more data and get a more accurate depiction
#of the relationship between complexity and rmse.

# INTRO 2

set.seed(9)
num_obs = nrow(NewAmes1)

train_index = sample(num_obs, size = trunc(0.50 * num_obs))
train_data = NewAmes1[train_index, ]
test_data = NewAmes1[-train_index, ]

fit_0 = lm(SalePrice ~ 1, data = train_data)
get_complexity(fit_0)

# train RMSE
sqrt(mean((train_data$SalePrice - predict(fit_0, train_data)) ^ 2))
# test RMSE
sqrt(mean((test_data$SalePrice - predict(fit_0, test_data)) ^ 2))

# train RMSE (2)
rmse(actual = train_data$SalePrice, predicted = predict(fit_0, train_data))
# test RMSE (2)
rmse(actual = test_data$SalePrice, predicted = predict(fit_0, test_data))

# create rmse function
get_rmse = function(model, data, response) {
  rmse(actual = subset(data, select = response, drop = TRUE),
       predicted = predict(model, data))
}

# test rmse function
get_rmse(model = fit_0, data = train_data, response = "SalePrice") # train RMSE
get_rmse(model = fit_0, data = test_data, response = "SalePrice") # test RMSE

# create five models
fit_1 = lm(SalePrice ~ LotArea, data = NewAmes1)
fit_2 = lm(SalePrice ~ LotArea+ MSSubClass, data = NewAmes1)
fit_3 = lm(SalePrice ~ LotArea + MSSubClass + YearBuilt, data = NewAmes1)
fit_4 = lm(SalePrice ~ LotArea + MSSubClass + YearBuilt +YearRemodAdd, data = NewAmes1)
fit_5 = lm(SalePrice ~ LotArea + MSSubClass + YearBuilt +YearRemodAdd +Fireplaces, data = NewAmes1)
model_list = list(fit_1, fit_2, fit_3, fit_4, fit_5)

train_rmse = sapply(model_list, get_rmse, data = train_data, response = "SalePrice")
test_rmse = sapply(model_list, get_rmse, data = test_data, response = "SalePrice")
model_complexity = sapply(model_list, get_complexity)

# plot the results

plot(model_complexity, train_rmse, type = "b",
     ylim = c(min(c(train_rmse, test_rmse)) - 0.02,
              max(c(train_rmse, test_rmse)) + 0.02),
     col = "dodgerblue",
     xlab = "Model Size",
     ylab = "RMSE")

lines(model_complexity, test_rmse, type = "b", col = "darkorange")

# EXERCISE 2
## 1
fitlist = list(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10, fit11, fit12, fit13, fit14, fit15)
fitcomplexity = sapply(fitlist, get_complexity)

newtraining_rmse = sapply(fitlist, get_rmse, data = train_data, response = "SalePrice")
newtesting_rmse = sapply(fitlist, get_rmse, data = test_data, response = "SalePrice")
fitcomplexity = sapply(fitlist, get_complexity)

plot(fitcomplexity, newtraining_rmse, type = "b",
     ylim = c(min(c(newtraining_rmse, newtesting_rmse)) - 0.02,
              max(c(newtraining_rmse, newtesting_rmse)) + 0.02),
     col = "yellow", main = "Model Size versus RMSE",
     xlab = "Model Size",
     ylab = "RMSE")
lines(fitcomplexity, newtesting_rmse, type = "b", col = "pink")

## 2 


# predict sale price
fit16 = lm(SalePrice ~ LotArea + MSSubClass + YearBuilt +YearRemodAdd +Fireplaces+BsmtFinSF1 +TotalBsmtSF +GrLivArea +FullBath +MoSold+YrSold+MiscVal+PoolArea+ScreenPorch+GarageArea+BsmtFinSF2+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF+BsmtFullBath+BsmtHalfBath+HalfBath+BedroomAbvGr+KitchenAbvGr+
             TotRmsAbvGrd+GarageCars+WoodDeckSF+OpenPorchSF+EnclosedPorch+X3SsnPorch, data = NewAmes1)
predict(fit16, NewAmes1, interval = "prediction")

# find train rmse
salepredictions1 = predict(fit16, train_data, interval = "prediction")
saleprediction1 = predict(fit16, train_data)
tr_rmse = sqrt(mean((train_data$SalePrice - saleprediction1) ^ 2))
# find test rmse
salepredictions2 = predict(fit16, test_data, interval = "prediction")
saleprediction2 = predict(fit16, test_data)
te_rmse =  sqrt(mean((test_data$SalePrice - saleprediction2) ^ 2))

## 3 (on PDF)

## 4 Extra Credit


