---
title: "Lab 2"
author: "Team Uno"
date: "1/30/2020"
output:
  pdf_document: default
html_document: default
---
  
  ```{r setup, echo=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, echo=FALSE}
ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")
head(ameslist)
```

# Introduction
### Headers in the Original Dataset
1. With Header = TRUE, the dataset formats certain variables as integers and factors, as mentioned.
2. With Header = FALSE, all the variables are marked as factors.
3. Without either Header = TRUE or Header = FALSE, the variable titles are gone and the variables are all factors.

### Initial Inspection

```{r ,verify list, echo=FALSE}
typeof(ameslist)
```
```{r, echo=FALSE}
unique(ameslist$GarageType)
GarageType = ameslist$GarageType
GarageTemp = model.matrix( ~ GarageType - 1, data=ameslist$GarageType )
```

```{r, echo=FALSE}
typeof(ameslist)
typeof(GarageTemp)
merge(ameslist,GarageTemp)
```

# Exercise 1

```{r, echo=FALSE}
Ames = ameslist[,-which(sapply(ameslist, class) == "factor")]
Ames1 = ameslist[,-which(sapply(ameslist, class) == "factor")]
Ames2 = ameslist[,-which(sapply(ameslist, class) == "factor")]
head(Ames)
```

```{r, echo=FALSE}
attach(ameslist)
plot(SalePrice, GrLivArea, main="Sale vs GrLiveArea",
     xlab="SalePrice ", ylab="GrLivArea ", pch=19)
abline(lm(GrLivArea~SalePrice), col="red")

ameslist[1229,]
```

# Building A Model

```{r, echo=FALSE}
attach(Ames)
lm.fit = lm(SalePrice ~ GrLivArea)
```
*GRLivArea represents the above ground living area (in other words, living room) available in the specified house in square feet.*
  
```{r, echo=FALSE}
plot(lm.fit)
```
*We suspect that some outliers do have a large influence on the data.*
  
```{r, echo=FALSE}
newlm.fit = lm(SalePrice ~ GrLivArea + LotArea)
plot(newlm.fit)
```
*Controlling for the area of the home does not change the qualitative conclusions because of the simple reasoning that higher lot areas will have a higher sale price and vice versa. However, for the quantitative results, outliers that are now being controlled for, would change the results of our statistics to a more accurate level.*
  
# Exercise 2
  
  ```{r, echo=FALSE}
amesplot.fit <- lm(SalePrice ~ Id + OverallQual + MasVnrArea + TotalBsmtSF + GrLivArea + HalfBath + Fireplaces + WoodDeckSF + ScreenPorch + YrSold + MSSubClass + OverallCond + BsmtFinSF1 + X1stFlrSF + BsmtFullBath + BedroomAbvGr + GarageYrBlt + OpenPorchSF + PoolArea + LotFrontage + YearBuilt + BsmtFinSF2 + X2ndFlrSF + BsmtHalfBath + KitchenAbvGr + GarageCars + EnclosedPorch + MiscVal + LotArea + YearRemodAdd + BsmtUnfSF + LowQualFinSF + FullBath + TotRmsAbvGrd + GarageArea + X3SsnPorch + MoSold, data=Ames)
summary(amesplot.fit)
```
1. Is there a relationship between the predictors and the response?
*Yes, there is an overall relationship between the predictors (being all the variables in Ames) and the actual Sale Price of the homes in Ames.*
2.Which predictors appear to have a statistically significant relationship to the response?
*The predictors Fireplaces,WoodDeckSF,ScreenPorch,OverallCond, BsmtFinSF1,BsmtFullBath, PoolArea, LotFrontage, YearBuilt, KitchenAbvGr, LotArea, and TotRmsAbvGrd because they have p-values below the statistically significant level of 0.05.*
3. What does the coefficient for the year variable suggest?
*The coefficient for the year built variable (3.164e+02) suggests that for every 1 year increase in the year built of the house, the sale price goes up by 3.164e+02.*
  
## Plot the relationship
  ```{r, echo=FALSE}
plot(amesplot.fit)
```
1. Comment on any problems you see with the fit. 
*The fitted values are in clusters and that could prove to be problematic for predicting sale price in the future. As well as potential outliers that could skew our analysis*
2.Do the residual plots suggest any unusually large outliers? 
  *The residuals suggest two unusually large outliers in the dataset. One at 524 and one at 1299 in the plot.*
3.Does the leverage plot identify any observations with unusually high leverage?
  *Yes, the cluster of points on hte leverage plot are around 0.0 to 0.1, with a few reaching 0.3. However, there is one point that reaches a leverage of 0.7.*
  
## Summary of Interaction Statistics
  ```{r, echo=FALSE}
newplot.fit <- lm(SalePrice ~ Id + OverallQual * OverallCond + MasVnrArea + TotalBsmtSF + GrLivArea + HalfBath + Fireplaces + WoodDeckSF + ScreenPorch + MSSubClass  + BsmtFinSF1 + X1stFlrSF + BsmtFullBath + BedroomAbvGr + GarageYrBlt * GarageCars+ OpenPorchSF + PoolArea : LotFrontage + YearBuilt + BsmtFinSF2 + X2ndFlrSF + BsmtHalfBath + KitchenAbvGr  + EnclosedPorch + MiscVal + LotArea + YearRemodAdd * YrSold + BsmtUnfSF + LowQualFinSF + FullBath + TotRmsAbvGrd + GarageArea + X3SsnPorch + MoSold, data=Ames)
summary(newplot.fit)
```
1. Do any interactions appear to be statistically significant?
  *The interaction between garage year built and garage cars, as well as the interaction between pool area and lot frontage are statistically significant, with p-values below 0.05.*
  
# Transformations
## Log Transformation
  ```{r, echo=FALSE}
names(Ames)
logtransform <- c("Id","MSSubClass","LotFrontage","LotArea","OverallQual","OverallCond","YearBuilt","YearRemodAdd","MasVnrArea","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","X2ndFlrSF","LowQualFinSF" ,"GrLivArea","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr","TotRmsAbvGrd","Fireplaces","GarageYrBlt","GarageCars", "GarageArea","WoodDeckSF","OpenPorchSF","EnclosedPorch","X3SsnPorch","ScreenPorch","PoolArea","MiscVal" ,"MoSold","YrSold","SalePrice")

Ames[logtransform] <- lapply(Ames[logtransform], log)
head(Ames)
```
## Squareroot Transformation
```{r, echo=FALSE}
squaretransform <- c("Id","MSSubClass","LotFrontage","LotArea","OverallQual","OverallCond","YearBuilt","YearRemodAdd","MasVnrArea","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","X2ndFlrSF","LowQualFinSF" ,"GrLivArea","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr","TotRmsAbvGrd","Fireplaces","GarageYrBlt","GarageCars", "GarageArea","WoodDeckSF","OpenPorchSF","EnclosedPorch","X3SsnPorch","ScreenPorch","PoolArea","MiscVal" ,"MoSold","YrSold","SalePrice")

Ames1[squaretransform] <- lapply(Ames1[squaretransform], sqrt)
head(Ames1)
```
## Squared Transformation
```{r, echo=FALSE}
squaredAmes <- (Ames2)^2

head(squaredAmes)
```
1. Do any of these make sense to include in a model of SalePrice? Comment on your findings.
*The log transformation produces some NaN values that aren't very helpful in intepreting the data. However, I believe taking the log of the data helps with the readability (in terms of axis scale) of the Ames dataset. On the other hand, transforming the data based on either squaring it or squaring it, doesn't have much of an impact on the relationship between variables, but would have an impact on the scale.*
  
# Bonus
  
1. How might we build a model to estimate the elasticity of demand from this dataset?
*We could possibly estimate an elasticity of demand model for this dataset by making a linear regression model, which is a represention of the linear relationship between a dependent variable and one or more independent variables. Then, after doing so, we could interpret the coefficients as such: x,y, and b. Using the x (independent) and y (dependent) values, we could find elasticity of demand through it equalling b (coefficient of x) multiplied by (x/y).*