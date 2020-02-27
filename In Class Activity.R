library(ggplot2)
library(tidyverse)
library(dplyr)
library(rpart)
library(caret)

Ames <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                   header = TRUE,
                   sep = ",")


model <- rpart(Fireplaces ~ TotRmsAbvGrd + OverallCond + OverallQual,
             method="class", data=Ames)

printcp(model) 
plotcp(model)  
summary(model) 



plot(model, uniform=TRUE, 
     main="Fireplaces Tree")
text(model, use.n=TRUE, all=TRUE, cex=.8)



