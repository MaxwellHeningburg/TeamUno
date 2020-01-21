library(tidyverse)
library(ggplot2)
library(gapminder)

ggplot2::mpg

gapminder
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point()


ggplot2::mpg
p2 <- ggplot(data = mpg,
            mapping = aes(x = displ, y = hwy))
p2 + geom_point() #This plot is does capture the relationship because as the cars
#engine size increase, it is less efficient on the highway.

ggplot2::mpg
p3 <- ggplot(data = mpg,
             mapping = aes(x = class, y = drv))
p3 + geom_point() #This plot is not useful because you are comparing two categorical
#variables against each other.

ggplot2::mpg
p4 <- ggplot(data = mpg,
             mapping = aes(color=class, x = displ, y = hwy,))
p4 + geom_point() #We can make the conclusion that bigger class cars like Suv's and 
#Pickups have a bigger engine, which causes a lower highway effiency.

p + geom_point() + geom_smooth(method = "lm") + scale_x_log10()

#The scale x log 10 function helps scale the data so you are able to see all the data
#points. Without this function, the graph would be a small portion and you would not
#be able to see the data and relationship between variables.

library(scales)
p + geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10(labels = scales::dollar)
p + geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10(labels = scales::dollar) #The dollar function changes to x axis
#values to dollar amounts so you are able to see on the graph gdpPercap

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(color="yellow") + scale_x_log10()

#Here, you have to put color=yellow in the geom point function to make the points yellow.
#In this graph, the points are yellow and the data is displayed so we can see the points.

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point() + geom_smooth(color = "orange", se = FALSE, size = 8, method = "lm") + scale_x_log10()

p + geom_point(alpha = 0.3) +
  geom_smooth(method = "gam") +
  scale_x_log10(labels = scales::dollar) +
  labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
       title = "Economic Growth and Life Expectancy",
       subtitle = "Data Points are country-years",
       caption = "Source: Gapminder")

library(scales)
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp, color = continent,fill = continent))
p + geom_point()
p + geom_point() + scale_x_log10(labels = dollar)
p + geom_point() + scale_x_log10(labels = dollar) + geom_smooth()

#The fill=continent 

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(mapping = aes(color = continent)) + geom_smooth() + scale_x_log10()

loans<- table(bank$housing, bank$loan)
names<- c("Housing Loans", "Other Loans")


barplot(loans, main="Loan Distribution",
        xlab="Number of Loans", col=c("White","Green"),
        legend =(names),beside=TRUE)


bank <- read.csv("~/SSC442/bank.csv")
age_balance_model = lm(age ~ balance, data = bank)
plot(age ~ balance, data = bank,
     xlab = "Balance",
     ylab = "Age",
     main = "Age vs Balance",
     pch  = 20,
     cex  = 1,
     col  = "gray")
abline(age_balance_model, lwd = 2, col = "darkorange")



#MEMO
#The information that I hope to convey with the first graph is the loan distribution. 
#The graph shows the amount of the people who currently have a loan with the bank. 
#It also shows if they have a house loan or another type of loan. 
#With this knowledge you can look at the loans policies to see if you can create a way for people to take more loans. 
#By changing the policies you can increase the frequency of clients borrowing.
#The next graph is a regression between the age of the person of the account and their balance. 
#The line of best fit trends upwards which means as age increases balance will also increase as well. 
#This trend gives you basic knowledge about your account owners and the trend of balances increasing as time goes along. 





  
  
