"We will try to predict the magnitude of price change, that is the absolute
value of the change based on the magnitude of previous days' price change"

library(dplyr)
library(ISLR)

#Reading the data
data <- Smarket

#Taking the absolute values
for (j in c(2:6, 8)){
  for (i in c(1:1250)) {
    if (data[i,j] < 0) {
      data[i,j] <- -1 * data[i,j]
    }
  }
}
rm(i,j)

#Splitting the data
train <- filter(data, Year < 2005)
test <- filter(data, Year == 2005)

#Building linear model
model.all <- lm(Today ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, train)
summary(model.all)

"Interesting. Lag1 and Volume does not affect today's price change magnitude
significantly. Lag2 through Lag5 on the other hand have virtually zero p-value.
The adjusted R2 is only 15.22%"

#Making the predictions
Prediction.all <- predict(model.all, test[,2:7])
Result.all <- as.data.frame(Prediction.all)
Result.all <- mutate(Result.all, Actual = test[,8])
rm(Prediction.all)

#Finding MSE
mse.all <- sum(Result.all$Prediction-Result.all$Actual) ** 2 / 252

#Building linear model using only chosen variables
model.chosen <- lm(Today ~ Lag2+Lag3+Lag4+Lag5, train)
summary(model.chosen)

Prediction.chosen <- predict(model.chosen, test[,2:7])
Result.chosen <- as.data.frame(Prediction.chosen)
Result.chosen <- mutate(Result.chosen, Actual = test[,8])

rm(Prediction.chosen)

#Finding the mse for chosen model
mse.chosen <- sum(Result.chosen$Prediction-Result.chosen$Actual) ** 2 / 252

#Evaluating MSE
sd(test[,8]) #0.39
"The mse for chosen model (5.28) is lower than the model with all variables
(8.75). Given that the standard deviation of Today is just 0.39 our MSE is very
high. Both the models have low R2 value around 0.15."

