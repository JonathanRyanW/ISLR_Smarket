"This time we are going to predict the actual change of the prices using linear
regression and then using that predictions to predict market direction. This
time we will use all the variables available for us."

library(dplyr)

#Reading and splitting data
library(ISLR)
data <- Smarket

train <- filter(data, Year < 2005)
test <- filter(data, Year == 2005)

#Builind linear regression model
model <- lm(Today ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, train)
summary(model)
"The p-values are again very bad. The R2 is also bad but better than the linear
regression with Lag1 and Lag2 only. Interestingly enough as we will see even
though this model has higher R2 it still predicts the test data worse than the
previous linear model."

#Making predictions
Predictions <- predict(model, test)
result <- data.frame(cbind(test$Direction, Predictions))
result <- rename(result, Actual = V1)

#Cleaning the result data frame
for (i in c(1:252)) {
  if (result[i,1] == 2) {
    result[i,1] <- "Up"
  } else{
    result[i,1] <- "Down"
  }
}

for (i in c(1:252)) {
  if (result[i,2] > 0) {
    result[i,2] <- "Up"
  } else{
    result[i,2] <- "Down"
  }
}
rm(i)

#Creating confusion matrix
table(result$Predictions, result$Actual)

#Metrics to evaluate the model
accuracy <- mean(result$Predictions == result$Actual)
sensitivity <- sum(result$Predictions == "Down" & result$Actual == "Down") /
  sum(result$Actual == "Down")
specificity <- sum(result$Predictions == "Up" & result$Actual == "Up") /
  sum(result$Actual == "Up")

"Our accuracy and sensitivity is lower than the linear model using only Lag1 and
Lag2. "
