"This time we are going to predict the actual change of the prices using linear
regression and then using that predictions to predict market direction"

library(dplyr)

#Reading and splitting data
library(ISLR)
data <- Smarket
train <- filter(data, Year < 2005)
test <- filter(data, Year == 2005)

#Builind linear regression model
model <- lm(Today ~ Lag1+Lag2, train)
summary(model)
"The p-values are again very bad. The R2 is also bad. OMG it is so bad."

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

"Our accuracy is better than the logistic model. We have 57.53% accuracy.
Our sensitivity and specificity are moderate (58.55% and 56.73%). The linear
model seems to be more conservative than the logistic model. Not too high but
not too low sensitivity and specificity.

If we are going to use these model as part of our strategy, the logistic model
with 75% specificity might be more useful as we can be pretty sure that when it
predicts that the market will go up, it will."

