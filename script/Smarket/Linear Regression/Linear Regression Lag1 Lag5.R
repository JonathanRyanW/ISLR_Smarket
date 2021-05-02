"This time we are going to predict the actual change of the prices using linear
regression and then using that predictions to predict market direction. This time
we will use Lag1 and Lag5 only as we found out that Lag5 actually has the lowest
p-value."

library(dplyr)

#Reading and splitting data
library(ISLR)
data <- Smarket
train <- filter(data, Year < 2005)
test <- filter(data, Year == 2005)

#Builind linear regression model
model <- lm(Today ~ Lag1+Lag5, train)
summary(model)
"The p-values are again very bad. This is very confusing. The p-value for Lag5
is lower than Lag2 but as we will see the results are worse than using Lag2
instead of Lag5."

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

"Why do we get lower accuracy and sensitivity even though our Lag5 p-value is
lower than the p-value of Lag2 in the Lag1,Lag2 model? Could it be that it is
by chance that even though the p-value for Lag5 is low, it is not significant 
while even though the p-value for Lag2 is high it is in fact more significant
than Lag5?

