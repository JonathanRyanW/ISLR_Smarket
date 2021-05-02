"This time we will try using only the Lag1 and Lag2 variable as both of them are
the variables with the lowest p-value."

library(ISLR)
library(dplyr)

#Splitting the data into training and test data
data <- Smarket
train <- filter(data, Year < 2005)
test <- filter(data, Year == 2005)

#Building logistic regression model
model <- glm(Direction ~ Lag1+Lag2, train, family = "binomial")
summary(model)

"Our model is yet again very bad. None of the variables have low p-value.
The lowest p-value is Lag1 with 0.295."

#Making predictions
contrasts(data$Direction)
"R assigns 1 for Up and 0 for Down"

Prediction <- predict(model, test[,-9], type = "response")
result <- as.data.frame(cbind(test[,9], Prediction))
result <- rename(result, Actual = V1)
rm(Prediction)

for (i in c(1:252)) {
  if (result[i,1] == 2) {
    result[i,1] <- "Up"
  } else{
    result[i,1] <- "Down"
  }
}

for (i in c(1:252)) {
  if (result[i,2] > 0.5) {
    result[i,2] <- "Up"
  } else{
    result[i,2] <- "Down"
  }
}
rm(i)

#Creating the confusion matrix
table(result$Prediction, result$Actual)

"This is a bit surprising. Our model seems to predict Up days very badly but
succeed in predicting Down days quiet well. The accuracy is only 48%."

accuracy <- mean(result$Prediction == result$Actual)
true_down <- 35/252
true_up <- 106/252

sensitivity <- 35 / (77 + 34)
specificity <- 106 / (97 + 44)
"Previously we get a model with high sensitivity of 69.36 percent and low
specificity of 31.2%. This time our model has low sensitivity 31.5% and high
specificity of 75.17%. Our new model also has a better accuracy than the previous
model with 55.95% accuracy.

We might use these model as part of our trading strategy, that is to trade long 
only if our second model predicts that today will go up and trade short if our
first model predicts that today will go down."
