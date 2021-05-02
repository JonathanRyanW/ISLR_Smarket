"In the previous script, Logistic Regression.R we split the data randomly into
training and test data. This time we will use data from 2001-2004 to predict
Direction of the 2005 data. This is what most companies do in real life. They
dont split stock market data randomly. They use historical data to predict
future movements. We conduct the experiment as if 2005 has not yet happenned."

library(ISLR)
library(dplyr)

#Splitting the data into training and test data
data <- Smarket
train <- filter(data, Year < 2005)
test <- filter(data, Year == 2005)

#Building logistic regression model
model <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
             train, family = "binomial")
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
true_down <- 77/252
true_up <- 44/252

sensitivity <- 77 / (77 + 34)
specificity <- 44 / (97 + 44)
"High sensitivity of 69.36 percent and low specificity of 31.2%."
