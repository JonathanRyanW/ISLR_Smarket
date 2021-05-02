"We will try to illustrate exactly how target leakage improves the performance
of our models. This time we will use Logistic Regression with target leakage."

#Loading packages
library(ISLR)
library(dplyr)

#Reading and splitting data
data <- Smarket
train <- filter(data, Year < 2005)
test <- filter(data, Year == 2005)

#Building logistic regression model with target leakage
model.all <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume+Today,
             train, family = "binomial")
summary(model.all)

#Making predictions
Prediction <- predict(model.all, test[,-9], type = "response")
result.all <- as.data.frame(cbind(test[,9], Prediction))
result.all <- rename(result.all, Actual = V1)
rm(Prediction)

#Cleaning the result dataframe
for (i in c(1:252)) {
  if (result.all[i,1] == 2) {
    result.all[i,1] <- "Up"
  } else{
    result.all[i,1] <- "Down"
  }
}

for (i in c(1:252)) {
  if (result.all[i,2] > 0.5) {
    result.all[i,2] <- "Up"
  } else{
    result.all[i,2] <- "Down"
  }
}
rm(i)

#Creating the confusion matrix
table(result.all$Prediction, result.all$Actual)

"Our model predicted that the S&P 500 will go up every single time except for 1
time it predicted Down and got it correct."

#Building logistic regression model with only Today as a predictor
model.Today <- glm(Direction ~ Today, train, family = "binomial")
summary(model.Today)

#Making predictions
Prediction <- predict(model.Today, test[,-9], type = "response")
result.Today <- as.data.frame(cbind(test[,9], Prediction))
result.Today <- rename(result.Today, Actual = V1)
rm(Prediction)

#cleaning the result.Today dataframe
for (i in c(1:252)) {
  if (result.Today[i,1] == 2) {
    result.Today[i,1] <- "Up"
  } else{
    result.Today[i,1] <- "Down"
  }
}

for (i in c(1:252)) {
  if (result.Today[i,2] > 0.5) {
    result.Today[i,2] <- "Up"
  } else{
    result.Today[i,2] <- "Down"
  }
}
rm(i)

#Creating the confusion matrix
table(result.Today$Prediction, result.Today$Actual)

"This is strange, our model gives the exact same predictions with or without
all the other variables."