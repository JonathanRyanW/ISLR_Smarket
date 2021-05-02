"We will build a QDA model to predict Direction based on the variables Lag1 and
Lag 2 only."

library(dplyr)
library(MASS)
library(ISLR)

data <- Smarket

#Splitting the data into training and test data
train <- filter(data, Year < 2005)
test <- filter(data, Year == 2005)

train <- dplyr::select(train, Lag1, Lag2, Direction)
test <- dplyr::select(test, Lag1, Lag2, Direction)

#Building the model
model <- qda(Direction ~ Lag1+Lag2, train)
model

#Making the predictions
Prediction <- predict(model, test[,1:2])

#Building the confusion matrix
confusion_matrix <- matrix(table(Prediction$class, test[,3]), nrow = 2)
colnames(confusion_matrix) <- c("Actual Down", "Actual Up")
rownames(confusion_matrix) <- c("Predicted Down", "Predicted Up")

rm(Prediction, test, train)

"Looking at the confusion matrix, we can see that the qda predicted up most
of the time ((81 + 121) / 252 = 80.15%)."

#Calculating measurements
accuracy <- (confusion_matrix[1,1] + confusion_matrix[2,2]) /
  sum(confusion_matrix)

sensitivity <- confusion_matrix[1,1] /
  (confusion_matrix[1,1] + confusion_matrix[2,1])

specificity <- confusion_matrix[2,2] /
  (confusion_matrix[1,2] + confusion_matrix[2,2])

"The QDA has high accuracy of 59.92% and specificity of 85.81%, but very low
sensitivity of 27.02%. This is the highest specificity that we have ever seen
so far."