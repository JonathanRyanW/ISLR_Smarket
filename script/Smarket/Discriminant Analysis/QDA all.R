"We will build a QDA model to predict Direction based on all the variables."

library(dplyr)
library(MASS)
library(ISLR)

data <- Smarket

#Splitting the data into training and test data
train <- filter(data, Year < 2005)
test <- filter(data, Year == 2005)

train <- dplyr::select(train, Lag1, Lag2, Lag3, Lag4, Lag5, Volume, Direction)
test <- dplyr::select(test, Lag1, Lag2, Lag3, Lag4, Lag5, Volume, Direction)

#Building the model
model <- qda(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, train)
model

#Making the predictions
Prediction <- predict(model, test[,1:6])

#Building the confusion matrix
confusion_matrix <- matrix(table(Prediction$class, test[,7]), nrow = 2)
colnames(confusion_matrix) <- c("Actual Down", "Actual Up")
rownames(confusion_matrix) <- c("Predicted Down", "Predicted Up")

rm(Prediction, test, train)

"Looking at the confusion matrix, we can see that the qda predicted down most
of the time ((82 + 111) / 252 = 76.58%)."

#Calculating measurements
accuracy <- (confusion_matrix[1,1] + confusion_matrix[2,2]) /
  sum(confusion_matrix)

sensitivity <- confusion_matrix[1,1] /
  (confusion_matrix[1,1] + confusion_matrix[2,1])

specificity <- confusion_matrix[2,2] /
  (confusion_matrix[1,2] + confusion_matrix[2,2])

"The QDA has low accuracy of 44.44% and specificity of 21.27%, but very high
sensitivity of 73.87%. This is the highest sensitivity that we have ever seen
so far."