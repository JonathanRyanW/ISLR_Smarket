"We will perform KNN to predict market direction based on many variables. We
will use data from 2001 to 2004 to predict price movement of the year 2005. This
time we will use all variables except Year, and Today"

library(ISLR)
library(dplyr)
library(class)

data <- Smarket

#Splitting data into training and test data
train <- filter(data, Year < 2005)
test <- filter(data, Year == 2005)

train <- select(train, Lag1, Lag2, Lag3, Lag4, Lag5, Volume, Direction)
test <- select(test, Lag1, Lag2, Lag3, Lag4, Lag5, Volume, Direction)

#Finding the best k
Error <- c()

for (i in c(1:20)){
  Prediction <- knn(train[,1:6], test[,1:6], train[,7], k = i)
  Error <- append(Error, mean(Prediction != test[,7]))
}

result <- as.data.frame(cbind(c(1:20), Error))

rm(Prediction, Error, i)

min(result$Error)
which(result$Error == min(result$Error))
"We can see that the smallest error that we get is with k = 7. We get an
error rate of 45,23%. This is a huge error rate. We will proceed with k = 7."

rm(result)

#Making Predictions with k = 7
Prediction <- knn(train[,1:6], test[,1:6], train[,7], k = 7)
Error <- mean(Prediction != test[,7])

Result <- as.data.frame(Prediction)
Result <- mutate(Result, Actual = test[,7])

rm(Prediction, test, train)

#Creating the Confusion Matrix
Confusion_matrix <- matrix(table(Prediction, test[,7]), nrow = 2)
colnames(Confusion_matrix) <- c("Actual Down", "Actual Up")
rownames(Confusion_matrix) <- c("Predicted Down", "Predicted Up")

#Calculating measurements
Accuracy <- (Confusion_matrix[1,1] + Confusion_matrix[2,2]) /
  sum(Confusion_matrix)

Sensitivity <- Confusion_matrix[1,1] /
  (Confusion_matrix[1,1] + Confusion_matrix[2,1])

Specificity <- Confusion_matrix[2,2] /
  (Confusion_matrix[1,2] + Confusion_matrix[2,2])

"This KNN performs worse than the linear regression in terms of accuracy and 
sensitivity, it has slightly better specificity (58,15%) than linear regression
with only Lag1 and Lag2 (56.73%)

There is no significant improvement in terms of accuracy and sensitivity."