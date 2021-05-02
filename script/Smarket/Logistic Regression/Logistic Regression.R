library(caTools)

#Building a logistic regression model
contrasts(data$Direction)
"R assigns 0 to Down and 1 to Up."

model <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data, family = "binomial")
summary(model)

"We can see that none of the variables are significantly affecting the Direction
variable. All of the p-values are large. None of them are smaller than 0.14.

The strongest predictor is Lag 1 with p-value of 0.145 and coefficient of 
-0.073074. This suggest that if yesterday's prices goes up than today's price
are less likely to go up.

The second strongest predictor is volume with a p-value of 0.392 and coef of
0.13. The positive coefficient tells us that the higher the today's volume the
more likely that today is an Up day.

The third strongest variable is Lag2. It makes sense
that Lag2 should be weaker than Lag1 since Lag1 is the more recent price and
such provides more predictive value. Lag2 also has negative coefficient, giving
the same interpretation as the Lag1 coefficient.

It should also be noted that the p-values for Lag3, Lag4, Lag5 are roughly the
same at about 0.83. This is a very large p-value. "

#Splitting data into training and test data
set.seed(1001)
sample <- sample.split(data, SplitRatio = 0.7)
train <- data[sample,]
test <- data[!sample,]

rm(sample)

#Building logistic regression model
model <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, train, family = "binomial")
summary(model)
"As before our model is really bad."

#Making predictions based on the model
prediction <- predict(model, test[,-9])
prediction <- predict(model, test[,-9], type = "response")
"Setting the type argument to responses will change the values to probability
instead of logit."

Prediction <- rep("Down", 417)
Prediction[prediction > 0.5] <- "Up"

result <- as.data.frame(cbind(test[,9], Prediction))
result <- rename(result, Actual = V1)
rm(Prediction, prediction)

for (i in c(1:417)) {
  if (result[i,1] == 2) {
    result[i,1] <- "Up"
  } else{
    result[i,1] <- "Down"
  }
}
rm(i)

#Creating Confusion Matrix
table(result$Prediction, result$Actual)
"Just to be clear, the rows are Actual and the columns are Predicted"

accuracy <- (62 + 160) / 417
sensitivity <- 62 / (62 + 133)
specificity <- 160 / (160 + 62)
"Our logistic model has a low accuracy and low sensitivity. There are 602 Down
out of 1250 observations (602/1250 = 48.16%). We could simply build a model
that predicts Direction randomly with the probability of assigning Down to be
48,16% and on average we will get 48,16% (???) accuracy. Our logistic model performs
only slightly better than this binomial random variable.

Another approach is to make a model that always predicts Up no matter what the
variables' values. This model will on average get 100% - 48,16% = 51.84% accuracy

I assumed that we are trying to predict Down days as usually it is the case that
predicting outcomes that we do not want to happen is more important than
predicting outcomes that we do want to happen. The sensitivity is very low.
"

