#loading packages
library(dplyr)
library(ggplot2)
library(corrplot)

#loading the dataset
library(ISLR)
data <- Smarket

#glancing at the dataset
names(data)
dim(data)
head(data)

"We have 9 variables and of 1250 observations. A total of 11250 values in our
dataset. Descriptions for the variable can be seen in the README.R script."

#Checking NAs
any(is.na(data))
any(is.null(data))

"Our dataset does not contain any NA nor Null value."

str(data)

"All of the variables are in their correct class. Note that the year variable
is in the numeric class, not date. We do not need to do any data cleaning."

summary(data)
"Here is summary statistics for our dataset."

#Finding the distribution of today
ggplot(data, aes(x = Today)) +
  geom_histogram() +
  theme_bw()

mean(data$Today) #0.0031384
sd(data$Today) #1.136334

"It looks like Today variable is normally distributed with mean around 0 and
standard deviation around 1. Today is very similar with the standard normal
variable."

tapply(data$Today, data$Direction, mean)
"Oh this is interesting. The mean percentage change when the market is down and
up is really similar. Down: -0.8578140, Up: 0.8029738. Does this trend holds
throughout the year?"

tapply(data$Today, data[,c(1,9)], mean)
"This table provides more clarity. The magnitude of price changes are very
similar between the Down and Up days in every year. Very beautiful. I wonder
why this happens? "

ggplot(data, aes(x = Lag1)) +
  geom_histogram()

ggplot(data, aes(x = Lag2)) +
  geom_histogram()

ggplot(data, aes(x = Lag3)) +
  geom_histogram()

ggplot(data, aes(x = Lag4)) +
  geom_histogram()

ggplot(data, aes(x = Lag5)) +
  geom_histogram()

"It looks like all the stock price change variables are normally distributed."

ggplot(data, aes(x = Volume)) +
  geom_histogram() +
  theme_bw()

mean(data$Volume) #1.478305
sd(data$Volume) #0.3603571

"Volume does not seem to have a bell-shaped curved and thus it is nor normally
distributed. There might be differences in the distribution for every year."

for (i in c(2001:2005)){
  print(
    ggplot(filter(data, Year == i), aes(x = Volume)) +
      geom_histogram() +
      ggtitle(paste("Volume Distribution",i)) +
      theme_bw()
  )
}
rm(i)
"There are differences in the distribution of Volume for every year. This
phenomenon needs further investigating"

table(data$Year)
"There are 252 observations for every year except 2001, they only have 242."

table(data$Direction)
"More observations are on the Up category (648) than the Down category (602)."

#Finding correlations

corr <- cor(data[,-9])
corrplot(corr, method = "color")

rm(corr)

"We can see that there are almost no correlation among the variables. Except
between year and volume. They are positively correlated. It is interesting that
the Lag variables are not correlated with each other. One might think that
the today's price change should be correlated with the yesterday's and
tomorrow's price change, but this is not the case. At least they don't have a
linear relationship. Other types of relationship, for example quadratic, is
still a possibility."

cor(data$Year, data$Volume) #0.5390065
"This is a moderate correlation. Not too weak and not too strong. This
correlation indicates that as time goes by the volume of stock traded on each
day is increasing. Maybe because more and more people are joining the trading
community and because of the economic growth people have more money to use as
a capital to trade stocks."

plot(data$Volume)
tapply(data$Volume, data$Year, sum)
"We can see that volume increase from 2001 to 2002 but remains in the same level
until 2004. In 2005 the increase happens again."

tapply(data$Volume, data[,c(1,9)], sum)
"Here is the same table this time divided again by Direction. We can see that
in either direction the volume is increasing throughout the years. It is
interesting that in 2001 and 2002 the volume when the market is down is actually
higher than when the market is going up. But this is not the case for 2003-2005.
In those years the market volume is higher for the Up days. As far as i know the
stock market volume tends to be higher in the days where the market is going up
than it is when the market is going down. Are there any particular reason why
in 2001 and 2002 the volume is higher on the Down days?"

