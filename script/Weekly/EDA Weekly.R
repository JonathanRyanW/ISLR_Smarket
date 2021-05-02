"We will do some EDA on the Weekly dataset. This dataset contains the weekly
percentage returns for the S&P 500 stock index between 1990 and 2010"

#Loading packages
library(ISLR)
library(dplyr)
library(ggplot2)

data <- Weekly
head(data)

any(is.na(data))
any(is.null(data))
"All the variables are the same with the Smarket data, which is the daily
percentage returns for the S&P 500 stock index between 2001 and 2005. The data
does not have any NA nor NULL value."

dim(data)

"There are 1089 observations."

table(data$Year)

"We have data for every week from 1991 to 2010. For some reason we only have 
47 observations for the year 1990."

str(data)
"Every variable is in their correct class. There is no need to clean the data as
it is already clean."

#Finding distribution for price changes
mean(data$Today)
sd(data$Today)

ggplot(data, aes(x = Today)) +
  geom_histogram() +
  theme_bw()

"It looks like Today is normally distributed with mean around 0. Today has a
standard deviation of 2.35. If we find the mean and sd of the Lag1-Lag5 variables
we should get similar results"

sapply(data, mean)
sapply(data, function(x){sd(c(x))})
"Lag1-Lag5 and Today all have mean between 14 and 15. Every single variable has
a similar standard deviation of 2.35. This is expected as each of the variable
holds basicly the same values."

#Finding trend of Volume throughout the years
tapply(data$Volume, data$Year, mean)
"Very strong increasing trend"

#Plotting the volumes throughout the years
Year <- c(1990:2010)
vol_year <- as.data.frame(cbind(year, c(tapply(data$Volume, data$Year, mean))))
vol_year <- rename(vol_year,  Volume = V2)
rm(Year)

ggplot(vol_year, aes(x = Year, y = Volume)) +
  geom_line() +
  theme_bw()

"Very clear increasing trend with a decrease in volume in the year 2010."

#Mean price movement for Up and Down days throughout the year.
tapply(data$Today, data$Direction, mean)
"We cab see that the mean magnitude of price change for Up and Down days are
very similar. We will find the value for every year"

tapply(data$Today, data[,c(1,9)], mean)
Year <- c(1990:2010)
magnitude <- as.data.frame(cbind(Year, tapply(data$Today, data[,c(1,9)], mean)))

ggplot(magnitude, aes(x = Year)) +
  geom_line(aes(y = Down), color = "red") +
  geom_line(aes(y = Up), color = "green") +
  ylab("Change (%)") +
  theme_bw()

"We can see that when the green line goes up the red line tends to go down and
vice versa. I wonder why this is the case."

