############################ Retail-Giant Sales Forecasting #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
# 5. Model evaluation

#####################################################################################

# 1. Business Understanding: 

#"Global Mart" is an online store super giant having worldwide operations.It takes orders and delivers across the globe. 
#It deals with all the major product categories - consumer, corporate & home office.
#The store caters to 7 different market segments and in 3 major categories.
#Now as a sales/operations manager, we need to finalise the plan for the next 6 months. 

## OBJECTIVE:

#The goal here is to forecast the sales and the demand for the next 6 months,
#that would help us to manage the revenue and inventory accordingly.

####################################################################################

# Data Understanding: 

# The data currently has the transaction level data, where each row represents a particular order made on the online store. 
# There are 27 attributes related to each such transaction. 
# The "Market" attribute has 7-factor levels representing the geographical market sector that the customer belongs to.
# The "Segment" attribute tells which of the 3 segments that customer belongs to. 

#####################################################################################

#  Data Load, Cleanup and Preparation: 

#Loading Neccessary libraries

#install.packages("forecast")
#install.packages("tseries")
#install.packages("graphics")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("lubridate")

library(forecast)
library(tseries)
library(graphics)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(plyr)


# Loading the Retail-Giant Sale data file into R
sales_data <- read.csv("Global Superstore.csv", header = T, stringsAsFactors = FALSE)

#Understanding Dimensions

dim(sales_data)
#51290 observation
#24 variables

#Structure of the dataset

str(sales_data)
# Here main continuous variables are : Sales,Quantity,Discount,Profit,Shipping.Cost
# Rest of the variables are categorical variables
# Order.Date and Ship.Date  are two time related data

#printing first few rows

head(sales_data)
#Here we can see different products ordered for different market and segments which also shows sales and profit for each.

#Exploring the data

summary(sales_data)
#Summary for few important continuous variabes are :
summary(sales_data$Sales)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.444    30.759    85.053   246.491   251.053 22638.480 

summary(sales_data$Profit)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-6599.98     0.00     9.24    28.61    36.81  8399.98  

summary(sales_data$Quantity)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   2.000   3.000   3.477   5.000  14.000 

#checking missing value

sum(is.na(sales_data))
#Since sum is 41296 so we have 41296 missing values

#We can also check this column wise by applying the is.na function for each column

sapply(sales_data, function(x) sum(is.na(x)))
#Here we can clearly see that Postal.Code contributes to all the missing values of 41296

percentMissingValues = (sum(is.na(sales_data$Postal.Code))/nrow(sales_data))*100
percentMissingValues
# Here missing values percentage for postal code = 80.51472 %

#Since missing value percent is very high and we don't need postal code column in our forecast analysis, 
#so we can remove the column Postal.Code


#Check if Row.Id is unique identifier
length(unique(sales_data$Row.ID))
#Count is 51290 which tells its unique row identifier.
#Lets remove it.

#Check if Order.ID is unique identifier
length(unique(sales_data$Order.ID))
#Count of unique order id is 25035 which tell we have multiple records for the same order id

#Check for duplicate rows

sum(duplicated(sales_data)) 
#0 - No duplicate rows

# It tells that combination  of fields along with order id is unique but order id itself is not unique
sales_data <- sales_data[,c("Market","Segment","Order.Date","Quantity","Sales","Profit")]

#Outlier treatment for important numeric variables which we will be using in our forcast analysis
#Function to find upper outlier values
findUpperOutlier <- function(x.var, df){with(df, quantile(x.var, 0.75) + (1.5 * (quantile(x.var, 0.75) - quantile(x.var, 0.25))))}

#Function to find lower outlier values
findLowerOutlier <- function(x.var, df){with(df, quantile(x.var, 0.25) - (1.5 * (quantile(x.var, 0.75) - quantile(x.var, 0.25))))}

#Outlier treatment for numeric variables using outlier functions findUpperOutlier and findLowerOutlier
#Here we identify the lower and upper outliers and set them to min or max valid values

#Outlier treatment for Sales variable
quantile(sales_data$Sales,seq(0,1,0.01))
ggplot(sales_data,aes(x=1:nrow(sales_data),y=Sales)) + geom_boxplot()
sales_data$Sales[which(sales_data$Sales > findUpperOutlier(sales_data$Sales,sales_data)[[1]])]<-findUpperOutlier(sales_data$Sales,sales_data)[[1]]
sales_data$Sales[which(sales_data$Sales < findLowerOutlier(sales_data$Sales,sales_data)[[1]])]<-findLowerOutlier(sales_data$Sales,sales_data)[[1]]
ggplot(sales_data,aes(x=1:nrow(sales_data),y=Sales)) + geom_boxplot()

#Outlier treatment for Profit variable
quantile(sales_data$Profit,seq(0,1,0.01))
ggplot(sales_data,aes(x=1:nrow(sales_data),y=Profit)) + geom_boxplot()
sales_data$Profit[which(sales_data$Profit > findUpperOutlier(sales_data$Profit,sales_data)[[1]])]<-findUpperOutlier(sales_data$Profit,sales_data)[[1]]
sales_data$Profit[which(sales_data$Profit < findLowerOutlier(sales_data$Profit,sales_data)[[1]])]<-findLowerOutlier(sales_data$Profit,sales_data)[[1]]
ggplot(sales_data,aes(x=1:nrow(sales_data),y=Profit)) + geom_boxplot()

#Outlier treatment for Quantity variable
quantile(sales_data$Quantity,seq(0,1,0.01))
ggplot(sales_data,aes(x=1:nrow(sales_data),y=Quantity)) + geom_boxplot()
sales_data$Quantity[which(sales_data$Quantity > findUpperOutlier(sales_data$Quantity,sales_data)[[1]])]<-findUpperOutlier(sales_data$Quantity,sales_data)[[1]]
sales_data$Quantity[which(sales_data$Quantity < findLowerOutlier(sales_data$Quantity,sales_data)[[1]])]<-findLowerOutlier(sales_data$Quantity,sales_data)[[1]]
ggplot(sales_data,aes(x=1:nrow(sales_data),y=Quantity)) + geom_boxplot()

#  Data Preparation: 

#Transform order date to date format
sales_data$Order.Date <- as.Date(as.POSIXlt(sales_data$Order.Date, format = "%d-%m-%Y"))
sales_data$Order.Date <- floor_date(as.Date(sales_data$Order.Date,"%d-%m-%Y"),"month")

sum(is.na(sales_data))
sales_data$MarketSegment <- str_replace(str_c(sales_data$Market,sales_data$Segment,sep = "_")," ","")
sales_data <- sales_data[,-c(1,2)]
##Extracting the month from order date    

analysis_data_grouped <- sales_data %>% group_by(MarketSegment,Order.Date) %>% summarise_all(sum)

###coefficient of variation calculation
coeff_var <- function(x){
  (sd(x)/mean(x))*100
}
##market_segment_avgprofit
market_segment_cv <- aggregate(Profit ~ MarketSegment,data = analysis_data_grouped,FUN = coeff_var)

p1 <- ggplot(market_segment_cv[c(1:10),], aes(x=as.factor(market_segment_cv[c(1:10),]$MarketSegment), y=as.numeric(market_segment_cv[c(1:10),]$Profit))) + geom_col(fill="tomato2") +   geom_label(aes(label=round(market_segment_cv[c(1:10),]$Profit,digits = 2)),vjust=-0.1) +  labs(x = "Market Segment", y = "Monthly Profit (Cov)")
p2 <- ggplot(market_segment_cv[c(11:21),], aes(x=as.factor(market_segment_cv[c(11:21),]$MarketSegment), y=as.numeric(market_segment_cv[c(11:21),]$Profit))) + geom_col(fill="tomato2") +   geom_label(aes(label=round(market_segment_cv[c(11:21),]$Profit,digits = 2)),vjust=-0.1) +  labs(x = "Market Segment", y = "Monthly Profit (Cov)")

grid.arrange(p1, p2, nrow=2)


df_names <- top_n(market_segment_cv,2,-Profit)$MarketSegment
df_names
###Two most consistently profitable segments are EU_Consumer and APAC_Consumer

###Seperating data for these two segments
groupby_MarketSegment_list <- dlply(analysis_data_grouped, .(MarketSegment))
##Creating data frames for the two most profitable amrket segments
assign(df_names[1], groupby_MarketSegment_list[[df_names[1]]][,-c(1,2)])
assign(df_names[2], groupby_MarketSegment_list[[df_names[2]]][,-c(1,2)])
EU_Consumer$Month <- 1:nrow(EU_Consumer)
APAC_Consumer$Month <- 1:nrow(APAC_Consumer)


################Model Building###########################
#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later

################Classical Model Building for Most Profitable Segment for Sales################

cols   <- c("red", "blue")
labels <- c("Raw", "Smoothed")
ylab1  <- c("Sales")
xlab1  <- c("Months from Jan 2011")
title  <- c("APAC Consumer Segment Sales: Jan 2011 - Dec 2014")

total_sales_1_timeser <- ts(APAC_Consumer$Sales)
timeser_sales_1 <- ts(APAC_Consumer[1:42,]$Sales)

#Smoothing the series - Moving Average Smoothing
plot(timeser_sales_1, main=title, xlab = xlab1, ylab = ylab1)
w <-2
smoothedseries <- stats::filter(timeser_sales_1, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser_sales_1)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

##Smoothing the series - HoltWinters Smoothing
##Please un-comment for testing HoltWinters Smoothing
##Commented as moving average has better results

# plot(timeser_sales_1, main=title, xlab = xlab1, ylab = ylab1)
# cols <- c("red", "blue", "green", "black")
# alphas <- c(0.02, 0.1, 0.8)
# labels <- c(paste("alpha =", alphas), "Original")
# for (i in seq(1,length(alphas))) {
#   smoothedseries <- HoltWinters(timeser_sales_1, alpha=alphas[i],
#                                 beta=FALSE, gamma=FALSE)
#   
#   lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
# }
# 
# legend("bottomleft", labels, col=cols, lwd=2)
# 
## #Here best alpha value is 0.8
# smoothedseries <- HoltWinters(timeser_sales_1, alpha=0.8,beta=FALSE, gamma=FALSE)

#Plot the smoothed time series
timevals_in <- APAC_Consumer[1:42,]$Month
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf)
#lmfit <- lm(Sales ~ Month, data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='yellow', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser_sales_1-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

#In case if below code gives error figure margins too large then enlarge the R studio plot area by using mouse drag
tsdiag(armafit)
armafit
#ARIMA(0,0,0) with zero mean 
#sigma^2 estimated as 25707777:  log likelihood=-417.9
#AIC=837.81   AICc=837.91   BIC=839.55

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
#Dickey-Fuller = -4.5955, Lag order = 3, p-value = 0.01
#Since p value is 0.01, so we can reject null hypothesis that its not stationary
#and can conclude that its stationary.

kpss.test(resi)
#KPSS Level = 0.031728, Truncation lag parameter = 1, p-value = 0.1
#Since p value is > 0.05 so we fail to reject null hypothesis and can conclude it is stationary

################Classic Model Evaluation  for Most Profitable Segment for Sales###############
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- APAC_Consumer[43:48,]$Month

global_pred_out <- predict(lmfit,data.frame(Month = timevals_out))

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(global_pred_out,APAC_Consumer[43:48,]$Sales)[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))

plot(total_sales_1_timeser, col = "black")
lines(class_dec_pred, col = "red")

#Forecast result using Moving Average Smoothing
#MAPE is 22.0445 when MA smoothing is used with window width = 2 and following model
#lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
# + Month, data=smootheddf)

#MAPE is 23.57189 when MA smoothing is used with window width = 1 and following model
#lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
# + Month, data=smootheddf)

#MAPE is 28.18505 when MA smoothing is used with window width = 2 and following model
#lmfit <- lm(Sales ~ Month, data=smootheddf)

#MAPE is 28.38564 when MA smoothing is used with window width = 1 and following model
#lmfit <- lm(Sales ~ Month, data=smootheddf)

#Forecase result using HoltWinters Smoothing
#MAPE is 28.38564 when HoltWinters smoothing is used with following model
#lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
# + Month, data=smootheddf)

#So the best one is the first one with MA smoothing and MAPE 22.0445
global_forecast <- predict(lmfit,data.frame(Month = c(49,50,51,52,53,54)))
global_forecast
#############Model Building and Evaluation using ARIMA for Most Profitable Segment for Sale
#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(timeser_sales_1)
autoarima
#ARIMA(0,1,0) 
#sigma^2 estimated as 74043307:  log likelihood=-429.64
#AIC=861.28   AICc=861.38   BIC=862.99

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser_sales_1 - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
#Dickey-Fuller = -4.3515, Lag order = 3, p-value = 0.01
#Since p value is 0.01, so we can reject null hypothesis that its not stationary
#and can conclude that its stationary.

kpss.test(resi_auto_arima)
#KPSS Level = 0.026503, Truncation lag parameter = 1, p-value = 0.1
#Since p value is > 0.05 so we fail to reject null hypothesis and can conclude it is stationary

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,APAC_Consumer[43:48,]$Sales)[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_sales_1_timeser, col = "black")
lines(auto_arima_pred, col = "red")

#MAPE is 27.88582 when ARIMA fit is used


################Model Building for Second Most Profitable Segment for Sales####################

cols   <- c("red", "blue")
labels <- c("Raw", "Smoothed")
ylab1  <- c("Sales")
xlab1  <- c("Months from Jan 2011")
title  <- c("Second Most Profitable Segment for Sale: Jan 2011 to Dec 2014")

total_sales_2_timeser <- ts(EU_Consumer$Sales)
timeser_sales_2 <- ts(EU_Consumer[1:42,]$Sales)

#Smoothing the series - Moving Average Smoothing
plot(timeser_sales_2, main=title, xlab = xlab1, ylab = ylab1)
w <-1
smoothedseries <- stats::filter(timeser_sales_2, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser_sales_2)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#smootheddf 1 and 42 values are null. Lets populate them from timeseries
smootheddf$Sales[1] <- EU_Consumer[1:42,]$Sales[1]
#smootheddf$Sales[2] <- sales_2_Indata$Sales[2]
#smootheddf$Sales[41] <- sales_2_Indata$Sales[41]
smootheddf$Sales[42] <- EU_Consumer[1:42,]$Sales[42]

##Smoothing the series - HoltWinters Smoothing
##Please un-comment for testing HoltWinters Smoothing
##Commentes as moving average has better results

# plot(timeser_sales_2, main=title, xlab = xlab1, ylab = ylab1)
# cols <- c("red", "blue", "green", "black")
# alphas <- c(0.02, 0.1, 0.8)
# labels <- c(paste("alpha =", alphas), "Original")
# for (i in seq(1,length(alphas))) {
#   smoothedseries <- HoltWinters(timeser_sales_2, alpha=alphas[i],
#                                 beta=FALSE, gamma=FALSE)
# 
#   lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
# }
# 
# legend("bottomleft", labels, col=cols, lwd=2)
# 
# #Here best alpha value is 0.8
# smoothedseries <- HoltWinters(timeser_sales_2, alpha=0.8,beta=FALSE, gamma=FALSE)

#Plot the smoothed time series
timevals_in <- EU_Consumer[1:42,]$Month
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

#lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
#            + Month, data=smootheddf)
lmfit <- lm(Sales ~ Month, data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='yellow', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser_sales_2-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

#In case if below code gives error figure margins too large then enlarge the R studio plot area by using mouse drag
tsdiag(armafit)
armafit
#ARIMA(2,0,0) with zero mean 
#sigma^2 estimated as 23301495:  log likelihood=-415.28
#AIC=836.56   AICc=837.19   BIC=841.77

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
#Dickey-Fuller = -7.0613, Lag order = 3, p-value = 0.01
#Since p value is 0.01, so we can reject null hypothesis that its not stationary
#and can conclude that its stationary.

kpss.test(resi)
#KPSS Level = 0.023653, Truncation lag parameter = 1, p-value = 0.1
#Since p value is > 0.05 so we fail to reject null hypothesis and can conclude it is stationary

################Classic Model Evaluation  for Most Profitable Segment for Sales###############
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

sales_2_Outdata <- EU_Consumer[43:48,]
timevals_out <- EU_Consumer[43:48,]$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))


#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(global_pred_out,EU_Consumer[43:48,]$Sales)[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
class_dec_pred
plot(total_sales_2_timeser, col = "black")
lines(class_dec_pred, col = "red")

#Forecase result using Moving Average Smoothing
#MAPE is 28.44215 when MA smoothing is used with window width = 2 and following model
#lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
# + Month, data=smootheddf)

#MAPE is  28.29182 when MA smoothing is used with window width = 1 and following model
#lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
# + Month, data=smootheddf)

#MAPE is 27.19629 when MA smoothing is used with window width = 2 and following model
#lmfit <- lm(Sales ~ Month, data=smootheddf)

#MAPE is 27.21197 when MA smoothing is used with window width = 1 and following model
#lmfit <- lm(Sales ~ Month, data=smootheddf)

#Forecase result using HoltWinters Smoothing
#MAPE is 27.2172 when HoltWinters smoothing is used with window width = 1 and following model
#lmfit <- lm(Sales ~ Month, data=smootheddf)

#So the best one is the first one with MA smoothing and MAPE 27.21197
global_forecast <- predict(lmfit,data.frame(Month = c(49,50,51,52,53,54)))
global_forecast
#############Model Building and Evaluation using ARIMA for Most Profitable Segment for Sale
#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(timeser_sales_2)
autoarima
#ARIMA(2,1,0)  
#sigma^2 estimated as 65060229:  log likelihood=-426.42
#AIC=858.84   AICc=859.49   BIC=863.98

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser_sales_2 - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
#Dickey-Fuller = -3.8781, Lag order = 3, p-value = 0.02405
#Since p value is 0.02405, so we can reject null hypothesis that its not stationary
#and can conclude that its stationary.

kpss.test(resi_auto_arima)
#KPSS Level = 0.053455, Truncation lag parameter = 1, p-value = 0.1
#Since p value is > 0.05 so we fail to reject null hypothesis and can conclude it is stationary

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,EU_Consumer[43:48,]$Sales)[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_sales_2_timeser, col = "black")
lines(auto_arima_pred, col = "red")

#MAPE is 27.94181 when ARIMA fit is used


################Classical Model Building for Most Profitable Segment for Quantity######

cols   <- c("red", "blue")
labels <- c("Raw", "Smoothed")
ylab1  <- c("Quantity")
xlab1  <- c("Months from Jan 2011")
title  <- c("Most Profitable Segment for Quantity: Jan 2011 to Dec 2014")

total_quan_1_timeser <- ts(APAC_Consumer$Quantity)
timeser_quan_1 <- ts(APAC_Consumer[1:42,]$Quantity)

#Smoothing the series - Moving Average Smoothing
plot(timeser_quan_1, main=title, xlab = xlab1, ylab = ylab1)
w <-1
smoothedseries <- stats::filter(timeser_quan_1, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser_quan_1)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

##Smoothing the series - HoltWinters Smoothing
##Please un-comment for testing HoltWinters Smoothing
##Commentes as moving average has better results

# plot(timeser_quan_1, main=title, xlab = xlab1, ylab = ylab1)
# cols <- c("red", "blue", "green", "black")
# alphas <- c(0.02, 0.1, 0.8)
# labels <- c(paste("alpha =", alphas), "Original")
# for (i in seq(1,length(alphas))) {
#   smoothedseries <- HoltWinters(timeser_quan_1, alpha=alphas[i],
#                                 beta=FALSE, gamma=FALSE)
# 
#   lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
# }
# 
# legend("bottomleft", labels, col=cols, lwd=2)
# 
# # #Here best alpha value is 0.8
# smoothedseries <- HoltWinters(timeser_quan_1, alpha=0.8,beta=FALSE, gamma=FALSE)

#Plot the smoothed time series
timevals_in <- APAC_Consumer[1:42,]$Month
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')

#Now, let's fit a additive/linear model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,1) + cos(0.5*Month) * poly(Month,1)
            + Month, data=smootheddf)
#lmfit <- lm(Quantity ~ Month, data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='yellow', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser_quan_1-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

#In case if below code gives error figure margins too large then enlarge the R studio plot area by using mouse drag
tsdiag(armafit)
armafit
#ARIMA(0,0,0) with zero mean 
#sigma^2 estimated as 25707777:  log likelihood=-417.9
#AIC=837.81   AICc=837.91   BIC=839.55

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
#Dickey-Fuller = -3.856, Lag order = 3, p-value =0.02492
#Since p value is 0.02492, so we can reject null hypothesis that its not stationary
#and can conclude that its stationary.

kpss.test(resi)
#KPSS Level = 0.15772, Truncation lag parameter = 1, p-value = 0.1
#Since p value is > 0.05 so we fail to reject null hypothesis and can conclude it is stationary

################Classic Model Evaluation  for Most Profitable Segment for Quantity###############
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- APAC_Consumer[43:48,]$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
global_pred_out
#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(global_pred_out,APAC_Consumer[43:48,]$Quantity)[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_quan_1_timeser, col = "black")
lines(class_dec_pred, col = "red")

#Forecase result using Moving Average Smoothing

#MAPE is 29.14779 when MA smoothing is used with window width = 1 and following model
#lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,1) + cos(0.5*Month) * poly(Month,1)
# + Month, data=smootheddf)

#MAPE is 29.24822 when MA smoothing is used with window width = 1 and following model
#lmfit <- lm(Quantity ~ Month, data=smootheddf)

#Forecase result using HoltWinters Smoothing
#MAPE is 29.14779 when HoltWinters smoothing is used with following model
#lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,1) + cos(0.5*Month) * poly(Month,1)
# + Month, data=smootheddf)

#So the best one is the first one with MA smoothing and MAPE 29.14779

#So, that was classical decomposition, now let's do an ARIMA fit
timeser_quan_1
autoarima <- auto.arima(timeser_quan_1)
autoarima
#ARIMA(0,1,0) 
#sigma^2 estimated as 24749:  log likelihood=-265.57
#AIC=533.13   AICc=533.23   BIC=534.84

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser_quan_1 - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
#Dickey-Fuller = -4.3322, Lag order = 3, p-value = 0.01
#Since p value is 0.01, so we can reject null hypothesis that its not stationary
#and can conclude that its stationary.

kpss.test(resi_auto_arima)
#KPSS Level = 0.030512, Truncation lag parameter = 1, p-value = 0.1
#Since p value is > 0.05 so we fail to reject null hypothesis and can conclude it is stationary

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,APAC_Consumer[43:48,]$Quantity)[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_quan_1_timeser, col = "black")
lines(auto_arima_pred, col = "red")

#MAPE is 25.90937 when ARIMA fit is used

predict(autoarima, n.ahead = 12)
################Classical Model Building for Second Most Profitable Segment for Quantity############

cols   <- c("red", "blue")
labels <- c("Raw", "Smoothed")
ylab1  <- c("Quantity")
xlab1  <- c("Months from Jan 2011")
title  <- c("Second Most Profitable Segment for Quantity: Jan 2011 to Dec 2014")

total_quan_2_timeser <- ts(EU_Consumer$Quantity)
timeser_quan_2 <- ts(EU_Consumer[1:42,]$Quantity)

#Smoothing the series - Moving Average Smoothing
plot(timeser_quan_2, main=title, xlab = xlab1, ylab = ylab1)
w <-1
smoothedseries <- stats::filter(timeser_quan_2, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser_quan_2)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

##Smoothing the series - HoltWinters Smoothing
##Please un-comment for testing HoltWinters Smoothing
##Commentes as moving average has better results

# plot(timeser_quan_2, main=title, xlab = xlab1, ylab = ylab1)
# cols <- c("red", "blue", "green", "black")
# alphas <- c(0.02, 0.1, 0.8)
# labels <- c(paste("alpha =", alphas), "Original")
# for (i in seq(1,length(alphas))) {
#   smoothedseries <- HoltWinters(timeser_quan_2, alpha=alphas[i],
#                                 beta=FALSE, gamma=FALSE)
# 
#   lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
# }
# 
# legend("bottomleft", labels, col=cols, lwd=2)
# 
# # #Here best alpha value is 0.8
# smoothedseries <- HoltWinters(timeser_quan_2, alpha=0.8,beta=FALSE, gamma=FALSE)

#Plot the smoothed time series
timevals_in <- EU_Consumer[1:42,]$Month
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')

#Now, let's fit a additive/linear model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf)
#lmfit <- lm(Quantity ~ Month, data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='yellow', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser_quan_2-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

#In case if below code gives error figure margins too large then enlarge the R studio plot area by using mouse drag
tsdiag(armafit)
armafit
#ARIMA(2,0,0) with zero mean  
#sigma^2 estimated as 7025:  log likelihood=-245.13
#AIC=496.27   AICc=496.9   BIC=501.48

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
#Dickey-Fuller = -6.6362, Lag order = 3, p-value = 0.01
#Since p value is 0.01, so we can reject null hypothesis that its not stationary
#and can conclude that its stationary.

kpss.test(resi)
#KPSS Level = 0.023472, Truncation lag parameter = 1, p-value = 0.1
#Since p value is > 0.05 so we fail to reject null hypothesis and can conclude it is stationary

################Classic Model Evaluation  for Most Profitable Segment for Sales###############
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months


timevals_out <- EU_Consumer[43:48,]$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
global_pred_out
#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(global_pred_out,EU_Consumer[43:48,]$Quantity)[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_quan_2_timeser, col = "black")
lines(class_dec_pred, col = "red")

#Forecase result using Moving Average Smoothing

#MAPE is 29.73104 when MA smoothing is used with window width = 1 and following model
#lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,1) + cos(0.5*Month) * poly(Month,1)
# + Month, data=smootheddf)

#Forecase result using HoltWinters Smoothing
#MAPE is 57.94372 when HoltWinters smoothing is used with following model
#lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
# + Month, data=smootheddf)

#So the best one is the first one with MA smoothing and MAPE 29.73104
global_pred_out <- predict(lmfit,data.frame(Month =c(49,50,51,52,53,54)))
global_pred_out
#############Model Building and Evaluation using ARIMA for Second Most Profitable Segment for Quantity
#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(timeser_quan_2)
autoarima
#ARIMA(2,1,0)
#sigma^2 estimated as 20466:  log likelihood=-261.19
#AIC=528.38   AICc=529.03   BIC=533.52

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser_quan_2 - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
#Dickey-Fuller = -3.5842, Lag order = 3, p-value = 0.04621
#Since p value is0.04621, so we can reject null hypothesis that its not stationary
#and can conclude that its stationary.

kpss.test(resi_auto_arima)
#KPSS Level = 0.048042, Truncation lag parameter = 1, p-value = 0.1
#Since p value is > 0.05 so we fail to reject null hypothesis and can conclude it is stationary

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,EU_Consumer[43:48,]$Quantity)[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_quan_2_timeser, col = "black")
lines(auto_arima_pred, col = "red")

#MAPE is 29.88284 when ARIMA fit is used

######################Summary of all 4 forcasts########################################

#Most Profitable Segment is : Consumer with Most Profitable Market :APAC  and corresponding COV is:  47.9307162042111"
#Second Most Profitable Segment is :Consumer with Second Most Profitable Market:  EU and corresponding COV is:  53.6190498372746"

#######################################################################################
#Forecast for Most profitable Segment(Segment=Consumer and Market=APAC) for Sales
#######################################################################################

#Classical Decomposition Result
#MAPE is 22.0445 when MA smoothing is used with window width = 2 and following model
#lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
# + Month, data=smootheddf)

#Forecase result using HoltWinters Smoothing
#MAPE is 28.38564 when HoltWinters smoothing is used with following model
#lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
# + Month, data=smootheddf)

#ARIMA Result
#MAPE is 27.88582 when ARIMA fit is used

#So the best one is the first one with classical decomposition with MAPE 22.0445 when MA smoothing is used.

#######################################################################################
#Forecast for Second Most profitable Segment(Segment=Consumer and Market=EU) for Sales
#######################################################################################

#Classical Decomposition Result
#MAPE is 27.21197 when MA smoothing is used with window width = 1 and following model
#lmfit <- lm(Sales ~ Month, data=smootheddf)

#Forecase result using HoltWinters Smoothing
#MAPE is 27.2172 when HoltWinters smoothing is used with window width = 1 and following model
#lmfit <- lm(Sales ~ Month, data=smootheddf)

#ARIMA Result
#MAPE is 27.94181 when ARIMA fit is used

#So the best one is the first one with classical decomposition and MA smoothing and MAPE 27.21197

#######################################################################################
#Forecast for Most profitable Segment(Segment=Consumer and Market=APAC) for Quantity
#######################################################################################

#Classical Decomposition Result
#MAPE is 29.14779 when MA smoothing is used with window width = 1 and following model
#lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,1) + cos(0.5*Month) * poly(Month,1)
# + Month, data=smootheddf)

#Forecase result using HoltWinters Smoothing
#MAPE is 29.14779 when HoltWinters smoothing is used with following model
#lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,1) + cos(0.5*Month) * poly(Month,1)
# + Month, data=smootheddf)

#ARIMA Result
#MAPE is 25.90937 when ARIMA fit is used


#So the best one is the one using ARIMA having MAPE 25.90937

###########################################################################################
#Forecast for Second Most profitable Segment(Segment=Consumer and Market=EU) for Quantity
###########################################################################################

#Classical Decomposition Result
#MAPE is 29.73104 when MA smoothing is used with window width = 1 and following model
#lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,1) + cos(0.5*Month) * poly(Month,1)
# + Month, data=smootheddf)

#Forecase result using HoltWinters Smoothing
#MAPE is 57.94372 when HoltWinters smoothing is used with following model
#lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
# + Month, data=smootheddf)


#ARIMA Result
#MAPE is 29.88284 when ARIMA fit is used

#So the best one is the first one with classical decomposition using MA smoothing having MAPE 29.73104

###############################################################################################################