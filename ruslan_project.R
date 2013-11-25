#####
Import the data, convert to time series, plot the data
#####

#read data
setwd("Documents/STAT434/nsa")
goog.data = read.table("goog.csv", header=T, sep=",")

# turn into time series
goog.data$DateObj = as.Date(goog.data$Date, format="%d-%b-%y")

#get returns
close.levels = goog.data$Close
date.levels = goog.data$DateObj

close = log(close.levels)
ret = diff(close)


#plot levels with shade
plot(date.levels, close.levels, type = "l",ylab = "Google Closing Price", xlab = "Date", main = "Google Stock Price")
rect(date.levels[133],min(close.levels),date.levels[193],max(close.levels), col = rgb(0,1,0,0.2))


#plot returns
date = date.levels[-length(date.levels)]
plot(date, ret, type = "l", ylab = "Google Returns", xlab = "Date", main = "Google Returns")
rect(date[132],min(ret),date[192],max(ret), col = rgb(0,1,0,0.2))

# to determine if we need an ARCH model, let's check squared returns
plot(basket$DATE, basket$RETURNS^2, type = "l", main = "Squared Basket Returns", ylab = "Squared Returns", xlab = "Date")


# data appears to show some heteroskedaticity in its variance
# check ARCH model
require(tseries)
arch1 = garch(basket$RETURNS, order = c(0,1))

