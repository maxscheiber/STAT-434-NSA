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
rect(date.levels[130],min(close.levels),date.levels[150],max(close.levels), col = rgb(0,1,0,0.2))


#plot returns
date = date.levels[-length(date.levels)]
plot(date, ret, type = "l", ylab = "Google Returns", xlab = "Date", main = "Google Returns")
rect(date[130],min(ret),date[150],max(ret), col = rgb(0,1,0,0.2))

# to determine if we need an ARCH model, let's check squared returns
plot(basket$DATE, basket$RETURNS^2, type = "l", main = "Squared Basket Returns", ylab = "Squared Returns", xlab = "Date")

#get some dates
snowden = basket$DATE >= strptime("2013-06-06", format="%Y-%m-%d") & basket$DATE <= strptime("2013-06-20", format="%Y-%m-%d")

#from 6/4 to 6/17
rect(basket$DATE[741],min(ret^2),basket$DATE[800],max(ret^2), col = rgb(0,1,0,0.2))

# data appears to show some heteroskedaticity in squared returns
# check ARCH model
require(tseries)
arch1 = garch(basket$RETURNS, order = c(0,1))
summary(arch1)


#get basket data for just june
basket.returns.june = basket$RETURNS[728:868]
basket.dates.june = basket$DATE[728:868]
basket.vol.june = basket$VOL[728:868]
plot(basket.dates.june,basket.returns.june, type = "l", main = "June Squared Basket Returns", ylab = "Squared Returns", xlab = "Date")


#arch for june
arch1.june = garch(basket.returns.june, order = c(0,1))
summary(arch1.june)

#create indicator variables for june 6th and june 20th
june6 = basket$DATE[752]
june20 = basket$DATE[820]

indic.june6 = basket$RETURNS * 0
indic.june6[750:756] = 1

indic.june20 = basket$RETURNS * 0
indic.june20[820:826] = 1

ret = basket$RETURNS
ret.lag1 = lag(ret, 1)
vol=basket$VOL

##trying this with our volatility model
vol.full.june6 = lm(vol ~ indic.june6)
summary(vol.full.june6)

#just for june
june.june6 = lm (vol[728:868] ~ indic.june6[728:868])
summary(june.june6)

##june 20th on our volatility data
vol.full.june20 = lm(vol ~ indic.june20)
summary(vol.full.june20)

#just for june
june.june20 = lm (vol[728:868] ~ indic.june20[728:868])
summary(june.june20)

#plot(basket.dates.june, predict(june.june20), type = "l", col = "red", ylab = "", xlab = "")
#par(new = T)
#plot(basket.dates.june,basket.vol.june,type ="l", ylab = "Volatility", xlab="Date", main = "June Estimated (Red) Volatility and Actual Volatility")

#both june 6 and 20th
june.june6and20 = lm (vol[728:868] ~ indic.june6[728:868] +  indic.june20[728:868])
summary(june.june6and20)
