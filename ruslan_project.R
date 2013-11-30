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


#get some dates
snowden = basket$DATE >= strptime("2013-06-06", format="%Y-%m-%d") & basket$DATE <= strptime("2013-06-20", format="%Y-%m-%d")

#from 6/4 to 6/17
rect(basket$DATE[741],min(ret^2),basket$DATE[800],max(ret^2), col = rgb(0,1,0,0.2))



#######
#ARCH(1) model for whole data
#######

## To determine if we need an ARCH model, let's check squared returns
plot(basket$DATE, basket$RETURNS^2, type = "l", main = "Squared Basket Returns", ylab = "Squared Returns", xlab = "Date")

## Check an ARCH(1)
require(tseries)
arch1.full = garch(basket$RETURNS, order = c(0,1))
summary(arch1.full)



########
#Closer examination of June volatility
########

## Get basket data for June
basket.returns.june = basket$RETURNS[728:868]
basket.dates.june = basket$DATE[728:868]
basket.vol.june = basket$VOL[728:868]
vol.june = vol[728:868]
ret.june = ret[728:868]

## Plot June squared returns
plot(basket.dates.june,basket.returns.june^2, type = "l", main = "June Squared Basket Returns", ylab = "Squared Returns", xlab = "Date")

## Indicator variables for June 6 and June 20
june6 = basket$DATE[752]
june20 = basket$DATE[820]

indic.june6 = basket$RETURNS * 0
indic.june6[750:756] = 1

indic.june20 = basket$RETURNS * 0
indic.june20[820:826] = 1

ret = basket$RETURNS
ret.lag1 = lag(ret, 1)
vol=basket$VOL

# Regression of June 6th indicator on full basket data
vol.full.june6 = lm(vol ~ indic.june6)
summary(vol.full.june6)

# Regression of June 20th indicator on full basket data
vol.full.june20 = lm(vol ~ indic.june20)
summary(vol.full.june20)

# Regression of June 6th indicator on June basket data
june.june6 = lm (vol[728:868] ~ indic.june6[728:868])
summary(june.june6)

# Regression of June 20th indicator on June basket data
june.june20 = lm (vol[728:868] ~ indic.june20[728:868])
summary(june.june20)

# Regression of June 20th + June 6th indicators on June basket data
june.june6and20 = lm (vol[728:868] ~ indic.june6[728:868] +  indic.june20[728:868])
summary(june.june6and20)

## ARCH(1) for June
arch1.june = garch(ret.june, c=(0,1))
summary(arch1.june)

### June doesn't seem a like a particular volatile month
# June in black, two other random 1 month blocks in red + blue + green
plot(vol.june,type = 'l', ylim=c(min(vol.june),max(vol.june)),ylab="Basket Returns", main="June Returns vs 3 Random Months")
par(new=T)
plot(vol[300:440],type = 'l',col='red', lty=3, ylim=c(min(vol.june),max(vol.june)),axes = F,ylab="")
par(new=T)
plot(vol[440:580],type = 'l',col='blue', lty=3, ylim=c(min(vol.june),max(vol.june)), axes = F,ylab="")
par(new=T)
plot(vol[900:1040],type = 'l',col='green', lty=3, ylim=c(min(vol.june),max(vol.june)),ylab="",axes=F)


########
#Closer examination of July
########
vol.july = vol[868:1021]
ret.july = ret[868:1021]
dates.july= basket$DATE[868:1021]

## July squared returns
plot(dates.july,ret.july^2, type = "l", main = "July Squared Basket Returns", ylab = "Squared Returns", xlab = "Date")

## ARCH(1) for july
arch1.july = garch(vol.july, c=(0,1))
summary(arch1.june)

# Like June, doesn't seem particularly volatile
plot(vol.july,type = 'l', ylim=c(min(vol.july),max(vol.july)),ylab="Basket Returns", main="July Returns vs 3 Random Months")
par(new=T)
plot(vol[300:440],type = 'l',col='red', lty=3, ylim=c(min(vol.june),max(vol.june)),axes = F,ylab="")
par(new=T)
plot(vol[440:580],type = 'l',col='blue', lty=3, ylim=c(min(vol.june),max(vol.june)), axes = F,ylab="")
par(new=T)
plot(vol[100:240],type = 'l',col='green', lty=3, ylim=c(min(vol.june),max(vol.june)),ylab="",axes=F)

# Let's look at volatility from May - end of July
plot(basket$DATE[576:1021], vol[576:1021],type = 'l', ylim=c(min(vol.june),max(vol.june)),ylab="Basket Returns", main="Plot of volatility from May - End of July")
