######
#Import the data, convert to time series, plot the data
#####

#read data
setwd("Documents/STAT434/nsa")
#goog.data = read.table("goog.csv", header=T, sep=",")

# turn into time series
#goog.data$DateObj = as.Date(goog.data$Date, format="%d-%b-%y")

#get returns
#close.levels = goog.data$Close
#date.levels = goog.data$DateObj

#close = log(close.levels)
#ret = diff(close)

#plot levels with shade
#plot(date.levels, close.levels, type = "l",ylab = "Google Closing Price", xlab = "Date", main = "Google Stock Price")
#rect(date.levels[130],min(close.levels),date.levels[150],max(close.levels), col = rgb(0,1,0,0.2))


#plot returns
#date = date.levels[-length(date.levels)]
#plot(date, ret, type = "l", ylab = "Google Returns", xlab = "Date", main = "Google Returns")
#rect(date[130],min(ret),date[150],max(ret), col = rgb(0,1,0,0.2))


#get some dates
#snowden = basket$DATE >= strptime("2013-06-06", format="%Y-%m-%d") & basket$DATE <= strptime("2013-06-20", format="%Y-%m-%d")

#from 6/4 to 6/17
#rect(basket$DATE[741],min(ret^2),basket$DATE[800],max(ret^2), col = rgb(0,1,0,0.2))



#######
#ARCH/GARCH models for whole data
#######

#let's make a new date variable to make our plots sexier
plotdate2 = as.character(basket$DATE)
plotdate = as.Date(plotdate2, format = ("%Y-%m-%d"))

## To determine if we need an ARCH model, let's check squared returns
## Looks like there may be spikes in the volatility
plotdate.full = plotdate[120:1022]

plot(basket$RETURNS[120:1022]^2, type = "l", main = "Squared Basket Returns", ylab = "Squared Returns", xlab = "Date", xaxt='n')

axis(1, at=120:length(basket$SIZE), labels = plotdate.full, tck = 0)

rect(JUNE,min(basket$RETURNS^2),AUGUST,max(basket$RETURNS^2), col = rgb(0,1,0,0.2))


## Check an ARCH(1) suggests that there is no heteroskedasticity
require(tseries)
arch1.full = garch(basket$RETURNS, order = c(0,1))
summary(arch1.full)

## ARCH(8) suggests heteroskedasticity emerging from the 6th and 7th
## lags
arch8.full = garch(basket$RETURNS, order = c(0,8))
summary(arch8.full)

## Check GARCH(1,1) shows that there is heteroskedasticity
spec = ugarchspec()
garch1.fit = ugarchfit(spec,basket$RETURNS)
garch1.fit



#########
##Closer examination of June volatility
#########

## Get basket data for June
ret = basket$RETURNS
vol=basket$VOL

vol.june = vol[728:868]
ret.june = ret[728:868]
dates.june = basket$DATE[728:868]

## ARCH(1) for June
arch1.june = garch(ret.june, order = c(0,1))
summary(arch1.june)

##### Basic GARCH(1,1) suggests heteroskedasticity
require(rugarch)
spec = ugarchspec()
garch1.june = ugarchfit(data = ret.june, spec=spec)

## June News Impact Curve
#newsimpact.june = newsimpact(garch1.june)
#plot(newsimpact$zx, newsimpact$zy, main = "News Impact Curve for June Basket Returns", xlab="Standarized Lagged Shocks", ylab = "Conditional Variance",type='l')

## GARCH(1,1) alternative fit for June also suggests heteroskedasticity
require(fGarch)
garch1.june.v2 = garchFit(formula = ~garch(1,1), data = ret[730:868])
summary(garch1.june.v2)

## Plot June squared returns - a proxy for volatility
## Looks like there may be some extra volatility towards 
## the end of june.
plot(ret.june^2, type = "h", main = "June Squared Basket Returns", ylab = "Squared Returns", xlab = "Date",xaxt='n')

axis(1, at=1:length(vol.june), labels = plotdate.june, tck = 0)

## Indicator variables for June 6 and June 20
june6 = basket$DATE[750]
june20 = basket$DATE[820]

#note that this indicator goes from june 6 - june 7
indic.june6 = basket$RETURNS * 0
indic.june6[750:764] = 1

#note that this indicator goes from june 20 - june 21
indic.june20 = basket$RETURNS * 0
indic.june20[820:834] = 1


## Regression of June 6th indicator on full basket data
# not significant
vol.full.june6 = lm(vol ~ indic.june6)
summary(vol.full.june6)

## Regression of June 20th indicator on full basket data
#not significant
vol.full.june20 = lm(vol ~ indic.june20)
summary(vol.full.june20)

## Regression of June 6th indicator on June basket data
## Not significant
june.june6 = lm (vol.june ~ indic.june6[728:868])
summary(june.june6)

## Regression of June 20th indicator on June basket data
## Is significant - could be an event
june.june20 = lm (vol.june ~ indic.june20[728:868])
summary(june.june20)

## Regression of June 20th indicator on June-end of July
june20.indic.endofjuly = lm(vol[JUNE:AUGUST] ~ indic.june20[JUNE:AUGUST])
summary(june20.indic.endofjuly)

## Regression of June 20th + June 6th indicators on June basket data - only the 20th is significant
june.june6and20 = lm (vol.june ~ indic.june6[728:868] +  indic.june20[728:868])
summary(june.june6and20)


## June doesn't seem a like a particularly volatile month
## June in black, two other random 1 month blocks in red + blue + green
#plot(vol.june,type = 'l', ylim=c(min(vol.june),max(vol.june)),ylab="Basket Returns", main="June Returns vs 3 Random Months")
#par(new=T)
#plot(vol[300:440],type = 'l',col='red', lty=3, ylim=c(min(vol.june),max(vol.june)),axes = F,ylab="")
#par(new=T)
#plot(vol[440:580],type = 'l',col='blue', lty=3, ylim=c(min(vol.june),max(vol.june)), axes = F,ylab="")
#par(new=T)
#plot(vol[900:1040],type = 'l',col='green', lty=3, ylim=c(min(vol.june),max(vol.june)),ylab="",axes=F)


##########
##Closer examination of July
#########
vol.july = vol[868:1021]
ret.july = ret[868:1021]
dates.july= basket$DATE[868:1021]

## July squared returns
plot(ret.july^2, type = "h", main = "July Squared Basket Returns", ylab = "Squared Returns", xlab = "Date", xaxt= 'n')

plotdate.july=plotdate[JULY:AUGUST]

axis(1, at=1:length(ret.july), labels = plotdate.july, tck = 0)

## ARCH(1) for july - significant!
arch1.july = garch(ret.july, order = c(0,1))
summary(arch1.july)

## ARCH(2) for july - 2nd lag is not significant
arch2.july = garch(ret.july, order = c(0,2))
summary(arch2.july)

## ARCH(8) for July suggests homoskedasticity
arch8.july = garch(ret.july, order = c(0,8))
summary(arch8.july)

## Garch(1,1) for July has different results
## Suggests homoskedastic variance
spec = ugarchspec()
garch1.july = ugarchfit(data = ret.july, spec=spec)
garch1.july

## Volatility seems to be centered around July 15-25th
## Likely because of AAPL, GOOG, FB earnings reports
## Plot of basket volatility + volatility of component part is below

fb.ret = fb.data$RETURNS
goog.ret = goog.data$RETURNS
msft.ret = msft.data$RETURNS
aapl.ret = aapl.data$RETURNS

plot(basket$DATE[868:1021], ret[868:1021]^2, type = 'h', main = "Basket & Component Parts Returns", xlab = "Date", ylab= "Returns")
par(new = T)
plot(basket$DATE[868:1021], fb.ret[868:1021]^2,axes=F,ylab="",xlab="",col = 'blue', lty = 3,type = 'h')
par(new = T)
plot(basket$DATE[868:1021], goog.ret[868:1021]^2,axes=F,ylab="",xlab="",col = 'green', lty = 3,type = 'h')
par(new = T)
plot(basket$DATE[868:1021], msft.ret[868:1021]^2,axes=F,ylab="",xlab="",col = 'red', lty = 3,type='h')
par(new = T)
plot(basket$DATE[868:1021], aapl.ret[868:1021]^2,axes=F,ylab="",xlab="",col = 'orange', lty = 3,type='h')


# Like June, doesn't seem particularly volatile
plot(vol.july,type = 'l', ylim=c(min(vol.july),max(vol.july)),ylab="Basket Returns", main="July Returns vs 3 Random Months")
par(new=T)
plot(vol[300:440],type = 'l',col='red', lty=3, ylim=c(min(vol.july),max(vol.july)),axes = F,ylab="")
par(new=T)
plot(vol[440:580],type = 'l',col='blue', lty=3, ylim=c(min(vol.july),max(vol.july)), axes = F,ylab="")
par(new=T)
plot(vol[100:240],type = 'l',col='green', lty=3, ylim=c(min(vol.june),max(vol.july)),ylab="",axes=F)

# Let's look at volatility from May - end of July
plot(basket$DATE[576:1021], vol[576:1021],type = 'l', ylim=c(min(vol.june),max(vol.june)),ylab="Basket Returns", main="Plot of volatility from May - End of July")

########
## Export basket returns
########

write.table(ret,"/Users/ruslan/Documents/STAT434/nsa/basket_returns.txt", sep="\t")


#######
## Volume analysis
#######
#Since volatility didn't work out, let's try volume
#It's an alternate way to measure the market's interest
#in the NSA

# First, let's see overall volume, with June + July highlighted - looks like it definitely spiked
plot(basket$SIZE, type="l", main="Hourly Volume of Trade", xlab="Date", ylab="Volume (mean number of shares)",xaxt='n')

axis(1, at=1:length(basket$SIZE), labels = plotdate, tck = 0)

rect(JUNE,min(basket$SIZE),AUGUST,max(basket$SIZE), col = rgb(0,1,0,0.2))

##Let's take a closer look at June - looks like somewhere around 20th is promising
#Black is volume, red is volatility 

plot(basket$SIZE[JUNE:JULY], type="l", main="Hourly Volume of Trade in June", xlab="Date", ylab="Volume (mean number of shares)", xaxt= 'n')

plotdate.june = plotdate[JUNE:JULY]
axis(1, at=1:length(basket$SIZE[JUNE:JULY]), labels = plotdate.june, tck = 0)

par(new = T)
plot(axes = F, basket$RETURNS[JUNE:JULY]^2, col = "red", type = 'l', ylab="",xlab='')

legend("topright", pch = c(19,19), col = c("red","black"), legend = c('Volume', 'Volatility'))


#par(new = T)
#plot(axes = F, vol[JUNE:JULY], col = "blue",type='l')

## Let's take a closer look at July - looks like July 15th - 30th is pretty volatile
plot(basket$SIZE[JULY:AUGUST], type="l", main="Hourly Volume of Trade in July", xlab="Date", ylab="Volume (mean number of shares)", xaxt= 'n',)

plotdate.july = plotdate[JULY:AUGUST]
axis(1, at=1:154, labels = plotdate.july, tck = 0)

# Did they really spike because of the NSA? Well, we know that GOOG had an earnings report on July 18th, and FB had an earnings report on July 24th.This reflects that we saw in the volatility.
#Earnings report dates: GOOG July 18th, FB July 24th, MSFT July 18th, Apple July 24th. Let's include 15th - 28th rectangle (our volatility is lagged by about 3 days)





