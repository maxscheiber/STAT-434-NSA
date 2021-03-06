#######
##Import the data, convert to time series, plot the data
#######

## Read in the data + set some date variables
setwd("Documents/STAT434/nsa")

JUNE = 728
JULY = 868
AUGUST = 1021

## Consider Google's hourly bucket data, as generated by hourly.py from WRDS
goog.data = read.table("goog_hourly.csv", header=T, sep=",")

# Convert date strings of form "20130102" into R Date objects
# WARNING: this overwrites the DATE column of the data frame
goog.data$DATE = strptime(as.character(goog.data$DATE), format="%Y%m%d %H:%M:%S")

# Convert from price to log
goog.data$RETURNS = c(0, diff(log(goog.data$PRICE)))

# Find the variance from May 2013 onward
# First, we need to determine the initial index to start at
start_idx = 30
# which(goog.data$DATE == strptime("20130501 09:00:00", format="%Y%m%d %H:%M:%S"), arr.ind=T)

# Lagged volatility over 100 time deltas up to idx, inclusive.
# Assumes we start from the beginning of data.
volatility = function(data, idx) {
	EV = mean(data[(idx - start_idx + 1):idx])
	dist_sum_sqrd = 0
	for (i in (idx - start_idx + 1):idx) {
		dist_sum_sqrd = dist_sum_sqrd + (EV - data[i])^2
	}
	return (dist_sum_sqrd / start_idx)
}

goog.data$VOL = seq(0, 1, length(goog.data$RETURNS))

# Compute volatilities
for (i in start_idx:length(goog.data$RETURNS)) {
	goog.data$VOL[i] = volatility(goog.data$RETURNS, i)
}

# Volatility over June
which(goog.data$DATE == strptime("20130603 09:00:00", format="%Y%m%d %H:%M:%S"), arr.ind=T) # 729
which(goog.data$DATE == strptime("20130701 09:00:00", format="%Y%m%d %H:%M:%S"), arr.ind=T) # 869
plot(goog.data$DATE[729:869], goog.data$VOL[729:869], type="l", main="Volatility of GOOG in June 2013", xlab="Date", ylab="Variance")

# Plots of volatilities
plot(goog.data$DATE, goog.data$VOL, type="l", main="Volatility of GOOG", xlab="Date", ylab="Variance")
par(new=T)
plot(goog.data$DATE, goog.data$RETURNS, type="l", main="", xlab="", ylab="", col=4, axes="F")
# throw the volatility scale on the right hand side
axis(4, pretty(goog.data$RETURNS))
mtext("Stock Return (Dollars)", side=4)

# Zoomed in version of the graph
window = 650

plot(goog.data$DATE[window:(window+200)], goog.data$VOL[window:(window+200)], type="l", main="Volatility of GOOG", xlab="Date", ylab="Variance (Dollars^2)")

# now, let's consider a basket of four tech stocks
# First up is GOOG
goog.data = read.table("goog_hourly.csv", header=T, sep=",")
goog.data$DATE = strptime(as.character(goog.data$DATE), format="%Y%m%d %H:%M:%S")
goog.data$RETURNS = c(0, diff(log(goog.data$PRICE)))

# Then AAPL
aapl.data = read.table("aapl_hourly.csv", header=T, sep=",")
aapl.data$DATE = strptime(as.character(aapl.data$DATE), format="%Y%m%d %H:%M:%S")
aapl.data$RETURNS = c(0, diff(log(aapl.data$PRICE)))

# Then MSFT
msft.data = read.table("msft_hourly.csv", header=T, sep=",")
msft.data$DATE = strptime(as.character(msft.data$DATE), format="%Y%m%d %H:%M:%S")
msft.data$RETURNS = c(0, diff(log(msft.data$PRICE)))

# Then FB
fb.data = read.table("fb_hourly.csv", header=T, sep=",")
fb.data$DATE = strptime(as.character(fb.data$DATE), format="%Y%m%d %H:%M:%S")
fb.data$RETURNS = c(0, diff(log(fb.data$PRICE)))

# Let us construct an equal-weighted basket
basket = aapl.data
basket$PRICE = 0.25 * (c(goog.data$PRICE, goog.data$PRICE[length(goog.data$PRICE)]) + aapl.data$PRICE + msft.data$PRICE + fb.data$PRICE)
basket$SIZE = 0.25 * (c(goog.data$SIZE, goog.data$SIZE[length(goog.data$SIZE)]) + aapl.data$SIZE + msft.data$SIZE + fb.data$SIZE)
basket$RETURNS = c(0, diff(log(basket$PRICE)))

# Run basket volatility
basket$VOL = seq(0, 1, length(basket$RETURNS))

for (i in start_idx:length(basket$RETURNS)) {
	basket$VOL[i] = volatility(basket$RETURNS, i)
}

# And plot them
plot(basket$DATE, basket$VOL, type="l", main="Volatility of Tech Basket", xlab="Date", ylab="Variance (Dollars^2)")
par(new=T)
plot(basket$DATE, basket$RETURNS, type="l", main="", xlab="", ylab="", col=4, axes="F")
# throw the volatility scale on the right hand side
axis(4, pretty(basket$RETURNS))
mtext("Basket Return (%)", side=4)

window = 800

plot(basket$DATE[window:(window+200)], basket$VOL[window:(window+200)], type="l", main="Volatility of Tech Basket", xlab="Date", ylab="Variance (%)")

########## TO DELETE STARTS HERE ############

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

######## TO DELETE ENDS HERE ##########


#########
# Heteroskedasticity + ARCH/GARCH models for whole data set
#########

# Let's make a new date variable to make our plots NICER
plotdate2 = as.character(basket$DATE)
plotdate3 = strsplit(s, split = "2013-")
plotdate4 = unlist(plotdate3)
plotdate.final = as.Date(plotdate4, format = ("%m-%d"))
plotdate = as.Date(plotdate2, format = ("%Y-%m-%d"))


## Let's visually check whether there's heteroskedasticity over the whole period using returns-squared as a proxy for vol
## Looks like there is!

plotdate.full = plotdate[120:1022]

plot(basket$RETURNS[120:1022]^2, type = "h", main = "Squared Basket Returns", ylab = "Squared Returns", xlab = "Date", xaxt='n')

axis(1, at=120:length(basket$SIZE), labels = plotdate.full, tck = 0)

rect(JUNE,min(basket$RETURNS^2),AUGUST,max(basket$RETURNS^2), col = rgb(0,1,0,0.2))


## Let's take a look at this with our moving volatility model
plot(vol[150:1022], type = "l", main = "Basket Moving Volatility", ylab = "Moving Volatility", xlab = "Date", xaxt='n')

axis(1, at=150:length(vol), labels = plotdate[150:1022], tck = 0)

rect(JUNE,0,AUGUST,max(basket$RETURNS^2), col = rgb(0,1,0,0.2))


## Check an ARCH(1) suggests homoskedasticity
require(tseries)
arch1.full = garch(basket$RETURNS, order = c(0,1))
summary(arch1.full)


## ARCH(8) suggests heteroskedasticity emerging from the 6th and 7th lags of shocks
arch8.full = garch(basket$RETURNS, order = c(0,8))
summary(arch8.full)


## GARCH(1,1), which is basically an infinite order ARCH, shows that there is heteroskedasticity
spec = ugarchspec()
garch1.fit = ugarchfit(spec,basket$RETURNS)
garch1.fit



#########
##Closer examination of June volatility
#########

## Get basket data for June
ret = basket$RETURNS
vol=basket$VOL

vol.june = vol[JUNE]:JULY]
ret.june = ret[JUNE:JULY]
dates.june = basket$DATE[728:868]
plotdate.june = plotdate[JUNE:JULY]

## Plot June squared returns - a proxy for volatility
## Looks like there may be some extra volatility towards 
## the end of June. Potentially a smaller spike in early June.
plot(ret.june^2, type = "h", main = "June Squared Basket Returns", ylab = "Squared Returns", xlab = "Date",xaxt='n')

axis(1, at=1:length(vol.june), labels = plotdate.june, tck = 0)


## Plot our June moving volatility seems to show simlar results
plot(vol.june, type='l', main = "June Volatility", ylab= 'Volatility', xlab = 'Date', xaxt='n')
axis(1, at=1:length(vol.june), labels = plotdate.june, tck = 0)

## ARCH(1) for June suggests no heteroskedasticity
arch1.june = garch(ret.june, order = c(0,1))
summary(arch1.june)

##### Basic GARCH(1,1) suggests heteroskedasticity
require(rugarch)
spec = ugarchspec()
garch1.june = ugarchfit(data = ret.june, spec=spec)


## GARCH(1,1) using a different GARCH package also suggests heteroskedasticity
require(fGarch)
garch1.june.v2 = garchFit(formula = ~garch(1,1), data = ret[730:868])
summary(garch1.june.v2)

## June News Impact Curve [ignore this for the writeup]
#newsimpact.june = newsimpact(garch1.june)
#plot(newsimpact$zx, newsimpact$zy, main = "News Impact Curve for June Basket Returns", xlab="Standarized Lagged Shocks", ylab = "Conditional Variance",type='l')


### Indicator variable testing ### 

#note that this indicator goes from june 6 - june 7
indic.june6 = basket$RETURNS * 0
indic.june6[750:763] = 1

#note that this indicator goes from june 20 - june 21
indic.june20 = basket$RETURNS * 0
indic.june20[820:833] = 1

## Regression of June 6th indicator on full basket vol
## is not significant (using MA volatility)
vol.full.june6 = lm(vol ~ indic.june6)
summary(vol.full.june6)

## Regression of June 6th indicator on full basket returns squared
## is not significant
vol.full.june6.retsq = lm(ret^2 ~ indic.june6)
summary(vol.full.june6.retsq)

## Regression of June 20th indicator on full basket data
## is not significant
vol.full.june20 = lm(vol ~ indic.june20)
summary(vol.full.june20)

## Regression of June 20th indicator on full basket returns squared
## is not significant
vol.full.june20.retsq = lm(ret^2 ~ indic.june20)
summary(vol.full.june20.retsq)


### The above results suggest that June 6th/20th were not not huge dates for the year of 2013.

## Regression of June 6th indicator on June basket data
## not significant (using MA volatility)
june.june6 = lm (vol.june ~ indic.june6[JUNE:JULY])
summary(june.june6)

## Regression of June 6 indicator on June basket returns-squared is not significant
june.june6.retsq = lm (ret[JUNE:JULY]^2 ~ indic.june6[JUNE:JULY])
summary(june.june6.retsq)


## Regression of June 20th indicator on June basket data using our MA volatility is significant!!
june.june20 = lm (vol.june ~ indic.june20[JUNE:JULY])
summary(june.june20)


## Regression of June 20th indicator on June basket data, using returns-squared is not significant!!
june.june20.retsq = lm (ret[JUNE:JULY]^2 ~ indic.june20[JUNE:JULY])
summary(june.june20.retsq)

## Was there a regime change post-20th through the rest of June?
## Dummy that is 0 before June 20th, 1 after (until the end of june) - NO EFFECT!
regime.june20 = vol * 0
regime.june20[820:JULY] = 1
regime.june20.vol.reg = lm (vol ~ regime.june20)
summary(regime.june20.vol.reg)

## Let's check from June 20th - July 11th - significant with the vol model
regime.june20.july20 = vol * 0
regime.june20.july20[820:920] = 1
regime.june20.july20.reg = lm (vol ~ regime.june20.july20)
summary(regime.june20.july20.reg)

## how about with the returns-squared model? Not significant! 
regime.june20.july20.reg.retsq = lm (ret^2 ~ regime.june20.july20)
summary(regime.june20.july20.reg.retsq)

## Regime-change post june 6? Also not significant :)
regime.june6 = vol * 0
regime.june6[750:JULY] = 1
regime.june6.vol.reg = lm (vol ~ regime.june6)
summary(regime.june6.vol.reg)

## Regression of June 20th indicator on June + July is not significant (using MA vol)
june20.indic.endofjuly = lm(vol[JUNE:AUGUST] ~ indic.june20[JUNE:AUGUST])
summary(june20.indic.endofjuly)

## Regression of June 20th indicator on June + July is not significant (returns-squared)
june20.indic.endofjuly.retsq = lm(ret[JUNE:AUGUST]^2 ~ indic.june20[JUNE:AUGUST])
summary(june20.indic.endofjuly.retsq)

## Regression of both June 20th + June 6th indicators on June basket data - only the 20th is significant
june.june6and20 = lm (vol.june ~ indic.june6[728:868] +  indic.june20[728:868])
summary(june.june6and20)

## Let's do CUSUM for June
require(strucchange)
cusum.june.vol = efp(vol.june ~ 1, type = "OLS-CUSUM")
cusum.june.retsq = efp(ret.june^2 ~ 1, type = "OLS-CUSUM")

plot(cusum.june.vol, main= "Cumulative Sum of Recursive Residuals - June Volatility",xaxt='n',xlab='', xlim = c(0,1))

plot(cusum.june.retsq, xlab="Dates", main= "Cumulative Sum of Recursive Residuals - June Volatility")




## June doesn't seem a like a particularly volatile month
## June in black, two other random 1 month blocks in red + blue + green [IGNORE FOR WRITEUP]
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

## Plot of July moving volatility
plot(vol.july, type = "l", main = "July Moving Volatility", ylab = "Squared Returns", xlab = "Date", xaxt= 'n')

plotdate.july=plotdate[JULY:AUGUST]

axis(1, at=1:length(vol.july), labels = plotdate.july, tck = 0)

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

axis(1, at=1:length(basket$SIZE[JUNE:JULY]), labels = plotdate.june, tck = 0)

par(new = T)
plot(axes = F, basket$RETURNS[JUNE:JULY]^2, col = "red", type = 'l', ylab="",xlab='')

legend("topright", pch = c(19,19), col = c("red","black"), legend = c('Volume', 'Volatility'))


## Let's take a closer look at July - looks like July 15th - 30th is pretty volatile
plot(basket$SIZE[JULY:AUGUST], type="l", main="Hourly Volume of Trade in July", xlab="Date", ylab="Volume (mean number of shares)", xaxt= 'n',)

plotdate.july = plotdate[JULY:AUGUST]
axis(1, at=1:154, labels = plotdate.july, tck = 0)

# Did they really spike because of the NSA? Well, we know that GOOG had an earnings report on July 18th, and FB had an earnings report on July 24th.This reflects that we saw in the volatility.
#Earnings report dates: GOOG July 18th, FB July 24th, MSFT July 18th, Apple July 24th. Let's include 15th - 28th rectangle (our volatility is lagged by about 3 days)


# Now, let's do some trade volume analysis of our basket of tech stocks.
# First, let's plout out what this time series looks like.

plot(basket$DATE, basket$SIZE, type="l", main="Hourly Volume of Trade", xlab="Date", ylab="Volume (mean number of shares)")

# It looks like trade volume really spiked in July. Let's take a bit of a closer look.

plot(basket$DATE[JUNE:AUGUST], basket$SIZE[JUNE:AUGUST], type="l", main="Hourly Volume of Trade", xlab="Date", ylab="Volume (meann number of shares)")

# Did they really spike because of the NSA? Well, we know that GOOG had an
# earnings report on July 18th, and FB had an earnings report on July 24th.

# Here's GOOG.
plot(goog.data$DATE[JUNE:AUGUST], goog.data$SIZE[JUNE:AUGUST], type="l", main="Hourly Volume of Trade", xlab="Date", ylab="Volume (meann number of shares)")

# Here's FB.
plot(fb.data$DATE[JUNE:AUGUST], fb.data$SIZE[JUNE:AUGUST], type="l", main="Hourly Volume of Trade", xlab="Date", ylab="Volume (meann number of shares)")

# Well, that kind of kills a lot of our hopes and dreams.


