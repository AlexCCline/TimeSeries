SunSpot <- read.csv("~/CSDA 5410/SunSpot.csv")
View(SunSpot)
library(forecast)

#time series 1749-1904(record#1862 to round up to include year 1903)
SunSpot.ts <- ts(SunSpot$Sunspots, start = c(1749,1), end = c(1904,3), freq = 12)
plot(SunSpot.ts, ylim = c(0,300), ylab = "SunSpots",xlab = "Month", bty = "l",
     xaxt = "n",xlim=c(1749,1995), col="dark blue")

#validation and training
nValid <- 959
nTrain <- length(SunSpot.ts)-nValid
train.ts <- window(SunSpot.ts, start = c(1749,1),end = c(1749, nTrain))
valid.ts <- window(SunSpot.ts, start = c(1749, nTrain+1), end=c(1749, nTrain+nValid))
SunSpot.lm <- tslm(train.ts ~ trend + I(trend^2))
SunSpot.lm.pred <- forecast(SunSpot.lm, h = nValid, level = 0)

#plot with trend line
plot(SunSpot.lm.pred, ylim = c(0,300), ylab = "SunSpots", xlab="Month", bty="l",
     xaxt="n", xlim=c(1749,1995), main = "", flty = 2)
axis(1, at = seq(1749, 1995,1), labels=format(seq(1749,1995,1)))
lines(SunSpot.lm$fitted, lwd =2)
lines(valid.ts)

#trend, season, random
library("spatialEco")
decomposedSunSpot <- decompose(SunSpot.ts, type = "multi")
plot(decomposedSunSpot, xlab = "Month", ylab = "SunSpots", ylim = c(0,250), bty = "l")


#forecast errors
names(SunSpot.lm.pred)
SunSpot.lm.pred$residuals
valid.ts - SunSpot.lm.pred$mean

#frequency of forecast errors
hist(SunSpot.lm.pred$residuals, ylab = "Frequency", xlab = "Forecast Error", bty ="l",
     main = "")


#___________________________________________________________________________________
SouvenirSales <- read.csv("~/CSDA 5410/SouvenirSales.csv")
View(SouvenirSales)
library(forecast)
#time series and plot
SouvenirSales.ts <- ts(SouvenirSales$Sales, start = c(1995,1), end = c(2002,3), freq = 12)
plot(SouvenirSales.ts, ylim = c(0,200000), ylab = "Sales",xlab = "Date", bty = "l",
     xaxt = "n",xlim=c(1995,2002), col="dark blue")

#data partitioning
fixed.nValid <- 12
fixed.nTrain <- length(SouvenirSales.ts) - fixed.nValid
train2.ts <- window(SouvenirSales.ts, start = c(1995,1), end = c(1995,fixed.nTrain))
valid2.ts <- window(SouvenirSales.ts, start = c(1995, fixed.nTrain+1), end = c(1995,fixed.nTrain+fixed.nValid))
naive.pred <- naive(train2.ts, h = fixed.nValid)
snaive.pred <- snaive(train2.ts, h = fixed.nValid)
accuracy(naive.pred, valid2.ts)
accuracy(snaive.pred, valid2.ts)

#model and forecasting
Souvenir.lm <- tslm(train2.ts ~ trend + I(trend^2))
Souvenir.lm.pred <- forecast(Souvenir.lm, h = nValid, level = 0)

#plot with trend line
plot(Souvenir.lm.pred, ylim = c(0,200000), ylab = "Sales", xlab="Date", bty="l",
     xaxt="n", xlim=c(1995,2002), main = "Souvenir.lm.pred", flty = 2)
axis(1, at = seq(1995, 2002,1), labels=format(seq(1995,2002,1)))
lines(Souvenir.lm$fitted, lwd =2)
lines(valid2.ts)
#plot naive
plot(naive.pred, ylim = c(0,200000), ylab = "Sales", xlab="Date", bty="l",
     xaxt="n", xlim=c(1995,2002), main = "Naive", flty = 2)
axis(1, at = seq(1995, 2002,1), labels=format(seq(1995,2002,1)))
lines(Souvenir.lm$fitted, lwd =2)
lines(valid2.ts)
#plot snaive
plot(snaive.pred, ylim = c(0,200000), ylab = "Sales", xlab="Date", bty="l",
     xaxt="n", xlim=c(1995,2002), main = "Snaive", flty = 2)
axis(1, at = seq(1995, 2002,1), labels=format(seq(1995,2002,1)))
lines(Souvenir.lm$fitted, lwd =2)
lines(valid2.ts)


#forecast errors
names(Souvenir.lm.pred)
Souvenir.lm.pred$residuals
valid2.ts - Souvenir.lm.pred$mean

#frequency of forecast errors
hist(Souvenir.lm.pred$residuals, ylab = "Frequency", xlab = "Forecast Error", bty ="l",
     main = "")



