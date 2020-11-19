SouvenirSales <- read.csv("~/CSDA 5330 Alex Cline/SouvenirSales.csv")
View(SouvenirSales)

#time series
Sales.ts <- ts(SouvenirSales$Sales, start = c(1995,1), end = c(1997,48), freq = 12)
plot(Sales.ts, xlab = "Time", ylab = "Sales", ylim = c(1000,90000), bty = "l")
lines(Sales.lm$fitted, lwd = 2)

#time series
install.packages("forecast")
library("forecast")
Sales.lm <- tslm(Sales.ts ~ trend + I(trend^2))
#par(mfrow = c(2,1)) #optional for comparison on one window
plot(Sales.ts, xlab = "Time", ylab = "Sales", ylim = c(1000,90000), bty = "l")
lines(Sales.lm$fitted, lwd = 2)
Sales.lm.zoom <- window(Sales.ts, start = c(1995,1), end = c(1997,48))
plot(Sales.lm.zoom, xlab = "Time", ylab = "Sales", ylim = c(1000,90000), bty = "l")
lines(Sales.lm$fitted, lwd = 2)

#trend, seasonal, random
install.packages("spatialEco")
library("spatialEco")
decomposedSales <- decompose(Sales.ts, type = "multi")
plot(decomposedSales, xlab = "Time", ylab = "Sales", ylim = c(1000,90000), bty = "l")

#logarithmic time series
Sales.ts2 <- ts(SouvenirSales, frequency = 12, start = c(1995,1))
SouvenirSales$Sales <- log(SouvenirSales$Sales)
plot.ts(SouvenirSales)


























