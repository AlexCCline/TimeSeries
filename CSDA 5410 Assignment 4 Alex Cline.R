GBIRevenue <- read.csv("~/CSDA 5410/GBIReveue.csv")
View(GBIRevenue)
library(forecast)

#holt winter time series______________________________________________________________________________________________________________
GBI.ts <-ts(GBIRevenue$Revenue, start = c(2008, 1), end = c(2012, 3), freq = 12)
Valid <- 15
Train <-length(GBI.ts) -Valid
train.ts <-window(GBI.ts, start = c(2008, 1), end = c(2008, Train))
valid.ts <-window(GBI.ts, start = c(2008, Train + 1), end = c(2008, Train + Valid))
hwin <- ets(train.ts, model = "ZNA")
hwin.pred <- forecast(hwin, h = Valid, level = 0)

plot(hwin.pred, ylim = c(100000, 7000000), ylab = "Revenue", xlab = "Month", bty = "l",
     xaxt = "n", xlim = c(2008, 2012.25), main = "Forecasting Revenue", flty = 2)
axis(1, at = seq(2008, 2012.25, 1), labels = format(seq(2008, 2012.25, 1)))
lines(hwin.pred$fitted, lwd = 2, col = "red")
lines(valid.ts)

#accuracy
accuracy(hwin.pred, valid.ts)

#forecast errors
hwin.pred$residuals
valid.ts - hwin.pred$mean

#frequency of forecast errors
hist(hwin.pred$residuals, ylab = "Frequency", xlab = "Forecast Error", bty ="l",
     main = "")


#holt winter time series forecast 1 year ahead________________________________________________________________________________________
GBI.ts <-ts(GBIRevenue$Revenue, start = c(2008, 1), end = c(2013, 3), freq = 12)
Valid <- 15
Train <-length(GBI.ts) -Valid
train.ts <-window(GBI.ts, start = c(2008, 1), end = c(2008, Train))
valid.ts2 <-window(GBI.ts, start = c(2008, Train + 1), end = c(2008, Train + Valid))
hwin <- ets(train.ts, model = "ZNA")
hwin.pred2 <- forecast(hwin, h = Valid, level = c(80,95))

plot(hwin.pred2, ylim = c(100000, 7000000), ylab = "Revenue", xlab = "Month", bty = "l",
     xaxt = "n", xlim = c(2008, 2013.25), main = "Forecasting Revenue", flty = 2)
axis(1, at = seq(2008, 2013.25, 1), labels = format(seq(2008, 2013.25, 1)))
lines(hwin.pred2$fitted, lwd = 2, col = "red")
lines(valid.ts2)

#accuracy
accuracy(hwin.pred2, valid.ts2)


#forecast errors
hwin.pred2$residuals
valid.ts2 - hwin.pred2$mean

#frequency of forecast errors
hist(hwin.pred2$residuals, ylab = "Frequency", xlab = "Forecast Error", bty ="l",
     main = "")


#Q5 Department Store Sales Holt Winter______________________________________________________________________________________________
DSS <- read.csv("~/CSDA 5410/DepartmentStoreSales.csv")
View(DSS)

DSS.ts <-ts(DSS$Sales, start = c(1, 1), end = c(29, 3), freq = 12)
Valid <- 4
Train <-length(DSS.ts) -Valid
train.ts <-window(DSS.ts, start = c(1, 1), end = c(1, Train))
valid.ts3 <-window(DSS.ts, start = c(1, Train + 1), end = c(1, Train + Valid))
hwin <- ets(train.ts, model = "ZNA", alpha = 0.2, beta = 0.15, gamma = 0.05)
hwin.pred3 <- forecast(hwin, h = Valid, level = 0)

plot(hwin.pred3, ylim = c(30000, 110000), ylab = "Sales", xlab = "Quarter", bty = "l",
     xaxt = "n", xlim = c(1, 29), main = "Forecasting Sales", flty = 2)
axis(1, at = seq(1, 29, 1), labels = format(seq(1, 29, 1)))
lines(hwin.pred3$fitted, lwd = 2, col = "red")
lines(valid.ts3)

#trend, season, random decomposed
library("spatialEco")
decomposedDSS <- decompose(DSS.ts, type = "multi")
plot(decomposedDSS, xlab = "Quarter", ylab = "Sales", bty = "l")

#accuracy
accuracy(hwin.pred3, valid.ts3)

#forecast errors
hwin.pred3$residuals
valid.ts3 - hwin.pred3$mean

#frequency of forecast errors
hist(hwin.pred3$residuals, ylab = "Frequency", xlab = "Forecast Error", bty ="l",
     main = "")




