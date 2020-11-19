Sept11Trav <- read.csv("C:/Users/accli/Downloads/Sept11Travel (1).csv")
View(Sept11Trav)
library(forecast)


Sept11Trav.ts <-ts(Sept11Trav$Air.RPM, start = c(1990, 1), end = c(2004.25, 3), freq = 12)
Valid <-30
Train <-length(Sept11Trav.ts) -Valid
train.ts <-window(Sept11Trav.ts, start = c(1990, 1), end = c(1990, Train))
valid.ts <-window(Sept11Trav.ts, start = c(1990, Train + 1), end = c(1990, Train + Valid))
hwin <- ets(train.ts, model = "MAA")
hwin.pred <- forecast(hwin, h = Valid, level = 0)

plot(hwin.pred, ylim = c(0, 180), ylab = "Air RPM", xlab = "Month", bty = "l",
     xaxt = "n", xlim = c(1990, 2004.25), main = "Forecasting Air RPM", flty = 2)
axis(1, at = seq(1990, 2004.25, 1), labels = format(seq(1990, 2004.25, 1)))
lines(hwin.pred$fitted, lwd = 2, col = "red")
lines(valid.ts)


#_________________________Gas Usage_________________________________________
Gas <- read.csv("C:/Users/accli/Downloads/Electric Usgae Forecasting.csv")
View(Gas)
library(zoo)

#Gas Use moving average
Gas.ts <-ts(Gas$Gas.Use, start = c(2005, 1), end = c(2007, 3), freq = 12)
MovAve.trailing <-rollmean(Gas.ts, k = 12, align = "right")
MovAve.centered <-ma(Gas.ts, order = 12)
plot(Gas.ts, ylim = c(0, 260),  ylab = "Gas Use", xlab = "Date",
     bty = "l", xaxt = "n", xlim = c(2005,2007.25),
     main = "Forecast Gas Usage with Right and Center Moving Average", 
     col = "dark red")
axis(1, at= seq(2005, 2007.25, 1), labels= format(seq(2005, 2007.25, 1)))
lines(MovAve.centered, lwd = 2, col = "blue")
lines(MovAve.trailing, lwd = 2, lty = 2, col = "green")
legend("bottomright",
       c( "Gas Use","Centered Moving Average", 
          "Trailing Moving Average"), lty=c(1,1,2), lwd=c(1,2,2), bty = "n") 

Valid <- 7
Train <-length(Gas.ts) -Valid
train.ts <-window(Gas.ts, start = c(2005, 1), end = c(2005, Train))
valid.ts <-window(Gas.ts, start = c(2005, Train + 1), end = c(2005, Train + Valid))
MovAve.trailing <-rollmean(train.ts, k = 12, align = "right")
last.MovAve <-tail(MovAve.trailing, 1)
MovAve.trailing.pred <-ts(rep(last.MovAve, Valid), start = c(2005, Train + 1),
                          end = c(2005, Train + Valid), freq = 12)

plot(train.ts, ylim = c(0, 260),  ylab = "Gas Use",
     xlab = "Date", bty = "l", xaxt = "n", xlim = c(2005,2007.25),
     main = "Forecasting Validation", col ="dark red")
axis(1, at = seq(2005, 2007.25, 1), labels = format(seq(2005, 2007.25, 1)))
lines(MovAve.trailing, lwd = 2, col = "blue")
lines(MovAve.trailing.pred, lwd = 2, col = "green", lty = 2)
lines(valid.ts)
accuracy(MovAve.trailing.pred, valid.ts)

#naive and snaive
fixed.nValid <- 7
fixed.nTrain <- length(Gas.ts) - fixed.nValid
train2.ts <- window(Gas.ts, start = c(2005,1), end = c(2005,fixed.nTrain))
valid2.ts <- window(Gas.ts, start = c(2005, fixed.nTrain+1), end = c(2005,fixed.nTrain+fixed.nValid))
naive.pred <- naive(train2.ts, h = fixed.nValid)
snaive.pred <- snaive(train2.ts, h = fixed.nValid)
accuracy(naive.pred, valid2.ts)
accuracy(snaive.pred, valid2.ts)


#model and forecasting
Gas.lm <- tslm(train2.ts ~ trend + I(trend^2))
Gas.lm.pred <- forecast(Gas.lm, h = nValid, level = 0)

#plot with trend line
plot(Gas.lm.pred, ylim = c(0,260), ylab = "Gas Use", xlab="Date", bty="l",
     xaxt="n", xlim=c(2005,2007.25), main = "Gas.lm.pred", flty = 2)
axis(1, at = seq(2005, 2007.25,1), labels=format(seq(2005,2007.25,1)))
lines(Gas.lm$fitted, lwd =2)
lines(valid2.ts)
#plot naive
plot(naive.pred, ylim = c(0,260), ylab = "Gas Use", xlab="Date", bty="l",
     xaxt="n", xlim=c(2005,2007.25), main = "Naive", flty = 2)
axis(1, at = seq(2005, 2007.25,1), labels=format(seq(2005,2007.25,1)))
lines(Gas.lm$fitted, lwd =2)
lines(valid2.ts)

#forecast errors
names(Gas.lm.pred)
Gas.lm.pred$residuals
valid2.ts - Gas.lm.pred$mean

#frequency of forecast errors
hist(Gas.lm.pred$residuals, ylab = "Frequency", xlab = "Forecast Error", bty ="l",
     main = "")


#___________________________Electric Usage_________________________________


#Electric Use moving average
Electric.ts <-ts(Gas$Electric.Use, start = c(2005, 1), end = c(2007, 3), freq = 12)
MovAve.trailing <-rollmean(Electric.ts, k = 12, align = "right")
MovAve.centered <-ma(Electric.ts, order = 12)
plot(Electric.ts, ylim = c(500, 2000),  ylab = "Electric Use", xlab = "Date",
     bty = "l", xaxt = "n", xlim = c(2005,2007.25),
     main = "Forecast Electric Usage with Right and Center Moving Average", 
     col = "dark red")
axis(1, at= seq(2005, 2007.25, 1), labels= format(seq(2005, 2007.25, 1)))
lines(MovAve.centered, lwd = 2, col = "blue")
lines(MovAve.trailing, lwd = 2, lty = 2, col = "green")
legend("bottomright",
       c( "Electric Use","Centered Moving Average", 
          "Trailing Moving Average"), lty=c(1,1,2), lwd=c(1,2,2), bty = "n") 

Valid <-7
Train <-length(Electric.ts) -Valid
train.ts <-window(Electric.ts, start = c(2005, 1), end = c(2005, Train))
valid.ts <-window(Electric.ts, start = c(2005, Train + 1), end = c(2005, Train + Valid))
MovAve.trailing <-rollmean(train.ts, k = 12, align = "right")
last.MovAve <-tail(MovAve.trailing, 1)
MovAve.trailing.pred <-ts(rep(last.MovAve, Valid), start = c(2005, Train + 1),
                          end = c(2005, Train + Valid), freq = 12)

plot(train.ts, ylim = c(500, 2000),  ylab = "Gas Use",
     xlab = "Date", bty = "l", xaxt = "n", xlim = c(2005,2007.25),
     main = "Forecasting Validation", col ="dark red")
axis(1, at = seq(2005, 2007.25, 1), labels = format(seq(2005, 2007.25, 1)))
lines(MovAve.trailing, lwd = 2, col = "blue")
lines(MovAve.trailing.pred, lwd = 2, col = "green", lty = 2)
lines(valid.ts)
accuracy(MovAve.trailing.pred, valid.ts)


#naive and snaive
fixed.nValid <- 7
fixed.nTrain <- length(Electric.ts) - fixed.nValid
train2.ts <- window(Electric.ts, start = c(2005,1), end = c(2005,fixed.nTrain))
valid2.ts <- window(Electric.ts, start = c(2005, fixed.nTrain+1), end = c(2005,fixed.nTrain+fixed.nValid))
naive.pred <- naive(train2.ts, h = fixed.nValid)
snaive.pred <- snaive(train2.ts, h = fixed.nValid)
accuracy(naive.pred, valid2.ts)
accuracy(snaive.pred, valid2.ts)


#model and forecasting
Electric.lm <- tslm(train2.ts ~ trend + I(trend^2))
Electric.lm.pred <- forecast(Electric.lm, h = Valid, level = 0)

#plot with trend line
plot(Electric.lm.pred, ylim = c(500,2000), ylab = "Electric Use", xlab="Date", bty="l",
     xaxt="n", xlim=c(2005,2007.25), main = "Electric.lm.pred", flty = 2)
axis(1, at = seq(2005, 2007.25,1), labels=format(seq(2005,2007.25,1)))
lines(Electric.lm$fitted, lwd =2)
lines(valid2.ts)
#plot naive
plot(naive.pred, ylim = c(500,2000), ylab = "Electric Use", xlab="Date", bty="l",
     xaxt="n", xlim=c(2005,2007.25), main = "Naive", flty = 2)
axis(1, at = seq(2005, 2007.25,1), labels=format(seq(2005,2007.25,1)))
lines(Electric.lm$fitted, lwd =2)
lines(valid2.ts)


#forecast errors
names(Electric.lm.pred)
Electric.lm.pred$residuals
valid2.ts - Electric.lm.pred$mean

#frequency of forecast errors
hist(Electric.lm.pred$residuals, ylab = "Frequency", xlab = "Forecast Error", bty ="l",
     main = "")

