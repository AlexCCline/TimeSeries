Gas.Usage <- read.csv("~/CSDA 5410/Gas Usage.csv")
View(Gas.Usage)

library(forecast)
library(zoo)

Gas.ts <-ts(Gas.Usage$Gas.Use, start = c(2005, 1), end = c(2007, 3), freq = 12)
MovAve.trailing <-rollmean(Gas.ts, k = 12, align = "right")
MovAve.centered <-ma(Gas.ts, order = 12)
plot(Gas.ts, ylim = c(0, 260),  ylab = "Gas Use", xlab = "Date",
     bty = "l", xaxt = "n", xlim = c(2005,2007.25),
     main = "Forecat Gas Usage with Right and Center Moving Average", 
     col = "dark red")
axis(1, at= seq(2005, 2007.25, 1), labels= format(seq(2005, 2007.25, 1)))
lines(MovAve.centered, lwd = 2, col = "blue")
lines(MovAve.trailing, lwd = 2, lty = 2, col = "green")
legend("bottomright",
       c( "Gas Use","Centered Moving Average", 
          "Trailing Moving Average"), lty=c(1,1,2), lwd=c(1,2,2), bty = "n") 

Valid <-12
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
axis(1, at = seq(2005, 2007, 1), labels = format(seq(2005, 2007, 1)))
lines(MovAve.trailing, lwd = 2, col = "blue")
lines(MovAve.trailing.pred, lwd = 2, col = "green", lty = 2)
lines(valid.ts)







