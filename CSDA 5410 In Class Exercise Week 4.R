Bike <- read.csv("C:/Users/accli/Downloads/BikeSharingDaily.csv")
View(Bike)
library(lubridate)
library(ggplot2)
library(forecast)
library(ggfortify)
#Ali's Code_________________________________________________________________________________________________________________
max.y<-max(Bike$cnt)
min.y<-min(Bike$cnt)
par(mfrow = c(1,1))
Bike.msts<-msts(Bike$cnt, seasonal.periods = c(7, 365.25))
plot(Bike.msts, ylim = c(min.y,max.y), ylab = "Bike Count", xlab = "Date", bty = "l",
     xaxt = "n", xlim = c(1,3), main = "Bike Sharing Count", col ="dark red")
axis(1, at = seq(1,3,3), labels = format(seq(2011,2013,1),tick = TRUE,  gap.axis =gaps))

Bike.msts %>% tbats() -> Bike.tbats
Bike.tbats.fc <- forecast(Bike.tbats, h = 365)
plot(Bike.tbats, ylim = c(min.y,max.y), ylab = "Bike Count", xlab = "Date", bty = "l",
     xaxt = "n", xlim = c(1,4), main = "Bike Sharing Count", col ="dark blue")
axis(1, at = seq(1,4,1), labels = format(seq(2011,2014,1),tick = TRUE,  gap.axis =gaps))

plot(Bike.tbats.fc, ylim = c(min.y,max.y), ylab = "Bike Count", xlab = "Date", bty = "l",
     xaxt = "n", xlim = c(1,4), main = "Forecast Bike Sharing Count with TBATS", col ="dark blue")
axis(1, at = seq(1,4,1), labels = format(seq(2011,2014,1),tick = TRUE,  gap.axis =gaps))


Bike.msts %>% stlm(s.window = "periodic", method = "ets") -> Bike.stlm
Bike.stlm.fc <- forecast(Bike.stlm, h = 365)
plot(Bike.stlm.fc, ylim = c(-2000,10000), ylab = "Bike Count", xlab = "Date", bty = "l",
     xaxt = "n", xlim = c(1,4), main = "Bike Sharing Count with STL+ETS", col ="dark blue")
axis(1, at = seq(1,4,1), labels = format(seq(2011,2014,1),tick = TRUE,  gap.axis =gaps))

Bike.msts.y <- msts(Bike$cnt, seasonal.periods = c(365.25))
Bike.stlm.y <- stlm(Bike.msts.y, s.window = "periodic", method = "ets")
Bike.stlm.fc.y <- forecast(Bike.stlm.y, h = 365)
plot(Bike.stlm.fc.y, ylim = c(-2000,10000), ylab = "Bike Count", xlab = "Date", bty = "l",
     xaxt = "n", xlim = c(1,4), main = "Bike Sharing Count STL with only yearly  season", col ="dark blue")
axis(1, at = seq(1,5,1), labels = format(seq(2011,2015,1),tick = TRUE,  gap.axis =gaps))

Bike.msts.y <- msts(Bike$cnt, seasonal.periods = c(365.25))
Bike.msts.y  %>% tbats() -> Bike.tbats.y
Bike.tbats.fc.y <- forecast(Bike.tbats.y, h = 365)


ggseasonplot(Bike.msts.y)+ylab("Bike Count")+ggtitle("Bike Sharing Yearly Season")


#_____________________________________________________________________________________________________________________________________
Bike <- read.csv("~/CSDA 5410/BikeSharingDaily.csv")
View(Bike)
library(lubridate)
library(ggplot2)
library(forecast)
library(ggfortify)


nTotal <- length(Bike$cnt[13004:13747])
Bike.hourly.msts <- msts(Bike$cnt[13004:13747], seasonal.periods = c(24,168), start = c(0,1))

nTrain <- 21 * 24
nValid <- nTotal - nTrain
yTrain.msts <- window(Bike.hourly.msts, start = c(0,1), end = c(0,nTrain))
yValid.msts <- window(Bike.hourly.msts, start = c(0, nTrain + 1), end = c(0,nTotal))

Bike.hourly.dshw.pred <- dshw(yTrain.msts, h = nValid)
Bike.hourly.dshw.pred.mean <- msts(Bike.hourly.dshw.pred$mean, seasonal.periods = c(24,168), start = c(0,nTrain+1))
accuracy(Bike.hourly.dshw.pred.mean, yValid.msts)

plot(yTrain.msts, xlim = c(0,4 + 3/7),xlab = "Week",ylab = "Hourly Bike Rentals",)
lines(Bike.hourly.dshw.pred.mean, lwd = 2, col = "blue")



Bike.daily.msts <- msts(Bike$cnt, seasonal.periods = c(7,365.25))
Bike.daily.tbats <- tbats(Bike.daily.msts)
Bike.daily.tbats.pred <- forecast(Bike.daily.tbats, h = 365)
Bike.daily.stlm <- stlm(Bike.daily.msts, s.window = "periodic", method = "ets")
Bike.daily.stlm.pred <- forecast(Bike.daily.stlm, h = 365)

par(mfrow = c(1,2))
plot(Bike.daily.tbats.pred, ylim = c(0,9000), xlab = "Year", ylab = "Daily Bike Rentals", main = "TBATS")
plot(Bike.daily.stlm.pred, ylim = c(0,9000), xlab = "Year", ylab = "Daily Bike Rentals", main = "STL + ETS")


