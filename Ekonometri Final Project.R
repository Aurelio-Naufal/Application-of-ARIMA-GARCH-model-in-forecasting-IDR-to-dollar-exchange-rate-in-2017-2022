library("prettydoc")
library("rmdformats")
library(fGarch)
library(aTSA)
library(FinTS) 
library(lmtest)
library(forecast)
library(TSA)
library(tseries)
library(xts)
library(readxl)
library(tidyverse)
library("dygraphs")
library(MLmetrics)
library(ggplot2)
library(car)

View(Weekly_Data)
Weekly_Data$Date <- as.Date(Weekly_Data$Date)
fullts <- ts(Weekly_Data[,5])
traints <- ts(Training_Data[,5])
testts <- ts(Test_Data[,5])

adf.test(fullts)
tsdisplay(fullts)
#Plot Harga
plot(fullts,
     xlab = "Minggu ke-",
     ylab = "Harga Penutupan",
     main = "Harga Penutupan Nilai Tukar Rupiah",
     lwd = 1,
     col = "blue") 
acf(fullts)

#Plot With Date 
plotz <- ggplot(Weekly_Data, aes(x=Date, y=Close)) +
  geom_line()
plotz

diff_ts <- diff(traints,differences = 1)
diff_ts

tsdisplay(diff_ts)
BoxCox.ar(diff_ts)
plot(diff_ts,
     xlab = "Waktu",
     ylab = "Harga penutupan",
     main = "Differencing (1) Harga Penutupan Saham PT Telekomunikasi 
Indonesia Tbk [TLKM]",
     lwd = 2,
     col = "blue")
# Uji Stasioner (ADF test)
adf.test(diff_ts)
acf(diff_ts)
pacf(diff_ts)
eacf(diff_ts)
auto.arima(traints,trace = T,d = 1)

#Calon model: ARIMA(0,1,0), ARIMA(1,1,1), dan ARIMA(0,1,2)
model1 <- Arima(traints, order = c(0,1,1), include.constant = FALSE)
model2 <- Arima(traints, order = c(1,1,1), include.constant = FALSE)
model3 <- Arima(traints, order = c(2,1,2), include.constant = FALSE)
cbind(model1, model2, model3)

model1.test <- Arima(traints, model=model1)
accuracy(model1.test)
model2.test <- Arima(traints, model=model2)
accuracy(model2.test)
model3.test <- Arima(traints, model=model3)
accuracy(model3.test)


coeftest(model1)
coeftest(model2)
coeftest(model3)

fit <- Arima(traints, order = c(2,1,2), include.constant = FALSE)
fit
coeftest(fit)
overfit1 <- Arima(traints, order = c(2,1,3), include.constant = FALSE)
cbind(fit, overfit1)
overfit2 <- Arima(traints, order = c(2,1,4), include.constant = FALSE)
cbind(overfit1, overfit2)
overfit1

coeftest(overfit1)

# Uji Stasioner
qqnorm(overfit1$residuals, pch = 16)
qqline(overfit1$residuals, lwd = 4, col = "blue")
adf.test(overfit1$residuals)
tsdiag(overfit1)
# Jarque-Bera
jarque.bera.test(overfit1$residuals)
# Independensi (Hanya berlaku asumsi kenormalan)
checkresiduals(overfit1)
adf.test(overfit1$residuals)
dwtest(overfit1$residuals)

acf(overfit1$residuals^2,main="ACF Squared Residual")
pacf(overfit1$residuals^2,main="PACF Squared Residual")

ArchTest(diff_ts)
for (i in 1:15) {
  ArchTest <- ArchTest(overfit1$residuals, lags=i, demean=TRUE)
  cat("P Value LM Test lag ke", i,"adalah" , ArchTest$p.value, "\n") }
ArchTest(overfit1$residuals, lags=3)
#ArchTest 

#GARCH MODEL

garch1<-garchFit(~arma(2,3)+garch(3,5),data = diff_ts, trace = F)
summary(garch1)

garch2<-garchFit(~arma(2,3)+garch(4,5),data = diff_ts, trace = F)
summary(garch2)

garch3<-garchFit(~arma(2,3)+garch(3,6),data = diff_ts, trace = F)
summary(garch3)


#plot simpangan baku
sb<-garch1@sigma.t
ragam<-garch1@sigma.t^2
#sequense kurg 1 obs sehingga harus dimodifikasi
Training_Data$Date <- as.Date(Training_Data$Date)
dates2<-Training_Data$Date[-1]
stdv=xts(sb,order.by = dates2)
vragam=xts(ragam,order.by = dates2)
plot(stdv, main="Simpangan Baku")


# GARCH Model
garch1test<-garchFit(~arma(2,3)+garch(3,5),data = diff_ts, trace = F)
summary(garch1test)

garch2test<-garchFit(~arma(2,3)+garch(4,5),data = diff_ts, trace = F)
summary(garch2test)

garch3test<-garchFit(~arma(2,3)+garch(3,6),data = diff_ts, trace = F)
summary(garch3test)

#Validasi Model
predict(garch1test, n.ahead = 10, trace = FALSE, mse = c("cond","uncond"),
        plot=TRUE, nx=NULL, crit_val=NULL, conf=NULL)


# Prediksi Model
garch1pred<-garchFit(~arma(2,3)+garch(3,5),data = fullts, trace = F)
summary(garch1pred)

predict(garch1pred, n.ahead = 10, trace = FALSE, mse = "cond",
       plot=TRUE, nx=NULL, crit_val=NULL, conf=NULL)


hasilpred <- c(15670.35,15816.38,15858.08,15859.36,15816.47,
               15801.39,15777.65,15758.04,15735.41,15716.50)

aktualdata <- ts(Validasi[,5])
aktualdata
MAPE(hasilpred,aktualdata)

plot(hasilpred,
     fcol = "blue",
     lwd = 2,
     main = "Peramalan ARIMA(0,1,0) melalui Data Training - Testing",
     xlab = "Waktu",
     ylab = "Harga penutupan")
lines(fullts,
      col = "red",
      lwd = 2)
legend("topleft",
       col = c("blue", "red"), 
       legend = c("Peramalan Nilai Testing","Nilai Aktual"),
       lwd = 2,
       bty = "n")
