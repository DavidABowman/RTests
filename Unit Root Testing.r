install.packages("dyn")
library("dyn")
names(hseinv)
summary(hseinv)
hseinv <- ts(hseinv, frequency = 1, start=1947)
year <- hseinv[,1]
inv <- hseinv[,2]
Pop. <- hseinv[,3]
PriceIndx. <- hseinv[,4]
#Generate the log of variables of interest
livnpc <- (inv/Pop.)
lprice <- log(PriceIndx.)
fit1 <- dyn$lm(linvpc ~ lprice)
summary(fit1)

#Time plots
plot.ts(linvpc)
plot.ts(lprice)
#generate first differences using the diff command, this is needed for the ADF regression
FD_linvpc <- diff(linvpc,1)
fit2 <- dyn$lm(FD_linvpc ~ lag(linvpc,-1) + lag(FD_linvpc,-1) + lag(FD_linvpc,-2))
summary(fit2)
# Use correct critical values - ADF Crit
FD_lprice <- diff(lprice,1)
fit3 <- dyn$lm(FD_lprice ~ lag(lprice,-1) + lag(FD_lprice,-1) + lag(FD_lprice,-2))
summary(fit3)
# make sure you use the correct critical values!!!

# code for Q7Q8
fit4 <- dyn$lm(FD_linvpc ~ lag(linvpc,-1) + lag(FD_linvpc,-1) + lag(FD_linvpc,-2) + seq_along(FD_linvpc))
summary(fit4)
# make sure you use the correct critical values
fit5 <- dyn$lm(FD_lprice ~ lag(lprice,-1) + lag(FD_lprice,-1) + lag(FD_lprice,-2) + seq_along(FD_lprice))
summary(fit5)
# make sure you use the correct critical values (not the ones reported in the output)
#Time plots of first differences (what do you observe?)
plot.ts(FD_linvpc)
plot.ts(FD_lprice)