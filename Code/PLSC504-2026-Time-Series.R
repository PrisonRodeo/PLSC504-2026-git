#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Intro materials...                                   ####
#
# PLSC 504 -- Fall 2026
#
# Introduction to Time Series Analysis
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set working directory (or not, whatever...)

setwd("~/Dropbox (Personal)/PLSC 504") # change as needed

# Packages, etc.:                                      ####
#
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("devtools","pak","readr","RCurl","gtools","texreg","lmtest",
     "plyr","zoo","tseries","forecast","dyn","urca","cointReg")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run that ^^^ code 4-5 times until you get all smileys. :)
#
# Also:

pak::pak("matthewclegg/egcm")
1
library(egcm)

# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Time-series plot example: Democratic House membership,
# 1789-2025...

DCong<-read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2026-git/master/Data/DCongPct.csv")

DCong$DemHousePct <- DCong$DemHousePct*100 # percentages
DCong$DemSenatePct <- DCong$DemSenatePct*100 # percentages

summary(DCong)

# Make it a time series:

DH.TS <- ts(DCong$DemHousePct,start=1789,end=2025,
            frequency=0.5)

# Time series plot:

pdf("DemHousePct26.pdf",6,5)
par(mar=c(4,4,2,2))
plot(DH.TS, t="l",lwd=2,
     xlab="Congress",ylab="Percent Democratic")
abline(h=50,lwd=1,lty=2)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basics...                                            ####
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Some simulations: ARIMA series, etc.
#
# Generate and plot an I(1) series:

set.seed(7222009)

T <- 200
I1 <- arima.sim(n=T,list(order=c(0,1,0)))

pdf("I1-Series26.pdf",6,5)
par(mar=c(4,4,2,2))
plot(I1,ylab="Values of Y",lwd=2)
abline(h=0,lty=2)
dev.off()

# Same series, differenced:

I1D <- diff(I1)

pdf("I1-Series-Differenced26.pdf",6,5)
par(mar=c(4,4,2,2))
plot(I1D,lwd=2,
     ylab=expression(paste("Values of ",Delta,"Y")))
abline(h=0,lty=2)
dev.off()

# Generate and plot some AR(1) series:

set.seed(7222009)
T <- 200

AR01 <- arima.sim(n=T,list(ar=c(-0.8),ma=c(0)))
AR05 <- arima.sim(n=T,list(ar=c(0.1),ma=c(0)))
AR09 <- arima.sim(n=T,list(ar=c(0.8),ma=c(0)))

pdf("AR-Series26.pdf",6,5)
par(mar=c(2,4,4,2))
par(mfrow=c(3,1))
plot(AR01,lwd=2,ylab="Values of Y",
     main=expression(paste(phi," = -0.80")))
abline(h=0,lty=2)
#text()
plot(AR05,lwd=2,col="darkblue",ylab="Values of Y",
     main=expression(paste(phi," = 0.10")))
abline(h=0,lty=2)
par(mar=c(4,4,4,2)) # reset X margin
plot(AR09,lwd=2,col="orange",xlab="Time",ylab="Values of Y",
     main=expression(paste(phi," = 0.80")))
abline(h=0,lty=2)
dev.off()

# Generate and plot some MA(1) series:

set.seed(7222009)
T <- 200

MA01 <- arima.sim(n=T,list(ar=c(0),ma=c(-0.8)))
MA05 <- arima.sim(n=T,list(ar=c(0),ma=c(0.1)))
MA09 <- arima.sim(n=T,list(ar=c(0),ma=c(0.8)))

pdf("MA-Series26.pdf",6,5)
par(mar=c(2,4,4,2))
par(mfrow=c(3,1))
plot(MA01,lwd=2,ylab="Values of Y",
     main=expression(paste(theta," = -0.80")))
abline(h=0,lty=2)
#text()
plot(MA05,lwd=2,col="darkblue",ylab="Values of Y",
     main=expression(paste(theta," = 0.10")))
abline(h=0,lty=2)
par(mar=c(4,4,4,2)) # reset X margin
plot(MA09,lwd=2,col="orange",xlab="Time",ylab="Values of Y",
     main=expression(paste(theta," = 0.80")))
abline(h=0,lty=2)
dev.off()

# Rho-Theta plot:

theta <- seq(from=-1,to=1,by=0.01)
rho <- theta / ((1 + theta^2))

pdf("MA-Series-Theta-Rho26.pdf",6,5)
par(mar=c(4,4,2,2))
plot(theta, rho, t="l",lwd=2,
     xlab=expression(theta),ylab=expression(rho))
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compare some ARIMA series...

set.seed(7222009)
ar1 <- arima.sim(n=T,list(ar=c(0.9),ma=c(0)))
ma1 <- arima.sim(n=T,list(ar=c(0),ma=c(0.9)))
arma11 <- arima.sim(n=T,list(ar=c(0.9),ma=c(0.9)))
i1 <- arima.sim(n=T,list(order=c(0,1,0)))

pdf("ARIMAcomparison26.pdf",7,6)
par(mar=c(4,4,2,2))
plot(i1,ylim=c(-11,22),lwd=2,ylab="Values of Y")
lines(ar1,lwd=2,col="orange")
lines(ma1,lwd=2,col="blue")
lines(arma11,lwd=2,col="darkgreen")
abline(h=0,lwd=0.8,lty=2)
legend(150,22,legend=c("I(1)","AR(1)","MA(1)","ARMA(1,1)"),
       col=c("black","orange","blue","darkgreen"),
       lwd=2,bty="n")
dev.off()

# ACFs and PACFs:
acfi1 <- acf(i1)
acfar1 <- acf(ar1)
acfma1 <- acf(ma1)
acfarma11 <- acf(arma11)

pacfi1 <- pacf(i1)
pacfar1 <- pacf(ar1)
pacfma1 <- pacf(ma1)
pacfarma11 <- pacf(arma11)

pdf("ARIMA-ACFs26.pdf",8,6)
par(mfrow=c(2,2))
plot(acfi1,main="I(1) series")
plot(acfar1,main="AR(1) series")
plot(acfma1,main="MA(1) series")
plot(acfarma11,main="ARMA(1,1) series")
dev.off()

pdf("ARIMA-PACFs26.pdf",8,6)
par(mfrow=c(2,2))
plot(pacfi1,main="I(1) series")
plot(pacfar1,main="AR(1) series")
plot(pacfma1,main="MA(1) series")
plot(pacfarma11,main="ARMA(1,1) series")
dev.off()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Go back to the Democratic Percent of Congress data (that
# we used above)...
#
# Plots of univariate ACFs and PACFs for the House data:

D.acf <- acf(DH.TS) # ACF

pdf("DemHouseACF26.pdf",6,5)
par(mar=c(4,4,2,2))
plot(D.acf,main="")
dev.off()

D.pacf <- pacf(DH.TS) # PACF

pdf("DemHousePACF26.pdf",6,5)
par(mar=c(4,4,2,2))
plot(D.pacf, main="")
dev.off()

# Fit ARIMA models:

DH.AR1 <- arima(DH.TS,order=c(1,0,0),method="ML") # AR(1)
summary(DH.AR1)

DH.ARMA11 <- arima(DH.TS,order=c(1,0,1),method="ML") # ARMA(1,1)
summary(DH.ARMA11)

# Model selection via LR test:

lrtest(DH.AR1,DH.ARMA11)

# Automated version:

DH.robot <- auto.arima(DH.TS)
summary(DH.robot)

# Plot residuals vs. time:

pdf("DH-ARIMA-residuals26.pdf",6,3)
par(mar=c(4,4,2,2))
plot(DH.AR1$residuals, ylab="AR(1) residuals",
       lwd=2)
abline(h=0,lty=2)
dev.off()

# Box-Pierce and Ljung-Box tests:

Box.test(DH.AR1$residuals)
Box.test(DH.AR1$residuals,type="Ljung")

# Forecasting:

DH.forecast <- forecast(DH.AR1,h=20,model="Arima")

pdf("DH-forecast26.pdf",7,6)
par(mar=c(4,4,2,2))
plot(DH.forecast, main="",lwd=2,xlab="Year",
     ylab="Democratic House Percentage",
     showgap=FALSE,col="black",fcol="red")
abline(h=50,lty=2)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UNIT ROOTS...                                        ####
#
# Unit Root vs. trend:

T <- 200
time <- seq(from=1,to=T)

set.seed(7222009)
us <- rnorm(T)
UnitRoot <- cumsum(us)
Trend <- -5+0.06*time + us
UR.TS <- ts(UnitRoot,start=1,end=T)
Trend.TS <- ts(Trend,start=1,end=T)

pdf("URvsTrend26.pdf",7,6)
par(mar=c(4,4,2,2))
plot(UR.TS,xlab="Time",ylab="Y",
     lwd=2,ylim=c(-12,11))
lines(Trend,col="orange",lwd=2,lty=5)
abline(h=0,lty=2)
legend(1,-8,bty="n",legend=c("Unit Root","Trend"),
       lwd=2,col=c("black","orange"), lty=c(1,5))
dev.off()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data (House and Senate votes, 1789-2025):

Congress <- read.csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2026-git/master/Data/CongressVotes25.csv") 

HVotes.TS <- ts(Congress$HouseVotes,start=1789,end=2025,
                frequency=1)
SVotes.TS <- ts(Congress$SenateVotes,start=1789,end=2025,
                frequency=1)

# Plot:

pdf("CongressVotes26.pdf",7,6)
par(mar=c(4,4,2,2))
plot(HVotes.TS,xlab="Year",ylab="Number of Floor Votes",
     lwd=2,col="darkblue")
lines(SVotes.TS,col="orange",lwd=2,lty=5)
legend("topleft",bty="n",legend=c("U.S. House","U.S. Senate"),
       lwd=2,col=c("darkblue","orange"), lty=c(1,5))
dev.off()

# Various unit root tests...
#
# "standard" D-F:

HDF<-ur.df(HVotes.TS,type="none",lags=0)
summary(HDF)

# Add a "drift":

HDF.D<-ur.df(HVotes.TS,type="drift",lags=0)
summary(HDF.D)

# Add a trend:

HDF.T<-ur.df(HVotes.TS,type="trend",lags=0)
summary(HDF.T)

# plot the D-F object:

pdf("H-DF-Trend26.pdf",7,6)
par(mar=c(4,4,2,2))
plot(HDF.T)
dev.off()

# Augmented D-F tests:

HADF.T1<-ur.df(HVotes.TS,type="trend",lags=1)
summary(HADF.T1)

pdf("H-ADF-TrendL1-26.pdf",7,6)
par(mar=c(4,4,2,2))
plot(HADF.T1)
dev.off()

HADF.BIC<-ur.df(HVotes.TS,type="trend",lags=6,selectlags="BIC")
summary(HADF.BIC)

plot(HADF.BIC)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# COINTEGRATION AND ECMs                               ####
#
# Simulating some cointegrated series...

T <- 150              # long-ish series
set.seed(7222009)
W <- cumsum(rnorm(T)) # the common bit - I(1)
X <- 2 + 0.8*W + 2*rnorm(T)
Y <- -2 + 0.8*W + 2*rnorm(T)

W.TS <- ts(W,start=1,end=T) # time series objects
X.TS <- ts(X,start=1,end=T)
Y.TS <- ts(Y,start=1,end=T)

pdf("CI-Series26.pdf",7,6)
par(mar=c(4,4,2,2))
plot(X.TS,xlab="Time",ylab="X / Y",
     col="darkblue",lwd=2,ylim=c(-12,12))
lines(Y.TS,col="orange",lwd=2)
abline(h=0,lty=2)
legend(1,11,bty="n",legend=c("X","Y"),
       lwd=2,col=c("darkblue","orange"), lty=c(1,1))
dev.off()

# Test for unit roots in X and Y:

summary(ur.df(X.TS,type="trend",lags=1))
summary(ur.df(Y.TS,type="trend",lags=1))

# Cointegrating regression:

CI.reg <- lm(X~Y)   # cointegrating regression
Zhats.TS <- ts(CI.reg$residuals,start=1,end=T)
summary(ur.df(Zhats.TS,type="trend",lags=1))

# Residual plot:

pdf("CI-Resids26.pdf",7,6)
par(mar=c(4,4,2,2))
plot(Zhats.TS,xlab="Time",ylab="Residuals Z",
     lwd=2,ylim=c(-8,8))
abline(h=0,lty=2)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Spurious regressions:

Stangs <- read.csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2026-git/master/Data/Mustangs.csv") 

SY.TS<-ts(Stangs$price,start=1964,end=2019)    # mustang prices
SX.TS<-ts(Stangs$paraguay,start=1964,end=2019) # Paraguay's POLITY

pdf("Spurious26.pdf",7,6)
par(mar=c(4,2,2,2))
plot(SX.TS,xlab="Year",ylab=" ",
     col="darkblue",lwd=2,yaxt="n")
par(new=T)
plot(SY.TS,xlab="Year",ylab=" ",col="orange",
     lty=5,lwd=2,yaxt="n")
legend(1965,24000,bty="n",legend=c("X","Y"),
       lwd=2,col=c("darkblue","orange"), lty=c(1,5))
dev.off()

# Regressions:

summary(lm(SY.TS~SX.TS))
summary(dyn$lm(SY.TS~lag(SX.TS,-1)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ECMs: Example

X<-c(10,10,10,20,10,10,10,10,10,12,14,16,18,20,20,20,20,20,20,20)
XTS <- ts(X,start=1,end=length(X))
DXTS<-ts(diff(X),start=2,end=length(X))

pdf("ECM-Example26.pdf",6,5)
par(mar=c(4,4,2,2))
plot(XTS,xlab="Time",ylab="Values",
     col="darkblue",lwd=2,ylim=c(-10,20))
lines(DXTS,col="orange",yaxt="n",xaxt="n",
      lty=5,lwd=2,xlim=c(1,20),xaxt="n")
legend(12,-4,bty="n",legend=c("X","Changes in X"),
       lwd=2,col=c("darkblue","orange"), lty=c(1,1))
dev.off()

# Generate Ys from two cointegrating regressions:

Y1 <- numeric(length(X))
Y2 <- numeric(length(X))

# Initial conditions: long-run equilibrium at t = 1
Y1[1] <- 5 + X[1]
Y2[1] <- 10 + 2 * X[1]

# For t = 2, use the same initial Y values.
# This gives the process a defined starting point.
Y1[2] <- Y1[1]
Y2[2] <- Y2[1]

# Generate Ys recursively:
for (t in 3:length(X)) {
  
  # lagged change in X: ΔX_(t-1)
  dX_lag <- X[t-1] - X[t-2]
  
  # Y1 ECM
  dY1 <- 1.0 * dX_lag -
    0.8 * (Y1[t-1] - 5.0 - 1.0 * X[t-1])
  
  # Y2 ECM
  dY2 <- 0.25 * dX_lag -
    0.2 * (Y2[t-1] - 10.0 - 2.0 * X[t-1])
  
  # Convert ΔY to Y
  Y1[t] <- Y1[t-1] + dY1
  Y2[t] <- Y2[t-1] + dY2
}

# make time-series objects:

Y1.ts<-ts(Y1,start=1,end=length(X))
Y2.ts<-ts(Y2,start=1,end=length(X))
DY1<-ts(dY1,start=1,end=length(X))
DY2<-ts(dY2,start=1,end=length(X))

# plot:

pdf("ECM-Example2-26.pdf",7,6)
par(mar=c(4,4,2,2))
plot(XTS,xlab="Time",ylab="Values",
     col="blue",lwd=3,ylim=c(0,50))
lines(Y1.ts,xlab="",ylab="",col="orange",yaxt="n",
      lty=5,lwd=2,xaxt="n")
lines(Y2.ts,col="darkgreen",yaxt="n",
      lty=3,lwd=2,xaxt="n")
legend(14,9,bty="n",legend=c("X","Y1","Y2"),
       lwd=c(3,2,2),col=c("blue","orange","darkgreen"),lty=c(1,5,3))
dev.off()

# Fitting ECMs: Congressional vote data...

Congress <- read.csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC504-2026-git/master/Data/CongressVotes25.csv") 

HVotes.TS <- ts(Congress$HouseVotes,start=1789,end=2025,
                frequency=1)
SVotes.TS <- ts(Congress$SenateVotes,start=1789,end=2025,
                frequency=1)

# Plot:

pdf("CongressVotes26-2.pdf",7,6)
par(mar=c(4,4,2,2))
plot(HVotes.TS,xlab="Year",ylab="Number of Floor Votes",
     col="darkblue",lwd=2)
lines(SVotes.TS,col="orange",lwd=2,lty=5)
legend(1790,1200,bty="n",legend=c("U.S. House","U.S. Senate"),
       lwd=2,col=c("darkblue","orange"), lty=c(1,5))
dev.off()

# Unit roots?

summary(ur.df(HVotes.TS,type="trend",lags=6,selectlags="BIC"))
summary(ur.df(SVotes.TS,type="trend",lags=6,selectlags="BIC"))

# Two-step ECM: Step one...

StepOne <- lm(SVotes.TS~HVotes.TS) # CI regression
summary(StepOne)

# Check residuals:

Zt.TS <- ts(StepOne$residuals,
            start=1789,end=2025) # make residual time series
# summary(ur.df(Zt.TS,type="trend",lags=6,selectlags="BIC")) # ADF
# summary(ur.kpss(Zt.TS,type="tau",lags="short"))            # KPSS

pdf("StepOneResiduals26.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Zt.TS,xlab="Year",ylab="Step One Residuals",
     lwd=2)
abline(h=0,lty=2)
dev.off()

# Step Two:

DSV.TS <- diff(SVotes.TS)              # difference Y
DHVLag.TS <- lag(diff(HVotes.TS),k=-1) # Lagged differenced X
Ztminus1.TS <- lag(Zt.TS,k=-1)         # Lagged residuals
df <- ts.intersect(DSV.TS,DHVLag.TS,Ztminus1.TS)

StepTwo <- lm(DSV.TS ~ DHVLag.TS + Ztminus1.TS, 
              data = df)
summary(StepTwo)

# One-Step ECM:

SVLag.TS <- lag(SVotes.TS,k=-1) # Lag Y
HVLag.TS <- lag(HVotes.TS,k=-1) # Lag X
df2 <- ts.intersect(DSV.TS,DHVLag.TS,SVLag.TS,HVLag.TS)
OneStep <- lm(DSV.TS~DHVLag.TS+SVLag.TS+HVLag.TS, 
              data=df2)
summary(OneStep)

# \fin