library(MASS)
library(moments)
library(car)
library(zoo)
library(lmtest)
library(sandwich)
library(strucchange)
library(urca)
library(vars)
library(lmtest)
library(vars)
library(ggplot2)
library(scales)
library(gridExtra)
library(lubridate)


setwd("~/Documents")

# Import historical data for TSX and VIX from 1990 to 2016
q1 <- read.csv("Data2.csv", sep=",", header = T)

#### QUESTION 1 ####

View(q1)

# Re-format factors as numeric
q1$VIX <- as.numeric(levels(q1$VIX))[q1$VIX]

# Compute the daily returns on SPX
for (i in 1:(length(q1$SPX)-1)) {
  q1$SPX.return[i+1] = (q1$SPX[i+1]/q1$SPX[i]-1)*100
  }

# SPX Return plot
plot(q1$Date, q1$SPX.return, type = "l", col = 'darkblue', 
     main = 'S&P 500 Historical Daily Returns', xlab = 'Date', ylab = 'Returns')
abline(h=0, lty=2)

# Plot the ACF of the SPX daily returns
acf(q1$SPX.return[2:length(q1$SPX.return)], main="ACF of S&P 500 Daily Returns", lag.max = 80)


#### QUESTION 2 ####

# Compute v(t,1)
q1$SPX.vol1 <- (q1$SPX.return)^2

q1$SPX.vol11 <- (q1$SPX.return-mean(q1$SPX.return, na.rm=T))^2

# Compute v(t,2)

q1$SPX.vol2 <- 0

for (i in 26:length(q1$SPX.vol1)) {
  q1$SPX.vol2[i] <- mean(q1$SPX.vol1[(i-24):i])
}

q1$SPX.vol2[1:25] <- NA

# Plotting VIX, v(t,1), and v(t,2)

plot(q1$Date[26:length(q1$VIX)], q1$VIX[26:length(q1$VIX)], type='l', col='blue', 
     main = 'Volatility Measures', xlab = 'Date', ylab='Volatility')
lines(q1$Date[26:length(q1$VIX)], q1$SPX.vol1[26:length(q1$VIX)], type='l', col='red')
lines(q1$Date[26:length(q1$VIX)], q1$SPX.vol2[26:length(q1$VIX)], type='l', col='black')
legend("top", c("VIX", "v(t,1)", "v(t,2)"), lty = c(1,1,1), 
       lwd=c(2,2,2),col=c("blue", "red", "black"), ncol=3, cex=0.75, bty="n")

#### QUESTION 3 ####

# Computing the historical mean, volatility and covolatility of the three series.
mean(q1$VIX[26:length(q1$VIX)], na.rm=T)
mean(q1$SPX.vol1[26:length(q1$SPX.vol1)])
mean(q1$SPX.vol2[26:length(q1$SPX.vol2)])

var(q1$VIX[26:length(q1$VIX)], na.rm=T)
var(q1$SPX.vol1[26:length(q1$SPX.vol1)])
var(q1$SPX.vol2[26:length(q1$SPX.vol2)])

a1 <- q1$VIX[26:length(q1$VIX)]
a2 <- q1$SPX.vol1[26:length(q1$SPX.vol1)]
a3 <- q1$SPX.vol2[26:length(q1$SPX.vol2)]

# (3,3) Covolatility Matrix of VIX, v(t,1), v(t,2) and it's eigenvalues/vectors

A <- matrix(c(cov(a1,a1, use = 'pairwise.complete.obs'), cov(a2,a1, use = 'pairwise.complete.obs'), 
              cov(a3,a1, use = 'pairwise.complete.obs'), cov(a1,a2, use = 'pairwise.complete.obs'),
              cov(a2,a2, use = 'pairwise.complete.obs'),cov(a3,a2, use = 'pairwise.complete.obs'), 
              cov(a1,a3, use = 'pairwise.complete.obs'), cov(a2,a3, use = 'pairwise.complete.obs'), 
              cov(a3,a3, use = 'pairwise.complete.obs')), 3,3)

eigen(A, symmetric='T')

##### QUESTION 4 ####


# Estimation of VAR model

y1<- q1$VIX[-c(as.numeric(rownames(q1[!complete.cases(q1),])))]
y2<- q1$SPX.vol1[-c(as.numeric(rownames(q1[!complete.cases(q1),])))]
y3<-q1$SPX.vol2[-c(as.numeric(rownames(q1[!complete.cases(q1),])))]
Y<-matrix(c(y1, y2, y3), ncol=3)
head(Y)
Z<-VAR(Y, p=1)
summary(Z)
plot(Z)
h<-c("y1", "y2", "y3")


# Matrix of Coefficients

Z_coef<-matrix(c(as.numeric(Z$varresult$y1$coefficients[c(1:3)]), 
                 as.numeric(Z$varresult$y2$coefficients[c(1:3)]), 
                 as.numeric(Z$varresult$y3$coefficients[c(1:3)])), 
               ncol=3)
rownames(Z_coef)<-h
colnames(Z_coef)<-h
Z_coef

#Vector of Constants
Z_cons<-matrix(c(as.numeric(Z$varresult$y1$coefficients[c(4)]), 
          as.numeric(Z$varresult$y2$coefficients[c(4)]), 
          as.numeric(Z$varresult$y3$coefficients[c(4)]) ), nrow=3)
colnames(Z_cons)<-c("coef")
rownames(Z_cons)<-h
Z_cons


#Residuals and Covariance-Variance Matrix of residuals
head(residuals(Z))
tail(residuals(Z))

Z_cov_MLE<-(1/as.numeric(Z$obs))*(t(residuals(Z)) %*% residuals(Z))
Z_cov_OLS<-(1/(as.numeric(Z$obs) -as.numeric(Z$K)*as.numeric(Z$p)- 1))*(t(residuals(Z)) 
                                                                        %*% residuals(Z))
Z_cov_OLS
Z_cov_MLE


#Eigenvalues and Eigenvectors
Z_lam_OLS<-eigen(Z_cov_OLS, symmetric='T')
Z_lam_MLE<-eigen(Z_cov_MLE, symmetric = 'T')
#Eigenvalues
Z_lam_OLS$values
Z_lam_MLE$values
#Eigenvectors
Z_lam_OLS$vectors
Z_lam_MLE$vectors

#Optimal VAR

VARselect(Y, lag.max = 100)
