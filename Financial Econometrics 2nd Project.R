#### Load Data and Transform*####

rm(list = ls())
vixcurrent<- read.csv(file="~/Downloads/vixcurrent.csv")
vixcurrent$Date <- as.Date(vixcurrent$Date, '%m/%d/%Y')
vix<-vixcurrent
View(vix)
rm(vixcurrent)



#Define useful vector & dataframes to be used soon, 
#& Intitalize vectors & packages
#& Define log and difference of log y_t

lvix<-vix
lvix[,2]<-log(lvix[,2])
lvix[,3]<-log(lvix[,3])
lvix[,4]<-log(lvix[,4])
lvix[,5]<-log(lvix[,5])
View(lvix)
lv<-lvix[,5]
dlv<-c(1:3219)
for (i in c(1:3219)) {
  dlv[i]<-lv[i+1]-lv[i]
}
install.packages("tseries")
install.packages("forecast")


####Q3 Part 1####

#Refute stationary assumption from:
acf(lv)
tseries::adf.test(lv, alternative = "stationary")
tseries::pp.test(lv, alternative = "stationary")
tseries::kpss.test(lv)

#Compute what would've been theoretical best Arima/AR/ARMA model:

forecast::auto.arima(lv)
fitAR11<-arima(lv, order = c(1,0,1))
print(fitAR11)
Box.test(residuals(fitAR11), lag = 2, type = "Ljung-Box")
ar(lv)

####Q3 Part 2####

#Refute stationary assumption from:
acf(dlv)
tseries::adf.test(dlv, alternative = "stationary")
tseries::pp.test(dlv, alternative = "stationary")
tseries::kpss.test(dlv)

#Compute what would've been theoretical best Arima/AR/ARMA model:

forecast::auto.arima(dlv)
dfitAR11<-arima(lv, order = c(1,0,1))
summary(dfitAR11)
Box.test(residuals(dfitAR11), lag = 2, type = "Ljung-Box")
Box.test(dlv, lag = 5, type = "Ljung-Box")
ar(dlv)

fdlv<-c(1:3219)
for (i in c(1:3219)) {
  fdlv[i]<- if(dlv[i]<median(dlv)) {
    0
  } else {
    1
}}
fdlv<-factor(fdlv)
tseries::runs.test(fdlv)

#### Q3 Part 3####

#Compute lagged difference log(y_t) squared
dlv_<-c(1:3218)
for (i in c(1:3218)) {
  dlv_[i]<-lv[i+1]-lv[i]
}

#Compute lagged difference log(y_t) squared
dlv_2<-c(1:3218)
for (i in c(1:3218)) {
  dlv_2[i]<-(lv[i+1]-lv[i])^2
}

#Remove first observation from dlm to allow for regression
dlvm1<-dlv[-c(1)]

#Run Linear Regresssion
lmdlvm1<-lm(dlvm1 ~ dlv_ + dlv_2)
summary(lmdlvm1)

#### Monte Carlo ####

x<-runif(10000000, min=1, max=2)

y<-c(1:10000000)
for(i in c(1:10000000)) {
  y[i]<-exp(x[i]^2)
}
mean(y)

#### Uniform of Uniform of Uniform####

obv<-200
mx<- 180

x<-ceiling(runif(obv, 0 , mx))
y<-runif(obv, 0 , 1)
x[1:10]
sum(x)

z<-c(1:sum(x))
for(i in 1:sum(x)) {
  for(j in 1:obv) {
    if(z[i]<=sum(x[1:j]) & z[i]>=sum(x[1:j-1]) ) {
      z[i]<-sum(x[1:j])-i+1
    }
  }
}
z[1]<-x[1]
h<-z

for(i in c(1:(sum(x)-1) )) {
  if(h[i]>h[i+1]){
    h[i]<-0
  } else{
      h[i]<-1
  }
}
h[length(h)]<-1

p<-h
p[1:300]

for (i in 1:sum(x)) {
  for(j in 1:obv) {
    if(j==(1+sum(h[1:i]))) {
      p[i]<-runif(1, 0, y[j])
    }
  }
}

x[4]
2*mean(p[(x[3]+x[1]+x[2]):(x[4]+x[3]+x[2]+x[1])])
y[4]
var(p[(x[3]+x[1]+x[2]):(x[4]+x[3]+x[2]+x[1])])

mean(p)
var(p)
