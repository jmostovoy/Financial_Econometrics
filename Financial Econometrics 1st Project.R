#### Load Data and Transform*####

rm(list = ls())
vixcurrent<- read.csv(file="~/Downloads/vixcurrent.csv")
vxocurrent<- read.csv(file="~/Downloads/vxocurrent.csv")
vixcurrent$Date <- as.Date(vixcurrent$Date, '%m/%d/%Y')
vxocurrent$Date <- as.Date(vxocurrent$Date, '%m/%d/%Y')
vxo<-vxocurrent[-611, ]
rownames(vxo)<-NULL
vix<-vixcurrent
View(vix)
View(vxo)
rm(vixcurrent)
rm(vxocurrent)

#Define useful vectors to be used soon, and 
#intitalize other vectors
twoyear_vix<-c(1:130)
twoyear_vxo<-c(1:130)
abc<-c(1:130)
xyz<-c(25:154)
month_vix<-c(1:129)
month_vxo<-c(1:129)
bcd<-c(1:129)
wxy<-c(2:130)
cde<-c(2:130)
max_y <- max(vxo[,5], vix[,5])
plot_colours <- c("blue", "red", "forestgreen", "yellow")
plot_colours1 <- plot_colours[c(1,2)]

###### Plot of VIX & VXO Evolution: All Data#####


# Graph using y axis that ranges from 0 to max_y. 
# Removal of axis labels, addition of y-axis limit, 
#specification of VIX's colour

plot(vix[,1], vix[,5], type="l", col=plot_colours1[1], ylim=c(0,max_y), ann=FALSE)

# Graph VXO data
lines(vix[,1], vxo[,5], type="l", lty=1, col=plot_colours1[2])

# Create a title
title(main="VIX vs.VXO Evolution: All Data", 
      col.main="forestgreen", font.main=4)

# Label the x and y axes
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab="Volatility Measure", col.lab=rgb(0,0.6,.7))

# Create a legend
legend(vix[12,1], 75, c("VIX", "VXO"), lty=c(1,1), lwd=c(2.5,2.5), col=plot_colours1)

###### Plot of VIX & VXO Evolution: 2004-2007 (1-755)#####

plot(vix[c(1:755),1], vix[c(1:755),5], type="l", col=plot_colours1[1], 
     ylim=c(0, 25), ann=FALSE)
lines(vix[c(1:755),1], vxo[c(1:755),5], type="l", lty=1, col=plot_colours1[2])
title(main="VIX vs.VXO Evolution: 2004-2007", col.main="forestgreen", font.main=4)
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab="Volatility Measure", col.lab=rgb(0,0.6,.7))
legend(vix[12,1], 10, c("VIX", "VXO"), lty=c(1,1), lwd=c(2.5,2.5), col=plot_colours1)


###### Plot of VIX & VXO Evolution: 756-1512#####

plot(vix[c(756:1512),1], vix[c(756:1512),5], type="l", col=plot_colours1[1], 
     ylim=c(0, max_y), ann=FALSE)
lines(vix[c(756:1512),1], vxo[c(756:1512),5], type="l", lty=1, col=plot_colours1[2])
title(main="VIX vs.VXO Evolution: 2007-2010", col.main="forestgreen", font.main=4)
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab="Volatility Measure", col.lab=rgb(0,0.6,.7))
legend(vix[756,1], 78, c("VIX", "VXO"), lty=c(1,1), lwd=c(2,2), col=plot_colours1)


###### Plot of VIX & VXO Evolution: 2010-2013 (Obs. 1513-2266)#####

plot(vix[c(1513:2266),1], vix[c(1513:2266),5], type="l", col=plot_colours1[1], 
     ylim=c(0, 55), ann=FALSE)
lines(vix[c(1513:2266),1], vxo[c(1513:2266),5], type="l", lty=1, col=plot_colours1[2])
title(main="VIX vs.VXO Evolution: 2010-2013", col.main="forestgreen", font.main=4)
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab="Volatility Measure", col.lab=rgb(0,0.6,.7))
legend(vix[2100,1], 45, c("VIX", "VXO"), lty=c(1,1), lwd=c(2,2), col=plot_colours1)

###### Plot of VIX & VXO Evolution: 2013-Oct2016 2267-3220#####

plot(vix[c(2267:3220),1], vix[c(2267:3220),5], type="l", col=plot_colours1[1], 
     ylim=c(0, 45), ann=FALSE)
lines(vix[c(2267:3220),1], vxo[c(2267:3220),5], type="l", lty=1, col=plot_colours1[2])
title(main="VIX vs.VXO Evolution: 2013-2016", col.main="forestgreen", font.main=4)
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab="Volatility Measure", col.lab=rgb(0,0.6,.7))
legend(vix[2267,1], 40, c("VIX", "VXO"), lty=c(1,1), lwd=c(2,2), col=plot_colours1)


#### Tests For Differences in Evolution Between Vix & VXO####

cor.test(vix[,2],vxo[,2])
cor.test(vix[,3],vxo[,3])
cor.test(vix[,4],vxo[,4])
cor.test(vix[,5],vxo[,5])

#Define New Variable, Diff=Vix-VXO

Diff<-vix[, c(2,3,4,5)]-vxo[, c(2,3,4,5)]
Diff<-cbind(vix$Date, Diff)
names(Diff)<-c("Date", "Vix-VXO Open", "Vix-VXO High", "Vix-VXO Low", "Vix-VXO Close")
View(Diff)

mean(Diff[,2])
mean(Diff[,5])
mean(Diff[,3])
mean(Diff[,4])

(var(Diff[,2]))^.5
(var(Diff[,5]))^.5
(var(Diff[,3]))^.5
(var(Diff[,4]))^.5


plot(Diff[,1], Diff[,5], type="l", col=plot_colours1[1], ann=FALSE)
title(main="VIX Minus VXO Evolution: All Data", col.main="forestgreen", font.main=4)
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab="Volatility Measure Differences", col.lab=rgb(0,0.6,.7))

Diff_perc<-c(1:3220)
for (i in c(1:3220)) {
  Diff_perc[i]<-(max(vix[i, 5],vxo[i, 5])-min(vix[i, 5], vxo[i, 5]))/(min(vix[i, 5],vxo[i, 5]))
}

plot(Diff[,1], Diff_perc, type="l", col=plot_colours1[1], ann=FALSE)
title(main="Percentage Change in Vix to VXO", col.main="forestgreen", font.main=4)
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab="Percentage Volatility Measure Differences", col.lab=rgb(0,0.6,.7))

####Rolling Data for VIX####


#Define function to Compute First Date of Each Month 
#(i.e. 1st, 2nd, 3rd or 4th)
firstDayMonth=function(x)
{           
  x=as.Date(as.character(x))
  day = format(x,format="%d")
  monthYr = format(x,format="%Y-%m")
  y = tapply(day,monthYr, min)
  first=as.Date(paste(row.names(y),y,sep="-"))
  as.factor(first)
}

#Use function to define a vector of the first months
a_vix<- firstDayMonth(vix$Date)
length(a_vix)

#Define a function to create a subset of all
#values between two dates
myfunc_vix <- function(x,y){vix[vix$Date >= x & vix$Date <= y,]}

#Loop to determine number of observations between a two year 
#period with varying initial date
for(i in abc) {
  twoyear_vix[i] <- nrow(myfunc_vix(as.Date(a_vix[abc[i]]), as.Date(a_vix[xyz[i]])))
}
twoyear_vix

#Loop to determine number of observations between a 
#one month  period with varying initial date
for(i in bcd) {
  month_vix[i] <- nrow(myfunc_vix(as.Date(a_vix[bcd[i]]), as.Date(a_vix[wxy[i]])))
}
month_vix<-c(1, month_vix)
month_vix

#Bind these two vectors
domain_vix<- rbind(month_vix, twoyear_vix)
domain_vix

#Loop to link the observation number of the 1st date 
#of each month with regard to the Vix data set
cde<-c(2:130)
for(i in cde) {
  domain_vix[1,i] <- domain_vix[1,i]+domain_vix[1,(i-1)]-1
}
domain_vix

#Loop to link the supremum of the set for which 
#we begin at the 1st date of each month and end 
#two years in the future while accounting for differences 
#in date intervals for 2 year periods
for(i in cde) {
  domain_vix[2,i] <- domain_vix[2,i]+domain_vix[1,i]-2
}
domain_vix

#Compute moving average means
mov_mean_vix<-c(1:130)
for(i in abc) {
  mov_mean_vix[i] <- mean(vix$VIX.Close[t(domain_vix[,i])])
}
mov_mean_vix

#Compute moving average variance
mov_var_vix<-c(1:130)
for(i in abc) {
  mov_var_vix[i] <- var(vix$VIX.Close[t(domain_vix[,i])])
}
mov_var_vix

#Define date range, and note the proper range 
#will be this value + 2 years
date_range_vix<-as.Date(a_vix[1:130])
date_range_vix

#Calculate Quantiles
mov_25_quantile_vix <- c(1:130)
for(i in abc) {
  mov_25_quantile_vix[i] <- unname(quantile(vix$VIX.Close[t(domain_vix[,i])],
                                        probs=c(.25)))
}
mov_25_quantile_vix

mov_75_quantile_vix <- c(1:130)
for(i in abc) {
  mov_75_quantile_vix[i] <- unname(quantile(vix$VIX.Close[t(domain_vix[,i])],
                                        probs=c(.75)))
}
mov_75_quantile_vix


DataSummary_vix<-data.frame(date_range_vix, mov_mean_vix, mov_var_vix, 
                        mov_25_quantile_vix, mov_75_quantile_vix)
View(DataSummary_vix)

##### Graph of Moving mean #####
# Removal of axis labels, addition of y-axis limit, 
#specification of VIX's colour
plot(DataSummary_vix[,1], DataSummary_vix[,2], type="l", col=plot_colours1[1], ann=FALSE)

# Create a title
title(main="Moving Mean of Volatility for VIX", col.main="forestgreen", font.main=3)

# Label the x and y axes
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab="Volatility Mean", col.lab=rgb(0,0.6,.7))

##### Graph of Moving Variance#####

plot(DataSummary_vix[,1], DataSummary_vix[,3], type="l", 
     col=plot_colours1[1], ann=FALSE)
title(main="Moving Variance of Volatility for VIX", col.main="forestgreen", font.main=3)
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab="Volatility Variance", col.lab=rgb(0,0.6,.7))

##### Graph of Moving 25th Quantile#####

plot(DataSummary_vix[,1], DataSummary_vix[,4], type="l", 
     col=plot_colours1[1], ann=FALSE)
title(main="Moving 25th Quantile of Volatility for VIX", col.main="forestgreen", font.main=3)
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab="25th Quantile", col.lab=rgb(0,0.6,.7))

##### Graph of Moving 75th Quantile#####
plot(DataSummary_vix[,1], DataSummary_vix[,5], type="l", 
     col=plot_colours1[1], ann=FALSE)
title(main="Moving 75th Quantile of Volatility for VIX", col.main="forestgreen", font.main=3)
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab="75th Quantile", col.lab=rgb(0,0.6,.7))

#### Graph of Volatility of Volatily####
plot(DataSummary_vix[,1], (DataSummary_vix[,3])^(1/2), 
     type="l", col=plot_colours1[1], ann=FALSE)
title(main="Moving Volatilty of Volatility for VIX", col.main="forestgreen", font.main=3)
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab="Volatilty of Volatility", col.lab=rgb(0,0.6,.7))

####Rolling Data for VXO####


a_vxo<- firstDayMonth(vxo$Date)
length(a_vxo)

myfunc_vxo <- function(x,y){vxo[vxo$Date >= x & vxo$Date <= y,]}

#Loop to determine number of observations between a two year 
#period with varying initial date
for(i in abc) {
  twoyear_vxo[i] <- nrow(myfunc_vxo(as.Date(a_vxo[abc[i]]), as.Date(a_vxo[xyz[i]])))
}
twoyear_vxo

#Loop to determine number of observations between a 
#one month  period with varying initial date
for(i in bcd) {
  month_vxo[i] <- nrow(myfunc_vxo(as.Date(a_vxo[bcd[i]]), as.Date(a_vxo[wxy[i]])))
}
month_vxo<-c(1, month_vxo)
month_vxo

#Bind these two vectors
domain_vxo<- rbind(month_vxo, twoyear_vxo)
domain_vxo

#Loop to link the observation number of the 1st date 
#of each month with regard to the Vix data set
cde<-c(2:130)
for(i in cde) {
  domain_vxo[1,i] <- domain_vxo[1,i]+domain_vxo[1,(i-1)]-1
}
domain_vxo

#Loop to link the supremum of the set for which 
#we begin at the 1st date of each month and end 
#two years in the future while accounting for differences 
#in date intervals for 2 year periods
for(i in cde) {
  domain_vxo[2,i] <- domain_vxo[2,i]+domain_vxo[1,i]-2
}
domain_vxo

#Compute moving average means
mov_mean_vxo<-c(1:130)
for(i in abc) {
  mov_mean_vxo[i] <- mean(vxo$Close[t(domain_vxo[,i])])
}
mov_mean_vxo

#Compute moving average variance
mov_var_vxo<-c(1:130)
for(i in abc) {
  mov_var_vxo[i] <- var(vxo$Close[t(domain_vxo[,i])])
}
mov_var_vxo

#Define date range, and note the proper range 
#will be this value + 2 years
date_range_vxo<-as.Date(a_vxo[1:130])
date_range_vxo

#Calculate Quantiles
mov_25_quantile_vxo <- c(1:130)
for(i in abc) {
  mov_25_quantile_vxo[i] <- unname(quantile(vxo$Close[t(domain_vxo[,i])],
                                        probs=c(.25)))
}
mov_25_quantile_vxo

mov_75_quantile_vxo <- c(1:130)
for(i in abc) {
  mov_75_quantile_vxo[i] <- unname(quantile(vxo$Close[t(domain_vxo[,i])],
                                        probs=c(.75)))
}
mov_75_quantile_vxo


DataSummary_vxo<-data.frame(date_range_vxo, mov_mean_vxo, mov_var_vxo, 
                        mov_25_quantile_vxo, mov_75_quantile_vxo)
View(DataSummary_vxo)

#### Tests of Comparision for Moving Vix vs. VXO Data####

#Check date function is consistent for Vix & VXO
cor.test(as.numeric(DataSummary_vix[,1]), as.numeric(DataSummary_vxo[,1]))

#Perform correlation tests for vix vs. vxo moving data
cor.test(DataSummary_vix[,2],DataSummary_vxo[,2])
cor.test(DataSummary_vix[,3],DataSummary_vxo[,3])
cor.test(DataSummary_vix[,4],DataSummary_vxo[,4])
cor.test(DataSummary_vix[,5],DataSummary_vxo[,5])

##### Graph of Moving mean for VXO#####
# Removal of axis labels, addition of y-axis limit, 
#specification of VIX's colour
plot(DataSummary_vxo[,1], DataSummary_vxo[,2], type="l", 
     col=plot_colours1[1], ann=FALSE)

# Create a title
title(main="Moving Mean of Volatility for VXO", col.main="forestgreen", font.main=3)

# Label the x and y axes
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab="Volatility Mean", col.lab=rgb(0,0.6,.7))

##### Graph of Moving Variance for VXO#####

plot(DataSummary_vxo[,1], DataSummary_vxo[,3], type="l", 
     col=plot_colours1[1], ann=FALSE)
title(main="Moving Variance of Volatility for VXO", col.main="forestgreen", font.main=3)
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab="Volatility Variance", col.lab=rgb(0,0.6,.7))

##### Graph of Moving 25th Quantile for VXO#####

plot(DataSummary_vxo[,1], DataSummary_vxo[,4], type="l", 
     col=plot_colours1[1], ann=FALSE)
title(main="Moving 25th Quantile of Volatility for VXO", col.main="forestgreen", font.main=3)
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab="25th Quantile", col.lab=rgb(0,0.6,.7))

##### Graph of Moving 75th Quantile for VXO#####
plot(DataSummary_vxo[,1], DataSummary_vxo[,5], type="l", 
     col=plot_colours1[1], ann=FALSE)
title(main="Moving 75th Quantile of Volatility for VXO", col.main="forestgreen", font.main=3)
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab="75th Quantile", col.lab=rgb(0,0.6,.7))

#### Graph of Moving Volatility of the VXO####
plot(DataSummary_vxo[,1], (DataSummary_vxo[,3])^(1/2), 
     type="l", col=plot_colours1[1], ann=FALSE)
title(main="Moving Volatilty of Volatility for VXO", col.main="forestgreen", font.main=3)
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab="Volatilty of Volatility", col.lab=rgb(0,0.6,.7))



vix_mov_stats<-c(mean(DataSummary_vix[,2]), mean(DataSummary_vix[,3]), 
                 mean(DataSummary_vix[,4]), mean(DataSummary_vix[,5]), 
                 mean((DataSummary_vix[,3])^.5))

vxo_mov_stats<-c(mean(DataSummary_vxo[,2]), mean(DataSummary_vxo[,3]), 
                 mean(DataSummary_vxo[,4]), mean(DataSummary_vxo[,5]), 
                 mean((DataSummary_vxo[,3])^.5))

data.frame()

vix_mov_stats2<- c(var(DataSummary_vix[,2]), var(DataSummary_vix[,3]), 
                   var(DataSummary_vix[,4]), var(DataSummary_vix[,5]), 
                   var((DataSummary_vix[,3])^.5))

vxo_mov_stats2<-c(var(DataSummary_vxo[,2]), var(DataSummary_vxo[,3]), 
                  var(DataSummary_vxo[,4]),
                  var(DataSummary_vxo[,5]), var((DataSummary_vxo[,3])^.5))

mov_stats<-data.frame(vix_mov_stats, vxo_mov_stats, vix_mov_stats2, vxo_mov_stats2, row.names = c("mean", "var", "25th quantile", " 75th quantile", "vol."))

View(mov_stats)
