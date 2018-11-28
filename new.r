setwd("E:\\Aradhana")  #setting the path to my data directory
getwd()  #getting the correct path as mentioned above                 
sfpd=read.csv("sfpd1.csv",header=TRUE) # reading my original dataset having 2215024 rows from 13 columns
dd_burg=separate(sfpd,"Date",c("M","D","Y"),sep = "/") #separating the "date" variable in separate columns as month,day and year
burg=filter(dd_burg,dd_burg$'Category'=="BURGLARY") # filtering only the "burglary" crimes  
burg_ym<- filter(burg,Y<2016) %>% group_by(Y) %>% count(M) %>% arrange()  #filtering and counting number of crimes based on month and year alone
#write.csv(burg_ym, "sfpd12.csv", row.names=FALSE, na="")

#statistical analysis if number of crimes
summary(burg_ym$n) #calculating 5 point summary
range(burg_ym$n)
sd(burg_ym$n)

#making the dataset into time series format
burg_ts=ts(burg_ym,start = 2003,end = 2015,frequency = 12) 
seasonplot(burg_ts[,3],s = 12,col=rainbow(16), year.labels=TRUE,main = "Burglary rate by year and month") #showing line chart with color definition
ggseasonplot(burg_ts[,3], polar=TRUE) # showing polar plot

fit.arima.burg <- auto.arima(burg_ts[,3]) #fitting ARIMA model
fit.arima.burg$model 
checkresiduals(fit.arima.burg) 
autoplot(fit.arima.burg)

fit.sn.burg <- snaive(burg_ts[,3]) #fitting seasonal naive method
fit.sn.burg$model
checkresiduals(fit.sn.burg)
autoplot(fit.sn.burg)

fit.n.burg <- naive(burg_ts[,3])#fitting naive method
fit.n.burg$model
checkresiduals(fit.n.burg)
autoplot(fit.n.burg)

#checking accuracy
acc.n=accuracy(fit.n.burg)
acc.sn=accuracy(fit.sn.burg)
acc.arima=accuracy(fit.arima.burg)
acc.n
acc.sn
acc.arima

#making accuracy table
acc.table=rbind.data.frame(acc.n,acc.sn,acc.arima)
acc.table
acc.table$method=c("naive",
                   "seasonal naive","arima")
acc.table
burg_model=acc.table %>% group_by(method) %>% summarise(SumErr=sum(ME,RMSE,MAE,MPE,MAPE,MASE,ACF1)) %>% arrange(SumErr)
burg_model

#forecasting through ARIMA
fc.ar <- forecast(fit.arima.burg,h = 24)
fc.ar$mean
autoplot(fc.ar$mean)
seasonplot(fc.ar$mean)

#forecasting with seasonal naive
fc.sn <- forecast(fit.sn.burg,h = 24)
fc.sn$mean
autoplot(fc.sn$mean)
seasonplot(fc.sn$mean)

#forecasting with naive
fc.n= forecast(fit.n.burg,h = 24)
fc.n$mean
autoplot(fc.n$mean)
seasonplot(fc.n$mean)



N=nrow(burg)
N
p=ncol(burg)
p
install_keras()
install.packages(dummies)
library(dummies)
X=dummy.data.frame(burg[1:12000,-p])
Y=burg[1:12000,p]
Y=to_categorical(burg)
data=cbind(X,Y)
burg1=burg_ym[1:1200,]
train_data=burg1[1:96,]
test_data=burg1[97:nrow(burg1),]
linear=lm(train_data)
summary(linear)
predicted=predict(linear,test_data)
predicted
tabl1=tab(predicted)
