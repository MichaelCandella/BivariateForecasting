library(forecast)
library(urca)
library(vars)
library(ggplot2)
library(astsa)
Period<-c(1:224)
WTIDF<-data.frame(Period,Oil$WTI)
Brentdf<-data.frame(Period,Oil$Brent)
#ggplot of WTI
ggplot(WTIDF,aes(x=WTIDF$Period,y=WTIDF$Oil.WTI))+
  geom_line(size=1.5,color="red")+
  ggtitle("World Texas Intermediate Crude Oil Prices over 224 Periods")+
  xlab("Periods (May 1987 to December 2005")+
  ylab("Crude Oil Prices per Barrel")+
  theme(plot.title=element_text(hjust=.5,face="bold"))

#ggplot of Brent
ggplot(Brentdf,aes(x=Brentdf$Period,y=Brentdf$Oil.Brent))+
  geom_line(size=1.5,color="red")+
  ggtitle("Brent Crude Oil Prices over 224 Periods")+
  xlab("Periods (May 1987 to December 2005")+
  ylab("Crude Oil Prices per Barrel")+
  theme(plot.title=element_text(hjust=.5,face="bold"))


#ggplot histogram with density curve for WTI
ggplot(Oil, aes(x=Oil$WTI))+geom_histogram(aes(y=..density..),binwidth = .5,colour="black",fill="#668cff")+geom_density(alpha=.5,fill="#ff1a1a")+
  ggtitle("Density Curve and Histogram of WTI Prices")+
  xlab("Oil Prices per Barrel")+
  theme(plot.title = element_text(hjust=.5,face="bold"))

#ggplot histogram with density curve for Brent
ggplot(Oil, aes(x=Oil$Brent))+geom_histogram(aes(y=..density..),binwidth = .5,colour="black",fill="#668cff")+geom_density(alpha=.5,fill="#ff1a1a")+
  ggtitle("Density Curve and Histogram of Brent Prices")+
  xlab("Oil Prices per Barrel")+
  theme(plot.title = element_text(hjust=.5,face="bold"))

#Convert WTI to Time Series Data
WTI<-ts(Oil$WTI,frequency=12,start=c(1987,5))

#Seasonal and Monthly Plots for Seasonality
seasonplot(WTI,ylab="WTI Oil Prices per Barrel")
monthplot(WTI,ylab = "WTI Oil Prices per Barrel")

#Seasonal and Monthly Plots for Brent
seasonplot(Brent,ylab="Brent Oil Prices per Barrel")
monthplot(WTI,ylab="Brent Oil Prices per Barrel")

#Testing for Stationarity
t0=c(1987,5)
t1=c(2004,12)
t2=c(2005,1)
WTI_train<-window(WTI,start=t0,end=t1)
WTI_test<-winddow(WTI,start=t2)

#ADF on training set
#none
WTI_ADF_NONE<-ur.df(WTI_train,type = "none",lags=12,selectlags = "AIC")
summary(WTI_ADF_NONE)

#Drift
WTI_ADF_Drift<-ur.df(WTI_train,type = "drift",lags=12,selectlags = "AIC")
summary(WTI_ADF_Drift)

#Trend
WTI_ADF_Trend<-ur.df(WTI_train,type = "trend",lags=12,selectlags = "AIC")
summary(WTI_ADF_Trend)

#Differenced Training Data
DWTI<-diff(WTI_train)

#DNone
DWTI_ADF_NONE<-ur.df(DWTI,type = "none",lags=12,selectlags = "AIC")
summary(DWTI_ADF_NONE)

#DDrift
DWTI_ADF_Drift<-ur.df(DWTI,type = "drift",lags=12,selectlags = "AIC")
summary(DWTI_ADF_Drift)

#DTrend
DWTI_ADF_Trend<-ur.df(DWTI,type = "trend",lags=12,selectlags = "AIC")
summary(DWTI_ADF_Trend)


#ARMA Modeling Differenced Set
 #Akaike
auto.arima(DWTI,max.p = 5,max.q = 5,max.order = 10,seasonal=FALSE,stepwise = FALSE,ic='aic')
sarima(DWTI,0,0,4)
 #Bayesian
auto.arima(DWTI,max.p = 5,max.q = 5,max.order = 10,seasonal=FALSE,stepwise = FALSE,ic='bic')
sarima(DWTI,0,0,2)
#VECM
  #Define Brent_Train and Brent Test
Brent<-ts(Oil$Brent,frequency=12,start=c(1987,5))
Brent_train<-window(Brent,start=t0,end=t1)
Brent_test<-window(Brent,start=t2)

  #construct Data Frame
df_level<-data.frame(WTI_train,Brent_train)

#cointegration test for WTI and Brent
vecm<-ca.jo(df_level,type='eigen',ecdet="const",K=2,spec="transitory")
summary(vecm)
#Error Correction Term
vecm.rls<-cajorls(vecm,r=1)
error<-vecm.rls$rlm$model['ect1']

#ADF Test for error correction term and ACF and PACF plotting
ADFECM<-ur.df(error$ect1,type="none",lags=12,selectlags="AIC")
summary(ADFECM)
acf2(error$ect1)

#VECM Results
cajorls(vecm)
coef(summary(cajorls(vecm)$rlm))

#Forecast
var.model<-vec2var(vecm)
H<-12
fc<-predict(var.model,n.ahead=H)


#tranforming forecast values
WTI_forecast<-ts(fc$fcst$WTI_train[1:H,1],frequency = 12,start=t2)
Brent_forecast<-ts(fc$fcst$Brent_train[1:H,1],frequency = 12,start=t2)

plot.ts(WTI_forecast)
print(WTI_forecast)

plot.ts(Brent_forecast)
print(Brent_forecast)

#RMSE and MAE for WTI
WRMSE<-sqrt(mean(WTI_forecast))
WMAE<-mean(abs(WTI_forecast))

#RMSE and MAE for Brent
BRMSE<-sqrt(mean(Brent_forecast))
WMAE<-mean(abs(Brent_forecast))