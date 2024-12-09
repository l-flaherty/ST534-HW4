#################Written By Liam Flaherty For ST534 HW 4###############################
#####1. Load Required Packages#####
library(stats)
library(tseries)
path="Academics/ST534 Time Series/Homework/Homework 4/Data3.csv"
data3=read.csv(path)
summary(data3)
str(data3)
ts_data3=ts(data3$Val)                #Convert from DF to Time Series Object#



###1a. EDA###
plot(data3, 
     type="l", 
     lwd="2",
     col="blue",
     main="Data Set 3",
     xlab="Time",
     ylab="Value")

whitenoise6=Box.test(ts_data3,                #Do we need to fit model?#
                     lag=6, 
                     type="Ljung-Box")   
whitenoise6                                   #p small \implies yes#

whitenoise12=Box.test(ts_data3, 
                      lag=12, 
                      type="Ljung-Box")
whitenoise12



###1b. Plot ACF and PACF###
length2=nrow(data3)

par(mfrow=c(1,2))
data3_acf=acf(ts_data3, lag.max=40, 
               main=paste0("Time Series Data Of Length ", length2, "\n", "Estimated ACF"), 
               ci.col="blue", 
               col="red", 
               lwd=4)
data3_acf                            #ACF dies out slowly#

data3_pacf=pacf(ts_data3, lag.max=40, 
                 main=paste0("Time Series Data Of Length ", length2, "\n", "Estimated PACF"), 
                 ci.col="blue", 
                 col="red", 
                 lwd=4)
data3_pacf                          #PACF cuts off at lag 2#



###1c. Augmented Dickey-Fuller (Unit Root Test)###
adf_result=adf.test(ts_data3)           #Null is that there is a root#
adf_result                              #since p is 0.2, don't reject null; assume non-stationary#

diff_data3=diff(ts_data3, differences = 1)

par(mfrow=c(1,1))
plot(diff_data3,
     main="Differenced Data",
     xlab="Time",
     ylab="Value",
     col="black",
     lwd=2)

par(mfrow=c(1,2))
data3diff_acf=acf(diff_data3, lag.max=40, 
              main=paste0("Differenced Time Series Data Of Length ", length2-1, "\n", "Estimated ACF"), 
              ci.col="blue", 
              col="red", 
              lwd=4)
data3diff_acf                            #ACF dies out slowly#

data3diff_pacf=pacf(diff_data3, lag.max=40, 
                main=paste0("Differenced Time Series Data Of Length ", length2-1, "\n", "Estimated PACF"), 
                ci.col="blue", 
                col="red", 
                lwd=4)
data3diff_pacf  



###1d. Fitting Models###
#AR(2)#
ar2=arima(ts_data3, order=c(2,0,0)) 
summary(ar2)

resid_ar2=residuals(ar2)
acf(resid_ar2,
    main="AR(2) Residual ACF",
    col="red",
    lwd=4)

pacf(resid_ar2,
     main="AR(2) Residual PACF",
     col="red",
     lwd=4)

AIC(ar2)
BIC(ar2)
resid_ar2=residuals(ar2)
result_ar2=Box.test(resid_ar2, lag=20, type="Ljung-Box")
result_ar2                                    #p-value still low \implies need to fit more#


#ARIMA Model Diagnostics#
ARIMA_model=vector()
aic=vector()              
bic=vector()
LBtest=vector()

for (p in 1:3) {
  for (q in 1:3) {
    model=arima(ts_data3, order=c(p-1,1,q-1))
    resid=residuals(model)
    
    ARIMA_model[3*(p-1)+q]=paste0("ARIMA(", p-1, ",1,", q-1, ")")
    aic[3*(p-1)+q]=round(AIC(model),2)
    bic[3*(p-1)+q]=round(BIC(model),2)
    LBtest[3*(p-1)+q]=round(Box.test(resid, lag=21, type="Ljung-Box")$p.value,2)
  }
}

df=data.frame(ARIMA_model, aic, bic, LBtest)
df



#ARIMA(2,1,1)#
arima211=arima(ts_data3, order=c(2,1,1)) 
summary(arima211)

resid_arima211=residuals(arima211)
acf(resid_arima211,
    main="ARIMA(2,1,1) Residual ACF",
    col="red",
    lwd=4)
pacf(resid_arima211,
     main="ARIMA(2,1,1) Residual PACF",
     col="red",
     lwd=4)



###1e. Forecast###
pred=predict(arima211, n.ahead = 12)
pred





#####2. Problem 2#####
alpha=0.05                            #given#
qnorm(alpha/2, lower.tail=FALSE)      #critical value#



