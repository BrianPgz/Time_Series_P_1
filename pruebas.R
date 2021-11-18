# Pruebas
# read
library(forecast)
library(timeSeries)
library(nortest)
library(tseries)
library(TSA)
library(lmtest)
library(ggplot2)
library(strucchange)
data=read.csv("monthly_beer.csv")

data_ts=ts(data = data$Monthly.beer.production, start=c(1956,1),end = c(1995,8),frequency = 12)
data_ts
ts.plot(data_ts)
bp.data <- breakpoints(data$Monthly.beer.production ~ 1)
summary(bp.data)



#point=cpt.mean(data_ts,method = "PELT") #342   en agosto de 1969
#point=breakpoints(data$Monthly.beer.production ~1, breaks = 1)


point=data_ts[324] 

part1=data_ts[1:324]
part1=as.data.frame(part1)
part2=data_ts[325:length(data_ts)]
part2=as.data.frame(part2)

one_time = ts(part1,start = c(1956,1),end = c(1982,12),frequency = 12)
ts.plot(one_time)

second_time=ts(part2,start = c(1983,1),end = c(1995,8),frequency = 12)
ts.plot(second_time)

"
Trabajemos con la parte 1

"
t1=seq(1956,1982.999,by=1/12)
bptest(one_time~t1)
adf.test(one_time)
kpss.test(one_time)

trans_one=log(one_time)
ts.plot(trans_one)
bptest(trans_one~t1)
adf.test(trans_one)
kpss.test(trans_one)

dif_one=diff(trans_one)
ts.plot(dif_one)
time=1:length(dif_one)
bptest(dif_one~time)
adf.test(dif_one)
kpss.test(dif_one)


fit=auto.arima(data_ts)
summary(fit)

residuals_num=as.numeric(fit$residuals)
t_res=1:length(residuals_num)
bptest(residuals_num~t_res)

"
Trabajemos con la parte 2

"

ts.plot(second_time)
t2=seq(1983,1995.59,by=1/12)
bptest(second_time~t2)#la segunda parte tiene varianza constante
adf.test(second_time)#rechaza Ho, es estacionaria
kpss.test(second_time)#aceptar Ho, es estacionaria

fit2=auto.arima(second_time)
summary(fit2)

residuals_num2=as.numeric(fit2$residuals)
plot(residuals_num2)
hist(residuals_num2)
t_res2=1:length(residuals_num2)
bptest(residuals_num2~t_res2) #los residuales de la segunda parte tienen varianza constante
jarque.bera.test(residuals_num2)#pero no son normales
shapiro.test(residuals_num2)#no son normales
t.test(residuals_num2)#media cero 
checkresiduals(fit2)
tsdiag(fit2)#hay mucha correlacion

#Vamos a hacer transformaciones

log_second=log(second_time)
ts.plot(log_second)
decom_log=decompose(log_second)
plot(decom_log)
log_arima=auto.arima(log_second)
summary(log_arima)
residual_log=as.numeric(log_arima$residuals)
t_t=1:length(residual_log)
bptest(residual_log~t_t)
jarque.bera.test(residual_log)
shapiro.test(residual_log)
tsdiag(log_arima)
checkresiduals(log_arima)

ARMA_forecast<- predict(log_arima, n.ahead =24)$pred 
ARMA_forecast_se <- predict(log_arima, n.ahead = 24)$se
ts.plot(log_second, xlim=c(1983,1997), main="Prediccion")
points(ARMA_forecast, type = "l", col = "blue")
#Intervalos de prediccion
points(ARMA_forecast - qnorm(0.975)*ARMA_forecast_se, type = "l", col ="red", lty = 2)
points(ARMA_forecast + qnorm(0.975)*ARMA_forecast_se, type = "l", col ="red", lty = 2)


forecast2anios<- forecast(log_arima, h= 24, level=c(80,95), fan = FALSE, lambda = NULL, biasadj = NULL)
forecast2anios
plot(forecast2anios)

#pasa todos los supuestos
#Vamos a hacer las predicciones 
