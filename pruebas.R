# Pruebas
# read
library(forecast)
library(timeSeries)
library(nortest)
library(tseries)
library(TSA)
library(lmtest)
library(ggplot2)
data=read.csv("monthly_beer.csv")

data_ts=ts(data = data$Monthly.beer.production, start=c(1956,1),end = c(1995,8),frequency = 12)
data_ts
ts.plot(data_ts)
bp.data <- breakpoints(data$Monthly.beer.production ~ 1)
summary(bp.data)



#point=cpt.mean(data_ts,method = "PELT") #342   en agosto de 1969
point=breakpoints(data$Monthly.beer.production, breaks = 1)


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




# tsdisplay(data_ts)
# 
# 
# t1=seq(1956,1995.59,by=1/12)
# bptest(data_ts~t1) #p-value casi cero no tiene varianza constante
# adf.test(data_ts)#p-value de .01 es estacionaria 
# kpss.test(data_ts)#p-value de 0.01 contradice a la anterior con no estacionaridad
# 
# 
# 
# 
# 
# 
# auto=auto.arima(data_ts)
# summary(auto)
# ggtsdisplay(auto$residuals)
# tsdiag(auto) #SE MUERE EL P-VALUE 
# checkresiduals(auto) #descartado
# 
# 
# 
# 
# sq_data= log(data_ts)
# ts.plot(sq_data)
# t1=seq(1956,1995.59,by=1/12)
# bptest(sq_data ~ t1) #varianza constante
# tsdisplay(sq_data)
# mod=auto.arima(sq_data)
# summary(mod)
# t1 = 1: length(mod$residuals)
# bptest(mod$residuals ~ t1)#no varianza constante
# ggtsdisplay(mod$residuals) #alta correlacion
# tsdiag(mod, gof.lag = 50)#muere el p-value
# checkresiduals(mod) #NO JALA EL P-VALUE PARA EL LOGARITMO 
# 
# 
# 
# 
# sqrt_m=sqrt(data_ts)
# ts.plot(sqrt_m)
# otro_mod=auto.arima(sqrt_m)
# summary(otro_mod)
# tsdiag(otro_mod, gof.lag = 50) #No jala la correlaion 
# 
# 
# diff_mod=(diff(data_ts))
# ts.plot(diff_mod)
# condiff=auto.arima(diff_mod)
# summary(condiff)
# tsdiag(condiff, gof.lag = 50)
# 
# 
# 
# diff_mod=diff(sq_data)
# ts.plot(diff_mod)
# condiff=auto.arima(diff_mod)
# summary(condiff)
# tsdiag(condiff, gof.lag = 50)
# 
# 
# 
# 
# sarim=sarima(log(data_ts),p=0,d = 1,q=1,P=0,D=1,Q=1,S=12)
# 
# 
# arim=arima(diff(log(data_ts)),order = c(1,0,1))
# arim
# tsdiag(arim)
# time= 1:length(arim$residuals)
# res_num=as.numeric(arim$residuals)
# bptest(res_num~time)#varianza constante de residuales
# jarque.bera.test(arim$residuals) #es normal
# shapiro.test(arim$residuals) #es normal 
# t.test(arim$residuals,mu=0)#media cero
# checkresiduals(arim)
# 
# 
# 
# 
# 
# 
