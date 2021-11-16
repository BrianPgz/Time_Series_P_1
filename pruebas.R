# Pruebas
# read
data=read.csv("monthly_beer.csv")

data_ts=ts(data = data$Monthly.beer.production, start=c(1956,1),end = c(1995,8),frequency = 12)
data_ts

tsdisplay(data_ts)

auto=auto.arima(data_ts)
summary(auto)
ggtsdisplay(auto$residuals)
tsdiag(auto) #SE MUERE EL P-VALUE 
checkresiduals(auto)




sq_data= log(data_ts)
t1=seq(1956,1995.59,by=1/12)
bptest(sq_data ~ t1)
tsdisplay(sq_data)
mod=auto.arima(sq_data)
summary(mod)

t1 = 1: length(mod$residuals)
bptest(mod$residuals ~ t1)
ggtsdisplay(mod$residuals)
tsdiag(mod, gof.lag = 50)
checkresiduals(mod) #NO JALA EL P-VALUE PARA EL LOGARITMO 




sqrt_m=sqrt(data_ts)
ts.plot(sqrt_m)
otro_mod=auto.arima(sqrt_m)
summary(otro_mod)
tsdiag(otro_mod, gof.lag = 50) #No jala la correlaion 


diff_mod=(diff(data_ts))
ts.plot(diff_mod)
condiff=auto.arima(diff_mod)
summary(condiff)
tsdiag(condiff, gof.lag = 50)



diff_mod=diff(sq_data)
ts.plot(diff_mod)
condiff=auto.arima(diff_mod)
summary(condiff)
tsdiag(condiff, gof.lag = 50)




sarim=sarima(log(data_ts),p=0,d = 1,q=1,P=0,D=1,Q=1,S=12)


arim=arima(diff(log(data_ts)),order = c(1,0,1))
arim
tsdiag(arim)
time= 1:length(arim$residuals)
res_num=as.numeric(arim$residuals)
bptest(res_num~time)#varianza constante de residuales
jarque.bera.test(arim$residuals) #es normal
shapiro.test(arim$residuals) #es normal 
t.test(arim$residuals,mu=0)#media cero




