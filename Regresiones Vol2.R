library(lubridate)
library(readr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(plyr)
library(forecast)
library(stats)
library(tseries)
library(performance)
library(quantmod)
library(lmtest)
library(moments)
library(dynlm)
library(fpp2)
library(readxl)
library(mlr)
library(urca)


#Exportamos todas nuestras variables

PWTI <- read_csv("Precios WTI(2).csv")
PWTI$Fechas     <- parse_date_time(PWTI$Date, "mdy")

PWTIM2<-read_csv("PWTIM2.csv")

#Filtramos lo necesario para manipular el archivo

for(i in 1:length(PWTIM2$X1)){
  if(is.na(PWTIM2$Price[i])){
    PWTIM2$Price[i] <- PWTIM2$Price[i-1] 
  }
  if(is.na(PWTIM2$Volumen[i])){
    PWTIM2$Volumen[i] <- PWTIM2$Volumen[i-1]
  }
}

PrimerosMes <- PWTIM2
PrimerosMes$Dia <- rep(NA,length(PWTIM2$X1))

for(j in 1:length(PWTIM2$X1)){
  PrimerosMes$Dia[j] <- day(dmy(PWTIM2$fecha[j]))
}

PrimerosMes <- PrimerosMes %>% filter(Dia == 1)

OPEC_tasa<-read_excel("TYOPEC.xlsx")

volumen <- PrimerosMes$Volumen
which(volumen %in% min(volumen))       #en el renglón 221 está el precio negativo
volumen[20] <- .01                    #cambiando el valor negativo por .01
min(volumen)                      

#Realizando nuestras series de tiempo

volumen=ts(PrimerosMes$Volumen,start=2002,frequency=12)
precio=ts(PrimerosMes$Price,start=2002,frequency=12)
precioprom=ts(PrimerosMes$PMENS,start=2002,frequency=12)
volumenprom=ts(PrimerosMes$VMENS,start=2002,frequency=12)
tasa=ts(OPEC_tasa$IRLTUSA,start=2002,frequency=12)
OPEC=ts(OPEC_tasa$OPECMEN,start=2002,frequency=12)

plot(precio)
plot(tasa)
plot(OPEC)

#transformamos todas a logaritmos

logvol=log(volumen)
logprecio=log(precio)
logprecioprom=log(precioprom)
logvolumenprom=log(volumenprom)
logtasa=log(tasa)
logOPEC=log(OPEC)

datos<-cbind(logvol,logprecio,logprecioprom,logvolumenprom,logtasa,logOPEC)
datos[is.na(datos)]=0
#generando nuestros modelos de regresiones

modelo1<-lm(logprecioprom~logvolumenprom+logtasa+logOPEC,data=datos)
summary(modelo1)
residuales<-modelo1$residuals
summary(residuales)
residualPlot(modelo1) #no son estacionarios, no se puede comprobar, debido a que se puede observar
#que de poco a poco se mueven en la media pero no oficialmente.

#Realizando la metodología de Engle-Granger

y=ur.df(residuales) #ur.df:raíz unitaria,prueba de dickey fuller
summary(y)
y@teststat #punto de comparación
y@cvaln #ubicar los valores críticos
#Si el valor de t es menor en valor absoluto al valor crítico de la prueba
#entonces los residuos no son estacionarios y la serie no está cointegrada.
# Por lo tanto nuestros errores sí son estacionarios. ya que 3.2>1.95

#selección de lags para Johansen

lagselect<-VARselect(datos,lag.max=7,type="const")
lagselect$selection #se elige el 2-1=1

#realizando ahora nuestras pruebas de cointegración

jotest=ca.jo(datos, type="trace", K=2, ecdet="const")
summary(jotest)

#después de observar nuestra prueba podemos concluir que hay al menos dos relaciones de 
#cointegración y máximo dos.




