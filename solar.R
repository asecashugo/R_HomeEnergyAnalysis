library(ggplot2)
library(anytime)
library(lubridate)
library(zoo)
prod<-read.csv("~/R/data/solar/prod5m.csv", stringsAsFactors = FALSE)
#View(titanic)
colnames(prod) <- c("time","W")

prod$time_unix <- anytime(prod$time)
#prod$time_factor <- as.factor(prod$time_unix)
prod$H <- as.numeric(format(prod$time_unix,'%H'))
prod$M <- as.numeric(format(prod$time_unix,'%m'))
prod$D <- yday(prod$time_unix)

# totales por horas y meses
Wave<-numeric(288)
Wmin<-numeric(288)
Wmax<-numeric(288)
H<-numeric(288)
M<-numeric(288)

totales <- data.frame(W,H,M)

for (h in 0:23){
  for (m in 1:12){
    totales$Wave[m+12*h] <- mean(subset(prod, M == m & H == h)$W)
    totales$Wmin[m+12*h] <- max(subset(prod, M == m & H == h)$W)
    totales$Wmax[m+12*h] <- min(subset(prod, M == m & H == h)$W)
    totales$M[m+12*h] <- m
    totales$H[m+12*h] <- h
  }
}

#diarias
KWh<-numeric(365)
D<-numeric(365)
diaria <- data.frame(KWh,D)

for (d in 1:365){
  diaria$D[d]<-d
  diaria$KWh[d]<-sum(subset(prod, D == d)$W)*5/6*1e-4
}
#average semanal
diaria$KWh_roll7 = rollmean(diaria$KWh, 7, na.pad=TRUE)
#average mensual
diaria$KWh_roll30 = rollmean(diaria$KWh, 30, na.pad=TRUE)

#grafico diarias
ggplot() + 
  geom_point(data=diaria,aes(x = D, y = KWh)) +
  geom_line(data=diaria,aes(x = D, y = KWh_roll7),color=2,size=1) +
  geom_line(data=diaria,aes(x = D, y = KWh_roll30),color=3,size=2)

#gráfico por meses
ggplot(totales, aes(x = H, y = Wave, fill = as.factor(M))) + 
  geom_ribbon(data=totales,aes(ymin=Wmin,ymax=Wmax),alpha=.5) +
geom_line(data=totales,aes(x = H, y = Wave,color = as.factor(M),size=1)) +
  facet_grid(~ M)

#alternativo con nube de puntos
ggplot(prod, aes(x = H, y = W, color = as.factor(M))) +
  facet_grid(~ M) +
  geom_point(alpha = 0.1, size = 1)
