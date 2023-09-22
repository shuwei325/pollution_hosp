library(stringr)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(reshape2)
library(tidyverse)
library(latticeExtra)
library(zoo)
library(RColorBrewer)
library(car)
library(astsa)

load(file="./Data/datos_finales2.RData")


datos_finales <- datos_finales %>% mutate(log_precip=log(precip+1),
                                          egreso_1 = lag(egreso),
                                          egreso_2 = lag(egreso,n=2),
                                          dif_egreso=egreso-lag(egreso)) %>% na.omit()

hist(datos_finales$log_precip)

datos_finales <- datos_finales %>% mutate(aerosol = distancia2) %>%
  drop_na()

# Egresos -----------------------------------------------------------------


#Egresos nacionales

egresos_CR <- datos_finales %>% group_by(date) %>% summarise(egreso = sum(egreso))
plot_CR1 <- egresos_CR %>% ggplot( aes(x=date, y=egreso)) +
  geom_line() +
  geom_point()

plot_CR1

#Egresos por región
plot_CR2 <- datos_finales %>% 
  ggplot( aes(x=date, y=egreso, group = Región, color = Región)) +
  geom_line() +
  geom_point()

plot_CR2


# Clima -----------------------------------------------------------------

#Tmax por región
datos_finales %>% 
  ggplot( aes(x=date, y=Tmax, group = Región, color = Región)) +
  geom_line() +
  geom_point()

#Tmin por región
datos_finales %>% 
  ggplot( aes(x=date, y=Tmin, group = Región, color = Región)) +
  geom_line() +
  geom_point()

#Precip por región
datos_finales %>% 
  ggplot( aes(x=date, y=precip, group = Región, color = Región)) +
  geom_line() +
  geom_point()

datos_finales %>% 
  ggplot( aes(x=date, y=log_precip, group = Región, color = Región)) +
  geom_line() +
  geom_point()

#AOD
datos_finales %>% filter(Región=="Brunca")%>%
  ggplot( aes(x=date, y=aerosol, group = Región, color = Región)) +
  geom_line() +
  geom_point()




# correlacion -------------------------------------------------------------

names(datos_finales)
corr <- round(cor(datos_finales[,-c(1,2,3,5)]), 4)

ggcorrplot(corr, type = "lower",
           lab = TRUE,lab_size = 3)

with(datos_finales,plot(egreso~log_precip))

scatterplotMatrix(~egreso+Tmax+Tmin+precip,data=datos_finales)



# Por región --------------------------------------------------------------

Region_B <- datos_finales %>% filter(Región == "Brunca")
Region_CN <- datos_finales %>% filter(Región == "Central Norte")
Region_CS <- datos_finales %>% filter(Región == "Central Sur")
Region_CH <- datos_finales %>% filter(Región == "Chorotega")
Region_HA <- datos_finales %>% filter(Región == "Huetar Atlántica")
Region_HN <- datos_finales %>% filter(Región == "Huetar Norte")
Region_PC <- datos_finales %>% filter(Región == "Pacífico Central")

Region<-Region_B

with(Region,plot(egreso,Tmax))
with(Region,plot(egreso,Tmin))
with(Region,plot(egreso,log_precip))
with(Region,plot(egreso,aerosol))

(corr<-round(cor(Region[,c(4,6,7,8)]), 4))
plot_corr <- ggcorrplot(corr, type = "lower",
                        lab = TRUE,lab_size = 3)

acf(Region$egreso,lag.max=100)
ccf(Region$egreso,Region$Tmax,lag.max=40)
ccf(Region$egreso,Region$Tmin,lag.max=40)
ccf(Region$egreso,Region$log_precip,lag.max=40)
ccf(Region$egreso,Region$aerosol,lag.max=40)

with(Region,lag2.plot (Tmax,egreso, 10))
with(Region,lag2.plot (Tmin,egreso, 10))
with(Region,lag2.plot (log_precip,egreso, 10))
with(Region,lag2.plot (aerosol,egreso, 10))

#modelos lineales

mod0<-lm(egreso~Tmax+Tmin+precip+aerosol,data=Region)
summary(mod0)
mod1<-lm(egreso~egreso_1+Tmax+Tmin+precip+aerosol,data=Region)
summary(mod1)
mod2<-lm(egreso~egreso_1+egreso_2+Tmax+Tmin+precip+aerosol,data=Region)
summary(mod2)
anova(mod1,mod2)

plot(Region$egreso,type="l")
points(fitted(mod2),type="l",col=2)

plot(Region$egreso,fitted(mod0))






