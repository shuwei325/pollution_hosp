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

names(datos_finales)

datos_finales <- datos_finales %>% mutate(precip_max_max=log(precip_max_max+1),
                                          precip_max_mean=log(precip_max_mean+1),
                                          precip_max_min=log(precip_max_min+1),
                                          precip_mean_mean=log(precip_mean_mean+1),
                                          n_precip_max_Q3=log(n_precip_max_Q3+1),
                                          aerosol = aerosol*1000,
                                          egreso_1 = lag(egreso),
                                          egreso_2 = lag(egreso,n=2),
                                          dif_egreso=egreso-lag(egreso)) #%>% na.omit()


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
  ggplot( aes(x=date, y=Tmax_max, group = Región, color = Región)) +
  geom_line() +
  geom_point()

datos_finales %>% 
  ggplot( aes(x=date, y=Tmax_mean, group = Región, color = Región)) +
  geom_line() +
  geom_point()

#Tmin por región
datos_finales %>% 
  ggplot( aes(x=date, y=Tmin_min, group = Región, color = Región)) +
  geom_line() +
  geom_point()

#Precip por región
datos_finales %>% 
  ggplot( aes(x=date, y=precip_max_max, group = Región, color = Región)) +
  geom_line() +
  geom_point()

#AOD
datos_finales %>% filter(Región=="Brunca")%>%
  ggplot( aes(x=epi.week, y=aerosol, color = Región)) +
  geom_line() +
  geom_vline(xintercept = c(10,22), linetype="dotted",  #mes de marzo a mayo.
             color = "blue", size=1.5) +
  facet_wrap(~year)

datos_finales %>% filter(Región=="Central Norte")%>%
  ggplot( aes(x=epi.week, y=aerosol, color = Región)) +
  geom_line() +
  geom_vline(xintercept = c(10,22), linetype="dotted",  #mes de marzo a mayo.
             color = "blue", size=1.5) +
  facet_wrap(~year)


# correlacion -------------------------------------------------------------

datos_finales_na<- datos_finales %>% drop_na()
corr <- round(cor(datos_finales_na[,-c(1,2,3,5)]), 4)

ggcorrplot(corr, type = "lower",
           lab = TRUE,lab_size = 3)

with(datos_finales_na,plot(egreso~Tmax_mean))
with(datos_finales_na,plot(egreso~Tmin_min))
with(datos_finales_na,plot(egreso~n_Tmin_Q1))
with(datos_finales_na,plot(egreso~amplitude_max_min))
with(datos_finales_na,plot(egreso~precip_max_max))
with(datos_finales_na,plot(egreso~precip_mean_mean))
with(datos_finales_na,plot(egreso~aerosol))

# scatterplotMatrix(~egreso+Tmax_max+Tmax_mean+Tmax_min+n_Tmax_Q3+
#                    Tmin_min+Tmin_mean+Tmin_max+n_Tmin_Q1,data=datos_finales)
# scatterplotMatrix(~egreso+precip_max+precip_median+precip_sum+precip_mean,data=datos_finales)


# Por región --------------------------------------------------------------

Region_B <- datos_finales %>% filter(Región == "Brunca") %>% na.omit()
Region_CN <- datos_finales %>% filter(Región == "Central Norte") %>% na.omit()
Region_CS <- datos_finales %>% filter(Región == "Central Sur") %>% na.omit()
Region_CH <- datos_finales %>% filter(Región == "Chorotega") %>% na.omit()
Region_HA <- datos_finales %>% filter(Región == "Huetar Atlántica") %>% na.omit()
Region_HN <- datos_finales %>% filter(Región == "Huetar Norte") %>% na.omit()
Region_PC <- datos_finales %>% filter(Región == "Pacífico Central") %>% na.omit()

egresos <- data.frame(B=Region_B$egreso,CN=Region_CN$egreso,CS=Region_CS$egreso,
                      CH=Region_CH$egreso,HA=Region_HA$egreso,HN=Region_HN$egreso,
                      PC=Region_PC$egreso)
corr<-cor(egresos)

(plot_corr <- ggcorrplot(corr, type = "lower",
                        lab = TRUE,lab_size = 3))

Region<-Region_CS


with(Region,plot(egreso~Tmax_mean))
with(Region,plot(egreso~Tmin_min))
with(Region,plot(egreso~n_Tmin_Q1))
with(Region,plot(egreso~amplitude_max_min))
with(Region,plot(egreso~precip_max_max))
with(Region,plot(egreso~precip_mean_mean))
with(Region,plot(egreso~aerosol))

corr <- round(cor(Region[,c(4,7,10,13,16,22,25,28)]), 4)

ggcorrplot(corr, type = "lower",
           lab = TRUE,lab_size = 3)


acf(Region$egreso,lag.max=100)
ccf(Region$egreso,Region$Tmax_mean,lag.max=40)
ccf(Region$egreso,Region$Tmin_min,lag.max=40)
ccf(Region$egreso,Region$n_Tmin_Q1,lag.max=40)
ccf(Region$egreso,Region$amplitude_max_min,lag.max=40)
ccf(Region$egreso,Region$precip_max_max,lag.max=40)
ccf(Region$egreso,Region$precip_mean_mean,lag.max=40)
ccf(Region$egreso,Region$aerosol,lag.max=40)
ccf(Region$egreso,Region$aerosol,lag.max=40,plot = FALSE)
with(Region,lag2.plot (aerosol,egreso, 20))



#modelos lineales

test<-Region %>% mutate(aerosol_10=lag(aerosol,10))


names(test)
corr <- round(cor(test[,c(4,7,10,13,16,22,25,28,32)], use ="complete.obs"), 4)

ggcorrplot(corr, type = "lower",
           lab = TRUE,lab_size = 3)

test1<-test %>% drop_na()

mod0<-lm(egreso~Tmax_mean+Tmin_min+n_Tmin_Q1+amplitude_max_min+
           precip_max_max+precip_mean_mean+aerosol_10,data=test1)
summary(mod0)

mod1<-lm(egreso~egreso_1+Tmax_mean+Tmin_min+n_Tmin_Q1+amplitude_max_min+
           precip_max_max+precip_mean_mean+aerosol_10,data=test1)
summary(mod1)

mod2<-lm(egreso~egreso_1+egreso_2+Tmax_mean+Tmin_min+n_Tmin_Q1+amplitude_max_min+
           precip_max_max+precip_mean_mean+aerosol_10,data=test1)
summary(mod2)

anova(mod1,mod2)

plot(Region$egreso,type="l")
points(fitted(mod2),type="l",col=2)

plot(test1$egreso,fitted(mod0))
plot(test1$egreso,fitted(mod1))
plot(test1$egreso,fitted(mod2))




















