library(stringr)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(reshape2)
library(tidyverse)
library(latticeExtra)
library(zoo)
library(RColorBrewer)


load(file="datos_finales.RData")

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

