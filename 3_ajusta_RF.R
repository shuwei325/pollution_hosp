library(tidyverse)
library(tidymodels)
library(vip)

load('Data/datos_finales2.RData')

datos_finales <- datos_finales %>% mutate(precip_max_max=log(precip_max_max+1),
                                          precip_max_mean=log(precip_max_mean+1),
                                          precip_max_min=log(precip_max_min+1),
                                          precip_mean_mean=log(precip_mean_mean+1),
                                          n_precip_max_Q3=log(n_precip_max_Q3+1),
                                          aerosol = aerosol*1000,
                                          egreso_1 = lag(egreso),
                                          egreso_2 = lag(egreso,n=2),
                                          dif_egreso=egreso-lag(egreso)) #%>% na.omit()

datos_finales <- datos_finales %>% mutate(aerosol_10=lag(aerosol,10)) %>% na.omit()

Region_B <- datos_finales %>% filter(Región == "Brunca")
Region_CN <- datos_finales %>% filter(Región == "Central Norte")
Region_CS <- datos_finales %>% filter(Región == "Central Sur")
Region_CH <- datos_finales %>% filter(Región == "Chorotega")
Region_HA <- datos_finales %>% filter(Región == "Huetar Atlántica")
Region_HN <- datos_finales %>% filter(Región == "Huetar Norte")
Region_PC <- datos_finales %>% filter(Región == "Pacífico Central")

Region <- Region_CS

set.seed(123)
data_split <- initial_time_split(Region, prop = 18/19)

data_train <- training(data_split)
data_test <- testing(data_split)




outcome <- "egreso"  
predictors_m1 <- c('egreso_1','Tmax_mean','Tmin_min','n_Tmin_Q1',
                   'amplitude_max_min','precip_max_max',
                   'precip_mean_mean','aerosol_10')

# data_recipe <- recipe(data_train, 
#                       formula = as.formula(paste(outcome, "~", paste(predictors, collapse = " + "))) %>%
#                         step_dummy(all_nominal(), -all_outcomes()) %>%
#                         step_zv(all_predictors()) %>%
#                         step_center(all_predictors()) %>%
#                         step_scale(all_predictors())
data_recipe_m1 <- recipe(data_train,
                      formula = as.formula(paste(outcome, "~", paste(predictors_m1, collapse = " + "))),
                      data = Region) 

rf_spec <- rand_forest(mode = "regression",
                       trees = 100,
                       mtry = 3) %>% 
  set_engine("ranger",importance = "impurity")
                      
rf_wf_m1 <- workflow() %>%
  add_recipe(data_recipe_m1) %>%
  add_model(rf_spec)
                      
rf_fit_m1 <- rf_wf_m1 %>% fit(data = data_train)

rf_vip_m1 <- rf_fit_m1 %>%
  extract_fit_parsnip() %>% vip()
                      
predictions <- rf_fit_m1 %>% predict(data_test)
                      
rf_results <- predictions %>% bind_cols(data_test) %>%
  yardstick::metrics(truth = egreso, estimate = .pred)
                      
print(rf_results)

data_plot <- predictions %>% bind_cols(data_test) %>% 
  select(date,egreso,.pred) %>%
  pivot_longer(cols = egreso:.pred,names_to = 'Type',values_to = 'Number')

ggplot()+
  geom_line(mapping = aes(x = date,y = Number,col = Type),data = data_plot)+
  xlab('Date')+
  theme_bw()

rf_vip_m1
                      
