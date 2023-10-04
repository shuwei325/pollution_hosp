library(tidyverse)
library(tidymodels)

load('Data/datos_finales2.RData')

datos_finales <- datos_finales %>% mutate(log_precip=log(precip+1),
                                          egreso_1 = lag(egreso),
                                          egreso_2 = lag(egreso,n=2),
                                          dif_egreso=egreso-lag(egreso)) %>% 
  na.omit()


Region_B <- datos_finales %>% filter(Región == "Brunca")
Region_CN <- datos_finales %>% filter(Región == "Central Norte")
Region_CS <- datos_finales %>% filter(Región == "Central Sur")
Region_CH <- datos_finales %>% filter(Región == "Chorotega")
Region_HA <- datos_finales %>% filter(Región == "Huetar Atlántica")
Region_HN <- datos_finales %>% filter(Región == "Huetar Norte")
Region_PC <- datos_finales %>% filter(Región == "Pacífico Central")

Region <- Region_PC

set.seed(123)
data_split <- initial_time_split(Region, prop = 18/19)
data_train <- training(data_split)
data_test <- testing(data_split)

outcome <- "egreso"  
predictors <- c('egreso_1','Tmax','Tmin','precip','aerosol')

# data_recipe <- recipe(data_train, 
#                       formula = as.formula(paste(outcome, "~", paste(predictors, collapse = " + "))) %>%
#                         step_dummy(all_nominal(), -all_outcomes()) %>%
#                         step_zv(all_predictors()) %>%
#                         step_center(all_predictors()) %>%
#                         step_scale(all_predictors())
data_recipe <- recipe(data_train,
                      formula = as.formula(paste(outcome, "~", paste(predictors, collapse = " + "))),
                      data = Region) 

rf_spec <- rand_forest(mode = "regression",
                       trees = 100,
                       mtry = 3)
                      
rf_wf <- workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(rf_spec)
                      

rf_fit <- rf_wf %>% fit(data = data_train)
                      
predictions <- rf_fit %>% predict(data_test)
                      
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

# rf_fit %>% 
#   pull_workflow() %>%
#   vip::vip()
                      
