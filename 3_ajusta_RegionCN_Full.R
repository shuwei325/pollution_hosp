library(tidyverse)
library(tidymodels)
library(vip)
library(doParallel)
library(modeltime)
library(modeltime.ensemble)
library(fastshap)

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

Region <- Region_CN

set.seed(123)

data_split <- initial_time_split(Region, prop = 18/19)

data_train <- training(data_split)
data_test <- testing(data_split)


outcome <- "egreso"  
# predictors <- c('egreso_1','Tmax_mean','Tmin_min','n_Tmin_Q1',
#                 'amplitude_max_min','precip_max_max',
#                 'precip_mean_mean','aerosol_10')
predictors <- names(datos_finales)[-c(1,2,3,4,5,31)]

data_recipe <- recipe(formula = as.formula(paste(outcome, "~", paste(predictors, collapse = " + "))),
                      data = data_train) 

data_split_train <- sliding_period(data_train,index = date,"year",
                                   lookback = 2,
                                   assess_stop = 1)

### RANDOM FOREST ##############

rf_spec <- rand_forest(mode = "regression",
                       trees = 1000,
                       mtry = tune(),
                       min_n = tune()) %>% 
  set_engine("ranger")

rf_wf <- workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(rf_spec)


registerDoParallel()

tune_res <- tune_grid(rf_wf,
                      resamples = data_split_train,grid = 20)

tune_res  %>% collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean,min_n,mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "RMSE")

rf_grid <- grid_regular(
  mtry(range = c(10, 20)),
  min_n(range = c(15, 25)),
  levels = 5
)



regular_res <- tune_grid(rf_wf,
                         resamples = data_split_train,
                         grid = rf_grid)

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "RMSE")


best_rmse <- select_best(regular_res,"rmse")

final_rf <- finalize_model(rf_spec ,best_rmse)

data_train_final <- data_train %>% filter(year %in% c(2016,2017,2018))

final_spec <- final_rf %>% 
  set_engine("ranger",importance = "impurity")

final_wf <- workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(final_spec)


final_fit_RF <- final_wf %>% fit(data = data_train_final)

modeltime_RF <- modeltime_table(final_fit_RF)

final_vip_RF <- final_fit_RF %>%
  extract_fit_parsnip() %>% vip()

final_vip_RF

data_actual <- data_train_final %>% bind_rows(data_test)
calibration_tbl <- final_fit_RF %>% modeltime_calibrate(new_data = data_test)

calibration_tbl_RF <- calibration_tbl %>% modeltime_forecast(new_data = data_test,
                                                             actual_data = data_actual,
                                                             conf_method = "conformal_split")

calibration_tbl_tr <- final_fit_RF %>% modeltime_calibrate(new_data = data_train_final)

calibration_tbl_tr_RF <- calibration_tbl %>% modeltime_forecast(new_data = data_train_final,
                                                                actual_data = data_train_final,
                                                                conf_method = "conformal_split")
calibration_tbl_RF %>%
  plot_modeltime_forecast()


calibration_tbl %>% modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = T)


### XGBOOST ##############

xgb_spec <- boost_tree(mode = "regression",
                       trees = 1000,
                       tree_depth = tune(),
                       min_n = tune(),
                       loss_reduction = tune(),
                       sample_size = tune(),
                       mtry = tune(),
                       learn_rate = tune()) %>% 
  set_engine("xgboost")

xgb_wf <- workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(xgb_spec)

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), data_train),
  learn_rate(),
  size = 30
)

stopImplicitCluster()
registerDoParallel()

tune_res <- tune_grid(xgb_wf,
                      resamples = data_split_train,
                      grid = xgb_grid,
                      control = control_grid(save_pred = T))
tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "RMSE")


best_rmse <- select_best(tune_res,"rmse")

final_xgb <- finalize_model(xgb_spec ,best_rmse)

data_train_final <- data_train %>% filter(year %in% c(2016,2017,2018))

final_spec <- final_xgb %>% 
  set_engine("xgboost")

final_wf <- workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(final_spec)


final_fit_XGB <- final_wf %>% fit(data = data_train_final)

modeltime_XGB <- modeltime_table(final_fit_XGB) 

final_vip_XGB <- final_fit_XGB %>%
  extract_fit_parsnip() %>% vip()

final_vip_XGB

data_actual <- data_train_final %>% bind_rows(data_test)
calibration_tbl <- final_fit_XGB %>% modeltime_calibrate(new_data = data_test)

calibration_tbl_XGB <- calibration_tbl %>% modeltime_forecast(new_data = data_test,
                                                              actual_data = data_actual,
                                                              conf_method = "conformal_split") 

calibration_tbl_tr <- final_fit_XGB %>% modeltime_calibrate(new_data = data_train_final)

calibration_tbl_tr_XGB <- calibration_tbl %>% modeltime_forecast(new_data = data_train_final,
                                                                 actual_data = data_train_final,
                                                                 conf_method = "conformal_split")


calibration_tbl_XGB %>%
  plot_modeltime_forecast()


calibration_tbl %>% modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = T)


## PROPHET###########

predictors_prop <- names(datos_finales)[-c(1,2,3,4,31)]
data_recipe_prop <- recipe(formula = as.formula(paste(outcome, "~", paste(predictors_prop, collapse = " + "))),
                           data = data_train) 

prop_spec <- prophet_reg(mode = "regression",
                         prior_scale_changepoints = tune(),
                         prior_scale_seasonality = tune()) %>% 
  set_engine("prophet")

prop_wf <- workflow() %>%
  add_recipe(data_recipe_prop) %>%
  add_model(prop_spec)


registerDoParallel()

tune_res <- tune_grid(prop_wf,
                      resamples = data_split_train,grid = 20)

tune_res  %>% collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean,prior_scale_changepoints,prior_scale_seasonality) %>%
  pivot_longer(prior_scale_changepoints:prior_scale_seasonality,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "RMSE")

prop_grid <- grid_regular(
  prior_scale_changepoints(range = c(0, 1)),
  prior_scale_seasonality(range = c(0, 1)),
  levels = 5
)


regular_res <- tune_grid(prop_wf,
                         resamples = data_split_train,
                         grid = prop_grid)

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  mutate(prior_scale_changepoints = factor(prior_scale_changepoints)) %>%
  ggplot(aes(prior_scale_seasonality, mean, color = prior_scale_changepoints)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "RMSE")

best_rmse <- select_best(regular_res,"rmse")

final_prop <- finalize_model(prop_spec ,best_rmse)

final_spec <- final_prop %>% 
  set_engine("prophet")

final_wf <- workflow() %>%
  add_recipe(data_recipe_prop) %>%
  add_model(final_spec)


final_fit_PROP <- final_wf %>% fit(data = data_train_final)

modeltime_PROP <- modeltime_table(final_fit_PROP)

calibration_tbl <- final_fit_PROP %>% modeltime_calibrate(new_data = data_test)

calibration_tbl_PROP <- calibration_tbl %>% modeltime_forecast(new_data = data_test,
                                                               actual_data = data_actual,
                                                               conf_method = "conformal_split")

calibration_tbl_tr <- final_fit_PROP %>% modeltime_calibrate(new_data = data_train_final)

calibration_tbl_tr_PROP <- calibration_tbl %>% modeltime_forecast(new_data = data_train_final,
                                                                  actual_data = data_train_final,
                                                                  conf_method = "conformal_split")
calibration_tbl_PROP %>%
  plot_modeltime_forecast()


calibration_tbl %>% modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = T)


### ENSEMBLE####

models_RegionB <- modeltime_table(final_fit_RF,final_fit_XGB,final_fit_PROP)

final_fit_ens <- models_RegionB %>% ensemble_average(type = "mean")

ens_cal <- final_fit_ens %>% modeltime_calibrate(new_data = data_test)


calibration_tbl_ENS <- ens_cal %>% modeltime_forecast(new_data = data_test,
                                                      actual_data = data_actual,
                                                      conf_method = "conformal_split") 

ens_tr <- final_fit_ens %>% modeltime_calibrate(new_data = data_train_final)

calibration_tbl_tr_ENS <- ens_tr %>% modeltime_forecast(new_data = data_train_final,
                                                        actual_data = data_train_final,
                                                        conf_method = "conformal_split")


calibration_tbl_ENS %>%
  plot_modeltime_forecast()

ens_cal %>% modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = T)

##VIPS##############
pfun_pr <- function(object, newdata) {  # needs to return a numeric vector
  calibration <- object %>% modeltime_calibrate(new_data = data_test)
  pred_pr <- calibration %>% 
    modeltime_forecast(new_data = newdata,
                       actual_data = data_train_final)
  vv <- pred_pr %>% filter(.key !='actual') %>% select(.value)
  as.numeric(vv$.value)
}

show('Calculo de VP-SHARP')
vipSH_RF <- vip(modeltime_RF,method = 'shap',train = data_train_final,
                feature_names = predictors_prop,pred_wrapper = pfun_pr)

vipSH_XGB <- vip(modeltime_XGB,method = 'shap',train = data_train_final,
                 feature_names = predictors_prop,pred_wrapper = pfun_pr)

vipSH_PROP <- vip(modeltime_PROP,method = 'shap',train = data_train_final,
                  feature_names = predictors_prop,pred_wrapper = pfun_pr)


save(calibration_tbl_RF,calibration_tbl_XGB,calibration_tbl_ENS,
     calibration_tbl_PROP,
     calibration_tbl_tr_RF,calibration_tbl_tr_XGB,calibration_tbl_tr_ENS,
     calibration_tbl_tr_PROP,final_vip_RF,final_vip_XGB,
     vipSH_RF,vipSH_XGB,vipSH_PROP,
     file = 'Results/RegionCN_Full.RData')


