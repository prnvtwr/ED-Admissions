rm(list=ls())
library(tidyverse)
library(stringr)
library(mgcv)
library(MASS)

setwd('/Users/pranav/Documents/ED')
wd <- "/Users/pranav/Documents/ED"


#Variable Lag Definition

var_lagger <- function(var_name, lags) {
  word_list <- list()
  for ( i in 1:lags){
    if (i == 1){
      word_list[[i]] <- paste(sprintf("s(%s",var_name),'lag',sprintf("%s)",i), collapse = NULL, sep = '.')
    }
    else {
      word_list[[i]] <- paste(sprintf("+s(%s",var_name),'lag',sprintf("%s)",i), collapse = NULL, sep = '.')
    }
    combined <- paste(word_list, collapse = "")
  }
  return(combined)
}


#Formula definitions

lag1_formula <- paste0("count ~",var_lagger('count',1),'+',var_lagger('Drt_mean',1),'+', var_lagger('MeanT_mean',1),'+',
                       var_lagger('MeanWS_mean',1),'+',var_lagger('pm10',1),'+',var_lagger('so2',1), '+',var_lagger('co',1),
                       '+ s(EWeek) + s(EYear,k=3)')
lag2_formula <-  paste0("count ~",var_lagger('count',2),'+',var_lagger('Drt_mean',2),'+', var_lagger('MeanT_mean',2),'+',
                        var_lagger('MeanWS_mean',2),'+',var_lagger('pm10',2),'+',var_lagger('so2',2), '+',var_lagger('co',2),
                        '+ s(EWeek) + s(EYear,k=3)')
lag3_formula <- paste0("count ~",var_lagger('count',3),'+',var_lagger('Drt_mean',3),'+', var_lagger('MeanT_mean',3),'+',
                       var_lagger('MeanWS_mean',3),'+',var_lagger('pm10',3),'+',var_lagger('so2',3), '+',var_lagger('co',3),
                       '+ s(EWeek) + s(EYear,k=3)')
lag4_formula <- paste0("count ~",var_lagger('count',4),'+',var_lagger('Drt_mean',4),'+', var_lagger('MeanT_mean',4),'+',
                       var_lagger('MeanWS_mean',4),'+',var_lagger('pm10',4),'+',var_lagger('so2',4), '+',var_lagger('co',4),
                       '+ s(EWeek) + s(EYear,k=3)')
#Models

#Cardiovascular Disease 
cat1_lag1 <- gam(formula(lag1_formula), data = ed_df_list[[1]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat1_lag2 <- gam(formula(lag2_formula), data = ed_df_list[[1]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat1_lag3 <- gam(formula(lag3_formula), data = ed_df_list[[1]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')


#Chronic Respiratory Disease 
cat2_lag1 <- gam(formula(lag1_formula), data = ed_df_list[[2]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat2_lag2 <- gam(formula(lag2_formula), data = ed_df_list[[2]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat2_lag3 <- gam(formula(lag3_formula), data = ed_df_list[[2]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')


#Diabetes Mellitus 
cat3_lag1 <- gam(formula(lag1_formula), data = ed_df_list[[3]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat3_lag2 <- gam(formula(lag2_formula), data = ed_df_list[[3]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat3_lag3 <- gam(formula(lag3_formula), data = ed_df_list[[3]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')


#Digestive Diseases 
cat4_lag1 <- gam(formula(lag1_formula), data = ed_df_list[[4]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat4_lag2 <- gam(formula(lag2_formula), data = ed_df_list[[4]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat4_lag3 <- gam(formula(lag3_formula), data = ed_df_list[[4]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')


#Endocrine Disorders 
cat5_lag1 <- gam(formula(lag1_formula), data = ed_df_list[[5]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat5_lag2 <- gam(formula(lag2_formula), data = ed_df_list[[5]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat5_lag3 <- gam(formula(lag3_formula), data = ed_df_list[[5]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')


#Genitourinary Disorders
cat6_lag1 <- gam(formula(lag1_formula), data = ed_df_list[[6]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat6_lag2 <- gam(formula(lag2_formula), data = ed_df_list[[6]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat6_lag3 <- gam(formula(lag3_formula), data = ed_df_list[[6]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')


#Infectious and Parasitic Diseases
cat7_lag1 <- gam(formula(lag1_formula), data = ed_df_list[[7]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat7_lag2 <- gam(formula(lag2_formula), data = ed_df_list[[7]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat7_lag3 <- gam(formula(lag3_formula), data = ed_df_list[[7]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')


#Musculoskeletal Disease
cat8_lag1 <- gam(formula(lag1_formula), data = ed_df_list[[8]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat8_lag2 <- gam(formula(lag2_formula), data = ed_df_list[[8]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat8_lag3 <- gam(formula(lag3_formula), data = ed_df_list[[8]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')

#Neurological and Sense Disorders
cat9_lag1 <- gam(formula(lag1_formula), data = ed_df_list[[9]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat9_lag2 <- gam(formula(lag2_formula), data = ed_df_list[[9]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat9_lag3 <- gam(formula(lag3_formula), data = ed_df_list[[9]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')

#Oral Diseases
cat10_lag1 <- gam(formula(lag1_formula), data = ed_df_list[[10]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat10_lag2 <- gam(formula(lag2_formula), data = ed_df_list[[10]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat10_lag3 <- gam(formula(lag3_formula), data = ed_df_list[[10]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')

#Respiratory Infection
cat11_lag1 <- gam(formula(lag1_formula), data = ed_df_list[[11]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat11_lag2 <- gam(formula(lag2_formula), data = ed_df_list[[11]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat11_lag3 <- gam(formula(lag3_formula), data = ed_df_list[[11]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')

#Skin Diseases
cat12_lag1 <- gam(formula(lag1_formula), data = ed_df_list[[12]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat12_lag2 <- gam(formula(lag2_formula), data = ed_df_list[[12]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')
cat12_lag3 <- gam(formula(lag3_formula), data = ed_df_list[[12]], family = poisson(link = 'log'), na.action = na.omit, method = 'REML')



#Save Models

out_lag1_models <- list(cat1_lag1 = cat1_lag1,
                        cat2_lag1 = cat2_lag1,
                        cat3_lag1 = cat3_lag1,
                        cat4_lag1 = cat4_lag1,
                        cat5_lag1 = cat5_lag1,
                        cat6_lag1 = cat6_lag1,
                        cat7_lag1 = cat7_lag1,
                        cat8_lag1 = cat8_lag1,
                        cat9_lag1 = cat9_lag1,
                        cat10_lag1 = cat10_lag1,
                        cat11_lag1 = cat11_lag1,
                        cat12_lag1 = cat12_lag1
                        )

out_lag2_models <- list(cat1_lag2 = cat1_lag2,
                        cat2_lag2 = cat2_lag2,
                        cat3_lag2 = cat3_lag2,
                        cat4_lag2 = cat4_lag2,
                        cat5_lag2 = cat5_lag2,
                        cat6_lag2 = cat6_lag2,
                        cat7_lag2 = cat7_lag2,
                        cat8_lag2 = cat8_lag2,
                        cat9_lag2 = cat9_lag2,
                        cat10_lag2 = cat10_lag2,
                        cat11_lag2 = cat11_lag2,
                        cat12_lag2 = cat12_lag2
                        )

out_lag3_models <- list(cat1_lag3 = cat1_lag3,
                        cat2_lag3 = cat2_lag3,
                        cat3_lag3 = cat3_lag3,
                        cat4_lag3 = cat4_lag3,
                        cat5_lag3 = cat5_lag3,
                        cat6_lag3 = cat6_lag3,
                        cat7_lag3 = cat7_lag3,
                        cat8_lag3 = cat8_lag3,
                        cat9_lag3 = cat9_lag3,
                        cat10_lag3 = cat10_lag3,
                        cat11_lag3 = cat11_lag3,
                        cat12_lag3 = cat12_lag3
                        )


save(out_lag1_models, file = paste0(wd,'/lag1_models.Rdata'))
save(out_lag2_models, file = paste0(wd,'/lag2_models.Rdata'))
save(out_lag3_models, file = paste0(wd,'/lag3_models.Rdata'))
#Model AIC Table
lag1_AIC <- list()
lag2_AIC <- list()
lag3_AIC <- list()

for (i in 1:length(out_lag1_models)){
  lag1_AIC[i] <- AIC(out_lag1_models[[i]])
  lag2_AIC[i] <- AIC(out_lag2_models[[i]])
  lag3_AIC[i] <- AIC(out_lag3_models[[i]])
}

AIC_data <- data.frame(Category = names(ed_df_list), Lag1 = unlist(lag1_AIC), Lag2 = unlist(lag2_AIC),Lag3 = unlist(lag3_AIC))
