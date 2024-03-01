rm(list = ls())
library(tidyverse)
library(stringr)
library(ggplot2)

setwd('/Users/pranav/Documents/ED')
wd <- "/Users/pranav/Documents/ED"

#Data cleaning and preparation

# Read ED data, replace missing values with 31dec2018 and extract epiweek, epiyear
ed <- read.csv('/Users/pranav/Documents/ED/Data/EDbySBODcat.csv') %>%
  mutate(date_adm = replace(date_adm, date_adm == '','31dec2018'))%>%
  mutate(EWeek = epiweek(as.Date(date_adm, '%d%b%Y'))) %>%
  mutate(EYear = epiyear(as.Date(date_adm, '%d%b%Y'))) %>%
  group_by(SBoDcate,EWeek,EYear) %>%
  summarise(count = sum(count))


#Environmental Variables

file <- read.csv('/Users/pranav/Documents/Dengue vs Covid/intermediary.csv') %>%
  dplyr::select(EWeek, EYear, Drt_mean, MaxT_mean, MeanT_mean, MinT_mean, MeanWS_mean, MaxWS_mean,pm10,o3,no2,so2,co) %>%
  filter(EYear >= 2014) %>%
  group_by(EWeek,EYear) %>%
  summarize_all(mean)


file3 <-read.csv('/Users/pranav/Documents/Dengue vs Covid/met_new.csv') %>%
  dplyr::select(EWeek, EYear,sp) %>%
  group_by(EWeek, EYear) %>%
  summarize_all(mean)

# DF with all ED categories
env_ed <- merge(ed,file, by = c('EWeek','EYear'))
ed_df <- merge(env_ed, file3, by = c('EWeek','EYear')) %>%
  filter(EYear <= 2018) 

# Individual dataframe for each category
categories <- unique(ed_df$SBoDcate)
ed_df_list <- split(ed_df,ed_df$SBoDcate)

for (category in categories){
  ed_df_list[[category]] <- ed_df_list[[category]]%>%
    arrange(EYear,EWeek) %>%
    mutate(across(count:sp,~lag(.x,n=1), .names = "{.col}.lag.1")) %>%
    mutate(across(count:sp,~lag(.x,n=2), .names = "{.col}.lag.2")) %>%
    mutate(across(count:sp,~lag(.x,n=3), .names = "{.col}.lag.3")) %>%
    mutate(across(count:sp,~lag(.x,n=4), .names = "{.col}.lag.4")) %>%
    mutate(across(count:sp,~lag(.x,n=5), .names = "{.col}.lag.5")) %>%
    mutate(across(count:sp,~lag(.x,n=6), .names = "{.col}.lag.6")) %>%
    mutate(across(count:sp,~lag(.x,n=7), .names = "{.col}.lag.7")) %>%
    mutate(across(count:sp,~lag(.x,n=8), .names = "{.col}.lag.8"))
}

#Remove categories
remove_cat <- c('Unintentional injuries','Intentional injuries', 'Mental disorders', 'Nutritional deficiencies', 'Maternal conditions',
                'Congenital Abnormalities', 'Malignant neoplasms', 'Other neoplasms',
                'Factors influencing health status and contact with health services', 'Ill-defined diseases', 
                'Ill-defined injuries/accidents', 'Not Categorised', 'Perinatal conditions')
ed_df_list <- ed_df_list[!names(ed_df_list) %in% remove_cat]
save(ed_df_list, file = paste0(wd,'/df_list.Rdata'))


#Partial autocorrelation and lag plots

var <- c('Count',
         'Total Daily Rainfall',
         'Mean Temperature',
         'Mean Windspeed',
         expression("PM"[10]*" Concentration"),
         expression("SO"[2]*" Concentration"),
         'CO Concentration')


pdf(paste0(wd,"/Lag_plots.pdf"),onefile = TRUE, width = 12, height = 12)
par(las=1,mfrow=c(2,4), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.7, mar = c(5.1, 5.1, 7.1, 2.1))
for (j in 1:length(names(ed_df_list))) {
  df <- ed_df_list[[j]]
  df_ts <- ts(df, frequency = 1)
  for (i in 4:16) {
    for (k in 1:8){
      plot(y= df$count, x=df[,(i + (13*k))], xlab =(var[[i-3]]), main = (sprintf(" Lag %s",(k))), 
           ylab =if( k == 1 | k == 5)'Count' else '', col=rgb(0.2,0.4,0.8,0.75), pch = 16 )
      abline(lm(df$count ~ df[,(i + (13*k))]), col = 'tomato3', lwd = 3)
      corr <- round(cor(df$count,df[,(i + (13*k))], use = 'complete.obs'),2)
      legend('topleft', paste(' r =', corr), bty = 'o')
      mtext(text = names(ed_df_list[j]), side = 3, line = -2, outer = TRUE, cex = 1.5)
    }
    par(mfrow = c(1,1),mar = c(5.1, 7.1, 5.1, 2.1) + 0.1)
    pacf_values <- pacf(ts(df[,i]), plot = FALSE, lag.max = 8)
    plot( pacf_values, xlab = 'Lag', ylab = 'Partial Autocorrelation', main =paste(names(ed_df_list[j])))
    mtext((var[[i-3]]), side = 3)
    par(las=1,mfrow=c(2,4), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.7, mar = c(5.1, 5.1, 7.1, 2.1))
  }
  
}

dev.off()


#PACF
pdf(paste0(wd,"/pacf_plots.pdf"),onefile = TRUE, width = 12, height = 12)
par(mfrow = c(2,2),mar = c(5.1, 7.1, 5.1, 2.1) + 0.1)
for (j in 1:length(names(ed_df_list))){
  df <- ed_df_list[[j]]
  df_ts <- ts(df, frequency = 52)
  pacf_values <- pacf(ts(df[,4]), plot = FALSE, lag.max = 8)
  plot(pacf_values, xlab = 'Lag', ylab = 'Partial Autocorrelation', main=paste(names(ed_df_list[j])))
}
dev.off()
  
  
