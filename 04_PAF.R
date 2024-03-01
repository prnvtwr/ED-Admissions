library(tidyverse)
library(mgcv)
library(stringr)
library(ggplot2)
library(patchwork)

setwd('/Users/pranav/Documents/ED')
wd <- "/Users/pranav/Documents/ED"

#Population Attributable Fraction

paf <- list()
paf_means <- list()

for (i in 1: 12){
  pred_cases <- (predict(out_lag3_models[[i]], ed_df_list[[i]]))
  term_cases <- as.data.frame(predict(out_lag3_models[[i]],ed_df_list[[i]], type ='terms'))
  
  paf[[i]] <- (term_cases/pred_cases) * 100
  paf_means[[i]] <- na.omit(paf[[i]]) %>%
    summarize(across(everything(), ~(mean(.x))))
  
}

#Variable Importance
var_paf_means <- do.call(rbind,paf_means)
rownames(var_paf_means) <- names(ed_df_list)

names_irr <- c('Total Daily Rainfall (mm)\n  [1-Week Lag]',
               'Total Daily Rainfall (mm)\n  [2-Week Lag]',
               'Total Daily Rainfall (mm)\n  [3-Week Lag]',
               'Mean Temperature (°C)\n   [1-Week Lag]',
               'Mean Temperature (°C)\n   [2-Week Lag]',
               'Mean Temperature (°C)\n   [3-Week Lag]',
               'Mean Wind Speed (km/h)\n  [1-Week Lag]',
               'Mean Wind Speed (km/h)\n   [2-Week Lag]',
               'Mean Wind Speed (km/h)\n   [3-Week Lag]',
               bquote(atop(PM[10]~'Concentration (µg/m³)', ~'[1-Week Lag]')),
               bquote(atop(PM[10]~'Concentration (µg/m³)', ~'[2-Week Lag]')),
               bquote(atop(PM[10]~'Concentration (µg/m³)', ~'[3-Week Lag]')),
               bquote(atop(SO[2]~'Concentration (ppb)', ~'[1-Week Lag]')),
               bquote(atop(SO[2]~'Concentration (ppb)', ~'[2-Week Lag]')),
               bquote(atop(SO[2]~'Concentration (ppb)', ~'[3-Week Lag]')),
               'CO Concentration (ppm)\n    [1-Week Lag] ',
               'CO Concentration (ppm)\n    [2-Week Lag] ',
               'CO Concentration (ppm)\n    [3-Week Lag] ')

pdf(paste0(wd,"/PAF_var.pdf"),onefile = TRUE, width = 18, height = 12)
par(las=1,mfrow=c(1,2),cex.lab=1.2,cex.axis=1.0,mar = c(5,13,4,2)+ 0.1)
for (k in 3:20){
  barplot(height = var_paf_means[,k], names= str_wrap(names(ed_df_list), width = 30), horiz = T, las = 1, main = names_irr[[k-2]],
          xlab = 'PAF (%)')
  }
dev.off()


#PAF vs. Observed Values

irr_cols <- c('Drt_mean.lag.1','Drt_mean.lag.2','Drt_mean.lag.3','MeanT_mean.lag.1','MeanT_mean.lag.2','MeanT_mean.lag.3',
              'MeanWS_mean.lag.1','MeanWS_mean.lag.2','MeanWS_mean.lag.3','pm10.lag.1','pm10.lag.2', 'pm10.lag.3','so2.lag.1','so2.lag.2',
              'so2.lag.3','co.lag.1','co.lag.2', 'co.lag.3')

smooth_cols <- c('`s(Drt_mean.lag.1)`','`s(Drt_mean.lag.2)`','`s(Drt_mean.lag.3)`','`s(MeanT_mean.lag.1)`','`s(MeanT_mean.lag.2)`',
                 '`s(MeanT_mean.lag.3)`','`s(MeanWS_mean.lag.1)`','`s(MeanWS_mean.lag.2)`','`s(MeanWS_mean.lag.3)`','`s(pm10.lag.1)`',
                 '`s(pm10.lag.2)`','`s(pm10.lag.3)`','`s(so2.lag.1)`','`s(so2.lag.2)`','`s(so2.lag.3)`','`s(co.lag.1)`',
                 '`s(co.lag.2)`','`s(co.lag.3)`')

panelNames = c('A1','A2','A3',
               'B1','B2','B3',
               'C1','C2','C3',
               'D1','D2','D3',
               'E1','E2','E3',
               'F1','F2','F3')
paf_observed_df <- bind_cols(paf[[1]], ed_df_list[[1]][,18:42])
plot(y = paf_observed_df[,], x= paf_observed_df$Drt_mean.lag.1,ylab = 'PAF', xlab = 'Total Daily Rainfall (mm)')



ggplot(paf_observed_df, aes_string(irr_cols[[1]], smooth_cols[[1]])) +
  geom_point(colour = 'dodgerblue3',alpha = 0.6)+
  geom_smooth(se = FALSE, colour = 'tomato3') +
  theme_classic() +
  labs( y = 'PAF (%)', x = names_irr[[1]], title = panelNames[[1]])

pdf(paste0(wd,"/PAF_observed.pdf"),onefile = TRUE, width = 18, height = 12)
for (i in 1:20){
  paf_observed <- bind_cols(paf[[i]], ed_df_list[[i]][,18:42])
  plots <- list()
  for (k in 1:18) {
    plots[[k]] <- ggplot(paf_observed, aes_string(irr_cols[[k]], smooth_cols[[k]])) +
      geom_point(colour = 'dodgerblue3',alpha = 0.6)+
      geom_smooth(se = FALSE, colour = 'tomato3') +
      theme_classic() +
      labs( y = 'PAF (%)', x = names_irr[[1]], title = panelNames[[1]])
  }
    plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] + plots[[6]] + plots[[7]] + plots[[8]]+
    plots[[9]] + plots[[10]] + plots[[11]] + plots[[12]] + plots[[13]] + plots[[14]] + plots[[15]] + plots[[16]]+
    plots[[17]] + plots[[18]] + plot_layout(ncol = 6) + plot_annotation(title = names(ed_df_list)[[i]])
  plot.new()
}

dev.off()


pdf(paste0(wd,"/PAF_observed.pdf"),onefile = TRUE, width = 18, height = 12)
paf_observed <- bind_cols(paf[[12]], ed_df_list[[12]][,18:55])
plots <- list()
for (k in 1:18) {
  plots[[k]] <- ggplot(paf_observed, aes_string(irr_cols[[k]], smooth_cols[[k]])) +
    geom_point(colour = 'dodgerblue3',alpha = 0.6)+
    geom_smooth(se = FALSE, colour = 'tomato3', method = 'gam') +
    theme_classic() + geom_hline(yintercept = 0, linetype = 'dashed', colour = 'azure3')+
    labs( y = 'PAF (%)', x = names_irr[[k]], title = panelNames[[k]])
}
plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] + plots[[6]] + plots[[7]] + plots[[8]]+
  plots[[9]] + plots[[10]] + plots[[11]] + plots[[12]] + plots[[13]] + plots[[14]] + plots[[15]] + plots[[16]]+
  plots[[17]] + plots[[18]] + plot_layout(ncol = 6) +
  plot_annotation(title = names(ed_df_list)[[12]], theme = theme(plot.title = element_text(hjust = 0.5)))

dev.off()