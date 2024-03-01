library(tidyverse)
library(mgcv)
library(stringr)
library(ggplot2)
library(patchwork)
library(jtools)
library(ggthemes)

setwd('/Users/pranav/Documents/ED')
wd <- "/Users/pranav/Documents/ED"


#Rename categories

names(ed_df_list) <- c("Cardiovascular Disease", "Chronic Respiratory Disease", "Diabetes Mellitus", "Digestive Disease",
                       "Endocrine Disorders", "Genitourinary Disorders", "Infectious and Parasitic Diseases", "Musculoskeletal Disease",
                       "Neurological and Sense Disorders", "Oral Diseases", "Respiratory Infection", "Skin Diseases")

#Population Attributable Fraction by exclusion
smooth_cols <- c('s(Drt_mean.lag.1)','s(Drt_mean.lag.2)','s(Drt_mean.lag.3)','s(MeanT_mean.lag.1)','s(MeanT_mean.lag.2)',
                 's(MeanT_mean.lag.3)','s(MeanWS_mean.lag.1)','s(MeanWS_mean.lag.2)','s(MeanWS_mean.lag.3)','s(pm10.lag.1)',
                 's(pm10.lag.2)','s(pm10.lag.3)','s(so2.lag.1)','s(so2.lag.2)','s(so2.lag.3)','s(co.lag.1)',
                 's(co.lag.2)','s(co.lag.3)')
paf_2 <- list()
paf2_df <- list()
for (i in 1:12){
  obv_counts <- ed_df_list[[i]]$count
  for (var in smooth_cols){
    exc_pred <- exp(predict(out_lag3_models[[i]], ed_df_list[[i]], exclude =var ))
    paf_2[[var]] <- ((obv_counts-exc_pred)/(obv_counts)) * 100
  }
  paf2_df[[i]] <- as.data.frame(do.call(cbind,paf_2)) %>%
    mutate(Category = names(ed_df_list[i]))
  paf2_df[[i]] <- bind_cols(paf2_df[[i]], ed_df_list[[i]][,18:55])
}

all_paf <- do.call(rbind,paf2_df)



#PAF Means
paf2_means <- list()
for (i in 1:12){
  paf2_means[[i]] <-na.omit(as.data.frame(paf2_df[[i]])) %>%
    summarize(across(everything(), ~(mean(.x))))
}

var_paf2_means <- do.call(rbind,paf2_means)
rownames(var_paf2_means) <- names(ed_df_list)

names_irr <- c('Total Daily Rainfall (mm)\n  [1-Week Lag]',
               'Total Daily Rainfall (mm)\n  [2-Week Lag]',
               'Total Daily Rainfall (mm)\n  [3-Week Lag]',
               'Mean Temperature (°C)\n   [1-Week Lag]',
               'Mean Temperature (°C)\n   [2-Week Lag]',
               'Mean Temperature (°C)\n   [3-Week Lag]',
               'Mean Wind Speed (km/h)\n  [1-Week Lag]',
               'Mean Wind Speed (km/h)\n  [2-Week Lag]',
               'Mean Wind Speed (km/h)\n  [3-Week Lag]',
               bquote(atop(PM[10]~'Concentration (µg/m³)', ~'[1-Week Lag]')),
               bquote(atop(PM[10]~'Concentration (µg/m³)', ~'[2-Week Lag]')),
               bquote(atop(PM[10]~'Concentration (µg/m³)', ~'[3-Week Lag]')),
               bquote(atop(SO[2]~'Concentration (ppb)', ~'[1-Week Lag]')),
               bquote(atop(SO[2]~'Concentration (ppb)', ~'[2-Week Lag]')),
               bquote(atop(SO[2]~'Concentration (ppb)', ~'[3-Week Lag]')),
               'CO Concentration (ppm)\n    [1-Week Lag] ',
               'CO Concentration (ppm)\n    [2-Week Lag] ',
               'CO Concentration (ppm)\n    [3-Week Lag] ')

pdf(paste0(wd,"/PAF2_var.pdf"),onefile = TRUE, width = 18, height = 12)
par(las=1,mfrow=c(1,2),cex.lab=1.2,cex.axis=1.0,mar = c(5,13,4,2)+ 0.1)
for (k in 1:18){
  barplot(height = var_paf2_means[,k], names= str_wrap(names(ed_df_list), width = 30), horiz = T, las = 1, main = names_irr[[k]],
          xlab = 'PAF (%)')
}
dev.off()

#PAF vs. Observed Values

irr_cols <- c('Drt_mean.lag.1','Drt_mean.lag.2','Drt_mean.lag.3','MeanT_mean.lag.1','MeanT_mean.lag.2','MeanT_mean.lag.3',
              'MeanWS_mean.lag.1','MeanWS_mean.lag.2','MeanWS_mean.lag.3','pm10.lag.1','pm10.lag.2', 'pm10.lag.3','so2.lag.1','so2.lag.2',
              'so2.lag.3','co.lag.1','co.lag.2', 'co.lag.3')
smooth_cols2 <- c('`s(Drt_mean.lag.1)`','`s(Drt_mean.lag.2)`','`s(Drt_mean.lag.3)`','`s(MeanT_mean.lag.1)`','`s(MeanT_mean.lag.2)`',
                 '`s(MeanT_mean.lag.3)`','`s(MeanWS_mean.lag.1)`','`s(MeanWS_mean.lag.2)`','`s(MeanWS_mean.lag.3)`','`s(pm10.lag.1)`',
                 '`s(pm10.lag.2)`','`s(pm10.lag.3)`','`s(so2.lag.1)`','`s(so2.lag.2)`','`s(so2.lag.3)`','`s(co.lag.1)`',
                 '`s(co.lag.2)`','`s(co.lag.3)`')

panelNames = c('A1','A2','A3',
               'B1','B2','B3',
               'C1','C2','C3',
               'D1','D2','D3',
               'E1','E2','E3',
               'F1','F2','F3')

pdf(paste0(wd,"/PAF_observed2.pdf"),onefile = TRUE, width = 18, height = 12)
paf2_observed <- bind_cols(paf2_df[[12]], ed_df_list[[12]][,18:55])
plots <- list()
for (k in 1:18) {
  plots[[k]] <- ggplot(paf2_observed, aes_string(irr_cols[[k]], smooth_cols2[[k]])) +
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
# Colours : 
# "#001219", "dodgerblue3","#005F73","#0A9396", "#94D2BD", "#E9D8A6", 'gold' '#EE9B00', '#CA6702', '#BB3E03' , '#AE2012', '#9B2245'
widths <- c('0.3' = 'gray90')

drt_lag1_cat <- c('Respiratory Infection', 'Infectious and Parasitic Diseases', 'Chronic Respiratory Disease',
                  'Neurological and Sense Disorders')
all_paf %>%
  mutate( highlight = ifelse(Category %in% drt_lag1_cat, 'Yes', 'No')) %>%
  ggplot(aes_string(y = `s(Drt_mean.lag.1)`, x = Drt_mean.lag.1, colour = Category, linewidth = highlight)) + 
  geom_smooth(se = FALSE, na.rm = TRUE,method = 'gam')+ theme_classic() + 
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'azure3')+
  scale_colour_manual(values= c("gray90", "dodgerblue3","gray90","gray90", "gray90", "gray90", 'gold',
                                        "gray90", '#CA6702', "gray90" , '#AE2012', "gray90")) + 
  scale_linewidth_manual(values= c(0.2,0.8))+
  labs( y = 'PAF (%)', x = names_irr[[1]], title = panelNames[[1]]) +
  guides(colour = guide_legend(title = "Category"), linewidth = "none") +
  theme(legend.position = c(0.2,0.35))

#Normal Plot

plots <- list()
for (k in 1:18){
  if (k == 18) {
    plots[[k]] <-ggplot(all_paf,aes_string( y = smooth_cols2[[k]], x = irr_cols[[k]], colour = "Category")) +
      geom_smooth(se = FALSE, na.rm = TRUE, method = 'gam', linewidth = 0.6) +
      geom_hline(yintercept = 0, linetype = 'dashed', colour = 'black')+ 
      labs( y = 'PAF (%)', x = names_irr[[k]], title = panelNames[[k]]) +
      guides(colour = guide_legend(title = "Category"), linewidth = "none") +
      theme_classic() + 
      theme(axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15))
  } else {
    plots[[k]] <-ggplot(all_paf,aes_string( y = smooth_cols2[[k]], x = irr_cols[[k]], colour = "Category")) +
      geom_smooth(se = FALSE, na.rm = TRUE, method = 'gam', linewidth = 0.6, show.legend = FALSE) +
      geom_hline(yintercept = 0, linetype = 'dashed', colour = 'black')+ 
      labs( y = 'PAF (%)', x = names_irr[[k]], title = panelNames[[k]]) +
      #guides(colour = guide_legend(title = "Category"), linewidth = "none") +
      theme_classic() + 
      theme(axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15))
  }
}
pdf(paste0(wd,"/PAF_combined.pdf"),onefile = TRUE, width = 18, height = 12)
plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] + plots[[6]] + plots[[7]] + plots[[8]]+
  plots[[9]] + plots[[10]] + plots[[11]] + plots[[12]] + plots[[13]] + plots[[14]] + plots[[15]] + plots[[16]]+
  plots[[17]] + plots[[18]] + plot_layout(ncol = 6)

dev.off()


#Split by physiological system

cat1 <- c('Cardiovascular Disease', 'Chronic Respiratory Disease', 'Respiratory Infection', 'Infectious and Parasitic Diseases')
cat2 <- c('Diabetes Mellitus', 'Endocrine Disorders', 'Genitourinary Disorders', 'Digestive Disease')
cat3 <- c('Musculoskeletal Disease', 'Neurological and Sense Disorders', 'Oral Diseases', 'Skin Diseases')

paf_cat1 <- all_paf[all_paf$Category %in% cat1,]
paf_cat2 <- all_paf[all_paf$Category %in% cat2,]
paf_cat3 <- all_paf[all_paf$Category %in% cat3,]
paf_cat <- list(paf_cat1, paf_cat2, paf_cat3)
#Cat Plots

plots_cat1 <- list()
for (k in 1:18){
  if (k == 18) {
    plots_cat1[[k]] <-ggplot(paf_cat1,aes_string( y = smooth_cols2[[k]], x = irr_cols[[k]], colour = "Category")) +
      geom_smooth(se = FALSE, na.rm = TRUE, method = 'gam', linewidth = 0.6, show.legend = TRUE) +
      geom_point(alpha = 0.07, size = 0.8)+
      geom_hline(yintercept = 0, linetype = 'dashed', colour = 'azure3')+ 
      labs( y = 'PAF (%)', x = names_irr[[k]], title = panelNames[[k]]) +
      guides(colour = guide_legend(title = "Category"), linewidth = "none") +
      scale_color_manual(values = c('steelblue3', 'tomato3', 'goldenrod', 'black'))+
      theme_classic() + 
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            legend.position = 'right',
            legend.text = element_text(size =13),
            legend.title = element_text(size =15))+
      coord_cartesian(ylim = c(-25,25))
  } else {
    plots_cat1[[k]] <-ggplot(paf_cat1,aes_string( y = smooth_cols2[[k]], x = irr_cols[[k]], colour = "Category")) +
      geom_smooth(se = FALSE, na.rm = TRUE, method = 'gam', linewidth = 0.6, show.legend = FALSE) +
      geom_point(alpha = 0.07, show.legend = FALSE, size = 0.8)+
      geom_hline(yintercept = 0, linetype = 'dashed', colour = 'azure3')+ 
      labs( y = 'PAF (%)', x = names_irr[[k]], title = panelNames[[k]]) +
      #guides(colour = guide_legend(title = "Category"), linewidth = "none") +
      theme_classic() + 
      scale_color_manual(values = c('steelblue3', 'tomato3', 'goldenrod', 'black'))+
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12))+
      coord_cartesian(ylim = c(-25,25))
  }
}
tiff(paste0(wd,"/PAF_group1.tiff"),units = 'in', width = 18, height = 8, res = 300)
plots_cat1[[1]] + plots_cat1[[2]] + plots_cat1[[3]] + plots_cat1[[4]] + plots_cat1[[5]] + plots_cat1[[6]] + plots_cat1[[7]] + plots_cat1[[8]]+
  plots_cat1[[9]] + plots_cat1[[10]] + plots_cat1[[11]] + plots_cat1[[12]] + plots_cat1[[13]] + plots_cat1[[14]] + plots_cat1[[15]] + plots_cat1[[16]]+
  plots_cat1[[17]] + plots_cat1[[18]] + plot_layout(ncol = 6)
dev.off()

plots_cat2 <- list()
for (k in 1:18){
  if (k == 18) {
    plots_cat2[[k]] <-ggplot(paf_cat2,aes_string( y = smooth_cols2[[k]], x = irr_cols[[k]], colour = "Category")) +
      geom_smooth(se = FALSE, na.rm = TRUE, method = 'gam', linewidth = 0.6) +
      geom_point(alpha = 0.07, size = 0.8)+
      geom_hline(yintercept = 0, linetype = 'dashed', colour = 'azure3')+ 
      labs( y = 'PAF (%)', x = names_irr[[k]], title = panelNames[[k]]) +
      guides(colour = guide_legend(title = "Category"), linewidth = "none") +
      scale_color_manual(values = c('steelblue3', 'tomato3', 'goldenrod', 'black'))+
      theme_classic() + 
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            legend.position = 'right',
            legend.text = element_text(size = 13),
            legend.title = element_text(size =15))+
      coord_cartesian(ylim = c(-25,25))
  } else {
    plots_cat2[[k]] <-ggplot(paf_cat2,aes_string( y = smooth_cols2[[k]], x = irr_cols[[k]], colour = "Category")) +
      geom_smooth(se = FALSE, na.rm = TRUE, method = 'gam', linewidth = 0.6, show.legend = FALSE) +
      geom_point(alpha = 0.07, show.legend = FALSE, size = 0.8)+
      geom_hline(yintercept = 0, linetype = 'dashed', colour = 'azure3')+ 
      labs( y = 'PAF (%)', x = names_irr[[k]], title = panelNames[[k]]) +
      #guides(colour = guide_legend(title = "Category"), linewidth = "none") +
      theme_classic() + 
      scale_color_manual(values = c('steelblue3', 'tomato3', 'goldenrod', 'black'))+
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12))+
      coord_cartesian(ylim = c(-25,25))
  }
}
tiff(paste0(wd,"/PAF_group2.tiff"),units = 'in', width = 18, height = 8, res = 300)
plots_cat2[[1]] + plots_cat2[[2]] + plots_cat2[[3]] + plots_cat2[[4]] + plots_cat2[[5]] + plots_cat2[[6]] + plots_cat2[[7]] + plots_cat2[[8]]+
  plots_cat2[[9]] + plots_cat2[[10]] + plots_cat2[[11]] + plots_cat2[[12]] + plots_cat2[[13]] + plots_cat2[[14]] + plots_cat2[[15]] + plots_cat2[[16]]+
  plots_cat2[[17]] + plots_cat2[[18]] + plot_layout(ncol = 6)
dev.off()

plots_cat3 <- list()
for (k in 1:18){
  if (k == 18) {
    plots_cat3[[k]] <-ggplot(paf_cat3,aes_string( y = smooth_cols2[[k]], x = irr_cols[[k]], colour = "Category")) +
      geom_smooth(se = FALSE, na.rm = TRUE, method = 'gam', linewidth = 0.6) +
      geom_point(alpha = 0.1, size = 0.8)+
      geom_hline(yintercept = 0, linetype = 'dashed', colour = 'azure3')+ 
      labs( y = 'PAF (%)', x = names_irr[[k]], title = panelNames[[k]]) +
      guides(colour = guide_legend(title = "Category"), linewidth = "none") +
      scale_color_manual(values = c('steelblue3', 'tomato3', 'goldenrod', 'black'))+
      theme_classic() + 
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            legend.position = 'right',
            legend.text = element_text(size = 13),
            legend.title = element_text(size = 15))+
      coord_cartesian(ylim = c(-25,25))
  } else {
    plots_cat3[[k]] <-ggplot(paf_cat3,aes_string( y = smooth_cols2[[k]], x = irr_cols[[k]], colour = "Category")) +
      geom_smooth(se = FALSE, na.rm = TRUE, method = 'gam', linewidth = 0.6, show.legend = FALSE) +
      geom_point(alpha = 0.1, show.legend = FALSE, size = 0.8)+
      geom_hline(yintercept = 0, linetype = 'dashed', colour = 'azure3')+ 
      labs( y = 'PAF (%)', x = names_irr[[k]], title = panelNames[[k]]) +
      #guides(colour = guide_legend(title = "Category"), linewidth = "none") +
      theme_classic() + 
      scale_color_manual(values = c('steelblue3', 'tomato3', 'goldenrod', 'black'))+
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12))+
      coord_cartesian(ylim = c(-25,25))
  }
}
tiff(paste0(wd,"/PAF_group3.tiff"),units = 'in', width = 18, height = 8, res = 300)
plots_cat3[[1]] + plots_cat3[[2]] + plots_cat3[[3]] + plots_cat3[[4]] + plots_cat3[[5]] + plots_cat3[[6]] + plots_cat3[[7]] + plots_cat3[[8]]+
  plots_cat3[[9]] + plots_cat3[[10]] + plots_cat3[[11]] + plots_cat3[[12]] + plots_cat3[[13]] + plots_cat3[[14]] + plots_cat3[[15]] + plots_cat3[[16]]+
  plots_cat3[[17]] + plots_cat3[[18]] + plot_layout(ncol = 6)
dev.off()

