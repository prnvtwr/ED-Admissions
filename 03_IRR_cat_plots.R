library(tidyverse)
library(stringr)
library(mgcv)
library(MASS)

setwd('/Users/pranav/Documents/ED')
wd <- "/Users/pranav/Documents/ED"


#IRRs across all categories
cols_to_keep <- c('count',"count.lag.1", 'count.lag.2','count.lag.3','Drt_mean.lag.1','Drt_mean.lag.2','Drt_mean.lag.3',
                  'MeanT_mean.lag.1','MeanT_mean.lag.2','MeanT_mean.lag.3','MeanWS_mean.lag.1','MeanWS_mean.lag.2', 'MeanWS_mean.lag.3',
                  'pm10.lag.1','pm10.lag.2','pm10.lag.3','so2.lag.1','so2.lag.2','so2.lag.3','co.lag.1','co.lag.2','co.lag.3',
                  'EWeek','EYear')

irr_cols <- c('Drt_mean.lag.1','Drt_mean.lag.2','Drt_mean.lag.3','MeanT_mean.lag.1','MeanT_mean.lag.2','MeanT_mean.lag.3',
              'MeanWS_mean.lag.1','MeanWS_mean.lag.2','MeanWS_mean.lag.3','pm10.lag.1','pm10.lag.2','pm10.lag.3','so2.lag.1','so2.lag.2',
              'so2.lag.3','co.lag.1','co.lag.2','co.lag.3')

irr_cat_list <- list()
irr_cat_var <- list()

#IRR Dataframes (all categories)
for (i in 1:length(ed_df_list)){
  for (var in irr_cols){
    pred_df <- na.omit(ed_df_list[[i]][cols_to_keep])%>%
      mutate(across(!c(var,count),~mean(.x)))%>%
      mutate(EYear = round(EYear)) %>%
      mutate(EWeek = round(EWeek))
    
    mean_df <- na.omit(ed_df_list[[i]][cols_to_keep]) %>%
      mutate(across(!count, ~mean(.x))) %>%
      mutate(EYear = round(EYear))%>%
      mutate(EWeek = round(EWeek))
    
    mean_pred <- as.vector(exp(predict(out_lag3_models[[i]], mean_df)))
    pred_rest <- predict(out_lag3_models[[i]], pred_df,se.fit = TRUE)
    mean <- exp(pred_rest$fit)
    lo <- exp(pred_rest$fit - (1.96 * pred_rest$se.fit))
    up <- exp(pred_rest$fit + (1.96* pred_rest$se.fit))
    x <- pred_df %>%
      dplyr::select(var)
    out <- cbind(mean/mean_pred,lo/mean_pred,up/mean_pred,x)
    colnames(out) <- c("mean_rr","lo_rr","up_rr", paste0(var))
    irr_cat_var[[var]] <-as.data.frame(out) %>%
      filter(up_rr < 'Inf')
    irr_cat_list[[i]] <- irr_cat_var
  }
}


#IRR plot function
tempcol_ci_null <- col2rgb('azure2')
tempcol_ci_null <- rgb(tempcol_ci_null[1],tempcol_ci_null[2],tempcol_ci_null[3],max = 255,alpha=0.5*255)
panelNames = c('A1','A2','A3',
               'B1','B2','B3',
               'C1','C2','C3',
               'D1','D2','D3',
               'E1','E2','E3',
               'F1','F2','F3')
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
          
gamPlotter_rr <- function(m,
                          l,
                          u,
                          x,
                          xlab,
                          ylab,
                          ci_col=tempcol_ci,#'azure3',
                          ci_col_null=tempcol_ci_null,#'azure2',
                          panelName){
  df <- cbind(l,u)
  #find CI values which cross 1 
  ci_ind <- apply(df, 1, function(x) as.numeric(!(x[1]<= 1 & x[2] >=1)))
  
  plot(x=c(min(x,na.rm=T),max(x,na.rm=T)),
         y=c(min(df),max(df)),
         col="white",
         ylab=ylab,
         xlab=xlab)  

  #Plotting Area
  
  polygon(x=c(x,max(x),rev(x),min(x)),
          y=c(l,u[length(u)],rev(u),l[1]),col=ci_col_null,border=ci_col_null)   #Entire CI
  #draw regions which do not overlap with CI
  polys <- rle(ci_ind)
  for (i in which(polys$values==1)){
    if (i == 1){
      start_ind = 1
    } else {
      start_ind = cumsum((polys$lengths))[i-1]
      
    }
    end_ind <-  cumsum((polys$lengths))[i]
    
    x_sub <- x[start_ind:end_ind]
    u_sub <- u[start_ind:end_ind]
    l_sub <- l[start_ind:end_ind]
    
    
    polygon(x=c(x_sub,max(x_sub),rev(x_sub),min(x_sub)),
            y=c(l_sub,u_sub[length(u_sub)],rev(u_sub),l_sub[1]),col=ci_col,border=ci_col)
    
  }
  
  tempcol <- col2rgb('orange2')
  tempcol <- rgb(tempcol[1],tempcol[2],tempcol[3],max = 255,alpha=0.5*255)
  abline(v=mean(x,na.rm=T),col='orange2',lwd=2)
  
  tempcol <- col2rgb('orange2')
  tempcol <- rgb(tempcol[1],tempcol[2],tempcol[3],max = 255,alpha=0.5*255)
  
  
  abline(h=1,col="darkgrey",lty=2)
  
  lines(x=x,y=m,lwd=2)
  
  #legend(x="topright",
         #bty='n',
         #legend=c("95% CI","IRR","Mean\n(Exposure)"),
         #pch=c(15,NA,NA),
         #lty=c(NA,1,1),
         #lwd=2,
         #col=c('steelblue4',"black","orange2"))  
  
  mtext(text=panelName,side=3,adj=0)
  
}

pdf(paste0(wd,"/IRR_cat_breakdown.pdf"),onefile = TRUE, width = 21, height = 12)
par(las=1,mfrow=c(3,6),cex.lab=1.2,cex.axis=1.0,mar = c(6,6,4,3)+ 0.1, mgp = c(4.5,1,0))


for (k in 1:12){
  a <- irr_cat_list[[k]]
  for (i in 1:18){
    a1<-a[[i]]%>%
      arrange(a[[i]][,4])
    
    gamPlotter_rr(m=a1[,1],
                  l=a1[,2],
                  u=a1[,3],
                  x=a1[,4],
                  xlab = names_irr[i],
                  ylab = if( i == 1 | i == 7 | i == 13)'IRR' else '',
                  ci_col='steelblue4',
                  ci_col_null=tempcol_ci_null,
                  panelName=paste0(panelNames[i]))
  
  }
  mtext(text = names(ed_df_list[k]), side = 3, line =-2, outer = TRUE, cex = 1.5)
}

dev.off()


meteor_list <- list(irr_cat_list[[11]][[1]],
                    irr_cat_list[[7]][[1]],
                    irr_cat_list[[8]][[2]],
                    irr_cat_list[[1]][[7]],
                    irr_cat_list[[2]][[7]],
                    irr_cat_list[[4]][[7]],
                    irr_cat_list[[6]][[7]],
                    irr_cat_list[[7]][[7]],
                    irr_cat_list[[8]][[7]],
                    irr_cat_list[[9]][[7]],
                    irr_cat_list[[11]][[7]],
                    irr_cat_list[[10]][[7]])


panelNames2 <- c('A1: Respiratory Infections','A2: Infectious and Parasitic Diseases','A3: Musculoskeletal Diseases',
               'B1: Cardiovascular Diseases','B2: Chronic Respiratory Diseases','B3: Digestive Diseases',
               'B4: Genitourinary Disorders','B5:  Infectious and Parasitic Diseases','B6: Musculoskeletal Diseases',
               'B7: Neurological and Sense Disorders','C1: Respiratory Infections','C2: Oral Diseases')

names_irr2 <- c('Total Daily Rainfall (mm)\n  [1-Week Lag]',
               'Total Daily Rainfall (mm)\n  [1-Week Lag]',
               'Total Daily Rainfall (mm)\n  [2-Week Lag]',
               'Mean Wind Speed (km/h)\n  [1-Week Lag]',
               'Mean Wind Speed (km/h)\n  [1-Week Lag]',
               'Mean Wind Speed (km/h)\n  [1-Week Lag]',
               'Mean Wind Speed (km/h)\n  [1-Week Lag]',
               'Mean Wind Speed (km/h)\n  [1-Week Lag]',
               'Mean Wind Speed (km/h)\n  [1-Week Lag]',
               'Mean Wind Speed (km/h)\n  [1-Week Lag]',
               'Mean Wind Speed (km/h)\n  [1-Week Lag]',
               'Mean Wind Speed (km/h)\n  [1-Week Lag]')

tiff(paste0(wd,"/IRR_meteor.tiff"), units = 'in', width = 10, height = 13, res = 300)
par(las=1,mfrow=c(4,3),cex.lab=1.6,cex.axis=1.4, mar = c(5.1,6.1,4.1,2.1), mgp = c(4,1,0))

for( k in 1:12){
  a <- meteor_list[[k]] %>%
    arrange(meteor_list[[k]][,4])
  gamPlotter_rr(m=a[,1],
                l=a[,2],
                u=a[,3],
                x=a[,4],
                xlab = names_irr2[k],
                ylab = if(k == 1 | k == 4 | k == 7 | k == 10)'IRR' else '',
                ci_col='steelblue4',
                ci_col_null=tempcol_ci_null,
                panelName=paste0(panelNames2[k]))
  
}

legend(x=10.5,
       y=1.9,
       bty='n',
       legend=c("95% CI","IRR","Mean\n(Exposure)"),
       pch=c(15,NA,NA),
       lty=c(NA,1,1),
       lwd=2,
       col=c('steelblue4',"black","orange2"))  

dev.off()


pollutant_list <- list(irr_cat_list[[1]][[12]],
                       irr_cat_list[[1]][[16]],
                       irr_cat_list[[1]][[17]],
                       irr_cat_list[[11]][[10]],
                       irr_cat_list[[11]][[14]],
                       irr_cat_list[[11]][[16]],
                       irr_cat_list[[11]][[17]],
                       irr_cat_list[[11]][[18]])

panelNames3 <- c("A1: Cardiovascular Diseases", "A2: Cardiovascular Diseases", "A3: Cardiovascular Diseases",
                 "B1: Respiratory Infections", "B2: Respiratory Infections", "B3: Respiratory Infections",
                 "B4: Respiratory Infections", "B5: Respiratory Infections")

names_irr3 <- c(bquote(atop(PM[10]~'Concentration (µg/m³)', ~'[3-Week Lag]')),
                 'CO Concentration (ppm)\n    [1-Week Lag] ',
                 'CO Concentration (ppm)\n    [2-Week Lag] ',
                 bquote(atop(PM[10]~'Concentration (µg/m³)', ~'[1-Week Lag]')),
                 bquote(atop(SO[2]~'Concentration (ppb)', ~'[2-Week Lag]')),
                 'CO Concentration (ppm)\n    [1-Week Lag] ',
                 'CO Concentration (ppm)\n    [2-Week Lag] ',
                 'CO Concentration (ppm)\n    [3-Week Lag] ')

tiff(paste0(wd,"/IRR_pollu.tiff"), units = 'in', width = 10, height = 10, res = 300)
par(las=1,mfrow=c(3,3),cex.lab=1.6,cex.axis=1.4, mar = c(6.5,6.5,4.1,2.1), mgp = c(5.0,1,0))

for( k in 1:8){
  a <- pollutant_list[[k]] %>%
    arrange(pollutant_list[[k]][,4])
  gamPlotter_rr(m=a[,1],
                l=a[,2],
                u=a[,3],
                x=a[,4],
                xlab = names_irr3[k],
                ylab = if(k == 1 | k == 4 | k == 7)'IRR' else '',
                ci_col='steelblue4',
                ci_col_null=tempcol_ci_null,
                panelName=paste0(panelNames3[k]))
  
}

legend(x=0.75,
       y=1.25,
       bty='n',
       legend=c("95% CI","IRR","Mean\n(Exposure)"),
       pch=c(15,NA,NA),
       lty=c(NA,1,1),
       lwd=2,
       col=c('steelblue4',"black","orange2"))  

dev.off()
#Model Fits
pdf(paste0(wd,"/model_fit.pdf"),onefile = TRUE, width = 7, height = 10)
par(las=1,mfrow=c(3,1),cex.lab=1.2,cex.axis=1.0,mar = c(6,6,4,3)+ 0.1)
for ( i in 1:12) {
  ts_df <- ts(ed_df_list[[i]]$count, start =c(min(ed_df_list[[i]]$EYear), min(ed_df_list[[i]]$EWeek)), frequency = 52)
  lag1_pred <- ts(exp(predict(out_lag1_models[[i]], ed_df_list[[i]])), start =c(min(ed_df_list[[i]]$EYear), min(ed_df_list[[i]]$EWeek)), frequency = 52)
  lag2_pred <- ts(exp(predict(out_lag2_models[[i]], ed_df_list[[i]])), start =c(min(ed_df_list[[i]]$EYear), min(ed_df_list[[i]]$EWeek)), frequency = 52)
  lag3_pred <- ts(exp(predict(out_lag3_models[[i]], ed_df_list[[i]])), start =c(min(ed_df_list[[i]]$EYear), min(ed_df_list[[i]]$EWeek)), frequency = 52)
  
  plot(ts_df, ylab = 'Count', xlab = '', lty = 1, lwd = 1)
  title('Lag 1', line = -1)
  lines(lag1_pred, col = 'dodgerblue3', lty = 2,lwd = 2)
  legend("topright", legend = c("Observed", "Predicted"), col = c("black", "dodgerblue3"), lty = c(1,2), bty = "n")
  plot(ts_df, ylab = 'Count', xlab = '', lty = 1, lwd = 1)
  title('Lag 2', line = -1)
  lines(lag2_pred, col = 'goldenrod', lty = 2,lwd = 2)
  legend("topright", legend = c("Observed", "Predicted"), col = c("black", "goldenrod"), lty = c(1,2), bty = "n")
  plot(ts_df, ylab = 'Count', xlab = '', lty = 1, lwd = 1)
  title('Lag 3', line = -1)
  lines(lag3_pred, col = 'tomato3', lty = 2,lwd = 2)
  legend("topright", legend = c("Observed", "Predicted"), col = c("black", "tomato3"), lty = c(1,2), bty = "n")
  mtext(text = names(ed_df_list[i]), side = 3, line =-2, outer = TRUE, cex = 1.5)
}

dev.off()

