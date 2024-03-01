
library(tidyverse)
library(stargazer)
library(ggplot2)
library(jtools)
library(stringr)
library(scales)
setwd('/Users/pranav/Documents/ED')
wd <- "/Users/pranav/Documents/ED"

#Category Names
names(ed_df_list) <- c("Cardiovascular Disease", "Chronic Respiratory Disease", "Diabetes Mellitus", "Digestive Disease",
                       "Endocrine Disorders", "Genitourinary Disorders", "Infectious and Parasitic Diseases", "Musculoskeletal Disease",
                       "Neurological and Sense Disorders", "Oral Diseases", "Respiratory Infection", "Skin Diseases")

#ED counts across admission categories
case_sums <- list()

for (i in 1:12){
  case_sums[i] <- sum(ed_df_list[[i]]$count)
}

sums_data<- data.frame(
  Category = as.character(unlist(names(ed_df_list))),
  Cases = as.integer(unlist(case_sums))
)
tiff(paste0(wd,"/total_cases.tiff"),units = 'in', width = 10, height = 6, res = 300)
ggplot(sums_data, aes(x = Cases, y = reorder(Category,Cases))) +
  geom_bar(position = 'dodge',stat = 'identity', fill = 'azure3')+
  theme_classic()+
  geom_text(data = subset(sums_data, Cases <400000),aes(label = Cases), position = position_dodge(0.9), hjust = -0.05)+
  geom_text(data = subset(sums_data, Cases >500000), aes(label = Cases), position = position_dodge(0.9), hjust = 1.10)+
  labs(x = 'Cases', y = NULL)+
  scale_x_continuous(labels = label_comma(), expand = c(0,0)) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15))
dev.off()

#Exposure summary stats
variable_summary <- ed_df_list[[1]] %>%
  select(Drt_mean,MeanT_mean,MeanWS_mean,pm10,so2,co)
stargazer(variable_summary,type = 'html',nobs = FALSE, iqr = TRUE, digits =2, out = 'var_summary.html')

#Case count summary
combined_df <- bind_rows(ed_df_list) %>%
  dplyr::select(EWeek,EYear,SBoDcate,count) %>%
  group_by(SBoDcate) %>%
  summarise(
    mean = mean(count),
    sd = sd(count),
    min = min(count),
    q1 = quantile(count,0.25),
    q3 = quantile(count,0.75),
    max = max(count)
  )