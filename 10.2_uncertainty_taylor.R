##### Aug.25: Use Talor diagram compare the uncertainty of F1 and F2 (dynamic spt model)
rm(list=ls())
setwd("/Volumes/Doris/spt_dynamic")
# Method 1
# 1-1
library(openair)

load("/Volumes/Doris/spt_dynamic/global_f1_resultTab.Rdata")
f1_results <- as.data.frame(results_tab)
load("/Volumes/Doris/spt_dynamic/global_f2_resultTab.Rdata")
f2_results <- as.data.frame(results_tab)
rm('results_tab')

comb_df_1 <- data.frame(f1_results[,1:4])
TaylorDiagram(comb_df_1,obs = 'Obs',mod = 'Fitted')
# dfbar <- cbind(longTab[,1:2],wcTab[,1:2])
# dfbar$method <- c(rep("Longhurst Regions",each=23),rep("Optical Classes",each=30))
# dfbar$method <- as.factor(dfbar$method)
comb_df_1 <- data.frame(f1_trend=f1_results$Trend_Value,f2_trend=f2_results$Trend_Value)
TaylorDiagram(comb_df_1,obs = 'f1_trend',mod = 'f2_trend')

# 1-2
library(openair)
load("/Volumes/Doris/spt_dynamic/global_f1_resultTab.1.Rdata")
f1_results <- as.data.frame(results_tab)
load("/Volumes/Doris/spt_dynamic/global_f2_resultTab.Rdata")
f2_results <- as.data.frame(results_tab)
rm('results_tab')

### 1-2-1
comb_df <- rbind(f1_results[,1:4],f2_results[,1:4])
comb_df$Fitted <- exp(comb_df$Fitted)
comb_df$Fitted[which(comb_df$Fitted>100)] <- NA
comb_df$model <- c(rep("F1",each=18093),rep("F2",each=18093))
# comb_df <- na.omit(comb_df)
TaylorDiagram(comb_df,obs = 'Obs',mod = 'Fitted',group = 'model')#,type = 'model')
TaylorDiagram(f1_results,obs = 'Obs',mod = 'Fitted')

### 1-2-2
comb_df <- cbind(f1_results[,1:4],f2_results[,2:4])
colnames(comb_df) <- c('site','trend_f1','obs_f1','fitted_f1','trend_f2','obs_f2','fitted_f2')
comb_df$fitted_f1 <- exp(comb_df$fitted_f1)
comb_df$fitted_f2 <- exp(comb_df$fitted_f2)
comb_df$fitted_f1[which(comb_df$fitted_f1>100)] <- NA
comb_df$fitted_f1[which(comb_df$fitted_f2>100)] <- NA

TaylorDiagram(comb_df,obs = 'obs_f1',mod = c('fitted_f1','fitted_f2'))






# Method 2
install.packages("plotrix")
library(plotrix)
