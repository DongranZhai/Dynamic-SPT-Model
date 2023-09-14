# Update: June.05.2023
##### Notes:       1. Uncertainty of two classification and comparison
########### Part I. Uncertainty of two classification and comparison ########
library(SimDesign)
# library(Metrics) # rmse package
library(hydroGOF) # nrmse package
library(ggplot2)
rm(list=ls())
setwd("/Volumes/Doris/spt_static")
##### Fitting simple linear regression annually on Longhurst regions
rm(list=ls())
load("/Volumes/Doris/spt_static/ts_long.Rdata")
trueid <- 1:23
lm_results <- array(data=NA,dim=c(23,4))
colnames(lm_results) <- c("Region","NRMSE_CHL","RMSE_CHL","SD_TR")

for(i in trueid){
  print(i)
  obs <- long_ts$Values[which(long_ts$trueid == i)]
  sim <- long_ts$Fitted[which(long_ts$trueid == i)]
  # convert trend values to percent per year ( from decimal change per month)
  tr_est <- 100*long_tr$Trend[which(long_tr$trueid == i)]
  tr_avrg <- mean(tr_est,na.rm=T)
  
  ### list two type of RMSE here
  ### CHL
  NRMSE_chl <- sqrt(mean((obs-sim)^2,na.rm=T))/sd(obs,na.rm=T)
  RMSE_chl <- sqrt(mean((obs-sim)^2,na.rm=T)) # better
  # temp <- mean(sim[j]-obs[j],na.rm=T)

  
  ### trend
  # NRMSE_tr <- mean(sqrt((tr_est-tr_avrg)^2),na.rm=T)/tr_avrg
  # RMSE_tr <- sqrt(mean((tr_est-tr_avrg)^2,na.rm=T))
  sd_tr <- sd(tr_est,na.rm=T)
  
  lm_results[i,1] <- i
  lm_results[i,2] <- NRMSE_chl
  lm_results[i,3] <- RMSE_chl
  lm_results[i,4] <- sd_tr
}

long_uncertain <- lm_results
save(long_uncertain,file="long_lm_uncertain.Rdata")

##### ##### Fitting simple linear regression annually on water classes
rm(list=ls())
load("/Volumes/Doris/spt_static/ts_oc.Rdata")
class <- 1:12
lm_results <- array(data=NA,dim=c(12,4))
colnames(lm_results) <- c("Region","NRMSE_CHL","RMSE_CHL","SD_TR")

for(i in class){
  print(i)
  obs <- oc_ts$Values[which(oc_ts$Region == i)]
  sim <- oc_ts$Fitted[which(oc_ts$Region== i)]
  # convert trend values to percent per year (from decimal change per month)
  tr_est <- 100*oc_tr$Trend[which(oc_tr$Region == i)]
  tr_avrg <- mean(tr_est,na.rm=T)
  
  ### list two type of RMSE here
  ### CHL
  NRMSE_chl <- sqrt(mean((obs-sim)^2,na.rm=T))/sd(obs,na.rm=T)
  RMSE_chl <- sqrt(mean((obs-sim)^2,na.rm=T)) # better
  
  ### trend
  # NRMSE_tr <- mean(sqrt((tr_est-tr_avrg)^2),na.rm=T)/tr_avrg
  # RMSE_tr <- sqrt(mean((tr_est-tr_avrg)^2,na.rm=T))
  sd_tr <- sd(tr_est,na.rm=T)
  
  lm_results[i,1] <- i
  lm_results[i,2] <- NRMSE_chl
  lm_results[i,3] <- RMSE_chl
  lm_results[i,4] <- sd_tr

}

oc_uncertain <- lm_results
save(oc_uncertain,file="oc_lm_uncertain.Rdata")

##### Box plot to compare the NRMSE and RMSE
##### Import two data frame
rm(list=ls())

load("/Volumes/Doris/spt_static/long_lm_uncertain.Rdata")
longTab <- as.data.frame(long_uncertain)
rm("long_uncertain")

load("/Volumes/Doris/spt_static/oc_lm_uncertain.Rdata")
ocTab <- as.data.frame(oc_uncertain)
rm("oc_uncertain")

dfbar <- rbind(longTab,ocTab)
dfbar$Region <- as.factor(dfbar$Region)
dfbar$method <- c(rep("Longhurst",each=23),rep("Optical class",each=12))
dfbar$method <- as.factor(dfbar$method)

##### A. NRMSE frequency box plot
p <- ggplot(dfbar,aes(x=method,y=NRMSE_CHL)) +
  geom_boxplot(fill=c("#3498db","#f39c12")) +
  scale_fill_manual(values=c("#3498db","#f39c12"))+
  scale_x_discrete(limits=c("Longhurst", "Optical class")) +
  ylab("NRMSE") +
  xlab("") +
  guides(fill=guide_legend(title=NULL))+
  theme_bw()
savename <- paste0("lm_nrmse.1.png")
ggsave(savename,units="in",width=5, height=3, dpi=600)
# ggsave(savename,units="in", width=3,  height=8.27, pointsize=12,dpi =600)

##### B. RMSE frequency box plot
p <- ggplot(dfbar,aes(x=method,y=RMSE_CHL)) +
  geom_boxplot(fill=c("#3498db","#f39c12"),outlier.shape = NA) + 
  scale_fill_manual(values=c("#3498db","#f39c12"))+
  scale_x_discrete(limits=c("Longhurst", "Optical class")) +
  coord_cartesian(ylim=c(0, 0.25)) +
  ylab("RMSE") +
  xlab("") +
  guides(fill=guide_legend(title=NULL))+
  theme_bw()

savename <- paste0("lm_rmse.1.png")
ggsave(savename,units="in",width=5, height=3, dpi=600)

##### C. SD frequency box plot (seems good)
p <- ggplot(dfbar,aes(x=method,y=SD_TR)) +
  geom_boxplot(fill=c("#3498db","#f39c12")) +
  scale_fill_manual(values=c("#3498db","#f39c12"))+
  scale_x_discrete(limits=c("Longhurst", "Optical class")) +
  ylab("Standard deviation") +
  xlab("") +
  guides(fill=guide_legend(title=NULL))+
  theme_bw()

savename <- paste0("lm_sd.1.png")
ggsave(savename,units="in",width=5, height=3, dpi=600)

######## Averaged global trend #################
load("/Volumes/Doris/Projects/time_series/ts_oc.Rdata")

global_mon <- array(data=NA,dim=c(280,29))
global_avg <- array(data=NA,dim=c(280,1))

for(i in 1:280){
  global_mon[i,] <- oc_ts$Values[which(oc_ts$month ==i)]
}

for(i in 1:280){
  global_avg[i,1] <- mean(global_mon[i,1:29],na.rm=T)
}

month <- 1:280
# global time series
global_ts <- ts(global_avg,start = c(1997,9),end = c(2020,12),frequency = 12)
model <- lm(global_avg~month)
# plot(global_ts)
plot(month, global_avg,type="l")
abline(model,col="orange",lwd=1.5)
trend <- model$coefficients[2] # 2.859218e-05

# decompose
ts_dec <- decompose(global_ts)
dec_model <- lm(ts_dec$trend~month)

plot(ts_dec)
abline(dec_model,col="orange",lwd=1.5)

# 1 year (a=12)
fltr <- c(1/2, rep(1, times = 23), 1/2)/24
global_trend <- filter(global_ts, filter = fltr, method = "convo", sides = 2)

filter_ts <- lm(global_trend~month) # 1.632456e-05
plot.ts(global_trend,xlab="Time",ylab="chl concentration",
        main="Averaged global chl trend in 1997-2020",
        sub="1-years moving average",lwd=1.5,col="dark green")


# OC
plot(x=c(1:304),y=as.vector(oc.1$Values[which(oc.1$Region==1)]),type='l',col='blue')
lines(x=c(1:304),y=as.vector(oc.1$Fitted[which(oc.1$Region==1)]),type='l',col='orange')

# green better (no inversetrend and fitted)
plot(x=c(1:304),y=as.vector(oc$Values[which(oc$Region==1)]),type='l',col='green')
lines(x=c(1:304),y=as.vector(oc$Fitted[which(oc$Region==1)]),type='l',col='red')

# Longhurst
# _1: trend inverse
plot(x=c(1:304),y=as.vector(long.1$Values[which(long.1$trueid==1)]),type='l',col='blue')
lines(x=c(1:304),y=as.vector(long.1$Fitted[which(long.1$trueid==1)]),type='l',col='orange')

# green better
plot(x=c(1:304),y=as.vector(long$Values[which(long$trueid==1)]),type='l',col='green')
lines(x=c(1:304),y=as.vector(long$Fitted[which(long$trueid==1)]),type='l',col='red')




