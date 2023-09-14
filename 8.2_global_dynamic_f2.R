# update: Sep.11 Caveats: cannot use log of data.
#                 global_chl_oc_df_w_costal.1.Rdata after remove coastal region
# Aug.22 sychronize to f1
# May.03 
#         - use dataset with seasonality
#         - as.factor(month) instead of as.vector(month)
#-------------------------------------------------------------
# Note: Build the Bayesian spatiotemporal model with dynamic optical class.
#       I. Parallel Computation of f8
#       II. DF of area 49&32 Combination in f8


library(bmstdr)
library(spTDyn)
library(sp)
library(ggplot2)
library(raster)
library(oceanmap)
library(dplyr)
library(iterators)
library(foreach)
library(doParallel)
library(SimDesign)
library(hydroGOF) #nrmse

########## Part I. Parallel Computation of f2 (previous f8) ##########
rm(list=ls())
setwd("/home/dzhai/spt_dynamic/f2")
load("/home/dzhai/spt_dynamic/global_chl_oc_df_w_costal.1.Rdata")
### remove rows containing NA
# Got: global_df,global_no_na
# length(table(na.ind))
sptmodel_df <- global_df
na.ind <- sptmodel_df$s.index[which(is.na(sptmodel_df$chl))]
na.ind <- na.ind[!duplicated(na.ind)]
sptmodel_df <- 
  sptmodel_df %>%  filter(!s.index %in% na.ind)

na.ind.1 <- sptmodel_df$s.index[which(is.na(sptmodel_df$oc))]
na.ind.1 <- na.ind.1[!duplicated(na.ind.1)]
sptmodel_df <- 
  sptmodel_df %>%  filter(!s.index %in% na.ind.1)
rm(list=c("global_df",'global_no_na'))

ind <- sptmodel_df$s.index # count # of site index
ind <- ind[!duplicated(sptmodel_df$s.index)]
# Set threads
niter <- floor(length(ind)/300) # to be determined
# 74

# notes: i is # of thread, j is time of iteration.
doParallelIteration <- function(i){
  # individual thread
  temp <- sptmodel_df
  temp <- temp %>% filter(temp$s.index %in% c(ind[(i*300-299):(i*300)]))
  print(i)
  f2 <- chl~sp(time)+month+as.factor(oc) # tp(as.factor(oc))
  model.2 <- Bsptime(package="sptDyn", model="GP", formula=f2, data=temp, 
                     coordtype="lonlat", coords=2:3, scale.transform = "NONE", n.report=3,
                     tol.dist = 0.02,rhotp = Norm(0,10^2),prior.sigma.eta=Gamm(a=2,b=1),
                     prior.beta0 = Norm(0,10^2),prior.phi = "FIXED",phi.t=3/1500,
                     N=6000,burn.in = 1000,cov.model = "exponential",
                     distance.method="geodetic:km",g_size = 1)
  
  out <- model.2$fit
  # dim(out$betasp)
  # betasp
  betasp <- c(t(out$betasp*100*12)) # percent per year
  betasp.nrow <- dim(out$betasp)[1]
  betasp.ncol <- dim(out$betasp)[2]
  sub_site <- rep((i*300-299):(i*300), each=betasp.ncol)
  betasp_d <- data.frame(site=as.factor(sub_site), betasp = betasp)
  # fitted
  fitted <- out$fitted[,1]
  # exp(out$fitted[,1]) scale transform from log to normal
  
  savename <- paste0(i,"_global_f2_df.Rdata")
  save(temp,betasp_d,fitted,file=savename)
}

##### set multiple cores doing parallel computation
# no_cores <- detectCores()
registerDoParallel(cores=15)
##### run
foreach(i=iter(c(21:35),.packages="bmstdr")) %dopar% { 
  doParallelIteration(i)
}
# Sep.05 AM-07: c(1:15)
# Sep.05 PM-07: c(16:20)
# Sep.11 AM : c(21:35)



### remove rows containing NA
rm(list=ls())
setwd("/home/dzhai/spt_dynamic/f2")
load("/home/dzhai/spt_dynamic/global_chl_oc_df_w_costal.Rdata")
### remove rows containing NA
# Got: global_df,global_no_na
# length(table(na.ind))
sptmodel_df <- global_df
na.ind <- sptmodel_df$s.index[which(is.na(sptmodel_df$chl))]
na.ind <- na.ind[!duplicated(na.ind)]
sptmodel_df <- 
  sptmodel_df %>%  filter(!s.index %in% na.ind)

na.ind.1 <- sptmodel_df$s.index[which(is.na(sptmodel_df$oc))]
na.ind.1 <- na.ind.1[!duplicated(na.ind.1)]
sptmodel_df <- 
  sptmodel_df %>%  filter(!s.index %in% na.ind.1)
rm(list=c("global_df",'global_no_na'))

ind <- sptmodel_df$s.index # count # of site index
ind <- ind[!duplicated(sptmodel_df$s.index)]
niter <- floor(length(ind)/300)
temp <- sptmodel_df
temp <- temp %>% filter(temp$s.index %in% c(ind[(niter*300+1):length(ind)]))

f2 <- chl~sp(time)+month+as.factor(oc) # tp(as.factor(oc))
model.2 <- Bsptime(package="sptDyn", model="GP", formula=f2, data=temp, 
                   coordtype="lonlat", coords=2:3, scale.transform = "NONE", n.report=5,
                   tol.dist = 0.02,rhotp = Norm(0,10^2),prior.sigma.eta=Gamm(a=2,b=1),
                   prior.beta0 = Norm(0,10^2),prior.phi = "FIXED",phi.t=3/1500,
                   N=6000,burn.in = 1000,cov.model = "exponential",
                   distance.method="geodetic:km",g_size = 1)

out <- model.2$fit
# dim(out$betasp)
# betasp
betasp <- c(t(out$betasp*100*12)) # percent per year
betasp.nrow <- dim(out$betasp)[1]
betasp.ncol <- dim(out$betasp)[2]
sub_site <- rep(c(18001:18093), each=betasp.ncol)
betasp_d <- data.frame(site=as.factor(sub_site), betasp = betasp)
# fitted
fitted <- out$fitted[,1]
# exp(out$fitted[,1]) scale transform from log to normal
savename <- paste0("75_global_f2_df.Rdata")
save(temp,betasp_d,fitted,file=savename)

########## Part II. Iterations Combination ##########
########## f2 model ##########
rm(list=ls())
setwd("/home/dzhai/spt_dynamic/f2")

# # of grid cells on global scale: 18093
# comb_49df <- matrix(NA,nrow=1,ncol=2)
results_tab <- array(NA,dim=c(22402,11))
colnames(results_tab) <- c("Site","Trend_Value","Obs","Fitted","LCI","UCI","RMSE","NRMSE","Bias")

s=1
# 29,30,39,52
for(i in c(1:75)){ # 1:61. 
  filename <- paste0(i,"_global_f2_df.Rdata")
  load(filename)
  # # forget to remove exp() change
  # fitted <- log(fitted)
  temp$fitted <- exp(fitted)
  ind <- temp$s.index
  ind <- ind[!duplicated(temp$s.index)]
  ### Parameters of current subset dataframe
  tick=1
  for(j in ind){
    print(s)
    subtemp <- temp[which(temp$s.index==j),]
    ### true CHL (average ts)
    chltemp <- mean(subtemp$chl)
    ### fitted CHL
    fittedtemp <- mean(subtemp$fitted)
    ### spt trends (average betasp)
    trtemp <- mean(betasp_d$betasp[which(as.numeric(betasp_d$site)==tick)])
    ### 95% credibility intervals for trend
    bounds <- c(trtemp-2*sd(betasp_d$betasp[which(as.numeric(betasp_d$site)==tick)]),
                trtemp+2*sd(betasp_d$betasp[which(as.numeric(betasp_d$site)==tick)]))
    ## another way for confident intervals
    ntemp <- length(betasp_d$betasp[which(as.numeric(betasp_d$site)==tick)])
    setemp <- sd(betasp_d$betasp[which(as.numeric(betasp_d$site)==tick)])/sqrt(ntemp)
    t.scoretemp <- qt(p=0.05/2,df=ntemp-1,lower.tail = F)
    margin.error <- t.scoretemp*setemp
    l.bound <- trtemp-margin.error
    u.bound <- trtemp+margin.error
    # determine if distribution does not contain 0
    if((bounds[1]>0 & bounds[2]>0) | (bounds[1]<0 & bounds[2]<0)){
      trtemp <- trtemp
    }else{
      trtemp <- NA
    }
    ### RMSE & NRMSE & Bias
    RMSE <- sqrt(mean((chltemp -fittedtemp)^2))
    # nrmse.sd <- sqrt(mean(subtemp$fitted-subtemp$chl)^2)/sd(subtemp$chl)
    nrmse.maxmin <- sqrt(mean(subtemp$fitted-subtemp$chl)^2)/diff(range(subtemp$chl))
    Bias <- bias(fittedtemp,chltemp,type = "bias")
    ### results table
    results_tab[s,] <- c(j,trtemp,chltemp,fittedtemp,bounds[1],bounds[2],l.bound,u.bound,RMSE,nrmse.maxmin,Bias)
    
    ### combine all of the betasp df
    # comb_49df <- rbind(comb_49df,betasp_d)
    s <- s+1
    tick <- tick+1
    
  }
}
save(results_tab,file="global_f2_resultTab.Rdata")


##
########## Part I.2. Parallel Computation of f8 ##########
rm(list=ls())
setwd("/home/dzhai/dynamic_spt/f8")
load("/home/dzhai/dynamic_spt/global_centre_df.Rdata")
### remove rows containing NA
# Got: global_df,global_no_na
na.ind <- global_no_na$s.index[which(is.na(global_no_na$chl))]
# length(table(na.ind))
sptmodel_df <- global_no_na
na.ind <- na.ind[!duplicated(na.ind)]
sptmodel_df <- 
  sptmodel_df %>%  filter(!s.index %in% na.ind)

na.ind.1 <- sptmodel_df$s.index[which(is.na(sptmodel_df$oc))]
na.ind.1 <- na.ind.1[!duplicated(na.ind.1)]
sptmodel_df <- 
  sptmodel_df %>%  filter(!s.index %in% na.ind.1)

rm(list=c("global_df",'global_no_na'))

ind <- sptmodel_df$s.index # count # of site index
ind <- ind[!duplicated(sptmodel_df$s.index)]
# Set threads
niter <- floor(length(ind)/300) # to be determined

# notes: i is # of thread, j is time of iteration.
doParallelIteration <- function(i){
  # individual thread
  temp <- sptmodel_df
  temp <- temp %>% filter(temp$s.index %in% c(ind[(i*300-299):(i*300)]))
  print(i)
  f8 <- chl~sp(time)+as.factor(oc) # tp(as.factor(oc))
  model.8 <- Bsptime(package="sptDyn", model="GP", formula=f8, data=temp, 
                     coordtype="lonlat", coords=2:3, scale.transform = "NONE", n.report=5,
                     tol.dist = 0.02,rhotp = Norm(0,10^2),prior.sigma.eta=Gamm(a=2,b=1),
                     prior.beta0 = Norm(0,10^2),prior.phi = "FIXED",phi.t=3/1500,
                     N=6000,burn.in = 1000,cov.model = "exponential",
                     distance.method="geodetic:km",g_size = 1)
  
  out <- model.8$fit
  # dim(out$betasp)
  # betasp
  betasp <- c(t(out$betasp*100*12)) # percent per year
  betasp.nrow <- dim(out$betasp)[1]
  betasp.ncol <- dim(out$betasp)[2]
  sub_site <- rep((i*300-299):(i*300), each=betasp.ncol)
  betasp_d <- data.frame(site=as.factor(sub_site), betasp = betasp)
  # fitted
  fitted <- out$fitted[,1]
  # exp(out$fitted[,1]) scale transform from log to normal
  
  savename <- paste0(i,"_global_f8_df.Rdata")
  save(temp,betasp_d,fitted,file=savename)
}

##### set multiple cores doing parallel computation
# no_cores <- detectCores()
registerDoParallel(cores=18)
##### run
foreach(i=iter(c(11,17,26,27,40,41,45,51:58,60),.packages="bmstdr")) %dopar% {
  doParallelIteration(i)
}
# f8: c(11,17,26,27,40,41,45,51:58,60)
# f8_deszc_oc: c(31:40)
##### rest other cells in area 49 and 32
### remove rows containing NA
rm(list=ls())
setwd("/home/dzhai/dynamic_spt/f8")
load("/home/dzhai/dynamic_spt/global_centre_df.Rdata")
na.ind <- global_no_na$s.index[which(is.na(global_no_na$chl))]
# length(table(na.ind))
sptmodel_df <- global_no_na
na.ind <- na.ind[!duplicated(na.ind)]
sptmodel_df <- 
  sptmodel_df %>%  filter(!s.index %in% na.ind)
na.ind.1 <- sptmodel_df$s.index[which(is.na(sptmodel_df$oc))]
na.ind.1 <- na.ind.1[!duplicated(na.ind.1)]
sptmodel_df <- 
  sptmodel_df %>%  filter(!s.index %in% na.ind.1)
rm(list=c("global_df",'global_no_na'))
ind <- sptmodel_df$s.index # count # of site index
ind <- ind[!duplicated(sptmodel_df$s.index)]
temp <- sptmodel_df
temp <- temp %>% filter(temp$s.index %in% c(ind[18001:18093]))

f8 <- chl~sp(time)+as.factor(oc) # tp(as.factor(oc))
model.8 <- Bsptime(package="sptDyn", model="GP", formula=f8, data=temp, 
                   coordtype="lonlat", coords=2:3, scale.transform = "NONE", n.report=5,
                   tol.dist = 0.02,rhotp = Norm(0,10^2),prior.sigma.eta=Gamm(a=2,b=1),
                   prior.beta0 = Norm(0,10^2),prior.phi = "FIXED",phi.t=3/1500,
                   N=6000,burn.in = 1000,cov.model = "exponential",
                   distance.method="geodetic:km",g_size = 1)

out <- model.8$fit
# dim(out$betasp)
# betasp
betasp <- c(t(out$betasp*100*12)) # percent per year
betasp.nrow <- dim(out$betasp)[1]
betasp.ncol <- dim(out$betasp)[2]
sub_site <- rep(c(18001:18093), each=betasp.ncol)
betasp_d <- data.frame(site=as.factor(sub_site), betasp = betasp)
# fitted
fitted <- out$fitted[,1]
# exp(out$fitted[,1]) scale transform from log to normal
savename <- paste0("61_global_f8_df.Rdata")
save(temp,betasp_d,fitted,file=savename)


