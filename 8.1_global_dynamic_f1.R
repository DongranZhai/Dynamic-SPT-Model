# update: Aug.06
#         - Log transformation
# Jul.28
#         new chl_oc_df with coastal region
# May.03 
#         - use dataset with seasonality
#         - as.factor(month) instead of as.vector(month)
#-------------------------------------------------------------
# Note: Build the Bayesian spatiotemporal model with dynamic optical class.
#       I. Parallel Computation of f4
#       II. DF of global combination in f4

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

########## Part I. Parallel Computation of f1 (previous f4) ##########
# load("/Volumes/Doris/spt_dynamic/global_chl_oc_df_w_costal.Rdata")
rm(list=ls())
setwd("/home/dzhai/spt_dynamic/f1")
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
# Set threads
niter <- floor(length(ind)/300) # to be determined
# 74

# notes: i is # of thread, j is time of iteration.
doParallelIteration <- function(i){
  # individual thread
  temp <- sptmodel_df
  temp <- temp %>% filter(temp$s.index %in% c(ind[(i*300-299):(i*300)]))
  print(i)
  f1 <- chl~sp(time)+month
  model.1 <- Bsptime(package="sptDyn", model="GP", formula=f1, data=temp, 
                     coordtype="lonlat", coords=2:3, scale.transform = "LOG", n.report=3,
                     tol.dist = 0.02,rhotp = Norm(0,10^2),prior.sigma.eta=Gamm(a=2,b=1),
                     prior.beta0 = Norm(0,10^2),prior.phi = "FIXED",phi.t=3/1500,
                     N=6000,burn.in = 1000,cov.model = "exponential",
                     distance.method="geodetic:km",g_size = 1)
  
  out <- model.1$fit
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
  
  savename <- paste0(i,"_global_f1_df.Rdata")
  save(temp,betasp_d,fitted,file=savename)
}

##### set multiple cores doing parallel computation
# no_cores <- detectCores() 
registerDoParallel(cores=15)
##### run
foreach(i=iter(c(61:74),.packages="bmstdr")) %dopar% {
  doParallelIteration(i)
}
# Aug.07-09: 1:15
# Aug.09-11: 16:30
# Aug.11-13: 31:45
# Aug.13-15: 46:60
# Aug.15-17:61:74
# Aug.16-17: 75

### remove rows containing NA
rm(list=ls())
setwd("/home/dzhai/spt_dynamic/f1")
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

f1 <- chl~sp(time)+month
model.1 <- Bsptime(package="sptDyn", model="GP", formula=f1, data=temp, 
                   coordtype="lonlat", coords=2:3, scale.transform = "LOG", n.report=3,
                   tol.dist = 0.02,rhotp = Norm(0,10^2),prior.sigma.eta=Gamm(a=2,b=1),
                   prior.beta0 = Norm(0,10^2),prior.phi = "FIXED",phi.t=3/1500,
                   N=6000,burn.in = 1000,cov.model = "exponential",
                   distance.method="geodetic:km",g_size = 1)

out <- model.1$fit
# dim(out$betasp)
# betasp
betasp <- c(t(out$betasp*100*12)) # percent per year
betasp.nrow <- dim(out$betasp)[1]
betasp.ncol <- dim(out$betasp)[2]
sub_site <- rep(c(ind[((niter*300)+1):length(ind)]), each=betasp.ncol)
betasp_d <- data.frame(site=as.factor(sub_site), betasp = betasp)
# fitted
fitted <- out$fitted[,1]
# exp(out$fitted[,1]) scale transform from log to normal
savename <- paste0("75_global_f1_df.Rdata")
save(temp,betasp_d,fitted,file=savename)


########## Part II. Iterations Combination ##########
########## f1 model (previous f4) ##########
rm(list=ls())
setwd("/home/dzhai/spt_dynamic/f1")

# # of grid cells on global scale: 23203
# comb_49df <- matrix(NA,nrow=1,ncol=2)
results_tab <- array(NA,dim=c(22402,11)) # number of ind
colnames(results_tab) <- c("Site","Trend_Value","Obs","Fitted","LCI","UCI","lci","uci","RMSE","NRMSE","Bias")

s=1
for(i in c(1:75)){# previous: 
  filename <- paste0(i,"_global_f1_df.Rdata")
  load(filename)
  
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
    RMSE <- sqrt(mean((chltemp -fittedtemp)^2,na.rm=T))
    # nrmse.sd <- sqrt(mean(subtemp$fitted-subtemp$chl)^2)/sd(subtemp$chl)
    nrmse.maxmin <- sqrt(mean(subtemp$fitted-subtemp$chl)^2)/diff(range(subtemp$chl))
    # NRMSE <- nrmse(as.matrix(fittedtemp),as.matrix(chltemp),na.rm=T,norm="sd")# unit: %
    Bias <- bias(fittedtemp,chltemp,type = "bias")
    ### results table
    results_tab[s,] <- c(j,trtemp,chltemp,fittedtemp,bounds[1],bounds[2],l.bound,u.bound,RMSE,nrmse.maxmin,Bias)
    
    ### combine all of the betasp df
    # comb_49df <- rbind(comb_49df,betasp_d)
    s <- s+1
    tick <- tick+1
    
  }
}
save(results_tab,file="global_f1_resultTab.Rdata")
##

################################################################
# compare trends to determine in which model trend estimates more close to 'true' value.
# f7
load("/Volumes/Doris/NP/sub49/sub49_f7_resultTab.Rdata")
tr_f7 <- mean(results_tab[,2],na.rm=T)
df_f7 <- as.data.frame(results_tab)

# f8
load("/Volumes/Doris/NP/sub49/sub49_f8_resultTab.Rdata")
tr_f8 <- mean(results_tab[,2],na.rm=T)
df_f8 <- as.data.frame(results_tab)

# f4
load("/Volumes/Doris/NP/sub49/sub49_f4_resultTab.Rdata")
tr_f4 <- mean(results_tab[,2],na.rm=T)
df_f4 <- as.data.frame(results_tab)

