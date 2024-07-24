# July.11.2024
# Note: Build the Bayesian spatiotemporal model with dynamic optical class.
#       I. Parallel Computation of DSTC

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

rm(list=ls())
setwd("/home/dzhai/reduce_res/dstc")
# notes: i is # of thread, j is time of iteration.
doParallelIteration <- function(i){
  # impart dataframe
  filename <- paste0("/home/dzhai/reduce_res/task_partition/",i,"_dynamic_res_input.Rdata")
  load(filename)

  print(i)
  dstc <- chl~tp(time)+as.factor(month)+sp(oc)
  model <- Bsptime(package="sptDyn", model="GP", formula=dstc, data=sptmodel_df.sub,
                     coordtype="lonlat", coords=2:3, scale.transform = "LOG", n.report=2,
                     rhotp = Norm(0,10^2),prior.sigma.eta=Gamm(a=2,b=1),prior.beta0 = Norm(0,10^2),
                     N=5000,burn.in = 0,cov.model = "exponential",tol.dist = 0,
                     spatial.decay = spT.decay(distribution = "FIXED",value = 3/500),
                     distance.method="geodetic:km",g_size = 1)
  
  out <- model$fit
  # dim(out$betasp)
  sub_site <- sptmodel_df.sub$s.index # count # of site index
  sub_site <- sub_site[!duplicated(sptmodel_df.sub$s.index)]
  # betasp <- c(t(out$betasp*12)) # per year
  # betasp_d <- data.frame(site=as.factor(sub_site), betasp = betasp)
  betasp <- out$betasp
  betatp <- out$betatp
  betat0p <- out$betat0p
  betap <- out$betap
  fitted <- out$fitted
  # fitted <- exp(out$fitted[,1]) # scale transform from log to normal
  model_input <- out$data
  phip <- out$phip
  rhotp <- out$rhotp
  sig2ep <- out$sig2ep
  sig2etap <- out$sig2etap
  comp <- out$computation.time
  PMCC <- out$PMCC
  
  savename <- paste0(i,"_res_dstc_df.Rdata")
  save(fitted,betasp,betap,betatp,betat0p,sub_site,
       model_input,phip,rhotp,sig2ep,sig2etap,comp,PMCC,
       file=savename)
}

##### set multiple cores doing parallel computation
no_cores <- detectCores()
# registerDoParallel(cores=8)
##### run
foreach(i=iter(c(1:16),.packages="bmstdr")) %dopar% {
  doParallelIteration(i)
}

foreach(i=iter(c(17:24),.packages="bmstdr")) %dopar% {
  doParallelIteration(i)
}

# check trace plot
for(j in c(1:24)){
  savename <- paste0("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/trace_plots/",j,"_trace.png")
  png(savename)
  betasp <- out$betasp
  plot(betasp[6,],type='l',main=paste0("trace for class=",j),
       xlab="iteration",ylab="raw trend")
  dev.off()
}
#scp -r dzhai@storm.pmc.ucsc.edu:/home/dzhai/reduce_res/dstc ~/Coding/Dynamic_SPT/reduce_res
########## Part II. Iterations Combination ##########
########## dstc model ##########
library(boot) # use bootstrap to obtain CI due to non-normal distribution

rm(list=ls())
setwd("/home/dzhai/reduce_res/dstc")
# setwd("~/Coding/Dynamic_SPT/reduce_res/dstc")
# CI Bootstrapt function to obtain the mean
Bmean <- function(data,indices) {
  d <- data[indices] # allows boot to select sample 
  return(mean(d))
} 

results_tab <- array(NA,dim=c(1074,11)) 
colnames(results_tab) <- c("Site","Trend_Value","Obs","Fitted","LCI","UCI","lci","uci","RMSE","NRMSE","Bias")

s=1
for(i in c(1:24)){
  filename <- paste0(i,"_res_dstc_df.Rdata")
  load(filename)
  model_input <- model_input[,c(1:10)]
  model_input$fitted <- exp(fitted[,1])
  ### Parameters of current subset dataframe
  tick=1
  for(j in sub_site){
    print(s)
    subtemp <- model_input[which(model_input$s.index==j),]
    ### true CHL (average ts)
    chltemp <- mean(subtemp$chl)
    ### fitted CHL
    fittedtemp <- mean(subtemp$fitted)
    ### trends (average betasp)
    trsp <- mean(betasp[tick,])
    trtptemp <- vector('numeric',length=304)
    for(k in c(1:304)){
      trtptemp[k] <- mean(betatp[k,])
    }
    trtp <- mean(trtptemp)
    trtemp <- trtp +trsp
    # trtemp <- mean(betasp_d$betasp[which(as.numeric(betasp_d$site)==tick)])
    # trtemp <- mean(betasp_d$betasp[which(as.vector(betasp_d$site)==j)])
    ### 95% credibility intervals for trend
    # bounds <- c(trtemp-2*sd(betasp_d$betasp[which(as.numeric(betasp_d$site)==tick)]),
    #             trtemp+2*sd(betasp_d$betasp[which(as.numeric(betasp_d$site)==tick)])) # Z=1.96
    bounds <- c(trtemp-2*sd(betasp[tick,]),trtemp+2*sd(betasp[tick,]))
    ## One: another way for confident intervals
    # ntemp <- length(betasp_d$betasp[which(as.numeric(betasp_d$site)==tick)])
    # setemp <- sd(betasp_d$betasp[which(as.numeric(betasp_d$site)==tick)])/sqrt(ntemp)
    # t.scoretemp <- qt(p=0.05/2,df=ntemp-1,lower.tail = F)
    # margin.error <- t.scoretemp*setemp
    # l.bound <- trtemp-margin.error
    # u.bound <- trtemp+margin.error
    ## Two: another way for confident intervals
    results <- boot(data=betasp[tick,], statistic=Bmean, R=1000) #tick
    citemp <- boot.ci(results, type="norm")
    l.bound <- citemp$normal[2]
    u.bound <- citemp$normal[3]
    # determine if distribution does not contain 0
    if((bounds[1]>0 & bounds[2]>0) | (bounds[1]<0 & bounds[2]<0)){
      trtemp <- trtemp
    }else{
      trtemp <- NA
    }
    # if((l.bound>0 & u.bound>0) | (l.bound<0 & u.bound<0)){
    #   trtemp <- trtemp
    # }else{
    #   trtemp <- NA
    # }
    ### RMSE & NRMSE & Bias
    RMSE <- sqrt(mean((chltemp -fittedtemp)^2))
    # nrmse.sd <- sqrt(mean(subtemp$fitted-subtemp$chl)^2)/sd(subtemp$chl)
    nrmse.maxmin <- sqrt(mean(subtemp$fitted-subtemp$chl)^2)/diff(range(subtemp$chl))
    Bias <- bias(fittedtemp,chltemp,type = "bias")
    ### results table
    results_tab[s,] <- c(j,trtemp,chltemp,fittedtemp,bounds[1],bounds[2],l.bound,u.bound,RMSE,nrmse.maxmin,Bias)
    s <- s+1
    tick <- tick+1
    
  }
}
save(results_tab,file="oc_res_dstc_sptp_resultTab.Rdata")
# scp dzhai@storm.pmc.ucsc.edu:/home/dzhai/reduce_res/dstc/oc_res_dstc_sptp_resultTab.Rdata ~/Coding/Dynamic_SPT/reduce_res
# results_tab <- results_tab[c(1:1074),]
# save(results_tab,file="oc_res_dstc_resultTab.Rdata")
########## Part III. Iterations Combination ##########
########## stc model ##########
library(boot) # use bootstrap to obtain CI due to non-normal distribution

rm(list=ls())
setwd("/home/dzhai/reduce_res/dstc")
# setwd("~/Coding/Dynamic_SPT/reduce_res/dstc")
# CI Bootstrapt function to obtain the mean
Bmean <- function(data,indices) {
  d <- data[indices] # allows boot to select sample 
  return(mean(d))
} 

results_tab <- array(NA,dim=c(1074,11)) 
colnames(results_tab) <- c("Site","Trend_Value","Obs","Fitted","LCI","UCI","lci","uci","RMSE","NRMSE","Bias")

s=1
for(i in c(1:24)){
  filename <- paste0(i,"_res_dstc_df.Rdata")
  load(filename)
  model_input <- model_input[,c(1:10)]
  model_input$fitted <- exp(fitted[,1])
  ### Parameters of current subset dataframe
  tick=1
  for(j in sub_site){
    print(s)
    subtemp <- model_input[which(model_input$s.index==j),]
    ### true CHL (average ts)
    chltemp <- mean(subtemp$chl)
    ### fitted CHL
    fittedtemp <- mean(subtemp$fitted)
    ### trends (average betasp)
    # trsp <- mean(betasp[tick,])
    trtptemp <- vector('numeric',length=304)
    for(k in c(1:304)){
      trtptemp[k] <- mean(betatp[k,])
    }
    trtp <- mean(trtptemp)*100
    trtemp <- trtp
    # trtemp <- mean(betasp_d$betasp[which(as.numeric(betasp_d$site)==tick)])
    # trtemp <- mean(betasp_d$betasp[which(as.vector(betasp_d$site)==j)])
    ### 95% credibility intervals for trend
    # bounds <- c(trtemp-2*sd(betasp_d$betasp[which(as.numeric(betasp_d$site)==tick)]),
    #             trtemp+2*sd(betasp_d$betasp[which(as.numeric(betasp_d$site)==tick)])) # Z=1.96
    bounds <- c(trtemp-2*sd(betatp),trtemp+2*sd(betatp))
    ## One: another way for confident intervals
    # ntemp <- length(betasp_d$betasp[which(as.numeric(betasp_d$site)==tick)])
    # setemp <- sd(betasp_d$betasp[which(as.numeric(betasp_d$site)==tick)])/sqrt(ntemp)
    # t.scoretemp <- qt(p=0.05/2,df=ntemp-1,lower.tail = F)
    # margin.error <- t.scoretemp*setemp
    # l.bound <- trtemp-margin.error
    # u.bound <- trtemp+margin.error
    ## Two: another way for confident intervals
    results <- boot(data=betatp, statistic=Bmean, R=1000) #tick
    citemp <- boot.ci(results, type="norm")
    l.bound <- citemp$normal[2]
    u.bound <- citemp$normal[3]
    # determine if distribution does not contain 0
    if((bounds[1]>0 & bounds[2]>0) | (bounds[1]<0 & bounds[2]<0)){
      trtemp <- trtemp
    }else{
      trtemp <- NA
    }
    # if((l.bound>0 & u.bound>0) | (l.bound<0 & u.bound<0)){
    #   trtemp <- trtemp
    # }else{
    #   trtemp <- NA
    # }
    ### RMSE & NRMSE & Bias
    RMSE <- sqrt(mean((chltemp -fittedtemp)^2))
    # nrmse.sd <- sqrt(mean(subtemp$fitted-subtemp$chl)^2)/sd(subtemp$chl)
    nrmse.maxmin <- sqrt(mean(subtemp$fitted-subtemp$chl)^2)/diff(range(subtemp$chl))
    Bias <- bias(fittedtemp,chltemp,type = "bias")
    ### results table
    results_tab[s,] <- c(j,trtemp,chltemp,fittedtemp,bounds[1],bounds[2],l.bound,u.bound,RMSE,nrmse.maxmin,Bias)
    s <- s+1
    tick <- tick+1
    
  }
}
save(results_tab,file="oc_res_stc_tp_resultTab.Rdata")
# scp dzhai@storm.pmc.ucsc.edu:/home/dzhai/reduce_res/dstc/oc_res_stc_tp_resultTab.Rdata ~/Coding/Dynamic_SPT/reduce_res

####
# dstc
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/oc_res_dstc_sptp_resultTab.Rdata")
results_tab <- as.data.frame(results_tab)
results_tab$Trend_Value <- results_tab$Trend_Value*12
save(results_tab,file="oc_res_dstc_sptp_resultTab.Rdata")
# long
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/long_res_dstc_sptp_resultTab.Rdata")
results_tab <- as.data.frame(results_tab)
results_tab$Trend_Value <- results_tab$Trend_Value*12
save(results_tab,file="long_res_dstc_sptp_resultTab.Rdata")
# stc
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/oc_res_stc_tp_resultTab.Rdata")
results_tab <- as.data.frame(results_tab)
results_tab$Trend_Value <- results_tab$Trend_Value # already contain *100
save(results_tab,file="oc_res_stc_tp_resultTab.Rdata")

