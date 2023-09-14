## Update: Aug.21.2023 
######################### RUN IN SILT #################################
##################### Including two parts
############## Part I. create sptmodel ################################
library(nlme)
library(spTimer)
library(sp)
library(iterators)
library(foreach)
library(doParallel)
library(R.matlab)

setwd("/home/dzhai/spt_wc/spt_30oc")
rm(list=ls())

# class <- 1:30
segment <- seq(1,3,1)

##### create function
doParallelIteration <- function(i,j){
  savename <- paste0(j,"_oc_input.Rdata")
  load(savename)
  time.data <- spT.time(t.series = 304,segments = 1)
  if(i==1){ 
    ### first segment of iteration (no initial parameter)
    Model <- spT.Gibbs(
      formula = chl~TT+M, data = spTmodel, model = "GPP",
      time.data = time.data,coords = ~longitude+latitude,
      knots.coords = knotgrid,scale.transform = "LOG",
      priors = spT.priors(model = "GPP",inv.var.prior = Gamm(a=2,b=1),
                          beta.prior = Norm(0,10^2),rho.prior = Norm(0,10^2)),
      nItr = 41000,nBurn = 1000,report=1,distance.method = "geodetic:km",tol.dist = 0,
      spatial.decay = spT.decay(distribution = "FIXED",value = 3/3000))
    savename <- paste0(j,"_oc_model_pt",i,".Rdata")
    
    model_input <- Model$data
    fitted <- Model$fitted
    betap <- Model$betap
    phi <- Model$phi
    rhop <- Model$rhop
    sig2eps <- Model$sig2eps
    sig2etap <- Model$sig2etap
    comp <- Model$computation.time
    PMCC <- Model$PMCC
    comp <- Model$computation.time
    leg <- length(rhop)
    init=spT.initials(model="GPP",sig2eps=sig2eps[leg],
                      sig2eta=sig2etap[leg],rho=rhop[leg],beta=betap[,leg])
    save(model_input,fitted,betap,phi,rhop,sig2eps,sig2etap,comp,PMCC,init,file=savename)
    rm(list=c("Model","model_input","fitted","betap","phi","rhop","sig2eps","sig2etap",
              "comp","PMCC"))
  }else{
    ### subsequent segments
    savename <- paste0(j,"_oc_model_pt",i-1,".Rdata")#load last iteration
    load(savename)
    
    Model <- spT.Gibbs(
      formula = chl~TT+M, data = spTmodel, model = "GPP",
      time.data = time.data,coords = ~longitude+latitude,
      knots.coords = knotgrid,scale.transform = "LOG",
      priors = spT.priors(model = "GPP",inv.var.prior = Gamm(a=2,b=1),
                          beta.prior = Norm(0,10^2),rho.prior = Norm(0,10^2)),
      nItr = 40000,nBurn = 0,report=5,distance.method = "geodetic:km",tol.dist = 0,
      spatial.decay = spT.decay(distribution = "FIXED",value = 3/3000),
      initials = init)
    savename <- paste0(j,"_oc_model_pt",i,".Rdata")
    model_input <- Model$data
    fitted <- Model$fitted
    betap <- Model$betap
    phi <- Model$phi
    rhop <- Model$rhop
    sig2eps <- Model$sig2eps
    sig2etap <- Model$sig2etap
    comp <- Model$computation.time
    PMCC <- Model$PMCC
    comp <- Model$computation.time
    leg <- length(rhop)
    init=spT.initials(model="GPP",sig2eps=sig2eps[leg],
                      sig2eta=sig2etap[leg],rho=rhop[leg],beta=betap[,leg])
    save(model_input,fitted,betap,phi,rhop,sig2eps,sig2etap,comp,PMCC,init,file=savename)
    rm(list=c("Model","model_input","fitted","betap","phi","rhop","sig2eps","sig2etap",
              "comp","PMCC"))
    
  }
}

##### set multiple cores doing parallel computation
no_cores <- detectCores()
registerDoParallel(4)

# Aug.23 AM: class <- c(3,5,6,9)
#         class <- c(10,11,16)
# Aug.23 PM: class <- c(22,26,28)
##### run
foreach(j=iter(class),.packages="spTimer") %dopar% {
  foreach(i=iter(segment),.packages="spTimer") %do% {
    doParallelIteration(i,j)
  }
}

##### another run
foreach(j=iter(class),.packages="spTimer") %dopar% {
  for(i in segment){
    doParallelIteration(i,j)
  }
}

# scp /Volumes/Doris/spt_static/spt_30oc/22_oc_input.Rdata dzhai@storm.pmc.ucsc.edu:/home/dzhai/spt_wc/spt_30oc
# scp /Volumes/Doris/spt_static/spt_30oc/26_oc_input.Rdata dzhai@storm.pmc.ucsc.edu:/home/dzhai/spt_wc/spt_30oc
# scp /Volumes/Doris/spt_static/spt_30oc/28_oc_input.Rdata dzhai@storm.pmc.ucsc.edu:/home/dzhai/spt_wc/spt_30oc

############# Part II. combine segments together ############################
library(spTimer)
library(fields)

##### Import chl initial data
rm(list=ls())
setwd("/home/dzhai/spt_wc/spt_30oc")

# load("/auto/home/dzhai/spt_oc/global.2/ESA_chl_v5_360_180.Rdata")
## or
# temp <- readMat("/auto/home/dzhai/spt_oc/ESA_v5_Data.mat")
# chl <- temp$chl
# ind <- which(is.na(chl))
# chl[ind] <- NA
# lons <- temp$lons
# lats <- temp$lats
# lon <- as.numeric(lons)
# lat <- as.numeric(lats)
# rm(list=c("lons","lats"))
lat <- seq(-89.5,89.5,by = 1)
lon <- seq(-179.5,179.5,by = 1)

time <- 1:304
class <- 1:30
segment <- seq(1,3,1)

fittedChl<- array(NA,dim=c(360,180,304))
total_betap <- array(NA,dim=c(13,120000,30)) #5000,33)) 
# total_betap: the second parameter changes back and forth
# total_betap: the third parameter based on number of class

for(j in class){
  print(paste0("class: ",j))
  ind <- 0
  for(i in segment){
    savename <- paste0("/home/dzhai/spt_wc/spt_30oc/",j,"_oc_model_pt",i,".Rdata")
    load(savename)
    ind <- ind+1
    print(paste0("  parallel time: ",ind))
    if(i == 1){
      temp <- array(NA,dim=c(nrow(betap),1))
      temp <- cbind(temp,betap)
      temp2 <- array(NA,dim=c(nrow(fitted),(length(segment)))) # amount of segments
      temp2[,i] <- fitted[,1]
    }else{
      temp <- cbind(temp,betap)
      temp2[,i] <- fitted[,1]
    }
  }
  betap <- temp[,-1]
  temp2 <- rowMeans(temp2,na.rm = T)
  fitted[,1] <- temp2 # ok
  
  lonind <- which(model_input$longitude>179.5)
  model_input$longitude[lonind] <- model_input$longitude[lonind]-360
  
  loc <- model_input[which(model_input$TT==1),2:3]
  locind <- array(NA, dim=c(nrow(loc),2))
  for(m in 1:nrow(loc)){
    locind[m,1] <- which(lon==loc[m,1])
    locind[m,2] <- which(lat == loc[m,2])
  }
  for(k in 1:length(time)){
    fittemp <- exp(fitted[which(model_input$TT==1),1])
    
    for(p in 1:length(fittemp)){
      fittedChl[locind[p,1],locind[p,2],k] <- fittemp[p]
    }
  } # ok
  
  ### plot the completed trace
  savename <- paste0("/home/dzhai/spt_wc/trace_plots/",j,"_trace.png")
  png(savename)
  plot(betap[2,],type='l',main=paste0("trace for class=",j),
       xlab="iteration",ylab="raw trend")
  dev.off()
  total_betap[,,j] <- betap[,]
}
setwd("/home/dzhai/spt_wc/trace_plots/")
save(total_betap,fittedChl,model_input,file="combined_30oc_result.Rdata")

# scp -r dzhai@storm.pmc.ucsc.edu:/home/dzhai/spt_wc/trace_plots/ /Volumes/Doris/spt_static/spt_30oc/



