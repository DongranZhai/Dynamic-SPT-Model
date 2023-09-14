## Update: July.27.2023 
######################### RUN IN SERVER #################################
##################### Including two parts #############################
############## Part I. create sptmodel ################################
library(nlme)
library(spTimer)
library(R.matlab)
library(sp)
library(iterators)
library(foreach)
library(doParallel)

##### data initialization
rm(list=ls())
setwd("/home/dzhai/sptmodel")
temp <- readMat("Longhurst_180.mat")
Longhurst <- temp$Longhurst
area <- sort(unique(Longhurst[!is.na(Longhurst)]))
area <- area[-c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54)] 
segment <- seq(1,5,1)

setwd("/home/dzhai/sptmodel/spt_long")
##### create function
doParallelIteration <- function(i,j){
  savename <- paste0(j,"_long_input.Rdata")
  load(savename)
  time.data <- spT.time(t.series=304,segments=1)
  if(i==1){
    ### first segment of iteration (no initial parameter)
    Model <- spT.Gibbs(
      formula = chl ~ TT+M, data = spTmodel, model = "GPP",
      time.data = time.data, coords = ~Longitude+Latitude,
      knots.coords = knotgrid, scale.transform = "LOG",
      priors = spT.priors(model = "GPP",inv.var.prior =Gamm(a=2,b=1),
                          beta.prior = Norm(0,10^2),rho.prior = Norm(0,10^2)),
      nItr = 41000, nBurn = 1000, report = 1, distance.method = "geodetic:km",
      spatial.decay = spT.decay(distribution = "FIXED",value = 3/3000,))
    savename <- paste0(j,"_BGC_model_pt",i,".Rdata")
    
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
    savename <- paste0(j,"_BGC_model_pt",i-1,".Rdata")#load last iteration
    load(savename)
    
    Model <- spT.Gibbs(
      formula = chl ~ TT+M, data = spTmodel, model = "GPP",
      time.data = time.data, coords = ~Longitude+Latitude,
      knots.coords = knotgrid, scale.transform = "LOG",
      priors = spT.priors(model = "GPP",inv.var.prior =Gamm(a=2,b=1),
                          beta.prior = Norm(0,10^2),rho.prior = Norm(0,10^2)),
      nItr = 40000, nBurn = 0, report = 1, distance.method = "geodetic:km",
      spatial.decay = spT.decay(distribution = "FIXED",value = 3/3000,),
      initials = init)
    savename <- paste0(j,"_BGC_model_pt",i,".Rdata")
   
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
registerDoParallel(no_cores)
#registerDoParallel(cores=16)

##### run
foreach(j=iter(area),.packages="spTimer") %dopar% {
  foreach(i=iter(segment),.packages="spTimer") %do% {
    temp <- getwd()
    setwd(temp)
    doParallelIteration(i,j)
  }
}


############# Part II. combine segments together ############################
library(spTimer)
library(fields)
library(R.matlab)

rm(list=ls())
## set up for Longhurst and empty arrays
time <- c(1:304)
temp <- readMat("/home/dzhai/sptmodel/Longhurst_180.mat")
Longhurst <- temp$Longhurst
lats <- seq(-89.5,89.5,length.out=180)
lons <- seq(-179.5,179.5,length.out=360)
area <- sort(unique(Longhurst[!is.na(Longhurst)]))
segment <- seq(1,5,1)

fittedChl<- array(NA,dim=c(360,180,304))
total_betap <- array(NA,dim=c(13,200000,length(area)))

ind <- 0
for(j in area){
  print(paste0("area: ",j))
  
  if(j %in% c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54)){
    print(paste0("  discard region ",j))
  }else{
    
    for(i in segment){
      savename <- paste0("/home/dzhai/sptmodel/spt_long/",j,"_BGC_model_pt",i,".Rdata")
      load(savename)
      ind <- ind+1
      print(paste0("  undergo ",ind))
      if(i == 1){
        temp <- array(NA,dim=c(nrow(betap),1))
        temp <- cbind(temp,betap)
        temp2 <- array(NA,dim=c(nrow(fitted),5)) # amount of segments
        temp2[,i] <- fitted[,1]
      }else{
        temp <- cbind(temp,betap)
        temp2[,i] <- fitted[,1]
      }
    }
    betap <- temp[,-1]
    temp2 <- rowMeans(temp2,na.rm = T)
    fitted[,1] <- temp2
    
    lonind <- which(model_input$Longitude>179.5)
    model_input$Longitude[lonind] <- model_input$Longitude[lonind]-360
    
    loc <- model_input[which(model_input$TT==1),2:3]
    locind <- array(NA, dim=c(nrow(loc),2))
    for(m in 1:nrow(loc)){
      locind[m,1] <- which(lons==loc[m,1])
      locind[m,2] <- which(lats == loc[m,2])
    }
    for(k in 1:length(time)){
      fittemp <- exp(fitted[which(model_input$TT==1),1]) # recover log-transformation
      
      for(p in 1:length(fittemp)){
        fittedChl[locind[p,1],locind[p,2],k] <- fittemp[p]
      }
    }
    
    ### plot the completed trace
    savename <- paste0("/home/dzhai/sptmodel/trace_plots/",j,"_trace.png")
    png(savename)
    plot(betap[2,],type='l',main=paste0("trace for region=",j),xlab="iteration",ylab="raw trend")
    dev.off()
    total_betap[,,j] <- betap
    
  }
  
}
save(total_betap,fittedChl,model_input,file="combined_iterations_longhurst.Rdata")
# Why I keep model_input here?
                     
                     








