## Update: July.27.2023 
######################### RUN IN LOCAL #################################
library(spTimer)
library(fields)
library(R.matlab)
library(SimDesign)
library(pracma) # filter outlier
library(hydroGOF) #nrmse

rm(list=ls())
setwd("/Volumes/Doris/spt_static/spt_long")
##### data initialization
temp <- read.csv(file="/Volumes/Doris/spt_static/spt_long/Longhurst_area.csv",header=F)
Area_values <- c(as.matrix(temp))
temp <- readMat("/Volumes/Doris/Projects/sptmodel/Longhurst_180.mat")
Longhurst <- temp$Longhurst

area <- sort(unique(Longhurst[!is.na(Longhurst)]))
time <- 1:304

load("/Volumes/Doris/spt_static/spt_long/combined_iterations_longhurst.Rdata")
# -> include total_betap,fittedChl,model_input

corc <- c(21,8,54,4,38,10,2,12,33,3,29,20,14,37,32,31,6,13,17,40,42,15,45,49,9,11,7,35,19,22,30,36,34,43,5,18,46,41,44,1,24,25,16,52,51,50,28,47,48,23,53,39,26,27)
area <- area[order(match(corc,area))]
load("/Volumes/Doris/raw_occci_chl/occci_chl_v6_new.Rdata")

resultsTab <- array(NA,dim=c(23,7)) 
colnames(resultsTab) <- c("Region","Trend Value","Lower Confidence Interval","Upper confidence interval","NRMSE","RMSE","Bias")
chlTab <- array(NA,dim=c(23))
bounds <- array(NA,dim=c(13,2))
trend_betap2 <- array(NA,dim=c(length(total_betap[2,,1]),23))
regionAve <- array(NA,dim=c(length(time),length(area)))
regionAve_fitted <- array(NA,dim=c(length(time),length(area)))

for(j in area){
  if(j %in% c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54)){
    Longhurst[which(Longhurst == j)] <- NA
  } 
}

chl_store <- chl
fittedChl_store <- fittedChl
##### for each region determine trends etc. and produce plots
tick <- 0
tick_AV <- 0
for(j in area){
  print(j) # total 54 area
  chl <- chl_store
  fittedChl <- fittedChl_store
  tick_AV <- tick_AV+1
  areaValues <- Area_values[tick_AV]
  
  if(j %in% c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54)){
    
  }else{
    tick <- tick+1
    ### for each region replace not required with NA
    boundind <- which(Longhurst != j | is.na(Longhurst))
    for(i in 1:length(time)){
      # satellite recording data
      temp <- chl[,,i]
      temp[boundind] <- NA
      chl[,,i] <- temp
      
      # sptmodel fitted data
      temp <- fittedChl[,,i]
      temp[boundind] <- NA
      fittedChl[,,j] <- temp
    }
    
    ### convert trend values to percent per year ( from decimal change per month)
    ### first, filter the outlier
    # total_betap[2,,j] <- hampel(total_betap[2,,j],5000) # we don't need this
    total_betap[2,,j] <- 12*100*total_betap[2,,j]
    
    ### calculate 95% credibility intervals for trend
    for(n in 1:13){
      bounds[n,] <- c(mean(total_betap[n,,j])-2*sd(total_betap[n,,j]),
                      mean(total_betap[n,,j])+2*sd(total_betap[n,,j]))
    }
    
    # if distribution does not contain 0
    if((bounds[2,1]>0 & bounds[2,2]>0) | (bounds[2,1]<0 & bounds[2,2]<0)){
      Longhurst[which(Longhurst==j)] <- mean(total_betap[2,,j])
    }else{
      Longhurst[which(Longhurst==j)] <- NA
    }
    
    ### RMSE (normalised to mean of each regions observations)
    ### allowing comparison between regions as well as within)
    # NRMSE <- mean(sqrt((fittedChl-chl)^2),na.rm = T)/mean(chl,na.rm = T)
    NRMSE <- nrmse(as.matrix(fittedChl),as.matrix(chl),na.rm=T,norm="sd")# unit: %
    # my method
    RMSE <- sqrt(mean((chl-fittedChl)^2,na.rm=T))
    # print(paste0("mean:",fittedChl_mean))
    # print(paste0("sum:",sum(fittedChl,na.rm=T)))
    # bias
    fittedChl_mean <- mean(fittedChl,na.rm=T)
    chl_mean <- mean(chl,na.rm=T)
    Bias <- bias(fittedChl_mean,chl_mean)
    
    ### table of results @remain questions@
    resultsTab[tick,] <- c(j,mean(total_betap[2,,j]),bounds[2,],NRMSE,RMSE,Bias)
    chlTab[tick] <- mean(chl,na.rm = T)*areaValues
    trend_betap2[,tick] <- total_betap[2,,j]
    
    ###### plot individual regions #########
    # ### a. scatter for space fitted vs obs
    # savename <- paste("~/Projects/sptmodel/",tick,"_fitted_obs_scatter.png")
    # savetitle <- paste0("fitted vs obs ",tick)
    # 
    # png(savename,type="cairo",units="in", width=2.76, height=2.76, pointsize=12, res=600)
    # par(mar=c(3,3,1.2,1),mgp=c(1.75,0.2,0),cex.lab=1.5,cex.axis=1.15,cex.main=1.5,cex.sub=1.5)
    # plot(c(chl),c(fittedChl),xlab="Observed chl",
    #      ylab="Modelled chl",family="serif",
    #      ylim=c(min(c(chl,fittedChl),na.rm=T),max(chl,na.rm=T)),
    #      xlim=c(min(c(chl,fittedChl),na.rm=T),max(chl,na.rm=T)),
    #      main=savetitle,tck=0.01)
    # abline(0,1,col="red")
    # abline(lm(c(fittedChl)~c(chl)),col="lightblue")
    # dev.off()
    
    # ### b. distributions of trend value
    # savename <- paste0("~/Projects/sptmodel/",tick,"_distribution.png")
    # savetitle <- paste0("distributions of trend value ",tick)
    # 
    # png(savename,type="cairo", units="in", width=2.76,height=2.76, pointsize=12, res=600)
    # par(mar=c(3,3,1.2,1),mgp=c(1.75,0.2,0),cex.lab=1.5,cex.axis=1.25,cex.main=1.5,cex.sub=1.5)
    # plot(density(total_betap[2,,j]),col="lightblue",xlab=expression(paste("Trend  ", "(%yr"^" -1",")")),
    #      ylab="Probability density",family="serif",
    #      xlim=c((min(total_betap[2,,j])-0.1),(max(total_betap[2,,j])+0.1)),
    #      main=savetitle,lwd=2,tck=0.01)
    # lines(density(total_betap[2,,j]),lwd=2)
    # dev.off()
    # 
    # ### region fit Comparison
    # ## initialize data
    # start_date <- ISOdate(1997,9,1)
    # end_date <- ISOdate(2013,12,1)
    # start_text <- as.Date("1997/9/1")
    # end_text <- as.Date("2013/12/1")
    # 
    # regionAve[,j]<-apply(chl,3,mean,na.rm=T)#average time series for region
    # regionAve_fitted[,j] <- apply(fittedChl,3,mean,na.rm=T)#average time series for region
    # yearaxis <- as.Date(seq(start_date,end_date,"month"))
    # yeartick <-seq(start_text,end_text,"year")
    # yeartick <- yeartick[-seq(1,17,2)] #every other year
    # 
    # savename <- paste0("~/Projects/sptmodel/",tick,"_Fit_Comparison.png")
    # png(savename,type="cairo",units="in", width=8.27, height=2.92, pointsize=12, res=600)
    # par(mar=c(1.2,4.3,1.2,1),mgp=c(1.75,0.2,0),cex.lab=1.5,cex.axis=1.25,cex.main=1.5,cex.sub=1.5)
    # plot(yearaxis,regionAve[,j],type='l',xlab="Time (m)",lwd=2,family="serif",
    #      ylab=expression(paste("Chl  ", "(mg m"^"-3",")")),xaxt='n',tck=0.01,
    #      ylim=c(0,max(regionAve[,j]))+0.05)  #average time series for region
    # axis.Date(1,at=yeartick,tck=0.01,family='serif')
    # lines(yearaxis,regionAve_fitted[,j],type='l',col='red',lwd=2)
    # dev.off()
  }

}

save(resultsTab,chlTab,trend_betap2,file="results_table_longhurst.Rdata")

########################################################################################
# longTab <- as.data.frame(resultsTab)
# write.table(longTab,file ="/Volumes/Doris/Projects/sptmodel/april.13/longTab.csv",
#           append = F,row.names = T,sep = ",")

# script
install.packages("pracma")
library(pracma)

load("/Volumes/Doris/Projects/sptmodel/april.13/results_table_longhurst.Rdata")
trend21 <- trend_betap2[,12]
trend21_filter <- hampel(trend21,5000)
trend21_filter_2 <- hampel(trend21,50000)
trend21_filter.1 <- as.numeric(trend21_filter_2$y)
plot(trend21_filter.1,type='l',main=paste0("trace for region 21"),
     xlab="iteration",ylab="raw trend")
hist(trend21_filter.1)

trend45 <- trend_betap2[,20]
trend45_filter <- hampel(trend45,5000)
trend45_filter_2 <- hampel(trend45,50000)
trend45_filter.1 <- as.numeric(trend45_filter$y)
plot(trend45_filter.1,type='l',main=paste0("trace for region 45"),
     xlab="iteration",ylab="raw trend")
hist(trend21_filter.1)

