## Update: July.27.2023 
library(spTimer)
library(sp)
library(R.m/atlab)
library(raster)
library(oceanmap)

setwd("/Volumes/Doris/spt_static/spt_long")
rm(list=ls())
##import data
temp <- readMat("Longhurst_180.mat") #used to identify longhursst regions
Longhurst <- temp$Longhurst
load("/Volumes/Doris/raw_occci_chl/occci_chl_v6_new.Rdata")
##temporary copies for restoring each loop
Longhurststore <- Longhurst
lonstore <- lon
latstore <- lat
chlstore <- chl
time <- 1:304
area <- sort(unique(Longhurst[!is.na(Longhurst)]))#list of longhurst areas to iterate through
area <- area[-c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54)] #remove coastal, polar, GoM, Archipelagic Deep basin, Mediterranean

for(i in area){
  print(i)
  #refer to backups as overwrite within j loop
  Longhurst <- Longhurststore
  lon <- lonstore
  lat <- latstore
  chl <- chlstore
  
  ##Selecting required region (i)
  #first replace not required with NA
  boundind <- which(Longhurst!=i | is.na(Longhurst))
  Longhurst[boundind] <- NA
  
  for(j in 1:length(time)) {
    temp <- chl[,,j]
    temp[boundind] <- NA
    chl[,,j] <- temp
  }
  #then cut down to square of required (removing unnecessary locations)
  
  boundind <- which(!is.na(Longhurst),arr.ind=T)
  maxlatbound <- lat[max(boundind[,2])]
  minlatbound <- lat[min(boundind[,2])]
  maxlonbound <- lon[max(boundind[,1])]
  minlonbound <- lon[min(boundind[,1])]
  
  ##sorting wrapping problems for spTmodel input (ie. in Pacific where jumps from -180 to 180 longitude)
  #if this is the case change to run from 0-360
  longuse <- Longhurst # leave copy of full region for later use
  swind <- 0
  if(maxlonbound>100 & minlonbound< -50){
    ind <- lon< 0
    longuse<- rbind(longuse[ind==F,],longuse[ind==T,])
    
    for(j in 1:length(time)){
      chl[,,j]<- rbind(chl[ind==F,,j],chl[ind==T,,j])
    }
    lon <- c(lon[lon>=0],lon[lon<0]+360)
    
    boundind <- which(!is.na(longuse),arr.ind=T)
    maxlonbound <- lon[max(boundind[,1])]
    minlonbound <- lon[min(boundind[,1])]
    
    swind <- 1
    
  } 
  
  
  ###################
  #load input into data frame for Spatio-temporal model
  #load covariates and convert all required to vector form (for input into spTimer)
  chl.dim <- dim(chl)
  
  coordtemp <- expand.grid(time,lon,lat) #coordinates for every chl point in space and time 
  chltemp <- as.vector(aperm(chl,c(3,1,2)))
  sitetemp <-rep(1:(chl.dim[1]*chl.dim[2]),each=304)  # site index
  #TT=rep(((time-mean(time))/sd(time)), times=(chl.dim[1]*chl.dim[2])) #use normalised time
  TT=rep(time, times=(chl.dim[1]*chl.dim[2])) 
  
  monthtemp <-as.factor(rep(c(9:12,rep(1:12,times=25)),times=(chl.dim[1]*chl.dim[2])))
  
  spTmodelFull <- data.frame(chl=chltemp,Longitude=coordtemp[[2]],Latitude=coordtemp[[3]],site=sitetemp,TT=TT,M=monthtemp)
  spTmodel <- spTmodelFull
  
  
  #remove sites where no data(i.e. NA for all time)
  temp <- aggregate(chl~site,spTmodel,sum,na.action=na.pass,na.rm=T)
  temp <- rep(temp[,2],each=304)
  spTmodel$site[temp==0] <- NA
  spTmodel <- spTmodel[!is.na(spTmodel$site),]
  spTmodel$site <- rep(1:(nrow(spTmodel)/length(time)),each=304) #replace site list with valid sites
  
  
  if(length(spTmodel)>(0.5*length(spTmodelFull))){          #ie. if more than 50% of dat in region
    
    
    
    rm(list=ls(pattern="temp")) #clear up of large temps
    rm(list=c("spTmodelFull","chl","TT"))
    gc()
    
    
    ##knot setup
    knotgrid <-spT.grid.coords(Longitude=c(max(spTmodel$Longitude-0.1),min(spTmodel$Longitude+0.1)),
                               Latitude=c(max(spTmodel$Latitude-0.1),min(spTmodel$Latitude+0.1)), 
                               by=c(round((max(spTmodel$Longitude)-min(spTmodel$Longitude))/4.5),
                                    round((max(spTmodel$Latitude)-min(spTmodel$Latitude))/4.5)))#4.5 degree grid spacing
    
    
    #selcting only those knots that are within the required region.
    
    #first draw polygon outline region
    longuse[which(is.na(longuse))] <- 0 
    reg_outline <- contourLines(lon,lat,longuse,levels=1)
    
    #are knots within region
    temp2 <- 0
    for(j in 1:length(reg_outline)){
      temp <- reg_outline[[j]]
      located <- point.in.polygon(knotgrid[,1],knotgrid[,2],temp$x,temp$y)
      located <- located + temp2
      temp2 <- located
    }
    #remove knots outside region
    ind <- located!=0
    knotgrid <- knotgrid[ind,]
    
    savename <- paste0(i,"_long_input.Rdata")
    save(knotgrid,spTmodel, file=savename)
    
  }
}

# plot the knotgrid to check if the class is correct on map
setwd("/Volumes/Doris/spt_static/spt_long")
for(a in area){
  load(paste0(a,"_long_input.Rdata"))
  print(a)
  savename <- paste0("Region ",a," knotgrid.png")
  jpeg(filename = savename,width = 700,height = 450)
  plot <- plot(x=knotgrid[,1],y=knotgrid[,2],type='p',col="dark red",
               main=paste0("Region ",a," knotgrid"),
               xlab = "Longitude",ylab = "Latitude")
  dev.off()
  
}


