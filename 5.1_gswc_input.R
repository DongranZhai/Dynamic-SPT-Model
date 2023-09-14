# Note: Formats the input files for use 
#       in the individual water class as read by spTimer.
######################################################################
library(spTimer)
library(sp)
library(R.matlab)
library(raster)
library(oceanmap)

setwd("/Volumes/Doris/spt_static/spt_30oc")
rm(list=ls())

##### Import gswc data dataframe and transform to matrix
load("/Volumes/Doris/spt_static/spt_30oc/gsoc30_df.Rdata")
wc_result <- wc_result[,-c(3:4)]
# create spatial points data frame
coordinates(wc_result) <- ~ lon + lat
# coerce to SpatialPixelsDataFrame
gridded(wc_result) <- T
# coerce to raster
temp <- raster(wc_result)
oc_mat <- raster2matrix(temp)
rm(list=c("temp","wc_result"))
# check if import is right
table(oc_mat)

##### Import chl initial data
load("/Volumes/Doris/raw_occci_chl/occci_chl_v6_new.Rdata")

# temporary copies for restoring each loop
wc_store <- oc_mat
chl_store <- chl
lon_store <- lon
lat_store <- lat
time <- 1:304
# class <- 1:30
class <- c(3,5,6,9,10,11,16,22,26,28)

for(i in class){
  print(i)
  #refer to backups as overwrite within j loop
  wc <- wc_store
  chl <- chl_store
  lon <- lon_store
  lat <- lat_store
  
  ##Selecting required region (i)
  #first replace not required with NA
  boundind <- which(wc!=i | is.na(wc))
  wc[boundind] <- NA
  
  for(j in 1:length(time)){
    temp <- chl[,,j]
    temp[boundind] <- NA
    chl[,,j] <- temp
  }
  
  # cut down to square regions
  boundind <- which(!is.na(wc),arr.ind=T)
  maxlatbound <- lat[max(boundind[,2])]
  minlatbound <- lat[min(boundind[,2])]
  maxlonbound <- lon[max(boundind[,1])]
  minlonbound <- lon[min(boundind[,1])]
  
  ########################################
  # load input into data frame for spatio-temporal model
  # load covariates and convert all required to vector form (for input into spTimer)
  chl.dim <- dim(chl)
  
  coordtemp <- expand.grid(time,lon,lat)
  chltemp <- as.vector(aperm(chl,c(3,1,2)))
  sitetemp <- rep(1:(chl.dim[1]*chl.dim[2]),each=chl.dim[3])
  TT <- rep(time,times=(chl.dim[1]*chl.dim[2]))
  monthtemp <- as.factor(rep(c(9:12,rep(1:12,times=25)),times=(chl.dim[1]*chl.dim[2])))
  
  spTmodel_df <- data.frame(chl=chltemp,longitude=coordtemp[[2]],latitude=coordtemp[[3]],
                            site=sitetemp,TT=TT,M=monthtemp)
  spTmodel <- spTmodel_df
  
  #remove sites where no data(NA for all time)
  temp <- aggregate(chl~site,spTmodel,FUN = sum,na.action = na.pass,na.rm = T)
  temp <- rep(temp[,2],each=304)
  spTmodel$site[temp==0] <- NA
  spTmodel <- spTmodel[!is.na(spTmodel$site),]
  spTmodel$site <- rep(1:(nrow(spTmodel)/length(time)),each=length(time))
  
  if(length(spTmodel)>(0.5*length(spTmodel_df))){
    
    rm(list=ls(pattern = "temp"))
    rm(list = c("spTmodel_df","chl","TT"))
    gc()
    
    # knot setup 
    knotgrid <-spT.grid.coords(Longitude=c(max(spTmodel$longitude-0.1),min(spTmodel$longitude+0.1)),
                               Latitude=c(max(spTmodel$latitude-0.1),min(spTmodel$latitude+0.1)),
                               by=c(round((max(spTmodel$longitude)-min(spTmodel$longitude))/4.5),
                                    round((max(spTmodel$latitude)-min(spTmodel$latitude))/4.5)))#4.5 degree grid spacing

    ## or
    # knotgrid <- spT.grid.coords(Longitude = c(max(spTmodel$longitude-0.1),min(spTmodel$longitude+0.1)),
    #                             Latitude = c(max(spTmodel$latitude-0.1),min(spTmodel$latitude+0.1)),
    #                             by=c(round((max(spTmodel$longitude)-min(spTmodel$longitude))/2.5),
    #                                  round((max(spTmodel$latitude)-min(spTmodel$latitude))/2.5)))
    # ## or
    # knotgrid <- spT.grid.coords(Longitude = c(maxlonbound,minlonbound),
    #                             Latitude = c(maxlatbound,minlatbound),
    #                             by=c(round((maxlonbound-minlonbound)/1.5),
    #                                  round((maxlatbound-minlatbound)/1.5)))
    
    ## select only those knots that are within the required region
    # first draw polygon outline region
    # wcuse[which(is.na(wcuse))] <- 0
    wc[which(is.na(wc))] <- 0
    reg_outline <- contourLines(lon,lat,wc,level=1)
    
    # are knots within region
    tick <- 0
    for(k in 1:length(reg_outline)){
      temp <- reg_outline[[k]]
      located <- point.in.polygon(knotgrid[,1],knotgrid[,2],temp$x,temp$y)
      located <- located + tick
      tick <- located
      
    }
    #remove knots outside region
    ind <- located != 0
    knotgrid <- knotgrid[ind,]
    knotgrid <- as.matrix(knotgrid)
    
    savename <- paste0(i,"_oc_input.Rdata")
    save(knotgrid,spTmodel,reg_outline,file = savename)
    
  }
  
}

######################################################
# plot the knotgrid to check if the class is correct on map
setwd("/Volumes/Doris/spt_static/spt_30oc")
for(a in class){
  load(paste0(a,"_oc_input.Rdata"))
  print(a)
  savename <- paste0("class ",a," knotgrid.png")
  jpeg(filename = savename,width = 700,height = 450)
  plot <- plot(x=knotgrid[,1],y=knotgrid[,2],type='p',col="dark red",pch=20,
               main=paste0("Water class ",a," knotgrid"),
               xlab = "Longitude",ylab = "Latitude")
  dev.off()
  
}

# scp /Volumes/Doris/spt_static/spt_30oc/16_oc_input.Rdata dzhai@storm.pmc.ucsc.edu:/home/dzhai/spt_wc/spt_30oc





