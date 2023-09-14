##### Note: June.12.2023 add fitted value
#####  June.05.2023 updated
#####     Part I. Generalize time series of 23 Longhurst regions
#####     Part II. Generalize time series of optical classes
library(sp)
library(zoo) # date format setting
library(tidyverse) # data preparation
library(viridis) # color pallete
library(R.matlab)

rm(list=ls())
setwd("/Volumes/Doris/spt_static")
############# Part I. Generalize time series of 23 Longhurst regions ############
##### Import chl spatial temporal data (360*180*280) and Longhurst index
# Longhurst
temp <- readMat("/Volumes/Doris/Projects/sptmodel/Longhurst_180.mat") #used to identify longhurst regions
Longhurst <- temp$Longhurst
rm("temp")
area <- sort(unique(Longhurst[!is.na(Longhurst)]))#list of longhurst areas to iterate through
area <- area[-c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54)] #remove coastal, polar, GoM, Archipelagic Deep basin, Mediterranean

chlValues <- array(data=NA,dim=c(6992,4)) # 304*23
colnames(chlValues) <- c("Date","Region","Values","Fitted")

# chl data
load("/Volumes/Doris/raw_occci_chl/occci_chl_v6_new.Rdata")
load("/Volumes/Doris/spt_static/month_avg_lm.Rdata")
# fitted_mvalue <- fitted_mvalue[,ncol(fitted_mvalue):1,]
##### A. monthly data
##### Group chl data into 23 Longhurst regions
Longhurststore <- Longhurst
lonsstore <- lon
latsstore <- lat
chlstore <- chl
fittedstore <- fitted_mvalue
time <- 1:304

ind <- 0
tick <- 0
for(i in area){
  print(paste0("Region: ",i))
  ind <- ind+1
  # refer to backups as overwrite within j loop
  Longhurst <- Longhurststore
  lon <- lonsstore
  lat <- latsstore
  chl <- chlstore
  fitted <- fittedstore
  
  ## Selecting required region (i)
  # first replace not required with NA
  boundind <- which(Longhurst!=i | is.na(Longhurst))
  Longhurst[boundind] <- NA
  
  for(j in 1:length(time)) {
    temp <- chl[,,j]
    temp[boundind] <- NA
    chl[,,j] <- temp
    
    temp <- fitted[,,j]
    temp[boundind] <- NA
    fitted[,,j] <- temp
    
    tick <- tick+1
    print(tick)
    
    ## averaging monthly chl at each region
    chlValues[tick,2] <- i
    chlValues[tick,3] <- as.numeric(mean(chl[,,j],na.rm=T))
    chlValues[tick,4] <- as.numeric(mean(fitted[,,j],na.rm=T))
  }
  
}

chl_df <- as.data.frame(chlValues)
# chl_df$Values <- as.numeric(as.character(chl_df$Values))
##### Setting date formate
Date <- seq(from = as.Date("1997/09/01",format="%Y/%m/%d"), 
            to = as.Date("2022/12/01",format="%Y/%m/%d"), 
            by = "1 month")
#Date_month <- format(Date,"%Y-%m")
chl_df$Date <- Date
chl_df$Region <- as.numeric(chl_df$Region)

##### Longhurst correct label
correctid <- c(23,8,3,7,2,1,10,12,14,18,
               5,6,4,15,11,13,21,20,19,16,
               17,22,9)
chl_df$trueid <- NA
for(i in 1:23){
  chl_df$trueid[which(chl_df$Region==area[i])] <- correctid[i]
}

##### B. annually data
load("/Volumes/Doris/raw_occci_chl/occci_chl_v6_new.Rdata")
##### set initial parameter
month <- 1:304
year <- 1997:2022
nyear <- length(year)
annChl <- array(data=NA,dim=c(length(lon),length(lat),nyear))

##### set up annual chl
# 1997 annual
annChl[,,1] <- apply(chl[,,1:4],c(1,2),mean,na.rm=T) # previous use median

# 1998-2022
ind <- 1
indtop <-5
for (i in 1998:2022){
  print(i)
  ind <- ind+1
  temp <- chl[,,indtop:(indtop+11)]
  annChl[,,ind] <- apply(temp,c(1,2),mean,na.rm=T) # previous use median
  indtop <- indtop+12
}

boundind <- which(is.na(annChl))
annChl[boundind] <- NA

chl_df$month <- 1:304
chl_df$annValues <- NA
##### Group chl data into 23 Longhurst regions
## Notes: make sure Longhurststore,lonstore,latstore is initial data
annChlstore <- annChl
# 1997
for(i in area){
  print(paste0("Region: ",i))
  
  Longhurst <- Longhurststore
  lon <- lonsstore
  lat <- latsstore
  annChl <- annChlstore
  
  boundind <- which(Longhurst!=i | is.na(Longhurst))
  Longhurst[boundind] <- NA
  
  temp <- annChl[,,1]
  temp[boundind] <- NA
  
  ## averaging annually chl at each region
  temp2<- as.numeric(mean(temp,na.rm=T))
  chl_df$annValues[which(chl_df$Region==i & chl_df$month==c(1,2,3,4))] <- temp2
}

# 1998-2022
for(i in area){
  print(paste0("Region: ",i))
  tick <- 5
  
  Longhurst <- Longhurststore
  lon <- lonsstore
  lat <- latsstore
  annChl <- annChlstore
  
  boundind <- which(Longhurst!=i | is.na(Longhurst))
  Longhurst[boundind] <- NA
  
  for(j in 2:nyear) {
    temp <- annChl[,,j]
    temp[boundind] <- NA
    annChl[,,j] <- temp
    
    ## averaging annually chl at each region
    temp2<- as.numeric(mean(annChl[,,j],na.rm=T))
    ## 
    row_ind <- which(chl_df$Region==i)
    row_ind <- row_ind[tick:c(tick+11)]
    chl_df$annValues[row_ind] <- temp2
    tick <- tick+12
  }
}

# temporali save chl_df
long_ts <- chl_df

### c. Trends
load("/Volumes/Doris/spt_static/annual_avg_lm.Rdata")
# trend <- trend[,ncol(trend):1]
# Longhurst
temp <- readMat("/Volumes/Doris/Projects/sptmodel/Longhurst_180.mat") #used to identify longhurst regions
Longhurst <- temp$Longhurst
rm("temp")
area <- sort(unique(Longhurst[!is.na(Longhurst)]))#list of longhurst areas to iterate through
area <- area[-c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54)] #remove coastal, polar, GoM, Archipelagic Deep basin, Mediterranean
lon <- seq(-179.5,179.5,1)
lat <- seq(-89.5,89.5,1)
tr_df <- array(data=NA,dim=c(1,4)) # 64800*23
colnames(tr_df) <- c("Longitude","Latitude","Region","Trend")

Longhurststore <- Longhurst
lonsstore <- lon
latsstore <- lat
trendstore <- trend

ind <- 0
tick <- 0
for(i in area){
  print(paste0("Region: ",i))
  ind <- ind+1
  # refer to backups as overwrite within j loop
  Longhurst <- Longhurststore
  lon <- lonsstore
  lat <- latsstore
  trend <- trendstore
  
  ## Selecting required region (i)
  # first replace not required with NA
  boundind <- which(Longhurst!=i | is.na(Longhurst))
  Longhurst[boundind] <- NA
  regiontemp <- as.vector(aperm(Longhurst,c(1,2)))
  
  trend[boundind] <- NA
  trendtemp <- as.vector(aperm(trend,c(1,2)))
  coordtemp <- expand.grid(lon,lat)
  coordtemp[,3] <- regiontemp
  coordtemp[,4] <- trendtemp
  colnames(coordtemp) <- c("Longitude","Latitude","Region","Trend")
  
  tr_df <- rbind(tr_df,coordtemp)
}

##### Longhurst correct label
correctid <- c(23,8,3,7,2,1,10,12,14,18,
               5,6,4,15,11,13,21,20,19,16,
               17,22,9)
tr_df$trueid <- NA
for(i in 1:23){
  tr_df$trueid[which(tr_df$Region==area[i])] <- correctid[i]
}

long_tr <- tr_df
save(long_ts,long_tr,file = "ts_long.Rdata")

############# Part II. Generalize time series of optical classes ##########
##### Import chl spatial temporal data (360*180*280) and water class index
# oc
rm(list=ls())
load("/Volumes/Doris/spt_static/ind_max_360_180.Rdata")
ind_max <- ind_max[,ncol(ind_max):1]
# # ind_max <- ind_max[nrow(ind_max):1,]
# wc_raster <- matrix2raster(ind_max, x = lon, y = lat, layer = 1)
# oc_df <- as.data.frame(wc_raster,xy = T)
# colnames(oc_df) <-c('lon','lat','wc')
# rm(list=c('wc_raster'))
# coast_ind <- which(oc_df$wc==13 |oc_df$wc==14)
# oc_df$wc[coast_ind] <- NA
# # create spatial points data frame
# coordinates(oc_df) <- ~ lon + lat
# # coerce to SpatialPixelsDataFrame
# gridded(oc_df) <- T
# # coerce to raster
# temp <- raster(oc_df)
# oc <- raster2matrix(temp)
# rm(list=c("temp","oc_df"))
# check if import is right
oc <- ind_max
coast_ind <- which(oc==13 |oc==14)
oc[coast_ind] <- NA
table(oc)
area <- length(table(oc))

chlValues <- array(data=NA,dim=c(3648,4)) # 304*12
colnames(chlValues) <- c("Date","Region","Values","Fitted")
load("/Volumes/Doris/raw_occci_chl/occci_chl_v6_new.Rdata")
load("/Volumes/Doris/spt_static/month_avg_lm.Rdata")
# fitted_mvalue <- fitted_mvalue[,ncol(fitted_mvalue):1,]
##### A. monthly data
##### Group chl data into 14 optical classes
ocstore <- oc
lonsstore <- lon
latsstore <- lat
chlstore <- chl
fittedstore <- fitted_mvalue
time <- 1:304

ind <- 0
tick <- 0
for(i in 1:area){
  print(paste0("Region: ",i))
  ind <- ind+1
  # refer to backups as overwrite within j loop
  oc <- ocstore
  lon <- lonsstore
  lat <- latsstore
  chl <- chlstore
  fitted <- fittedstore
  
  ## Selecting required region (i)
  # first replace not required with NA
  boundind <- which(oc!=i | is.na(oc))
  oc[boundind] <- NA
  
  for(j in 1:length(time)) {
    temp <- chl[,,j]
    temp[boundind] <- NA
    chl[,,j] <- temp
    
    temp <- fitted[,,j]
    temp[boundind] <- NA
    fitted[,,j] <- temp
    
    tick <- tick+1
    print(tick)
    
    ## averaging monthly chl at each region
    chlValues[tick,2] <- i
    chlValues[tick,3] <- as.numeric(mean(chl[,,j],na.rm=T))
    chlValues[tick,4] <- as.numeric(mean(fitted[,,j],na.rm=T))
  }
  
}

chl_df <- as.data.frame(chlValues)
# chl_df$Values <- as.numeric(as.character(chl_df$Values))
##### Setting date formate
Date <- seq(from = as.Date("1997/09/01",format="%Y/%m/%d"), 
            to = as.Date("2022/12/01",format="%Y/%m/%d"), 
            by = "1 month")
#Date_month <- format(Date,"%Y-%m")
chl_df$Date <- Date
# chl_df$Region <- as.numeric(chl_df$Region)

##### B. annually data
##### set initial parameter
month <- 1:304
year <- 1997:2022
nyear <- length(year)
annChl <- array(data=NA,dim=c(length(lon),length(lat),nyear))

##### set up annual chl
# 1997 annual
annChl[,,1] <- apply(chl[,,1:4],c(1,2),mean,na.rm=T)

# 1998-2020
ind <- 1
indtop <-5
for (i in 1998:2022){
  print(i)
  ind <- ind+1
  temp <- chl[,,indtop:(indtop+11)]
  annChl[,,ind] <- apply(temp,c(1,2),mean,na.rm=T)
  indtop <- indtop+12
}

boundind <- which(is.na(annChl))
annChl[boundind] <- NA

chl_df$month <- 1:304
chl_df$annValues <- NA
## Notes: make sure ocstore,lonstore,latstore is initial data
# # Method 1
# annChlstore <- annChl
# oc <- ocstore
# # 1997
# for(i in 1:area){
#   print(paste0("Region: ",i))
# 
#   oc <- ocstore
#   lon <- lonsstore
#   lat <- latsstore
#   annChl <- annChlstore
# 
#   boundind <- which(oc!=i | is.na(oc))
#   oc[boundind] <- NA
# 
#   temp <- annChl[,,1]
#   temp[boundind] <- NA
# 
#   ## averaging annually chl at each region
#   temp2<- as.numeric(mean(temp,na.rm=T))
#   chl_df$annValues[which(chl_df$Region==i & chl_df$month==c(1,2,3,4))] <- temp2
# }
# 
# # 1998-2020
# for(i in 1:area){
#   print(paste0("Region: ",i))
#   tick <- 5
# 
#   oc <- ocstore
#   lon <- lonsstore
#   lat <- latsstore
#   annChl <- annChlstore
# 
#   boundind <- which(oc!=i | is.na(oc))
#   oc[boundind] <- NA
# 
#   for(j in 2:nyear) {
#     temp <- annChl[,,j]
#     temp[boundind] <- NA
#     annChl[,,j] <- temp
# 
#     ## averaging annually chl at each region
#     temp2<- as.numeric(mean(annChl[,,j],na.rm=T))
#     ##
#     row_ind <- which(chl_df$Region==i)
#     row_ind <- row_ind[tick:c(tick+11)]
#     chl_df$annValues[row_ind] <- temp2
#     tick <- tick+12
#   }
# }
# 
# # temporali save chl_df
# oc_ts <- chl_df
# save(oc_ts,file = "ts_oc.Rdata")

# Method 2
annChlstore <- annChl
oc <- ocstore
# 1997
for(i in 1:area){
  print(paste0("Region: ",i))
  
  temp <- chl_df$Values[which(chl_df$Region==i & chl_df$month==c(1,2,3,4))] 
  
  ## averaging annually chl at each region
  temp2<- as.numeric(mean(temp,na.rm=T))
  chl_df$annValues[which(chl_df$Region==i & chl_df$month==c(1,2,3,4))] <- temp2
}

# 1998-2020
for(i in 1:area){
  print(paste0("Region: ",i))
  tick <- 5
  for(j in 2:nyear) {
    row_ind <- which(chl_df$Region==i)
    row_ind <- row_ind[tick:c(tick+11)]
    ## averaging annually chl at each region
    temp2<- as.numeric(mean(chl_df$Values[row_ind],na.rm=T))
    chl_df$annValues[row_ind] <- temp2
    tick <- tick+12
  }
}

# temporali save chl_df
oc_ts <- chl_df

### c. Trends
load("/Volumes/Doris/spt_static/annual_avg_lm.Rdata")
# trend <- trend[,ncol(trend):1]
# oc
load("/Volumes/Doris/spt_static/ind_max_360_180.Rdata")
ind_max <- ind_max[,ncol(ind_max):1]
oc <- ind_max
coast_ind <- which(oc==13 |oc==14)
oc[coast_ind] <- NA
table(oc)
area <- length(table(oc))
tr_df <- array(data=NA,dim=c(1,4)) # 64800*12
colnames(tr_df) <- c("Longitude","Latitude","Region","Trend")

ocstore <- oc
lonsstore <- lon
latsstore <- lat
trendstore <- trend

ind <- 0
tick <- 0
for(i in 1:area){
  print(paste0("Region: ",i))
  ind <- ind+1
  # refer to backups as overwrite within j loop
  oc <- ocstore
  lon <- lonsstore
  lat <- latsstore
  trend <- trendstore
  
  ## Selecting required region (i)
  # first replace not required with NA
  boundind <- which(oc!=i | is.na(oc))
  oc[boundind] <- NA
  regiontemp <- as.vector(aperm(oc,c(1,2)))
  
  trend[boundind] <- NA
  trendtemp <- as.vector(aperm(trend,c(1,2)))
  coordtemp <- expand.grid(lon,lat)
  coordtemp[,3] <- regiontemp
  coordtemp[,4] <- trendtemp
  colnames(coordtemp) <- c("Longitude","Latitude","Region","Trend")
  
  tr_df <- rbind(tr_df,coordtemp)
}

oc_tr <- tr_df
save(oc_ts,oc_tr,file = "ts_oc.Rdata")


