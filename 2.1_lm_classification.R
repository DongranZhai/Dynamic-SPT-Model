
# Update June.05.2023
#.      new CHL dataset (to Dec.2022)
##### Notes: This script is:
#####        1. Detecting annual trend using linear model
#####        2. With superimposed Longhurst outline
#####           1) Annual trend using linear model 
#####           2) Significant annual trend using linear model
#####        3. With superimposed wc outline
#####           1) Annual trend using linear model
#####           2) Significant annual trend using linear model

#############################################
library(dplyr)
library(ggplot2)
library(raster)
library(oceanmap)
library(scales)
library(rgdal)
library(mapproj)
library(munsell)
library(R.matlab)

########### Part I. detecting annual trend using linear model ###########
rm(list=ls())
setwd("/Volumes/Doris/spt_static")
load("/Volumes/Doris/raw_occci_chl/occci_chl_v6_new.Rdata")

##### set initial parameter
month <- 1:304
year <- 1997:2022
nyear <- length(year)
annChl <- array(data=NA,dim=c(length(lon),length(lat),nyear))

##### set up annual chl
# 1997 annual
annChl[,,1] <- apply(chl[,,1:4],c(1,2),mean,na.rm=T)

# 1998-2022
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

##### simple linear model
trend <- array(data=NA,dim=c(length(lon),length(lat)))
naNum <- array(data=0,dim=c(length(lon),length(lat)))
pValue <- array(data=NA,dim=c(length(lon),length(lat)))
fitted_value <- array(data=NA,dim = c(length(lon),length(lat),nyear))

tick <- 0
for(i in 1:length(lon)){
  for(j in 1:length(lat)){
    naNum[i,j] <- sum(is.na(annChl[i,j,]))
    
    if (naNum[i,j] <= 23){
      fit <- lm(annChl[i,j,] ~ year,na.action = na.omit)
      trend[i,j] <- fit$coefficients[2]
      pValue[i,j] <- summary(fit)$coefficients[8]
      
      tick_na <- 0 
      for(k in 1:nyear){ #length(fit$fitted.values)
        if(is.na(annChl[i,j,k])){
          fitted_value[i,j,k] <- NA
          tick_na <- tick_na + 1
        }else{
          fitted_value[i,j,k] <- fit$fitted.values[k-tick_na]
        }
      }
      tick <- tick + 1 
    } else {
      
    }
  }
}

# keep NA values
temp <- readMat("/Volumes/Doris/Projects/sptmodel/Longhurst_180.mat") #used to identify longhursst regions
Longhurst <- temp$Longhurst
nlon <- length(lon)
nlat <- length(lat)
area <- sort(unique(Longhurst[!is.na(Longhurst)]))#list of longhurst areas to iterate through
area <- area[-c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54)] #remove coastal, polar, GoM, Archipelagic Deep basin, Mediterranean

for(i in 1:nlon){
  for(j in 1:nlat){
    if(Longhurst[i,j] %in% area){
      Longhurst[i,j] <- Longhurst[i,j]
    }else{
      Longhurst[i,j] <- NA
    }
  }
}

ind <- which(is.na(Longhurst))
trend[ind] <- NA
pValue[ind] <- NA
naNum[ind] <- NA

savename <- paste0("annual_avg_lm.Rdata")
save(trend,pValue,naNum,fitted_value,lon,lat,file = savename)

########### Part II. detecting monthly trend using linear model ###########
rm(list=ls())
setwd("/Volumes/Doris/spt_static")
load("/Volumes/Doris/raw_occci_chl/occci_chl_v6_new.Rdata")
##### simple linear model
month <- 1:304
nmonth <- length(month)
mtrend <- array(data=NA,dim=c(length(lon),length(lat)))
naNum <- array(data=0,dim=c(length(lon),length(lat)))
mpValue <- array(data=NA,dim=c(length(lon),length(lat)))
fitted_mvalue <- array(data=NA,dim = c(length(lon),length(lat),nmonth))

tick <- 0
for(i in 1:length(lon)){
  for(j in 1:length(lat)){
    naNum[i,j] <- sum(is.na(chl[i,j,]))
    
    if (naNum[i,j] <= 300){
      fit <- lm(chl[i,j,] ~ month,na.action = na.omit)
      mtrend[i,j] <- fit$coefficients[2]
      mpValue[i,j] <- summary(fit)$coefficients[8]
      
      tick_na <- 0 
      for(k in 1:nmonth){ #length(fit$fitted.values)
        if(is.na(chl[i,j,k])){
          fitted_mvalue[i,j,k] <- NA
          tick_na <- tick_na + 1
        }else{
          fitted_mvalue[i,j,k] <- fit$fitted.values[k-tick_na]
        }
      }
      tick <- tick + 1 
    } else {
      
    }
  }
}

# keep NA values
temp <- readMat("/Volumes/Doris/Projects/sptmodel/Longhurst_180.mat") #used to identify longhursst regions
Longhurst <- temp$Longhurst
nlon <- length(lon)
nlat <- length(lat)
area <- sort(unique(Longhurst[!is.na(Longhurst)]))#list of longhurst areas to iterate through
area <- area[-c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54)] #remove coastal, polar, GoM, Archipelagic Deep basin, Mediterranean

for(i in 1:nlon){
  for(j in 1:nlat){
    if(Longhurst[i,j] %in% area){
      Longhurst[i,j] <- Longhurst[i,j]
    }else{
      Longhurst[i,j] <- NA
    }
  }
}

ind <- which(is.na(Longhurst))
mtrend[ind] <- NA
mpValue[ind] <- NA

savename <- paste0("month_avg_lm.Rdata")
save(mtrend,mpValue,fitted_mvalue,lon,lat,file = savename)

########### Part III. Annual trend using linear model (Longhurst) ###########
rm(list=ls())
setwd("/Volumes/Doris/spt_static")
load("/Volumes/Doris/spt_static/annual_avg_lm.Rdata")
##### set up longhurst outline
# plot input
long<-readOGR(dsn="/Volumes/Doris/Projects/spt_wc/wmap", layer="Longhurst_world_v4_2010")
long_df <- fortify(long)
wmap<-readOGR(dsn="/Volumes/Doris/Projects/spt_wc/wmap", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea
com <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#color palette

#centrepoints for region labelling
long_df$lonc <- NA
long_df$latc <- NA

centreps_df <- as.data.frame(coordinates(long))
centreps_df[5,] <- c(-55.3,40.7) #small number of regions centrepoints not optimal
centreps_df[33,] <- c(149.3,39) #small number of regions centrepoints not optimal
centreps_df[36,] <- c(160.6,-36.3)
centreps_df[51,] <- c(75,-36.2)

for(i in c(4,5,6,7,8,9,10,18,22,23,31,32,33,34,35,36,37,38,39,40,41,51,52)){
  long_df$lonc[which(long_df$id==i)] <- centreps_df$V1[i]
  long_df$latc[which(long_df$id==i)] <- centreps_df$V2[i] 
}

# correct labels
long_df$trueid <- NA
provs_order <- c(NA,NA,NA,13,11,15,4,10,1,8,NA,NA,NA,NA,NA,NA,NA,14,NA,NA,NA,2,3,NA,NA,NA,NA,NA,NA,NA,18,19,12,16,17,23,20,6,5,7,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,21,22,NA,NA)#slightly different due to null region
for(i in 1:54){
  long_df$trueid[which(long_df$id==i)] <- provs_order[i]
}

# ############# Without significant
# ##### trend plot
# ## trim trend value
# trendvalue <- trend*100
# 
# # temptrend <- trendvalue[,ncol(trendvalue):1]
# # temptrend <- temptrend[nrow(temptrend):1,]
# trend_raster <- matrix2raster(trendvalue, x = lon, y = lat, layer = 2)
# trend_df <- as.data.frame(trend_raster,xy = T)
# colnames(trend_df) <-c('lon','lat','trend')
# rm(list=c('trend_raster'))
# 
# plot <- ggplot() +
#   geom_raster(data = trend_df, aes(x = lon,y = lat,fill = trend)) +
#   scale_fill_gradientn(colors = com,limits =c(-2,2),na.value = 'black',oob=squish) +
#   geom_polygon(data=long_df,aes(x=long,y=lat,group=group),colour='black',alpha = 0,size=0.4)+
#   geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 )+
#   geom_text(data=long_df,aes(x=lonc,y=latc,label=trueid,family="Times"),color = 'black',size=4) +
#   coord_equal() + 
#   coord_cartesian(ylim = c(-75, 75),xlim = c(-180,180),expand=F)+
#   scale_x_continuous(breaks = seq(-180,180,60)) +
#   scale_y_continuous(breaks = seq(-90,90,30)) +
#   guides(fill = guide_colorbar(title=expression(paste("Trend  ", "(%yr"^" -1",")"))), title.position = "right",
#          title.theme = element_text(angle = 90),
#          barwidth = unit(.5, "cm"),barheight = unit(7.5, "cm"),title.hjust = .5) +
#   xlab("Longitude") +
#   ylab("Latitude") +
#   # ggtitle(paste0("1997-2020 global annual mean chl trend")) +
#   theme(legend.position = 'right',plot.margin = margin(t = .3, r = .5, b = .3, l = .3, unit = "cm"))
# 
# savename <- paste0("lm_ann_avg_Long.png")
# ggsave(savename, width=8.27, height=3.44, dpi=300)

########### Significant annual trend using linear model
sigtrend <- array(data=NA,dim=c(length(lon),length(lat)))
for(i in 1:length(lon)){
  for(j in 1:length(lat)){
    if(is.na(pValue[i,j])){
      sigtrend[i,j] <- NA
    }else if(pValue[i,j]< 0.05){
      sigtrend[i,j] <- trend[i,j]
    }else{
      sigtrend[i,j] <- 0
    }
  }
}

# convert trend values to percent per year ( from decimal change per month)
trendvalue <- sigtrend*100

# temptrend <- trendvalue[,ncol(trendvalue):1]
# temptrend <- temptrend[nrow(temptrend):1,]
trend_raster <- matrix2raster(trendvalue, x = lon, y = lat, layer = 2)
trend_df <- as.data.frame(trend_raster,xy = T)
colnames(trend_df) <-c('lon','lat','sigtrend')
rm(list=c('trend_raster'))

plot <- ggplot() +
  geom_raster(data = trend_df, aes(x = lon,y = lat,fill = sigtrend)) +
  scale_fill_gradientn(colors = com,limits =c(-2,2),na.value = 'black',oob=squish) +
  geom_polygon(data=long_df,aes(x=long,y=lat,group=group),colour='black',alpha = 0,size=0.4)+ 
  geom_polygon(data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 )+
  geom_text(data=long_df,aes(x=lonc,y=latc,label=trueid,family="Times"),color = 'black',size=4) +
  coord_equal() + 
  coord_cartesian(ylim = c(-75, 75),xlim = c(-180,180),expand=F)+
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-90,90,30)) +
  guides(fill = guide_colorbar(title=expression(paste("Trend  ","(%yr"^" -1",")")),
                               title.position = "top",
                               barwidth = unit(.4, "cm"),barheight = unit(3.5, "cm"),title.vjust =1)) +
  xlab("Longitude") +
  ylab("Latitude") +
  #ggtitle(paste0("1997-2020 global annual median significant chl trend")) +
  theme_bw() +
  theme(legend.position = 'right',legend.direction = "vertical",
        legend.title = element_text(size = 10,angle = 90),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))

ggsave(filename = paste0("lm_yr_Long.png"), width=8.27, height=3.44, dpi=300)


########### Part IV. Annual trend using linear model (wc) ##############
rm(list=ls())
setwd("/Volumes/Doris/spt_static")
wmap<-readOGR(dsn="/Volumes/Doris/Projects/spt_wc/wmap", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea
com <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#color palette

##### trim trend (360*180)
load("/Volumes/Doris/spt_static/annual_avg_lm.Rdata")
sigtrend <- array(data=NA,dim=c(length(lon),length(lat)))
for(i in 1:length(lon)){
  for(j in 1:length(lat)){
    if(is.na(pValue[i,j])){
      sigtrend[i,j] <- NA
    }else if(pValue[i,j]< 0.05){
      sigtrend[i,j] <- trend[i,j]
    }else{
      sigtrend[i,j] <- 0
    }
  }
}

trendvalue <- sigtrend*100

trend_raster <- matrix2raster(trendvalue, x = lon, y = lat, layer = 1)
trend_df <- as.data.frame(trend_raster,xy = T)
colnames(trend_df) <-c('lon','lat','sigtrend')
rm(list=c('trend_raster'))

##### wc outline (8640*4320)
load("/Volumes/Doris/spt_static/ind_max_og_filter.Rdata")
otl <- ind_max
rm("ind_max")
# remove class 11 - 14, and trim -70~70
# coast_ind <- which(otl==11 | otl==12 | otl==13 | otl==14)
# otl[coast_ind] <- NA
lat_ind <- which(lat>=70 | lat<= -70)
otl[,lat_ind] <- NA

otl <- otl[,ncol(otl):1]
# otl <- otl[nrow(otl):1,]
otl_raster <- matrix2raster(otl, x = lon, y = lat, layer = 1)
otl_df <- as.data.frame(otl_raster,xy = T)
colnames(otl_df) <-c('longitude','latitude','outline')
rm(list=c('otl_raster'))

##### create chl map plot
plot <- ggplot() +
  geom_raster(data = trend_df, aes(x = lon,y = lat,fill = sigtrend)) +
  scale_fill_gradientn(colors = com,limits=c(-2,2),na.value = 'black',oob=squish) + # limits =c(-2,2)
  stat_contour(data=otl_df,aes(x=longitude,y=latitude,z=outline),
               geom="contour",colour="black",size=0.25) +
  geom_polygon(data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25)+
  coord_equal() + 
  coord_cartesian(ylim = c(-75, 75),xlim = c(-180,180),expand=F)+
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-90,90,30)) +
  guides(fill = guide_colorbar(title=expression(paste("Trend  ","(%yr"^" -1",")")),
                               title.position = "top",
                               barwidth = unit(.4, "cm"),barheight = unit(3.5, "cm"),title.vjust =1)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  theme(legend.position = 'right',legend.direction = "vertical",
        legend.title = element_text(size = 10,angle = 90),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))

ggsave(filename = paste0("lm_yr_wc.png"), width=8.27, height=3.44, dpi=300)


