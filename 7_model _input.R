# update: Jul.27
#         - use dataset with seasonality
#         - as.factor(month) instead of as.vector(month)
#-------------------------------------------------------------
# Note: This script is about generating a data frame, which contains:
#       optical class and chl data. (Global)
#       I. Extract global raster of chl and oc.
#       III. Create data frame contains global.
#       IV. (optional) check plot

library(R.matlab)
library(bmstdr)
library(spTDyn)
library(raster)
library(maptools)
library(oceanmap)
library(ggplot2)
library(reshape2)

rm(list=ls())
setwd("/Volumes/Doris/spt_dynamic")
########## Part I. Create data frame ##########
# load("/Volumes/Doris/raw_occci_chl/monthly_chl_rm_coastal_occci.Rdata")
load("/Volumes/Doris/raw_occci_chl/occci_chl_v6_new.Rdata")
load("/Volumes/Doris/spt_dynamic/oc_rm_coastal.Rdata")
rm("oc_glts") # monoc is after removing coastal region

chl.dim <- dim(monoc)
time <- 1:304
coordtemp <- expand.grid(time,lon,lat)
chltemp <- as.vector(aperm(chl,c(3,1,2)))
octemp <- as.vector(aperm(monoc,c(3,1,2)))

# corresponding Longhurst area
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
areatemp <- array(data=NA, dim = c(360,180,chl.dim[3]))
for(i in 1:chl.dim[3]){
  print(i)
  areatemp[,,i] <- Longhurst
}
areatemp <- as.vector(aperm(areatemp,c(3,1,2)))
sitetemp <- rep(1:(chl.dim[1]*chl.dim[2]),each=chl.dim[3])
YY <- as.vector(rep(c(rep(1997,time=4),rep(c(1998:2022),time=12)),times=(chl.dim[1]*chl.dim[2])))
MM <- as.factor(rep(c(9:12,rep(1:12,times=25)),times=(chl.dim[1]*chl.dim[2])))
TT <- rep(time,times=(chl.dim[1]*chl.dim[2]))

global_df <- data.frame(s.index=sitetemp,longitude=coordtemp[[2]],latitude=coordtemp[[3]],
                        year=YY,month=MM,time=TT,chl=chltemp,oc=octemp,area=areatemp)
na.ind <- which(!is.na(global_df$area))
global_no_na <- global_df[na.ind,]
save(global_df,global_no_na,file="global_chl_oc_df_w_costal.1.Rdata")

########## Part IV. Check plot ##########
### Longhurst
optical_palette <- c('1'='#9C27B0','2'='#512DA8','3'='#3742fa','4'='#448AFF','5'='#00BCD4','6'='#B2EBF2',
                     '7'='#32ff7e','8'='#badc58','9'='#FFEB3B','10'='#f78fb3','11'='#ffbe76','12'='#FFA000',
                     '13'='#eb4d4b','14'='#d63031')
label <- c('1','2','3','4','5','6','7','8','9','10','11','12','13','14')
lat <- seq(-89.5,89.5,by = 1) # npsg
lon <- seq(-179.5,179.5,by = 1) # npsg
# lat <- seq(-89.5,89.5,by = 1) # centre
# lon <- seq(0.5,359.5,by = 1) # centre

oc_raster  <- Longhurst
oc_raster <- matrix2raster(oc_raster, x = lon, y = lat, layer = 1)
oc_df <- as.data.frame(oc_raster ,xy = T)
colnames(oc_df) <-c('lon','lat','values')

p <- ggplot() +
  geom_raster( data = oc_df , aes(x = lon,y = lat,fill = values)) +
  scale_fill_gradientn(colours = optical_palette , na.value = 'black') +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  # scale_x_continuous(breaks = seq(-180,180,60)) + # npsg
  scale_x_continuous(breaks = seq(-180,180,60)) + # centre
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_legend(title = 'class number',title.position = "left",title.hjust = .5, show.limits = T,
                             label.position = 'bottom', direction = 'horizontal',nrow = 1, byrow = T,
                             barwidth = unit(10, "cm"),barheight = unit(.5, "cm")),color = 'legend' ) +
  labs(title = paste0('Long '),title.hjust=.5,
       x = "Longitude", y = "Latitude") +
  theme(legend.position = 'bottom')

savename <- paste0('long_check.png')
ggsave(savename,width=8.27, height=3.44, dpi=300)

### optical class (example: second layer)
optical_palette <- c('1'='#9C27B0','2'='#512DA8','3'='#3742fa','4'='#448AFF','5'='#00BCD4','6'='#B2EBF2',
                     '7'='#32ff7e','8'='#badc58','9'='#FFEB3B','10'='#f78fb3','11'='#ffbe76','12'='#FFA000',
                     '13'='#eb4d4b','14'='#d63031')
label <- c('1','2','3','4','5','6','7','8','9','10','11','12','13','14')
lat <- seq(-89.5,89.5,by = 1) # npsg
lon <- seq(-179.5,179.5,by = 1) # npsg
# lat <- seq(-89.5,89.5,by = 1) # centre
# lon <- seq(0.5,359.5,by = 1) # centre

oc_raster  <- oc_glts[,,2] # oc_npsg
oc_raster <- matrix2raster(oc_raster, x = lon, y = lat, layer = 1)
oc_df <- as.data.frame(oc_raster ,xy = T)
colnames(oc_df) <-c('lon','lat','values')

class_number <- 2
p <- ggplot() +
  geom_raster( data = oc_df , aes(x = lon,y = lat,fill = values)) +
  scale_fill_gradientn(colours = optical_palette , na.value = 'black') +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  # scale_x_continuous(breaks = seq(-180,180,60)) + # npsg
  scale_x_continuous(breaks = seq(0,360,60)) + # centre
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_legend(title = 'class number',title.position = "left",title.hjust = .5, show.limits = T,
                             label.position = 'bottom', direction = 'horizontal',nrow = 1, byrow = T,
                             barwidth = unit(10, "cm"),barheight = unit(.5, "cm")),color = 'legend' ) +
  labs(title = paste0('Class ', class_number),title.hjust=.5,
       x = "Longitude", y = "Latitude") +
  theme(legend.position = 'bottom')

savename <- paste0('oc',class_number,'.png') # npsg
ggsave(savename,width=8.27, height=3.44, dpi=300)

### chlorophyll (example: second layer)
library(palr)
pal <- chl_pal(palette = TRUE)
lat <- seq(-89.5,89.5,by = 1) # npsg
lon <- seq(-179.5,179.5,by = 1) # npsg
# lat <- seq(-89.5,89.5,by = 1) # centre
# lon <- seq(0.5,359.5,by = 1) # centre

oc_raster  <- chl[,,2] # chl_npsg
oc_raster <- matrix2raster(oc_raster, x = lon, y = lat, layer = 1)
oc_df <- as.data.frame(oc_raster ,xy = T)
colnames(oc_df) <-c('lon','lat','values')

class_number <- 2
p <- ggplot() +
  geom_raster( data = oc_df , aes(x = lon,y = lat,fill = values)) +
  scale_fill_gradientn(colours = optical_palette , na.value = 'black') +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  # scale_x_continuous(breaks = seq(-180,180,60)) + # npsg
  scale_x_continuous(breaks = seq(0,360,60)) + # centre
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_legend(title = 'class number',title.position = "left",title.hjust = .5, show.limits = T,
                             label.position = 'bottom', direction = 'horizontal',nrow = 1, byrow = T,
                             barwidth = unit(10, "cm"),barheight = unit(.5, "cm")),color = 'legend' ) +
  labs(title = paste0('Chl ', class_number),title.hjust=.5,
       x = "Longitude", y = "Latitude") +
  theme(legend.position = 'bottom')

savename <- paste0('chl_',class_number,'.png') # npsg
ggsave(savename,width=8.27, height=3.44, dpi=300)


