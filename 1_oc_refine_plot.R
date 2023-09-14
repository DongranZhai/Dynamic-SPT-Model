# Update: June.05.2023

##################################### Script Notation #####################################
########## Part I: Define the contour of water classes, then superimpose wc outline 
#                 on both annual trends and significant trends map
########## Part II: This script is mainly for remove the cloud, coastal and unprevalent regions:
#                  1) abandon 13 and 14 (coastal region) water class;
#                  2) trim the latitude between -70 ~ 70, focus on the tropical and subtropical region,
#                  because the data at polar and subpolar are shadowed by the cloud
#                  3) keep the same NA regions with Longhurst
library(ggplot2)
library(oceanmap)
library(rgdal)
library(mapproj)
library(munsell)
library(scales)
library(R.matlab)
library(dplyr)

rm(list=ls())
setwd("/Volumes/Doris/spt_static")
################ Part I. Visualize optical class #######################
##### land
wmap<-readOGR(dsn="/Volumes/Doris/Projects/spt_wc/wmap", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]

##### wc outline and filled (8640*4320)
load("/Volumes/Doris/spt_static/ind_max_og_filter.Rdata")
otl <- ind_max
nlon <- length(lon)
nlat <- length(lat)
rm("ind_max")
# remove class 13 - 14, and trim -70~70
coast_ind <- which(otl==13 | otl==14)
otl[coast_ind] <- NA
lat_ind <- which(lat>=70 | lat<= -70)
otl[,lat_ind] <- NA

# Keep same NA regions with Long
temp <- readMat("/Volumes/Doris/Projects/sptmodel/Longhurst_180.mat") #used to identify longhursst regions
Longhurst <- temp$Longhurst
##### 360*180 Longhurt to 8640*4320
temp <- Longhurst
r <- raster(temp)
extent(r) <- extent(c(-180,180,-90,90))
r_1 <- raster(nrow=8640,ncol=4320)
r_1 <- resample(r,r_1,method = "ngb")
temp <- as.matrix(r_1)
Longhurst <- temp

area <- sort(unique(Longhurst[!is.na(Longhurst)]))#list of longhurst areas to iterate through
area <- area[-c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54)] #remove coastal, polar, GoM, Archipelagic Deep basin, Mediterranean

for(i in 1:nlon){
  for(j in 1:nlat){
    if(Longhurst[i,j] %in% area == T){
      Longhurst[i,j] <- Longhurst[i,j]
    }else{
      Longhurst[i,j] <- NA
    }
  }
}

ind <- which(is.na(Longhurst))

otl <- otl[,ncol(otl):1]
# otl <- otl[nrow(otl):1,]
otl[ind] <- NA
otl_raster <- matrix2raster(otl, x = lon, y = lat, layer = 1)
otl_df <- as.data.frame(otl_raster,xy = T)
colnames(otl_df) <-c('longitude','latitude','outline')
rm(list=c('otl_raster'))


# ##### create wc defination map plot
# optical_palette <- c('1'='#9C27B0','2'='#512DA8','3'='#3742fa','4'='#448AFF','5'='#00BCD4','6'='#B2EBF2',
#                      '7'='#0fb9b1','8'='#badc58','9'='#32ff7e','10'='#FFCCBC','11'='#FFEB3B','12'='#FFA000')
# optical_palette <- c('1'='#9C27B0','2'='#512DA8','3'='#3742fa','4'='#448AFF','5'='#00BCD4','6'='#B2EBF2',
#                      '7'='#32ff7e','8'='#badc58','9'='#FFEB3B','10'='#ffbe76','11'='#FFA000','12'='#eb4d4b')
optical_palette <- c('1'='#9C27B0','2'='#512DA8','3'='#3742fa','4'='#448AFF','5'='#00BCD4','6'='#B2EBF2',
                     '7'='#7bed9f','8'='#badc58','9'='#FFEB3B','10'='#F79F1F','11'='#EE5A24','12'='#EA2027')
label <- c('1','2','3','4','5','6','7','8','9','10','11','12')

plot <- ggplot() +
  geom_raster(data=otl_df,aes(x=longitude,y=latitude,group=outline,fill=factor(outline))) +
  scale_fill_manual(values = optical_palette , na.value = 'black',limits=label) +
  stat_contour(data=otl_df,aes(x=longitude,y=latitude,z=outline),
               geom="contour",colour="black",size=0.25) +
  coord_equal() + 
  geom_polygon(colour="black",size=0.25) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  guides(fill = guide_legend(title = 'class number',title.position = "left",title.hjust = .5, show.limits = T,
                             label.position = 'bottom', direction = 'horizontal',nrow = 1, byrow = T,
                             barwidth = unit(10, "cm"),barheight = unit(.5, "cm")),color = 'legend') +
  coord_cartesian(xlim = c(-180, 180),ylim = c(-75, 75), expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  labs(title = "", x = "Longitude", y = "Latitude") + # Globally Dominant Optical Water Class
  theme(legend.position = 'bottom',legend.direction = "horizontal",
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))

ggsave("synna_12wc.png",width=8.2, height=4.3,dpi=300)


################ Part II. optical class dataframe ####################

load("/Volumes/Doris/spt_static/ind_max_360_180.Rdata")
ind_max <- ind_max[,ncol(ind_max):1]
ind_max <- ind_max[nrow(ind_max):1,]
wc_raster <- matrix2raster(ind_max, x = lon, y = lat, layer = 1)
oc_df <- as.data.frame(wc_raster,xy = T)
colnames(oc_df) <-c('lon','lat','wc')
rm(list=c('wc_raster'))

# create spatial points data frame
coordinates(oc_df) <- ~ lon + lat
# coerce to SpatialPixelsDataFrame
gridded(oc_df) <- T
# coerce to raster
temp <- raster(oc_df)
oc_mat <- raster2matrix(temp)
rm(list=c("temp","oc_df"))
# check if import is right
table(oc_mat)




