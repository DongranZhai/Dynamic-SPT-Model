## Update: Aug.21.2023 
######################### RUN IN LOCAL #################################
library(rgdal)
library(ggplot2)
library(mapproj)
library(munsell)
library(oceanmap)
library(dplyr)
library(scales)
library(R.matlab)

rm(list=ls())
setwd("/Volumes/Doris/spt_static/spt_30oc")
##### Import gsoc data dataframe and transform to matrix
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
oc <- oc_mat
rm("oc_mat")
# oc <- oc[,ncol(oc):1]


class <- 1:30
time <- 1:304
n_c <- length(class)

##### Import chl initial data
load("/Volumes/Doris/raw_occci_chl/occci_chl_v6_new.Rdata")

load("/Volumes/Doris/spt_static/spt_30oc/results_tables_30oc.Rdata")
trend <- resultsTab[,2]

### Option 1: determine if 0 is contained in 95% credibility intervals
# for(i in class){
#   #print(i)
#   if(i %in% c(26,35)){
#     trend[i] <- 0 # temporaly set the value as 0
#   }else if((resultsTab[i,3]>0 & resultsTab[i,4]>0) | (resultsTab[i,3]<0 & resultsTab[i,4]<0)){
#     trend[i] <- trend[i]
#   }else{
#     trend[i] <- 0
#   }
# }

for(i in class){
  #print(i)
  if((resultsTab[i,3]>0 & resultsTab[i,4]>0) | (resultsTab[i,3]<0 & resultsTab[i,4]<0)){
    trend[i] <- trend[i]
  }else{
    trend[i] <- 0
  }
}

# Option 2: without determine if 0 is contained in 95% credibility intervals
# for(i in class){
#   #print(i)
#   if(i %in% c(26,35)){
#     trend[i] <- 0 # temporaly set the value as 0
#   # }else if((resultsTab[i,3]>0 & resultsTab[i,4]>0) | (resultsTab[i,3]<0 & resultsTab[i,4]<0)){
#   #   trend[i] <- trend[i]
#   }else{
#     trend[i] <- trend[i]
#   }
# }

# oc <- oc[nrow(oc):1,]
oc_df <- matrix2raster(oc,x=lon,y=lat,layer=1)
oc_df <- as.data.frame(oc_df,xy=T)
oc_df$trend <- NA
colnames(oc_df) <- c("longitude","latitude","class","trend")

for(i in class){
  oc_df$trend[which(oc_df$class==i)] <- trend[i]
}

##### trim oc to 8640*4320
load("/Volumes/Doris/spt_static/ind_max_og_filter.Rdata")
oc_outline <- ind_max
rm("ind_max")
tempoc <- oc_outline[,ncol(oc_outline):1]
oc_raster <- matrix2raster(tempoc, x = lon, y = lat, layer = 1)
outline_df <- as.data.frame(oc_raster,xy = T)
colnames(outline_df) <-c('longitude','latitude','outline')
rm(list=c('oc_raster',"oc_outline"))

### land
wmap<-readOGR(dsn="/Volumes/Doris/spt_static/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea

##### create trend map plot ( with 8640*4320 outline)
colorPalette <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#colour palette

plot <- ggplot() +
  geom_raster(data=oc_df,aes(x=longitude,y=latitude,group=class,fill=trend)) +
  scale_fill_gradientn(colors = colorPalette,na.value = "black",limits=c(-3,3),oob=squish) +
  stat_contour(data=outline_df,aes(x=longitude,y=latitude,z=outline),
               geom="contour",colour="black",size=0.25) +
  coord_equal() +
  geom_polygon(colour="black",size=0.25) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  guides(fill=guide_colorbar(title=expression(paste("Trend  ", "(%yr"^" -1",")")))) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  theme_bw() +
  theme(text=element_text(family = "Times"))

ggsave("trend_static_30oc.png",width=8.27, height=3.44, dpi=300)

# #### trend map plot ( with 360*180 outline)
# oc_df$outline <- oc_outline
# colorPalette <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#colour palette
# 
# setwd("/Users/doris/Projects/spt_oc/spt_results/results_51class_mat")
# plot <- ggplot() +
#   geom_raster(data=oc_df,aes(x=longitude,y=latitude,group=class,fill=trend)) +
#   scale_fill_gradientn(colors = colorPalette,na.value = "black",limits=c(-2,2),oob=squish) +
#   stat_contour(data=oc_df,aes(x=longitude,y=latitude,z=outline),
#                geom="contour",colour="black",size=0.25) +
#   coord_equal() +
#   geom_polygon(colour="black",size=0.25) +
#   geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
#   guides(fill=guide_colorbar(title=expression(paste("Trend  ", "(%yr"^" -1",")")))) +
#   ylab("Latitude") +
#   xlab("Longitude") +
#   coord_cartesian(ylim = c(-75, 75),expand=F) +
#   scale_x_continuous(breaks = seq(-180,180,60)) +
#   scale_y_continuous(breaks = seq(-60,60,30)) +
#   annotate("text",x=-133,y=-38.1, label="21",size=4,colour='black',family="Times") +
#   theme_bw() +
#   theme(text=element_text(family = "Times"))
# 
# ggsave("trend_map_m21i_reduced.png",width=8.27, height=3.44, dpi=300)
