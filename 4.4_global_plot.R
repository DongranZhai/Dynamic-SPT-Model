## Update: July.27.2023 
######################### RUN IN LOCAL #################################
library(rgdal)
library(ggplot2)
library(mapproj)
library(munsell)
library(scales)

rm(list=ls())
setwd("/Volumes/Doris/spt_static/spt_long")
##### load data, map file, and setup fonts
load("/Volumes/Doris/spt_static/spt_long/results_table_longhurst.Rdata")
#-> including resultsTab,chlTab,trend_betap2
long<-readOGR(dsn="/Volumes/Doris/spt_static/map_data", layer="Longhurst_world_v4_2010")
long_df <- fortify(long)
wmap<-readOGR(dsn="/Volumes/Doris/spt_static/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea
trend <- resultsTab[,2]

for(i in 1:23){
  if((resultsTab[i,3]>0 & resultsTab[i,4]>0) | (resultsTab[i,3]<0 & resultsTab[i,4]<0)){
    trend[i] <- trend[i]
  }else{
    trend[i] <- 0
  }
}

long_df$trend <- NA
provs_order <- c(NA,NA,13,11,15,4,10,1,8,NA,NA,NA,NA,NA,NA,NA,14,NA,NA,NA,2,3,NA,NA,NA,NA,NA,NA,NA,18,19,12,16,17,23,20,6,5,7,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,21,22,NA,NA,NA)
for(i in 1:54){
  long_df$trend[which(long_df$id==i)] <- trend[provs_order[i]]
}

#centrepoints for region labelling
long_df$lonc <- NA
long_df$latc <- NA
centreps_df <- as.data.frame(coordinates(long))

# ************************************************** #
# -> this part totally follow matt
centreps_df[5,] <- c(-55.3,40.7) #small number of regions centrepoints not optimal
centreps_df[33,] <- c(149.3,39) #small number of regions centrepoints not optimal
centreps_df[36,] <- c(160.6,-36.3)
centreps_df[51,] <- c(75,-36.2)
# ************************************************** #
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

##### create trend map plot
colorPalette <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#colour palette
plot <- ggplot(data=long_df,aes(x=long,y=lat,group=group,fill=trend)) +
  scale_fill_gradientn(colors = colorPalette,na.value = "black",limits=c(-3,3),oob=squish) +
  geom_polygon(colour="black",size=0.25) +
  geom_text(data=long_df,aes(x=lonc,y=latc,label=trueid,family="Times"),size=4) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  coord_equal() +
  guides(fill=guide_colorbar(title=expression(paste("Trend  ", "(%yr"^" -1",")")))) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  annotate("text",x=-133,y=-38.1, label="21",size=4,colour='black',family="Times") +
  theme_bw() +
  theme(text=element_text(family = "Times"))
  
ggsave("trend_static_long.png",width=8.27, height=3.44, dpi=300)











