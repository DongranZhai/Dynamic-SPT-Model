# Update: May.20.2024
#June.05.2023
##################################### Script Notation #####################################
########## Part I: Define the contour of optical classes, with removing the cloud, coastal and unprevalent regions:
#                  1) abandon 13 and 14 (coastal region) water class;
#                  2) trim the latitude between -70 ~ 70, focus on the tropical and subtropical region,
#                  because the data at polar and subpolar are shadowed by the cloud
#                  3) keep the same NA regions with Longhurst
########## Part II: Geographically subdivided dominant optical classes in terms of ocean basin.
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
                     '7'='#32ff7e','8'='#badc58','9'='#FFEB3B','10'='#FFA000')
label <- c('1','2','3','4','5','6','7','8','9','10')

plot <- ggplot() +
  geom_raster(data=otl_df,aes(x=longitude,y=latitude,group=outline,fill=factor(outline))) +
  scale_fill_manual(values = optical_palette , na.value = 'black',limits=label) +
  stat_contour(data=otl_df,aes(x=longitude,y=latitude,z=outline),
               geom="contour",colour="black",size=0.25) +
  coord_equal() + 
  geom_polygon(colour="black",size=0.25) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  guides(fill = guide_legend(title = 'class number',title.position = "bottom",title.hjust = .5, show.limits = T,
                             label.position = 'bottom', direction = 'horizontal',nrow = 1, byrow = T,
                             barwidth = unit(10, "cm"),barheight = unit(.5, "cm")),color = 'legend') +
  coord_cartesian(xlim = c(-180, 180),ylim = c(-75, 75), expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  labs(title = "", x = "Longitude", y = "Latitude") + # Globally Dominant Optical optical Class
  theme(legend.position = 'bottom',plot.margin = margin(t = .3, r = .5, b = .3, l = .3, unit = "cm"))
ggsave("synna_10wc.png",width=8.2, height=4.3,dpi=300)

################ Part II. Visualization global geographical separated optical class ####################
##### Notes: Simplify the global ocean and sea table,
#####        merging that just remains the five basins
rm(list=ls())
##### set initial parameters
# land
wmap<-readOGR(dsn="/Volumes/Doris/Projects/sptmodel/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea
# ocean
ocean_tab <- read.csv(file='/Volumes/Doris/Projects/wc_gs/ocean_basin_gswc.csv',
                      header = T,fill=T,fileEncoding = "UTF-8")
# palette
basin_palette <- c('1'='#C4E538','2'='#C4E538','3'='#C4E538','4'='#C4E538',
                   '5'='#F79F1F','6'='#F79F1F','7'='#9980FA','8'='#54a0ff','9'='#fab1a0',
                   '10'='#95afc0','11'='#95afc0','12' = '#e84393')
##### centroids of basins and label natty
cent_id <- aggregate(cbind(Longitude, Latitude) ~ Group, data=ocean_tab, FUN=mean)
cent_id$label <- c("North Pacific Ocean","North Pacific Ocean","",
                   "South Pacific Ocean","North Atlantic Ocean","South Atlantic Ocean",
                   "Indian Ocean","Arctic Ocean","Southern Ocean","","","Antarctic Circumpolar Current")
cent_id[1,2:3] <- c(150,22)
cent_id[2,2:3] <- c(-150,30)
cent_id[4,2:3] <- c(-120,-34)
cent_id[5,2:3] <- c(-36.5,37)
cent_id[6,2:3] <- c(-23,-34)
cent_id[7,2:3] <- c(80,-17)
cent_id[8,2:3] <- c(-5,70)
cent_id[9,2:3] <- c(0,-65)
cent_id$Longitude <- as.numeric(cent_id$Longitude)
cent_id$Latitude <- as.numeric(cent_id$Latitude)

##### plot the primary basins
# plot <- ggplot() +
#   geom_polygon(data=ocean_tab,aes(x = Longitude,y = Latitude,group=Name,fill = factor(Group)),
#                color="black",size=0.25) +
#   scale_fill_manual(values = basin_palette, na.value = '#1e272e') +
#   geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),
#                 color="black", fill="#535c68",size=0.15 ) +
#   scale_x_continuous(breaks = seq(-180,180,60)) +
#   scale_y_continuous(breaks = seq(-90,90,30)) +
#   coord_cartesian(ylim = c(-75, 75),expand=F) +
#   geom_text(data = cent_id,aes(x=Longitude,y=Latitude,label=label),
#             family = "Times New Roman",size=3,fontface="bold",alpha=0.8) +
#   labs(title = "Name of Main Ocean Basins (IHO)", x = "longitude", y = "latitude") +
#   theme(legend.position = "none",plot.title = element_text(hjust = 0.5))
# ggsave("ocean_seas.1.png",width=8.5, height=4.3, dpi=300)

##### check which polygon(region) the picked pixel is in
ocean_tab <- read.csv(file='/Volumes/Doris/Projects/wc_gs/ocean_basin_gswc.csv',
                      header = T,fill=T,fileEncoding = "UTF-8")
# optical class data frame
load("/Volumes/Doris/Projects/spt_wc/spt_results/ind_max_360_180.Rdata")
wc <- ind_max
nlon <- length(lon)
nlat <- length(lat)
rm("ind_max")
# remove class 11 - 14, and trim -70~70
coast_ind <- which(wc==11 | wc==12 | wc==13 | wc==14)
wc[coast_ind] <- NA
lat_ind <- which(lat>=70 | lat<= -70)
wc[,lat_ind] <- NA

# Keep same NA regions with Long
temp <- readMat("/Volumes/Doris/Projects/sptmodel/Longhurst_180.mat") #used to identify longhursst regions
Longhurst <- temp$Longhurst
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

wc <- wc[,ncol(wc):1]
wc <- wc[nrow(wc):1,]
wc[ind] <- NA
wc_raster <- matrix2raster(wc, x = lon, y = lat, layer = 1)
wc_df <- as.data.frame(wc_raster,xy = T)
colnames(wc_df) <-c('lon','lat','wc')
rm(list=c('wc_raster'))
wc_df$Regions <- NA

basins_labels <- c("North Pacific Ocean","North Pacific Ocean","South Pacific Ocean",
                   "South Pacific Ocean","North Atlantic Ocean","South Atlantic Ocean",
                   "Indian Ocean","Arctic Ocean","Southern Ocean","Seas","Seas","ACC")

for(i in c(1:12)){
  print(i)
  tempwc <- wc_df[1:2]
  ocean_tab_temp <- ocean_tab%>%
    filter(Group==i)%>%
    dplyr::select(Longitude,Latitude)
  result <- pointsInPolygon(tempwc, ocean_tab_temp, logical = T)
  wc_df$Regions[result] <- basins_labels[i]
}
#### merge small sample class i.e. 7->6, 10->11
tick <- which(wc_df$Regions == "ACC" & wc_df$wc == 4)
wc_df$Regions[tick] <- 'South Pacific Ocean'
tick <- which(wc_df$Regions == "ACC" & wc_df$wc == 9)
wc_df$wc[tick] <- 8
tick <- which(wc_df$wc == 7)
wc_df$wc[tick] <- 6
wc_df$Regions[tick] <- c("North Atlantic Ocean")

# tick <- which(wc_df$wc == 10)
# wc_df$wc[tick] <- 9

# just 1 grid of class 5 in Arctic Ocean, merging to AO
# tick <- which(wc_df$wc == 5 & wc_df$Regions == "Arctic Ocean")
# wc_df$Regions[tick] <- "North Atlantic Ocean"

# class6&9
tick <- which(wc_df$wc == 6 & wc_df$Regions == c('South Pacific Ocean'))
wc_df$Regions[tick] <- 'North Pacific Ocean'
tick <- which(wc_df$wc == 6 & wc_df$Regions == c("South Atlantic Ocean"))
wc_df$Regions[tick] <- "North Atlantic Ocean"
tick <- which(wc_df$wc == 9 & wc_df$Regions == c('South Pacific Ocean'))
wc_df$Regions[tick] <- 'North Pacific Ocean'
tick <- which(wc_df$wc == 9 & wc_df$Regions == c("South Atlantic Ocean"))
wc_df$Regions[tick] <- "North Atlantic Ocean"

##### define the optical classes into basins
class <- 1:11
basins_label <- c("North Pacific Ocean","South Pacific Ocean",
                   "North Atlantic Ocean","South Atlantic Ocean",
                   "Indian Ocean","Arctic Ocean","Southern Ocean","Seas","ACC")

wc_result <- as.data.frame(array(data=NA, dim=c(1,5)))
colnames(wc_result) <- c("lon","lat","wc","Regions","gswc")
for(i in 1:length(basins_label)){
  tempwc <- wc_df
  # pick one basin
  print(basins_label[i])
  temp <- tempwc %>%
    filter(Regions==basins_label[i])
  table <- table(temp$wc)
  print(table)
  # reorder optical class for labeling
  temp <- temp[with(temp, order(wc)),]
  
  # gswc labeling
  temp$gswc <- as.numeric(as.factor(rank(temp$wc,na.last = "keep")))
  for (j in 1:length(temp$gswc)){

    if(temp$Regions[j]=="South Pacific Ocean"){
      temp$gswc[j] <- temp$gswc[j] + 6
    }else if(temp$Regions[j]=="North Atlantic Ocean"){
      temp$gswc[j] <- temp$gswc[j] + 11
    }else if(temp$Regions[j]=="South Atlantic Ocean"){
      temp$gswc[j] <- temp$gswc[j] + 17
    }else if(temp$Regions[j]=="Indian Ocean"){
      temp$gswc[j] <- temp$gswc[j] + 22
    }else if(temp$Regions[j]=="Arctic Ocean"){
      temp$gswc[j] <- NA
    }else if(temp$Regions[j]=="Southern Ocean"){
      temp$gswc[j] <- NA
    }else if(temp$Regions[j]=="Seas"){
      temp$gswc[j] <- NA
    }else if(temp$Regions[j]=="ACC"){
      temp$gswc[j] <- temp$gswc[j] + 29
    }
  }
  # correspding to original optical class df
  wc_result <- rbind(wc_result,temp)
  
}
wc_result <- wc_result[-1,]

wc_result <- wc_df %>%
  left_join(wc_result,by =c("lon","lat","wc","Regions"))

filename <- paste0("gswc30_df.Rdata")
save(wc_result,file =filename)

######################################### GS optical class ###########################
##### plot the geographical separated optical class
load("/Volumes/Doris/Projects/spt_wc/spt_results/gswc30_df.Rdata")
wmap<-readOGR(dsn="/Volumes/Doris/Projects/spt_wc/wmap", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]

load("/Volumes/Doris/Projects/spt_wc/spt_results/ind_max_og_filter.Rdata")
otl <- ind_max
rm("ind_max")
# remove class 11 - 14, and trim -70~70
coast_ind <- which(otl==11 | otl==12 | otl==13 | otl==14)
otl[coast_ind] <- NA
lat_ind <- which(lat>=70 | lat<= -70)
otl[,lat_ind] <- NA

otl <- otl[,ncol(otl):1]
# otl <- otl[nrow(otl):1,]
otl_raster <- matrix2raster(otl, x = lon, y = lat, layer = 1)
otl_df <- as.data.frame(otl_raster,xy = T)
colnames(otl_df) <-c('longitude','latitude','outline')
rm(list=c('otl_raster'))

# optical_palette <- c('1'='#9C27B0','2'='#3742fa','3'='#448AFF','4'='#00BCD4','5'='#B2EBF2',
#                      '6'='#badc58','7'='#32ff7e','8'='#FFEB3B','9'='#FFA000','10'='#512DA8',
#                      '11'='#3742fa','12'='#448AFF','13'='#00BCD4','14'='#B2EBF2','15'='#0fb9b1',
#                      '16'='#badc58','17'='#32ff7e','18'='#FFEB3B','19'='#FFA000','20'='#512DA8',
#                      '21'='#3742fa','22'='#448AFF','23'='#00BCD4','24'='#B2EBF2','25'='#badc58',
#                      '26'='#32ff7e','27'='#FFEB3B','28'='#FFA000','29'='#00BCD4','30'='#badc58',
#                      '31'='#FFCCBC','32'='#FFEB3B','33'='#FFppp0o9A000','34'='#448AFF','35'='#0fb9b1',
#                      '36'='#badc58','37'='#32ff7e','38'='#FFEB3B','39'='#FFA000')
turbo_pal <- viridis::viridis(n = 37,option = "H")

plot <- ggplot() +
  geom_raster(data=wc_result,aes(x=lon,y=lat,group=Regions,fill=factor(gswc))) +
  scale_fill_manual(values = turbo_pal, na.value = 'black') +
  # scale_fill_viridis(option = "H",discrete = TRUE) +
  stat_contour(data=otl_df,aes(x=longitude,y=latitude,z=outline),
               geom="contour",colour="black",size=0.25) +
  coord_equal() + 
  geom_polygon(colour="black",size=0.25) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  guides(fill = guide_legend(title = 'class number',title.position = "bottom",
                             title.hjust = .5, show.limits = T,label.position = 'bottom', 
                             direction = 'horizontal',nrow = 1, byrow = T)) + #color = 'legend'
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  labs(title = "", x = "Longitude", y = "Latitude") + # Geographically Separated Optical optical Class
  theme(legend.position = 'bottom',plot.margin = margin(t = .3, r = .5, b = .3, l = .3, unit = "cm"))
ggsave("gswc30.png",width=8.5, height=4.3, dpi=300) # 4.3





