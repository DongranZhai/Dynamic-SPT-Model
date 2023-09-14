# Note: Plotting dynamic model
# Update: May.22: change plotting way
#       I. global under f4 model.
#       II. global under f8 model.

library(rgdal)
library(ggplot2)
library(mapproj)
library(munsell)
library(oceanmap)
library(dplyr)
library(scales)

########## Part I. Global under f4 model ##########
rm(list=ls())
setwd("/Volumes/Doris/spt_dynamic")
load("/Volumes/Doris/spt_dynamic/global_f1_resultTab.Rdata")
load("/Volumes/Doris/spt_dynamic/global_chl_oc_df_w_costal.Rdata")
results_tab <- as.data.frame(results_tab)

#### coordinate
ind <- global_no_na$s.index
temp <- global_no_na[which(global_no_na$s.index==ind),c(1:3)]
temp <- temp[!duplicated(temp$s.index),]
colnames(temp) <- c('Site','Longitude','Latitude')
# lon and lat have already been centred
comb_df <- merge(results_tab,temp,by='Site')

range(results_tab$Trend_Value,na.rm=T)
mean(results_tab$Trend_Value,na.rm=T)
sum(is.na(results_tab$Trend_Value))

### land
wmap<-readOGR(dsn="/Volumes/Doris/spt_static/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea
# mapp_df <- fortify(map("world2",plot=FALSE,fill=TRUE))

### pallete
colorPalette <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#colour palette

p <- ggplot() +
  geom_tile( data = comb_df , aes(x = Longitude,y = Latitude,fill = Trend_Value)) +
  scale_fill_gradientn(colors = colorPalette,na.value = "black",limits=c(-3,3),oob=squish) +
  coord_equal() +
  # geom_polygon(colour="black",size=0.25) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  # geom_polygon(data=map("world", wrap=c(0,360))) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  # scale_x_continuous(breaks = seq(0,360,60)) + # centre
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_colorbar(title=expression(paste("Trend  ", "(%yr"^" -1",")")),
                               title.position = "top",
                               barwidth = unit(.4, "cm"),barheight = unit(3.5, "cm"),title.vjust =1)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = 'right',legend.direction = "vertical",
        legend.title = element_text(size = 10,angle = 90),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))

ggsave("global_dynamic_f1.png",width=8.27, height=3.44, dpi=300)

########## Part II. Global under f2 model ##########
rm(list=ls())
setwd("/Volumes/Doris/Dynamic_spt")
load("/Volumes/Doris/Dynamic_spt/global_f2_resultTab.Rdata")
load("/Volumes/Doris/Dynamic_spt/global_chl_oc_df.Rdata")
results_tab <- as.data.frame(results_tab)

#### coordinate
ind <- global_no_na$s.index
temp <- global_no_na[which(global_no_na$s.index==ind),c(1:3)]
temp <- temp[!duplicated(temp$s.index),]
colnames(temp) <- c('Site','Longitude','Latitude')
# lon and lat have already been centred
comb_df <- merge(results_tab,temp,by='Site')

### land
wmap <- readOGR(dsn="/Volumes/Doris/Projects/sptmodel/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea
# mapp_df <- fortify(map("world2",plot=FALSE,fill=TRUE))

### pallete
colorPalette <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#colour palette

p <- ggplot() +
  geom_tile(data = comb_df , aes(x = Longitude,y = Latitude,fill = Trend_Value)) +
  scale_fill_gradientn(colors = colorPalette,na.value = "black",limits=c(-2,2),oob=squish) +
  coord_equal() +
  # geom_polygon(colour="black",size=0.25) +
  geom_polygon(data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  # geom_polygon(data=map("world", wrap=c(0,360))) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  # scale_x_continuous(breaks = seq(0,360,60)) + # centre
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_colorbar(title=expression(paste("Trend  ", "(10"^"-3"," %yr"^" -1",")")),
                               title.position = "top",
                               barwidth = unit(.4, "cm"),barheight = unit(3.5, "cm"),title.vjust =1)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = 'right',legend.direction = "vertical",
        legend.title = element_text(size = 10,angle = 90),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))

ggsave("global_dynamic_f2.png",width=8.27, height=3.44, dpi=300)


########## Part III. Updated plotting: Global under f1 model ##########
rm(list=ls())
setwd("/Volumes/Doris/spt_dynamic")
load("/Volumes/Doris/spt_dynamic/global_f1_resultTab.Rdata")
load("/Volumes/Doris/spt_dynamic/global_chl_oc_df_w_costal.Rdata")
results_tab <- as.data.frame(results_tab)

#### coordinate
# ind <- global_no_na$s.index
# temp <- global_no_na[which(global_no_na$s.index==ind),c(1:3)]
# temp <- temp[!duplicated(temp$s.index),]
# colnames(temp) <- c('Site','Longitude','Latitude')
# # lon and lat have already been centred
# comb_df <- merge(results_tab,temp,by='Site')

# range(results_tab$Trend_Value,na.rm=T)
# mean(results_tab$Trend_Value,na.rm=T)
# sum(is.na(results_tab$Trend_Value))

ind <- global_df$s.index
temp <- global_df[which(global_df$s.index==ind),c(1:3)]
temp <- temp[!duplicated(temp$s.index),]
colnames(temp) <- c('Site','Longitude','Latitude')
comb_df <- merge(temp,results_tab,by='Site',all.x=T,all.y=T)

# Stipple
comb_df$Sig <- NA
comb_df$Sig[which(is.na(comb_df$Trend_Value))] <- "No"
comb_df$Sig[which(!is.na(comb_df$Trend_Value))] <- "Yes"
comb_df$Sig <- as.factor(comb_df$Sig)

### land
wmap<-readOGR(dsn="/Volumes/Doris/spt_static/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea
# mapp_df <- fortify(map("world2",plot=FALSE,fill=TRUE))

### pallete
colorPalette <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#colour palette

p <- ggplot() +
  geom_tile(data = comb_df , aes(x = Longitude,y = Latitude,fill = Trend_Value)) +
  scale_fill_gradientn(colors = colorPalette,na.value = "white",limits=c(-3,3),oob=squish) + 
  geom_point(data =comb_df, aes(x = Longitude,y = Latitude,col=Sig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(Yes = alpha("white", 0),No = alpha("#353b48", 0.5))) + #
  coord_equal() +
  # geom_polygon(colour="black",size=0.25) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  # geom_polygon(data=map("world", wrap=c(0,360))) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  # scale_x_continuous(breaks = seq(0,360,60)) + # centre
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_colorbar(title=expression(paste("Trend  ", "(%yr"^" -1",")")),
                               title.position = "top",
                               barwidth = unit(.4, "cm"),barheight = unit(3.5, "cm"),title.vjust =1)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = 'right',legend.direction = "vertical",
        legend.title = element_text(size = 10,angle = 90),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))

ggsave("global_dynamic_f1_update.png",width=8.27, height=3.44, dpi=300)

########## Part IV. Updated plotting: Global under f2 model ##########
rm(list=ls())
setwd("/Volumes/Doris/spt_dynamic")
load("/Volumes/Doris/spt_dynamic/global_f2_resultTab.Rdata")
load("/Volumes/Doris/spt_dynamic/global_chl_oc_df_w_costal.Rdata")
results_tab <- as.data.frame(results_tab)

#### coordinate
# ind <- global_no_na$s.index
# temp <- global_no_na[which(global_no_na$s.index==ind),c(1:3)]
# temp <- temp[!duplicated(temp$s.index),]
# colnames(temp) <- c('Site','Longitude','Latitude')
# # lon and lat have already been centred
# comb_df <- merge(results_tab,temp,by='Site')

ind <- global_df$s.index
temp <- global_df[which(global_df$s.index==ind),c(1:3)]
temp <- temp[!duplicated(temp$s.index),]
colnames(temp) <- c('Site','Longitude','Latitude')
# lon and lat have already been centred
comb_df <- merge(temp,results_tab,by='Site',all.x=T,all.y=T)


range(results_tab$Trend_Value,na.rm=T)
mean(results_tab$Trend_Value,na.rm=T)
sum(is.na(results_tab$Trend_Value))

# Stipple
comb_df$Sig <- NA
comb_df$Sig[which(is.na(comb_df$Trend_Value))] <- "No"
comb_df$Sig[which(!is.na(comb_df$Trend_Value))] <- "Yes"
comb_df$Sig <- as.factor(comb_df$Sig)

### land
wmap<-readOGR(dsn="/Volumes/Doris/spt_static/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea
# mapp_df <- fortify(map("world2",plot=FALSE,fill=TRUE))

### pallete
colorPalette <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#colour palette

p <- ggplot() +
  geom_tile(data = comb_df , aes(x = Longitude,y = Latitude,fill = Trend_Value)) +
  scale_fill_gradientn(colors = colorPalette,na.value = "white",limits=c(-2,2),oob=squish) + 
  geom_point(data =comb_df, aes(x = Longitude,y = Latitude,col=Sig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(Yes = alpha("white", 0),No = alpha("#353b48", 0.5))) + #
  coord_equal() +
  # geom_polygon(colour="black",size=0.25) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  # geom_polygon(data=map("world", wrap=c(0,360))) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  # scale_x_continuous(breaks = seq(0,360,60)) + # centre
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_colorbar(title=expression(paste("Trend  ", "(%yr"^" -1",")")),
                               title.position = "top",
                               barwidth = unit(.4, "cm"),barheight = unit(3.5, "cm"),title.vjust =1)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = 'right',legend.direction = "vertical",
        legend.title = element_text(size = 10,angle = 90),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))

ggsave("global_dynamic_f2_update.1.png",width=8.27, height=3.44, dpi=300)
