# Notes: This script is to compare uncertainty of f4 and f8.
#        I. Without deseasonality data
#       II. why magnitude differenct between with/not deszn?

library(rgdal)
library(ggplot2)
library(mapproj)
library(munsell)
library(oceanmap)
library(dplyr)
library(scales)
library(car)

rm(list=ls())
setwd("/Volumes/Doris/Dynamic_spt")
####################### I. Without deseasonality data #######################
load("/Volumes/Doris/Dynamic_spt/global_f1_resultTab.Rdata")
f4_results <- as.data.frame(results_tab)
load("/Volumes/Doris/Dynamic_spt/global_f2_resultTab.Rdata")
f8_results <- as.data.frame(results_tab)

# CI value
f4_ci <- f4_results$UCI - f4_results$LCI
f8_ci <- f8_results$UCI - f8_results$LCI
hist(f4_ci)
hist(f8_ci)
# we would like to see f8 reduce the uncertainty, thus, diff>0 is good and as expectation.
diff <- f4_ci-f8_ci
hist(diff)
boxplot(diff)
site <- f4_results$Site
diffci_df <- data.frame(Site=site,Diff=diff)

#### coordinate
load("/Volumes/Doris/Dynamic_spt/global_chl_oc_df.Rdata")
ind <- global_df$s.index
temp <- global_df[which(global_df$s.index==ind),c(1:3)]
temp <- temp[!duplicated(temp$s.index),]
colnames(temp) <- c('Site','Longitude','Latitude')
comb_df <- merge(diffci_df,temp,by='Site',all.x=T,all.y=T)
range(comb_df$Diff,na.rm=T)
#  -0.2123951  0.2059667
comb_df$Diff <- comb_df$Diff*100

# Stipple
comb_df$Sig <- NA
comb_df$Sig[which(is.na(comb_df$Diff))] <- "No"
comb_df$Sig[which(!is.na(comb_df$Diff))] <- "Yes"
comb_df$Sig <- as.factor(comb_df$Sig)

# check plot
### land
wmap <- readOGR(dsn="/Volumes/Doris/Projects/sptmodel/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea
# mapp_df <- fortify(map("world2",plot=FALSE,fill=TRUE))

### pallete
colorPalette <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#colour palette

p <- ggplot() +
  geom_tile(data = comb_df , aes(x = Longitude,y = Latitude,fill = Diff)) +
  scale_fill_gradientn(colors = colorPalette,na.value = "white",limits=c(-20,20),oob=squish) + # na.value = "black"
  geom_point(data =comb_df, aes(x = Longitude,y = Latitude,col=Sig),size=0.00000001,show.legend = F) +
  scale_color_manual(values = c(Yes = alpha("white", 0),No = alpha("#353b48", 0.5))) + #
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
  guides(fill = guide_colorbar(title=expression(paste("Uncertainty change (%)")),
                               title.position = "top",
                               barwidth = unit(.4, "cm"),barheight = unit(3.5, "cm"),title.vjust =1)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = 'right',legend.direction = "vertical",
        legend.title = element_text(size = 10,angle = 90),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))

ggsave("diff_ci.2.png",width=8.27, height=3.44, dpi=300)

######### II. Confident interval map in f1 ###########
rm(list=ls())
setwd("/Volumes/Doris/spt_dynamic")
load("/Volumes/Doris/spt_dynamic/global_f1_resultTab.Rdata")
load("/Volumes/Doris/spt_dynamic/global_chl_oc_df_w_costal.Rdata")
results_tab <- as.data.frame(results_tab)

ind <- global_df$s.index
temp <- global_df[which(global_df$s.index==ind),c(1:3)]
temp <- temp[!duplicated(temp$s.index),]
colnames(temp) <- c('Site','Longitude','Latitude')
comb_df <- merge(temp,results_tab,by='Site',all.x=T,all.y=T)

# Length of CI
comb_df$CI <- comb_df$UCI-comb_df$LCI
range(comb_df$CI,na.rm=T) # 0.5049948 1.0745791
hist(comb_df$CI)
comb_df$ci <- comb_df$uci-comb_df$lci
range(comb_df$ci,na.rm=T) # 0.007000437 0.014896239
comb_df$ci <- comb_df$ci*100
hist(comb_df$ci)

### land
wmap<-readOGR(dsn="/Volumes/Doris/spt_static/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea
# mapp_df <- fortify(map("world2",plot=FALSE,fill=TRUE))

# ### pallete
# colorPalette <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#colour palette

p <- ggplot() +
  geom_tile(data = comb_df , aes(x = Longitude,y = Latitude,fill = ci)) + #CI
  scale_fill_gradientn(colors =rev(heat.colors(10)),na.value = "black",limits=c(0,1.5),oob=squish) + 
  coord_equal() +
  # geom_polygon(colour="black",size=0.25) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  # geom_polygon(data=map("world", wrap=c(0,360))) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_colorbar(title=expression(paste("Confident interval %")),
                               title.position = "top",
                               barwidth = unit(.4, "cm"),barheight = unit(3.5, "cm"),title.vjust =1)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = 'right',legend.direction = "vertical",
        legend.title = element_text(size = 10,angle = 90),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))

ggsave("global_confint_f1.png",width=8.27, height=3.44, dpi=300)

######### II. Confident interval map in f2 ###########
rm(list=ls())
setwd("/Volumes/Doris/Dynamic_spt")
load("/Volumes/Doris/Dynamic_spt/global_f2_resultTab.Rdata")
load("/Volumes/Doris/Dynamic_spt/global_chl_oc_df.Rdata")
results_tab <- as.data.frame(results_tab)

ind <- global_df$s.index
temp <- global_df[which(global_df$s.index==ind),c(1:3)]
temp <- temp[!duplicated(temp$s.index),]
colnames(temp) <- c('Site','Longitude','Latitude')
comb_df <- merge(temp,results_tab,by='Site',all.x=T,all.y=T)

# Length of CI
comb_df$CI <- comb_df$UCI-comb_df$LCI
range(comb_df$CI,na.rm=T)
hist(comb_df$CI)

### land
wmap <- readOGR(dsn="/Volumes/Doris/Projects/sptmodel/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea
# mapp_df <- fortify(map("world2",plot=FALSE,fill=TRUE))

# ### pallete
# colorPalette <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#colour palette

p <- ggplot() +
  geom_tile(data = comb_df , aes(x = Longitude,y = Latitude,fill = CI)) +
  scale_fill_gradientn(colors =rev(heat.colors(10)),na.value = "black",limits=c(0,0.5),oob=squish) + 
  coord_equal() +
  # geom_polygon(colour="black",size=0.25) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  # geom_polygon(data=map("world", wrap=c(0,360))) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_colorbar(title=expression(paste("Confident interval")),
                               title.position = "top",
                               barwidth = unit(.4, "cm"),barheight = unit(3.5, "cm"),title.vjust =1)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = 'right',legend.direction = "vertical",
        legend.title = element_text(size = 10,angle = 90),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))

ggsave("global_ci_f2.png",width=8.27, height=3.44, dpi=300)


######### why magnitude differenct between with/not deszn? ###########
load("/Volumes/Doris/Dynamic_spt/global_f4_resultTab.Rdata")
f4 <- as.data.frame(results_tab)
load("/Volumes/Doris/Dynamic_spt/global_f4_resultTab_deszn.Rdata")
f4_deszn <- as.data.frame(results_tab)

range(f4$Trend_Value,na.rm=T)
# -6.467199 18.104448
range(f4_deszn$Trend_Value,na.rm=T)
# -0.9494588 13.6591582

# CI value
f4_ci <- f4$UCI - f4$LCI
f4_deszn_ci <- f4_deszn$UCI - f4_deszn$LCI
hist(f4_ci)
range(f4_ci)
# 0.3253152 0.7131697
hist(f4_deszn_ci)
range(f4_deszn_ci)
# 0.03407684 0.38401024

hist(f4$Bias)
hist(f4_deszn$Bias)
