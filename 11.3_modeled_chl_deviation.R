# May.06.2024
# CHL deviation: modeled from observed.
library(raster)
library(oceanmap)
library(ggplot2)
library(scales)
library(stats)

########## Part I. f2 model ##########
rm(list=ls())
setwd("/home/dzhai/spt_dynamic/f2_ocfix")

# # of time series on each class: 304*29
dev_tab <- array(NA,dim=c(1,6)) # 18240 = 304*29
colnames(dev_tab) <- c("index","time","obs","fitted","rmse","sd")
# dev_tab$Year <- rep(c(rep(1997,times=4),rep(c(1998:2022),each=12)),times=360*180)
# dev_tab$Month <- rep(c(c(9:12),rep(c(1:12),times=25)),times=360*180)
# dev_tab$Time <- rep(c(1:304),time=360*180)

for(i in c(1,2,301,302,303,304,305,306,4,501,502,503,504,601,602,603,7,8,901,902,903,
           1001,1002,1003,1004,1005,1006,1101,1102,1201,1202,13,14,15,1601,1602,1603,
           17,18,19,20,2101,2102,2103,2201,2202,23,2401,2402,2403,25,2601,2602,2603,
           27,2801,2802,2803,2804,29)){ # 60
  filename <- paste0(i,"_dynamic_f2_df.Rdata")
  load(filename)
  print(i)
  # # forget to remove exp() change
  # fitted <- log(fitted)
  # temp$fitted <- exp(fitted)
  temp_df <- temp[,c(1:3,6:10)]
  temp_df$fitted <- fitted
  regiontemp <- array(NA,dim=c(304,6))
  colnames(regiontemp) <- c("index","time","obs","fitted",'rmse',"sd")
  for(j in c(1:304)){
    tempoc <- i
    # obs chl
    tempobs <- temp_df$chl[which(temp_df$time == j)] # # of this class
    avrg_obs <- mean(tempobs,na.rm=T)
    
    # fitted chl
    tempfitted <- temp_df$fitted[which(temp_df$time == j)] # # of this class
    avrg_fitted <- mean(tempfitted,na.rm=T)
    
    # rmse
    rmse <- mean(sqrt((tempfitted-tempobs)^2),na.rm=T)/mean(tempobs,na.rm=T)
    sd <- sd(tempfitted,na.rm=T) # deviation of mean
    
    # df
    regiontemp[j,] <- c(tempoc,j,avrg_obs,avrg_fitted,rmse,sd)
    
  }
  dev_tab <- rbind(dev_tab,regiontemp)

}
dev_tab <- dev_tab[-1,]
save(dev_tab,file="f2_devTab.0508.Rdata")
# scp

########## Part II. f1 model ##########
rm(list=ls())
setwd("/home/dzhai/spt_dynamic/f1_ocfix")

# # of time series on each class: 304*29
dev_tab <- array(NA,dim=c(1,6)) # 18240 = 304*29
colnames(dev_tab) <- c("index","time","obs","fitted","rmse","sd")
# dev_tab$Year <- rep(c(rep(1997,times=4),rep(c(1998:2022),each=12)),times=360*180)
# dev_tab$Month <- rep(c(c(9:12),rep(c(1:12),times=25)),times=360*180)
# dev_tab$Time <- rep(c(1:304),time=360*180)

for(i in c(1,2,301,302,303,304,305,306,4,501,502,503,504,601,602,603,7,8,901,902,903,
           1001,1002,1003,1004,1005,1006,1101,1102,1201,1202,13,14,15,1601,1602,1603,
           17,18,19,20,2101,2102,2103,2201,2202,23,2401,2402,2403,25,2601,2602,2603,
           27,2801,2802,2803,2804,29)){ # 60
  filename <- paste0(i,"_dynamic_f1_df.Rdata")
  load(filename)
  print(i)
  # # forget to remove exp() change
  # fitted <- log(fitted)
  # temp$fitted <- exp(fitted)
  temp_df <- temp[,c(1:3,6:10)]
  temp_df$fitted <- fitted
  regiontemp <- array(NA,dim=c(304,6))
  colnames(regiontemp) <- c("index","time","obs","fitted","rmse","sd")
  for(j in c(1:304)){
    tempoc <- i
    # obs chl
    tempobs <- temp_df$chl[which(temp_df$time == j)] # # of this class
    avrg_obs <- mean(tempobs,na.rm=T)
    
    # fitted chl
    tempfitted <- temp_df$fitted[which(temp_df$time == j)] # # of this class
    avrg_fitted <- mean(tempfitted,na.rm=T)
    
    # rmse
    rmse <- mean(sqrt((tempfitted-tempobs)^2),na.rm=T)/mean(tempobs,na.rm=T)
    sd <- sd(tempfitted,na.rm=T) # deviation of mean
    
    # df
    regiontemp[j,] <- c(tempoc,j,avrg_obs,avrg_fitted,rmse,sd)
    
  }
  dev_tab <- rbind(dev_tab,regiontemp)
  
}
dev_tab <- dev_tab[-1,]
save(dev_tab,file="f1_devTab.0508.Rdata")

########## Part III. Make up models and classses ##########
### F2: DSTC
load("/Users/doris_zhai/Coding/Dynamic_SPT/dynamic/f2_devTab.0508.Rdata")
dev_tab <- as.data.frame(dev_tab)
f2_dev <- dev_tab
dev_tab$ocfix <- c(rep(c(1,2),each=304),rep(3,times=304*6),rep(4,each=304),
                   rep(5,times=304*4),rep(6,times=304*3),rep(c(7,8),each=304),
                   rep(9,times=304*3),rep(10,times=304*6),rep(11,times=304*2),
                   rep(12,times=304*2),rep(c(13,14,15),each=304),
                   rep(16,times=304*3),rep(c(17,18,19,20),each=304),
                   rep(21,times=304*3),rep(22,times=304*2),rep(23,each=304),
                   rep(24,times=304*3),rep(25,each=304),rep(26,times=304*3),
                   rep(27,each=304),rep(28,times=304*4),rep(29,each=304))

table(dev_tab$index)
table(dev_tab$ocfix)
#
subdev <- array(NA,dim=c(1,6))
colnames(subdev) <- c("time","obs","fitted","rmse","sd","ocfix")
for(i in c(3,5,6,9,10,11,12,16,21,22,24,26,28)){ # 13
  print(i)
  tempocfix <- dev_tab[which(dev_tab$ocfix == i),c(2:7)]
  
  subtemp <- array(NA,dim=c(304,6))
  colnames(subtemp) <- c("time","obs","fitted","rmse","sd","ocfix")
  for(j in c(1:304)){
    # obs
    tempobs <- mean(tempocfix$obs[which(tempocfix$time == j)])
    # fitted
    tempfitted <- mean(tempocfix$fitted[which(tempocfix$time == j)])
    # rmse
    temprmse <- mean(tempocfix$rmse[which(tempocfix$time == j)])
    # sd
    tempsd <- mean(tempocfix$sd[which(tempocfix$time == j)])
    
    subtemp[j,] <- c(j,tempobs,tempfitted,temprmse,tempsd,i)
    
  }
  subdev <- rbind(subdev,subtemp)
}
#
for(i in c(1,2,4,7,8,13,14,15,17,18,19,20,23,25,27,29)){ # 16
  print(i)
  tempdev_tab <- dev_tab[,2:7] %>%
    filter(ocfix == i)
  
  subdev <- rbind(subdev,tempdev_tab)
}
subdev <- subdev[-1,]

### F1: STC
load("/Users/doris_zhai/Coding/Dynamic_SPT/dynamic/f1_devTab.0508.Rdata")
dev_tab <- as.data.frame(dev_tab)
f1_dev <- dev_tab
dev_tab$ocfix <- c(rep(c(1,2),each=304),rep(3,times=304*6),rep(4,each=304),
                   rep(5,times=304*4),rep(6,times=304*3),rep(c(7,8),each=304),
                   rep(9,times=304*3),rep(10,times=304*6),rep(11,times=304*2),
                   rep(12,times=304*2),rep(c(13,14,15),each=304),
                   rep(16,times=304*3),rep(c(17,18,19,20),each=304),
                   rep(21,times=304*3),rep(22,times=304*2),rep(23,each=304),
                   rep(24,times=304*3),rep(25,each=304),rep(26,times=304*3),
                   rep(27,each=304),rep(28,times=304*4),rep(29,each=304))

table(dev_tab$index)
table(dev_tab$ocfix)
#
# subdev <- array(NA,dim=c(1,5))
# colnames(subdev) <- c("time","obs","fitted","rmse","ocfix")
for(i in c(3,5,6,9,10,11,12,16,21,22,24,26,28)){ # 13
  print(i)
  tempocfix <- dev_tab[which(dev_tab$ocfix == i),c(2:7)]
  
  subtemp <- array(NA,dim=c(304,6))
  colnames(subtemp) <- c("time","obs","fitted","rmse","sd","ocfix")
  for(j in c(1:304)){
    # obs
    tempobs <- mean(tempocfix$obs[which(tempocfix$time == j)])
    # fitted
    tempfitted <- mean(tempocfix$fitted[which(tempocfix$time == j)])
    # rmse
    temprmse <- mean(tempocfix$rmse[which(tempocfix$time == j)])
    # sd
    tempsd <- mean(tempocfix$sd[which(tempocfix$time == j)])
    
    subtemp[j,] <- c(j,tempobs,tempfitted,temprmse,tempsd,i)
    
  }
  subdev <- rbind(subdev,subtemp)
}
#
for(i in c(1,2,4,7,8,13,14,15,17,18,19,20,23,25,27,29)){ # 16
  print(i)
  tempdev_tab <- dev_tab[,2:7] %>%
    filter(ocfix == i)
  
  subdev <- rbind(subdev,tempdev_tab)
}
subdev$group <- rep(c("DSTC","STC"), each=304*29)
dev_all <- subdev
save(f2_dev,f1_dev,dev_all,file="modeled_chl_deviations.0508.Rdata")

########## Part IV. Plotting on ocfix according to OC ##########
# plotting in 2.4_ts_plotting script (not good)
# plotting in 6_diagnose_plotting for boxplot




################
# gsoc
rm(list=ls())
# Import gswc data dataframe and transform to matrix
load("/home/dzhai/spt_dynamic/gsoc30_df.Rdata")
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
so_ind <- which(oc==30)
oc[so_ind] <- NA
table(oc)
area <- length(table(oc))

# dev_tab$ocfix <- c(rep(as.character(c(1,2,4,7,8,13,14,15,17,18,19,20,23,25,27,29)),each=304),
#                    as.character(rep(3,times=304*6)),as.character(rep(5,times=304*4)),as.character(rep(6,times=304*3)),
#                    as.character(rep(9,times=304*3)),as.character(rep(10,times=304*6)),as.character(rep(11,times=304*2)),
#                    as.character(rep(12,times=304*2)),as.character(rep(16,times=304*3)),as.character(rep(21,times=304*3)),
#                    as.character(rep(22,times=304*2)),as.character(rep(24,times=304*3)),as.character(rep(26,times=304*3)),
#                    as.character(rep(28,times=304*4)))

# 1    2    4    7    8   13   14   15   17   18   19   20   23   25   27   29  301  302  303  304 
# 304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304 
# 305  306  501  502  503  504  601  602  603  901  902  903 1001 1002 1003 1004 1005 1006 1101 1102 
# 304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304 
# 1201 1202 1601 1602 1603 2101 2102 2103 2201 2202 2401 2402 2403 2601 2602 2603 2801 2802 2803 2804 
# 304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304 
