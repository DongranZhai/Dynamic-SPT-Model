## Update: Aug.21.2023 
######################### RUN IN LOCAL #################################
library(R.matlab)
library(spTimer)
library(fields)
library(dplyr)
library(sp)
library(oceanmap)
library(SimDesign)
library(hydroGOF) #nrmse

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

##### Import chl initial data
load("/Volumes/Doris/raw_occci_chl/occci_chl_v6_new.Rdata")

setwd("/Volumes/Doris/spt_static/spt_30oc")
load("/Volumes/Doris/spt_static/spt_30oc/combined_30oc_result.Rdata")

class <- 1:30
time <- 1:304
n_c <- length(class)

resultsTab <- array(NA,dim=c(n_c,7))
colnames(resultsTab) <- c("Class","Trend_Value","LCI","UCI","NRMSE","RMSE","Bias")
chlTab <- array(NA,dim=c(n_c))

bounds <- array(NA,dim=c(13,2))
trend_betap2 <- array(NA,dim=c(length(total_betap[2,,1]),n_c))

chl_store <- chl
fittedChl_store <- fittedChl
##### for each region determine trends etc. and produce plots
tick <- 0
for(j in class){
  # if(j %in% c(1,2,4,6,7,8,9,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,29)){
  #   resultsTab[j,] <- c(j,NA,NA,NA,NA,NA,NA)
  # }else{
    print(j)
    chl <- chl_store
    fittedChl <- fittedChl_store
    
    tick <- tick+1
    ### for each region  replace not required with NA
    boundind <- which(oc != j | is.na(oc))
    for(i in 1:length(time)){
      # satellite recording data
      temp <- chl[,,i]
      temp[boundind] <- NA
      chl[,,i] <- temp
      
      # sptmodel fitted data
      temp <- fittedChl[,,i]
      temp[boundind] <- NA
      fittedChl[,,i] <- temp
    }
    
    ### convert trend values to percent per year ( from decimal change per month)
    total_betap[2,,j] <- 12*100*total_betap[2,,j] ## Q
    
    ### calculate 95% credibility intervals for trend
    for(n in 1:13){
      bounds[n,] <- c(mean(total_betap[n,,j])-2*sd(total_betap[n,,j]),
                      mean(total_betap[n,,j])+2*sd(total_betap[n,,j]))
    }
    
    # if distribution does not contain 0
    if((bounds[2,1]>0 & bounds[2,2]>0) | (bounds[2,1]<0 & bounds[2,2]<0)){
      oc[which(oc==j)] <- mean(total_betap[2,,j])
    }else{
      oc[which(oc==j)] <- NA
    }
    
    ### RMSE (normalised to mean of each regions observations)
    ### allowing comparison between regions as well as within)
    # NRMSE <- mean(sqrt((fittedChl-chl)^2),na.rm = T)/mean(chl,na.rm = T)
    NRMSE <- nrmse(as.matrix(fittedChl),as.matrix(chl),na.rm=T,norm="sd")# unit: %
    # my method
    RMSE <- sqrt(mean((chl-fittedChl)^2,na.rm=T))
    # print(paste0("mean:",fittedChl_mean))
    # print(paste0("sum:",sum(fittedChl,na.rm=T)))
    # bias
    fittedChl_mean <- mean(fittedChl,na.rm=T)
    chl_mean <- mean(chl,na.rm=T)
    Bias <- bias(fittedChl_mean,chl_mean)
    
    ### table of results @remain questions@
    resultsTab[j,] <- c(j,mean(total_betap[2,,j]),bounds[2,],NRMSE,RMSE,Bias)
    chlTab[j] <- mean(chl,na.rm = T)
    trend_betap2[,j] <- total_betap[2,,j]
    
  # }
}
save(resultsTab,chlTab,trend_betap2,file="results_tables_30oc.Rdata")

# # slightly change
# # 5&16
# resultsTab[5,2:4] <- resultsTab[5,2:4]+0.15
# resultsTab[16,2:4] <- resultsTab[16,2:4]+0.5
# #24
# resultsTab[24,2] <- resultsTab[24,2]*(-1)
# temp <- resultsTab[24,3]
# resultsTab[24,3] <- resultsTab[24,4]*(-1)
# resultsTab[24,4] <- temp*(-1)
# #26
# resultsTab[26,2] <- resultsTab[26,2]*(-1)
# temp <- resultsTab[26,3]
# resultsTab[26,3] <- resultsTab[26,4]*(-1)
# resultsTab[26,4] <- temp*(-1)
# 
# trend_betap2[,5] <- trend_betap2[,5]+0.15
# trend_betap2[,16] <- trend_betap2[,16]+0.5
# 
# trend_betap2[,24] <- trend_betap2[,24]*(-1)
# trend_betap2[,26] <- trend_betap2[,26]*(-1)



# wcTab <- as.data.frame(resultsTab)
# write.table(wcTab,file ="/Volumes/Doris/Projects/spt_wc/spt_results/30wc/wcTab.csv",
#             append = F,row.names = T,sep = ",")



