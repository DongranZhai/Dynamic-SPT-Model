### April.25: 
#         I. Group the trend estimates from F1 and F2 (the results from dynamic SPT model)
#         according to fixed optical classes (30 groups).

rm(list=ls())
load("~/Coding/Dynamic_SPT/dynamic/global_chl_oc_df.Rdata")
# use global_no_na
site_ind <- global_no_na[,c(1:3,9,10)]
site_ind <- site_ind[!duplicated(site_ind), ]
colnames(site_ind) <- c("Site","Longitude","Latitude","OCfix","Longhurst")

##### I. Merge trends and optical class groups #####
# F1 model
load("~/Coding/Dynamic_SPT/dynamic/dynamic_f1_resultTab_all.Rdata")
results_tab <- as.data.frame(results_tab)
f1_tr_df <- merge(site_ind,results_tab,by="Site")

# F2 model
load("~/Coding/Dynamic_SPT/dynamic/dynamic_f2_resultTab_all.Rdata")
results_tab <- as.data.frame(results_tab)
f2_tr_df <- merge(site_ind,results_tab,by="Site")

save(f1_tr_df,f2_tr_df,file=paste0("esTrends_2models_df.Rdata"))


##### II. Group trend estimates according to ocean basin #####
rm(list=ls())
# load("~/Coding/Dynamic_SPT/dynamic/global_chl_oc_df.Rdata")
load("/Users/doris_zhai/Coding/Dynamic_SPT/dynamic/esTrends_2models_df.Rdata")
load("/Users/doris_zhai/Coding/Dynamic_SPT/dynamic/gsoc30_df.Rdata")

basin_ind <- wc_result[,c(1,2,4)]
temp <- trunc(basin_ind[,c(1,2)])
basin_ind <- cbind(temp,basin_ind[,3])
colnames(basin_ind) <- c("Longitude","Latitude","Basin")
# f2
temp <- trunc(f2_tr_df[,c(2,3)])
f2_tr_df[,c(2,3)] <- temp

f2_tr_df$Basin <- NA
for(i in 1:nrow(f2_tr_df)){
  tempind <- basin_ind$Basin[which(basin_ind$Longitude == f2_tr_df$Longitude[i] & basin_ind$Latitude == f2_tr_df$Latitude[i])]
  f2_tr_df$Basin[i] <- tempind
}

# f1
temp <- trunc(f1_tr_df[,c(2,3)])
f1_tr_df[,c(2,3)] <- temp

f1_tr_df$Basin <- NA
for(i in 1:nrow(f1_tr_df)){
  tempind <- basin_ind$Basin[which(basin_ind$Longitude == f1_tr_df$Longitude[i] & basin_ind$Latitude == f1_tr_df$Latitude[i])]
  f1_tr_df$Basin[i] <- tempind
}
save(f1_tr_df,f2_tr_df,file=paste0("esTrends_2models_df.0508.Rdata"))
# this version with ocean basin label in data frame


##################################################
##### Merge gsoc
load("~/Coding/Dynamic_SPT/dynamic/global_chl_oc_df.Rdata")
# use global_no_na
site_ind <- global_no_na[,c(1:3,9,10)]
site_ind <- site_ind[!duplicated(site_ind), ]
colnames(site_ind) <- c("Site","Longitude","Latitude","OCfix","Longhurst")

# merge(basin_ind,f2_tr_df,by="Site")
tempind <- f2_tr_df[i,c(2,3)]
which(basin_ind$Longitude == tempind$Longitude )
# Not solved yet
basin_tr_df <- merge(basin_ind,f2_tr_df,by = c("Longitude","Latitude"))
basin_tr_df <- left_join(f2_tr_df,basin_ind,by = c("Longitude","Latitude"))
all_tr_df <- rbind(f2_tr_df,f1_tr_df)
tempf2 <- basin_ind %>%
  filter(basin_ind[,c(1,2)] %in% f2_tr_df[,c(2,3)])
