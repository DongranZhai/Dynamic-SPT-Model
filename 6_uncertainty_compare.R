############## Compare Longhurst and wc in boxplot of NRMSE, RMSE and Bias ###################
##### Notes: 
##### update: Aug.21.2023
#####      boxplot of Longhurst and wc side by side
#####        for both NRMSE, RMSE, bias, R^2
library(ggplot2)
library(grid)

############### Part I. Longhurst Regions ####################
##### Import Longhurst Data
rm(list=ls())
setwd("/Volumes/Doris/spt_static/uncertainty_compare")

load("/Volumes/Doris/spt_static/spt_long/results_table_longhurst.Rdata")
longTab <- as.data.frame(resultsTab)
rm("resultsTab")
longTab$newRegion <- as.factor(c(1:23))
colnames(longTab) <- c("ID","Trend_est","LCI","UCI","NRMSE","RMSE","Bias","newRegion") 

### A. NRMSE bar chart
p <- ggplot(longTab,aes(x=newRegion,y=NRMSE)) +
  geom_bar(position="dodge",stat="identity",fill = "#3498db")+
  coord_flip()+
  scale_fill_manual(values=c("slateblue","tan2"),)+
  ylab("NRMSE") +
  xlab("Longhurst Region") +
  theme_bw()+
  guides(fill=guide_legend(title=NULL))+
  theme(axis.ticks.length=unit(-0.1,"cm"),
        axis.text.x = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),
        text=element_text(family="Times New Roman",size=16))
savename <- paste0("long_bar_nrmse.png")
ggsave(savename,units="in", width=3,  height=8.27, pointsize=12,dpi =600)

### B. RMSE bar chart
p <- ggplot(longTab,aes(x=newRegion,y=RMSE)) +
  geom_bar(position="dodge",stat="identity",fill = "#3498db")+
  coord_flip()+
  scale_fill_manual(values=c("slateblue","tan2"),)+
  ylab("RMSE") +
  xlab("Longhurst Region") +
  theme_bw()+
  guides(fill=guide_legend(title=NULL))+
  theme(axis.ticks.length=unit(-0.1,"cm"),
        axis.text.x = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),
        text=element_text(family="Times New Roman",size=16))
savename <- paste0("long_bar_rmse.png")
ggsave(savename,units="in", width=3,  height=8.27, pointsize=12,dpi =600)

### C. Bias bar chart
p <- ggplot(longTab,aes(x=newRegion,y=Bias)) +
  geom_bar(position="dodge",stat="identity",fill = "#3498db")+
  coord_flip()+
  scale_fill_manual(values=c("slateblue","tan2"),)+
  ylab("Bias") +
  xlab("Longhurst Region") +
  theme_bw()+
  guides(fill=guide_legend(title=NULL))+
  theme(axis.ticks.length=unit(-0.1,"cm"),
        axis.text.x = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),
        text=element_text(family="Times New Roman",size=16))
savename <- paste0("long_bar_bias.png")
ggsave(savename,units="in", width=3,  height=8.27, pointsize=12,dpi =600)

### D. trends and uncertainty (violin)
trend <- array(data=NA,dim = c(4600000,1))
n = 1
for(i in 1:23){
  # trend_betap2_filter <- hampel(trend_betap2[,i],5000)
  trend[n:(n+199999),1] <- trend_betap2[,i]# trend_betap2_filter
  n <- n + 200000
}

region <- rep(c(1:23),each=200000)
daf <- cbind(trend,region)
daf <- as.data.frame(daf)
colnames(daf) <- c("Trend","Region")
daf$Region <- as.factor(daf$Region )


# daf <- as.data.frame(trend_betap2)
# colnames(daf) <- as.factor(c(1:23))

# daf <- as.data.frame(t(trend_betap2))
# rownames(daf) <- as.factor(c(1:23))


p <- ggplot(daf,aes(x=Region,y=Trend,fill=Region)) + 
  geom_violin(adjust=2,scale="width",fill = "#3498db",col = "#3498db")+ # 
  # geom_point(stat="identity",color = "#3498db") +
  # geom_errorbar(aes(ymin=LCI,ymax=UCI),width=.05,
  #               position=position_dodge(0.9),color = "#3498db") +
  coord_flip()+
  # scale_fill_manual(values="#3498db")+
  ylab(expression(paste("Trend  ", "(%yr"^" -1",")"))) +
  xlab("Regions") +
  scale_y_continuous(breaks=seq(-4.5,4.5,1.5),limits=c(-4.5,4.5),expand = c(0, 0)) +
  # scale_y_continuous(breaks=seq(-5,5,2.5),limits=c(-5,5)) +
  theme_bw()+
  guides(fill=guide_legend(title=NULL))+
  theme(axis.ticks.length=unit(-0.1,"cm"),
        axis.text.x = element_text(margin=unit(c(0.4,0.4,0.4,0.1), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.4,0.4,0.4,0.1), "cm")),
        text=element_text(family="Times New Roman",size=8),
        legend.position="none")
savename <- paste0("long_uncertainty_violin.1.png")
ggsave(savename,units="in",width=5, height=3, dpi=600)



### E. trends and uncertainty (violin)
rm(list=ls())
setwd("/Volumes/Doris/Projects/uncertainty_bias_plot")

load("/Volumes/Doris/Projects/spt_wc/spt_results/30wc/results_tables30.change.Rdata")
wcTab <- as.data.frame(resultsTab)
rm("resultsTab")
wcTab$newwc <- as.factor(c(1:30))

trend <- array(data=NA,dim = c(6000000,1))
n = 1
for(i in 1:30){
  # trend_betap2_filter <- hampel(trend_betap2[,i],5000)
  trend[n:(n+199999),1] <- trend_betap2[,i]# trend_betap2_filter
  n <- n + 200000
}

region <- rep(c(1:30) ,each=200000)
daf <- cbind(trend,region)
daf <- as.data.frame(daf)
colnames(daf) <- c("Trend" ,"Class")
daf$Class<- as.factor(daf$Class )


# daf <- as.data.frame(trend_betap2)
# colnames(daf) <- as.factor(c(1:23))

# daf <- as.data.frame(t(trend_betap2))
# rownames(daf) <- as.factor(c(1:23))


p <- ggplot(daf,aes(x=Class,y=Trend,fill=Class)) + 
  geom_violin(scale="width",fill = "#f39c12",col = "#f39c12")+ # adjust=2,
  # geom_point(stat="identity",color = "#3498db") +
  # geom_errorbar(aes(ymin=LCI,ymax=UCI),width=.05,
  #               position=position_dodge(0.9),color = "#3498db") +
  coord_flip()+
  # scale_fill_manual(values="#3498db")+
  ylab(expression(paste("Trend  ", "(%yr"^" -1",")"))) +
  xlab("Classes") +
  scale_y_continuous(breaks=seq(-4.5,4.5,1.5),limits=c(-4.5,4.5),expand = c(0, 0)) +
  # scale_y_continuous(breaks=seq(-5,5,2.5),limits=c(-5,5)) +# ,minor_breaks = seq(-2,2,1),
  theme_bw()+
  guides(fill=guide_legend(title=NULL))+
  theme(axis.ticks.length=unit(-0.1,"cm"),
        axis.text.x = element_text(margin=unit(c(0.4,0.4,0.4,0.1), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.4,0.4,0.4,0.1), "cm")),
        text=element_text(family="Times New Roman",size=8),
        legend.position="none")
savename <- paste0("wc_uncertainty_violin.1.png")
ggsave(savename,units="in",width=5, height=3, dpi=600)



############### Part III. Combined two approaches ####################
##### Import two data frame
rm(list=ls())
setwd("/Volumes/Doris/spt_static/uncertainty_compare")

load("/Volumes/Doris/spt_static/spt_long/results_table_longhurst.Rdata")
longTab <- as.data.frame(resultsTab)
rm("resultsTab")
longTab$ID <- as.factor(c(1:23))
# longTab <- longTab[,-1]
colnames(longTab) <- c("Group","Trend_est","LCI","UCI","NRMSE","RMSE","Bias","ID")

load("/Volumes/Doris/spt_static/spt_30oc/results_tables_30oc.Rdata")
wcTab <- as.data.frame(resultsTab)
rm("resultsTab")
wcTab$ID <- as.factor(c(1:30))
# wcTab <- wcTab[,-1]
colnames(wcTab) <- c("Group","Trend_est","LCI","UCI","NRMSE","RMSE","Bias","ID")

dfbar <- rbind(longTab,wcTab)
dfbar$method <- c(rep("Longhurst Regions",each=23),rep("Optical Classes",each=30))
dfbar$method <- as.factor(dfbar$method)

##### A. NRMSE frequency box plot
p <- ggplot(dfbar,aes(x=method,y=NRMSE)) +
  geom_boxplot(fill=c("#3498db","#f39c12")) +
  scale_fill_manual(values=c("#3498db","#f39c12"))+
  scale_x_discrete(limits=c("Longhurst Regions", "Optical Classes")) +
  ylab("NRMSE (%)") +
  xlab("") +
  guides(fill=guide_legend(title=NULL))+
  theme_bw()

savename <- paste0("box_nrmse.png")
ggsave(savename,units="in",width=5, height=3, dpi=600)
# ggsave(savename,units="in", width=3,  height=8.27, pointsize=12,dpi =600)

##### B. RMSE frequency box plot
p <- ggplot(dfbar,aes(x=method,y=RMSE)) +
  geom_boxplot(fill=c("#3498db","#f39c12")) +
  scale_fill_manual(values=c("#3498db","#f39c12"))+
  scale_x_discrete(limits=c("Longhurst Regions", "Optical Classes")) +
  ylab("RMSE") +
  xlab("") +
  guides(fill=guide_legend(title=NULL))+
  theme_bw()

savename <- paste0("box_rmse.png")
ggsave(savename,units="in",width=5, height=3, dpi=600)

##### C. Bias frequency box plot
p <- ggplot(dfbar,aes(x=method,y=Bias)) +
  geom_boxplot(fill=c("#3498db","#f39c12")) +
  scale_fill_manual(values=c("#3498db","#f39c12"))+
  scale_x_discrete(limits=c("Longhurst Regions", "Optical Classes")) +
  ylab("Bias") +
  xlab("") +
  guides(fill=guide_legend(title=NULL))+
  theme_bw()

savename <- paste0("box_bias.png")
ggsave(savename,units="in",width=5, height=3, dpi=600)

# update 04.22:wc29 with combined two water class
##### Import two data frame
rm(list=ls())

load("/Volumes/Doris/Projects/sptmodel/april.13/results_table_longhurst_yearly_filter.Rdata")
longTab <- as.data.frame(resultsTab)
rm("resultsTab")
longTab$new <- as.factor(c(1:23))
longTab <- longTab[,-1]
colnames(longTab) <- c("Trend_Value","LCI","UCI","NRMSE","RMSE","Bias","new")

load("/Volumes/Doris/Projects/spt_wc/spt_results/29wc/result_com.Rdata")
resultsTab <- result_com
rm("result_com")
wcTab <- as.data.frame(resultsTab)
rm("resultsTab")
wcTab$new <- as.factor(c(1:29))
wcTab <- wcTab[,-1]

dfbar <- rbind(longTab,wcTab)
dfbar$method <- c(rep("Longhurst",each=23),rep("Bio-optical water class",each=29))
dfbar$method <- as.factor(dfbar$method )

##### A. NRMSE frequency box plot
setwd("/Volumes/Doris/Projects/uncertainty_bias_plot/")
p <- ggplot(dfbar,aes(x=method,y=NRMSE,fill = method)) +
  geom_boxplot() + # fill=c("#3498db","#f39c12")
  scale_fill_manual(values=c("#f39c12","#3498db"))+
  scale_x_discrete(limits=c("Longhurst", "Bio-optical water class")) +
  ylab("NRMSE") +
  xlab("") +
  guides(fill=guide_legend(title=NULL))+
  theme_bw() +
  theme(legend.position="none")
savename <- paste0("box_nrmse.png")
ggsave(savename,units="in",width=5, height=3, dpi=600)
# ggsave(savename,units="in", width=3,  height=8.27, pointsize=12,dpi =600)

##### B. RMSE frequency box plot
p <- ggplot(dfbar,aes(x=method,y=RMSE,fill = method)) +
  geom_boxplot() + # fill=c("#3498db","#f39c12")
  scale_fill_manual(values=c("#f39c12","#3498db"))+
  scale_x_discrete(limits=c("Longhurst", "Bio-optical water class")) +
  ylab("RMSE") +
  xlab("") +
  guides(fill=guide_legend(title=NULL))+
  theme_bw() +
  theme(legend.position="none")
savename <- paste0("box_rmse.png")
ggsave(savename,units="in",width=5, height=3, dpi=600)

##### C. Bias frequency box plot
p <- ggplot(dfbar,aes(x=method,y=Bias,fill = method)) +
  geom_boxplot() + # fill=c("#3498db","#f39c12")
  scale_fill_manual(values=c("#f39c12","#3498db"))+
  scale_x_discrete(limits=c("Longhurst", "Bio-optical water class")) +
  ylab("Bias") +
  xlab("") +
  guides(fill=guide_legend(title=NULL))+
  theme_bw() +
  theme(legend.position="none")
savename <- paste0("box_bias.png")
ggsave(savename,units="in",width=5, height=3, dpi=600)




########################## draft ##############################
# plot <- ggplot(longTab ,aes(x=newRegion,y=NRMSE,fill=NRMSE))+
#   geom_boxplot()
# ggsave("long_box.png")
# ,width=8.5, height=3.44, dpi=300
#RMSE bar chart
dafbar <- rbind(ResultsTab_space,ResultsTab)
corr <- c(rep("STC",each=23),rep("TC",each=23))
dafbar <- cbind(dafbar,corr)
dafbar <- as.data.frame(dafbar)
dafbar$corr <- as.factor(dafbar$corr)
dafbar$NRMSE <- c(ResultsTab_space[,5],ResultsTab[,5])#weird error converting to data frame ledas to factorisation
dafbar$Region <- rep(c(1:23),times=2)
dafbar$Region <- as.factor(dafbar$Region)  

savename <- paste0("~/Ch1-scripts/Violin_bar.png")
png(savename, 
    type="cairo",
    units="in", 
    width=3, 
    height=8.27, 
    pointsize=12, 
    res=600)
p <- ggplot(dafbar,aes(x=Region,y=NRMSE,fill=corr))
p+geom_bar(position="dodge",stat="identity")+coord_flip()+
  scale_fill_manual(values=c("slateblue","tan2"))+ theme_bw()+
  guides(fill=guide_legend(title=NULL))+theme(axis.ticks.length=unit(-0.1,"cm"),axis.text.x = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")), axis.text.y = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),text=element_text(family="Times New Roman",size=16))
#guides(fill=guide_legend(title=NULL))+ggtitle("RMSE per region")+theme(axis.ticks.length=unit(-0.1,"cm"),axis.text.x = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")), axis.text.y = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),axis.line=element_line(size=0.72),text=element_text(size=10,family="Times New Roman"))
dev.off()









