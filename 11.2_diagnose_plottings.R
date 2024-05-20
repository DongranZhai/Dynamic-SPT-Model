### April.25
#          Diagnose plotting
# Raincloud plot: Statistical matrix for significant difference comparison between two models (formulas)
library(tidyverse)
library(gghalves)
library(ggplot2)
library(scales)
library(rstatix) # provides pipe-friendly R functions for easy statistical analyses
library(ggprism)
library(ggpubr) # for creating easily publication ready plots
library(patchwork) # organize ggplot figures
library(ggpmisc)

setwd("~/Coding/Dynamic_SPT/dynamic")
##########  Part I. significant difference comparison ##########
load("~/Coding/Dynamic_SPT/dynamic/esTrends_2models_df.Rdata")
results_df <- rbind(f2_tr_df,f1_tr_df)
ndata <- nrow(f1_tr_df)
labeltemp <- rep(c("DSTC","STC"),each=ndata)
results_df$Label <- labeltemp

##### trend plot
groupcolors <- c("lightslateblue","goldenrod1") # pallette
range(results_df$Trend_Value,na.rm = T)

# whole
p <- ggplot(data = results_df,aes(x=Label, y=Trend_Value, fill=Label, group=Label)) +
      geom_violin(color=NA, alpha=0.35) +
      geom_boxplot(errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
      scale_fill_manual(values = groupcolors) +
      scale_y_continuous(limits = c(-2.6, 2), expand = c(0, 0)) +
      scale_x_discrete(labels = c('DSTC','STC')) +
      labs(y=expression(paste("Trend  ", "(%·yr"^" -1",")")),x=NULL) +
      theme_classic() +
      theme(legend.position = "bottom",
            axis.title = element_text(size = 16,face="bold",color = "black"),
            axis.text = element_text(size=13,face="bold",color = "black"))
p
ggsave("2models_compare_trends.1.png",dpi = 300, height = 5, width = 5, unit = 'in') # landscape

# half
p <- ggplot(data = results_df,aes(x=Label, y=Trend_Value, fill=Label, group=Label)) +
  geom_half_violin(side = "r", color=NA, alpha=0.35) +
  geom_half_boxplot(side = "r",errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  geom_half_point_panel(side = "l", shape=21, size=2, color="white") +
  scale_fill_manual(values = groupcolors) +
  scale_y_continuous(limits = c(-2.6, 2), expand = c(0, 0)) +
  scale_x_discrete(labels = c('STC','DSTC')) +
  labs(y=expression(paste("Trend  ", "(%yr"^" -1",")")),x=NULL) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16, color = "black"),
        axis.text = element_text(size=13, color = "black"))
ggsave("2models_compare_trends.half.png",dpi = 300, height = 5, width = 5, unit = 'in') # landscape


##### RMSE
groupcolors<-c("#66c5b8", "#f6db2b") # pallette
range(results_df$RMSE,na.rm = T)

# p <- ggplot(data = results_df,aes(x=Label, y=RMSE, fill=Label, group=RMSE)) +
#   geom_bar(width=0.6,position="dodge",stat="identity") +
#   scale_fill_manual(values = groupcolors) +
#   scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
#   scale_x_discrete(labels = c('STC','DSTC')) +
#   labs(y="RMSE",x=NULL) +
#   theme_classic() +
#   theme(legend.position = "bottom",
#         axis.title = element_text(size = 16, color = "black"),
#         axis.text = element_text(size=13, color = "black"))
# ggsave("2models_compare_rmse.png",dpi = 300, height = 5, width = 5, unit = 'in') # landscape

p <- ggplot(data = stat.test,aes(x=Label, y=RMSE, fill=Label, group=Label)) +
  geom_boxplot(outlier.shape = NA) + #position="dodge",stat="identity",width=0.6
  stat_summary(fun.y = mean, color = "#ee5253", position = position_dodge(0.75),
               geom = "point", shape = 8, size = 3,show.legend = FALSE)+
  scale_fill_manual(values = groupcolors) +
  scale_y_continuous(limits = c(0, 0.1), expand = c(0, 0)) +
  scale_x_discrete(labels = c('STC','DSTC')) +
  labs(y="RMSE",x=NULL) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16, color = "black"),
        axis.text = element_text(size=13, color = "black"))
ggsave("2models_compare_rmse.png",dpi = 300, height = 5, width = 5, unit = 'in') # landscape

# NRMSE
groupcolors<-c("#66c5b8", "#f6db2b") # pallette
range(results_df$NRMSE,na.rm = T)

stat.test <- results_df %>%
  group_by(OCfix)

p <- ggplot(data = stat.test,aes(x=Label, y=NRMSE, fill=Label, group=Label)) +
  geom_boxplot(outlier.shape = NA) + #position="dodge",stat="identity",width=0.6
  stat_summary(fun.y = mean, color = "#ee5253", position = position_dodge(0.75),
               geom = "point", shape = 8, size = 3,show.legend = FALSE)+
  scale_fill_manual(values = groupcolors) +
  scale_y_continuous(limits = c(0, 0.25), expand = c(0, 0)) +
  scale_x_discrete(labels = c('STC','DSTC')) +
  labs(y="NRMSE",x=NULL) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16, color = "black"),
        axis.text = element_text(size=13, color = "black"))
ggsave("2models_compare_nrmse.png",dpi = 300, height = 5, width = 5, unit = 'in') # landscape
  
################################################################################
##### Part II. Distribution of parameters in each optical classes in STC and DSTC ##########
load("~/Coding/Dynamic_SPT/dynamic/esTrends_2models_df.Rdata")
results_df <- rbind(f2_tr_df,f1_tr_df)
ndata <- nrow(f1_tr_df)
labeltemp <- rep(c("DSTC","STC"),each=ndata)
results_df$Label <- labeltemp
# results_df$Label <- as.factor(results_df$Label)
results_df$OCfix <- as.factor(results_df$OCfix)
results_df <- na.omit(results_df)

groupcolors <- c("lightslateblue","goldenrod1") # pallette
# groupcolors <- c("#024378","#de7b34") # pallette
##### CI (30 optical class)
results_df$ci <- results_df$uci-results_df$lci
range(results_df$ci,na.rm=T)
results_df$CI <- results_df$UCI-results_df$LCI
range(results_df$CI,na.rm=T)

### With significant p-value
# OR (BETTER!)
# Facet bar ****
stat.test <- results_df %>%
  group_by(OCfix) %>%
  t_test(ci ~ Label) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test <- stat.test %>% 
  add_xy_position(x = "Label",dodge = 0.8)

p <- ggplot(data = results_df,aes(x=Label,y=ci,color=Label,group=Label)) + # x="OCfix",y="ci",color="Label",palette = groupcolors
  geom_bar(width=0.6,position="dodge",stat="identity",aes(fill=Label)) + #,add = "mean_sd",
  scale_color_manual(values = groupcolors) + # c("DSTC"="lightslateblue","STC"="goldenrod1")
  scale_fill_manual(values = groupcolors) +
  scale_y_continuous(limits = c(0,0.19), expand = c(0, 0)) +
  scale_x_discrete(labels = c("DSTC","STC")) +
  labs(y=expression(paste("95% Credibility intervals")),x=NULL) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.y = element_text(size=30, face="bold", colour = "black"),
        axis.line.x = element_line(color="black", linewidth = 0.3),
        axis.line.y = element_line(color="black", linewidth = 0.3),
        axis.text = element_text(face="bold",size=16, color = "black"),
        strip.text = element_text(face="bold", size=18,lineheight=5.0),
        strip.background = element_rect(fill="#C9C5C4", colour="black",linewidth=1))+
  facet_wrap(~OCfix)
p <- p+stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0,bracket.nudge.y = -0.015,
                          bracket.size = 0.3,size = 10,linetype = 1) #
p
ggsave("30oc_ci_facet_barsig.1.png",dpi = 300, height = 20, width = 20, unit = 'in') # landscape

# # Barplot
# stat.test <- results_df %>%
#   group_by(OCfix) %>%
#   t_test(ci ~ Label) %>%
#   adjust_pvalue(method = "bonferroni") %>%
#   add_significance("p.adj")
# stat.test <- stat.test %>% 
#   add_xy_position(x = "OCfix",dodge = 0.8)
# # one
# p <- ggplot(data = results_df,aes(x=OCfix,y=ci,color=Label)) + # x="OCfix",y="ci",color="Label",palette = groupcolors
#   geom_bar(width=0.6,position="dodge",stat="identity",aes(fill=Label)) +
#   scale_color_manual(values = groupcolors) +
#   scale_fill_manual(values = groupcolors) +
#   coord_flip() +
#   scale_y_continuous(limits = c(0,0.15), expand = c(0, 0)) +
#   scale_x_discrete(labels = c(c(1:29))) +
#   labs(y=expression(paste("Confidence Intervals")),x=NULL) +
#   theme_classic() +
#   theme(legend.position = "bottom",
#         axis.title = element_text(size = 16, color = "black"),
#         axis.text = element_text(size=13, color = "black"))
# 
# p <- p+stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01) # 
# ggsave("30oc_ci_barsig.png",dpi = 300, height = 20, width = 15, unit = 'in') # landscape

# # two
# p <- ggplot(data = results_df,aes(x=OCfix, y=ci,fill=Label)) +
#   # geom_bar(errorbar.draw = F, width=1, linewidth=0.5,position="dodge",stat="identity",outlier.shape = NA) +
#   geom_bar(width=0.6,position="dodge",stat="identity") +
#   scale_fill_manual(values = groupcolors) +
#   coord_flip() +
#   scale_y_continuous(limits = c(0,0.15), expand = c(0, 0)) +
#   scale_x_discrete(labels = c(c(1:29))) +
#   labs(y=expression(paste("Confidence Intervals")),x=NULL) +
#   theme_classic() +
#   theme(legend.position = "bottom",
#         axis.title = element_text(size = 16, color = "black"),
#         axis.text = element_text(size=13, color = "black"))
# ggsave("30oc_cicompare_models.png",dpi = 300, height = 20, width = 10, unit = 'in') # landscape


# Facet boxplot
p <- ggplot(data = results_df,aes(x=Label,y=ci,group=Label)) + # x="OCfix",y="ci",color="Label",palette = groupcolors
  geom_boxplot(position="dodge",aes(fill=Label)) + #,add = "mean_sd",,scale="width"
  scale_fill_manual(values = groupcolors) +
  scale_y_continuous(limits = c(0,0.15), expand = c(0, 0)) +
  scale_x_discrete(labels = c("STC","F2")) +
  labs(y=expression(paste("Confidence Intervals")),x=NULL) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16, color = "black"),
        axis.text = element_text(size=13, color = "black"))+
  facet_wrap(~OCfix)
p <- p+stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0,
                          bracket.size = 0.3,size = 10,linetype = 1) #
ggsave("30oc_ci_facet_boxsig.png",dpi = 300, height = 20, width = 20, unit = 'in') # landscape

#####
# trends
results_df_group <- results_df %>%
  group_by(OCfix)

range(results_df$Trend_Value,na.rm = T)
p <- ggplot(data = results_df_group,aes(x=OCfix, y=Trend_Value,fill=Label)) + #,color=Label, group=OCfix
  geom_boxplot(position="dodge",scale="width") + # color=NA
  # geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  # geom_half_point_panel(side = "l", shape=21, size=2, color="white") +
  scale_fill_manual(values = groupcolors) +
  coord_flip() +
  scale_y_continuous(limits = c(-2.5, 2.5), expand = c(0, 0)) +
  scale_x_discrete(labels = c(c(1:29))) +
  labs(y=expression(paste("Trend ", "(%yr"^" -1",")")),x=paste0("Optical classes")) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16, color = "black"),
        axis.text = element_text(size=13, color = "black"))
ggsave("30oc_trends_models.png",dpi = 300, height = 15, width = 12, unit = 'in') # landscape

# NRMSE
# range(results_df$NRMSE,na.rm = T)
# p <- ggplot(data = results_df,aes(x=OCfix, y=NRMSE,fill=Label)) + #,color=Label, group=OCfix
#   # geom_half_violin(side = "r",alpha=0.8,position="dodge",scale="width") + # color=NA
#   geom_half_boxplot(errorbar.draw = F, width=1, linewidth=0.5,position="dodge",outlier.shape = NA) +
#   # geom_half_point_panel(side = "l", shape=21, size=2, color="white") +
#   scale_fill_manual(values = groupcolors) +
#   coord_flip() +
#   scale_y_continuous(limits = c(0,0.4), expand = c(0, 0)) +
#   scale_x_discrete(labels = c(c(1:29))) +
#   labs(y=expression(paste("NRMSE")),x=NULL) +
#   theme_classic() +
#   theme(legend.position = "bottom",
#         axis.title = element_text(size = 16, color = "black"),
#         axis.text = element_text(size=13, color = "black"))

# p <- ggplot(data = results_df_group,aes(x=OCfix, y=NRMSE,fill=Label)) +
#   # geom_bar(errorbar.draw = F, width=1, linewidth=0.5,position="dodge",stat="identity",outlier.shape = NA) +
#   geom_bar(width=0.6,position="dodge",stat="identity") +
#   scale_fill_manual(values = groupcolors) +
#   coord_flip() +
#   scale_y_continuous(limits = c(0,0.75), expand = c(0, 0)) +
#   scale_x_discrete(labels = c(c(1:29))) +
#   labs(y=expression(paste("NRMSE")),x=NULL) +
#   theme_classic() +
#   theme(legend.position = "bottom",
#         axis.title = element_text(size = 16, color = "black"),
#         axis.text = element_text(size=13, color = "black"))
# ggsave("30oc_nrmse_models.png",dpi = 300, height = 20, width = 10, unit = 'in') # landscape
# 
# # Bias
# range(results_df$Bias,na.rm = T)
# p <- ggplot(data = results_df_group,aes(x=OCfix, y=Bias,fill=Label)) +
#   # geom_bar(errorbar.draw = F, width=1, linewidth=0.5,position="dodge",stat="identity",outlier.shape = NA) +
#   geom_bar(width=0.6,position="dodge",stat="identity") +
#   scale_fill_manual(values = groupcolors) +
#   coord_flip() +
#   scale_y_continuous(limits = c(-0.25,0.15), expand = c(0, 0)) +
#   scale_x_discrete(labels = c(c(1:29))) +
#   labs(y=expression(paste("Bias")),x=NULL) +
#   theme_classic() +
#   theme(legend.position = "bottom",
#         axis.title = element_text(size = 16, color = "black"),
#         axis.text = element_text(size=13, color = "black"))
# ggsave("30oc_bias_models.png",dpi = 300, height = 20, width = 10, unit = 'in') # landscape

################################################################################
########## Part III. Fitted VS Obs (Spearman) ##########
load("~/Coding/Dynamic_SPT/dynamic/esTrends_2models_df.Rdata")
results_df <- rbind(f2_tr_df,f1_tr_df)
ndata <- nrow(f1_tr_df)
labeltemp <- rep(c("DSTC","STC"),each=ndata)
results_df$Label <- labeltemp
# results_df$Label <- as.factor(results_df$Label)
results_df$OCfix <- as.factor(results_df$OCfix)
results_df <- na.omit(results_df)

##### Obs CHL vs Fitted CHL
### two
range(results_df$Fitted)
groupcolors <- c("lightslateblue","goldenrod1") 

p <- ggplot(results_df,aes(x=Obs,y=Fitted,group=Label,color=Label)) +
  geom_point(alpha=0.8)+
  geom_abline(intercept=0, slope=1,color="#C9C5C4",linewidth=1.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values=groupcolors) +
  labs(y=expression(paste("Fitted CHL ", "(mg·m"^"-3",")")),
       x=expression(paste("Observed CHL ", "(mg·m"^"-3",")"))) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_text(size=20, face="bold", colour = "black"),
        axis.line.x = element_line(color="black", linewidth = 0.5),
        axis.line.y = element_line(color="black", linewidth = 0.5),
        axis.text = element_text(face="bold",size=18, color = "black"),
        strip.text = element_text(face="bold", size=18,lineheight=5.0),
        strip.background = element_rect(fill="#C9C5C4", colour="black",linewidth=1))+
  facet_wrap(~Label)
p
ggsave("fitted_obs_models_facet.1.png",dpi = 300, height =10, width = 15, unit = 'in') # landscape

# ### one
# f1_df <- results_df %>%
#   filter(Label == "STC")
# f2_df <- results_df %>%
#   filter(Label == "DSTC")
# 
# p1 <- ggplot(f1_df,aes(x=Obs,y=Fitted)) +
#   geom_point(color="#66c5b8",alpha=0.8)+
#   geom_abline(intercept=0, slope=1,color="#C9C5C4",linewidth=1.5) +
#   labs(y=expression(paste("Fitted CHL ", "(mg·m"^"-3",")")),
#        x=expression(paste("Observed CHL ", "(mg·m"^"-3",")"))) +
#   theme_classic() +
#   theme(axis.title = element_text(size = 16, color = "black"),
#         axis.text = element_text(size=13, color = "black"))
# 
# p2 <- ggplot(f2_df,aes(x=Obs,y=Fitted)) +
#   geom_point(color="#f6db2b",alpha=0.8)+
#   geom_abline(intercept=0, slope=1,color="#C9C5C4",linewidth=1.5) +
#   labs(y=expression(paste("Fitted CHL ", "(mg·m"^"-3",")")),
#        x=expression(paste("Observed CHL ", "(mg·m"^"-3",")"))) +
#   theme_classic() +
#   theme(axis.title = element_text(size = 16, color = "black"),
#         axis.text = element_text(size=13, color = "black"))
# p1+p2
# ggsave("fitted_obs_models.png",dpi = 300, height = 20, width = 30, unit = 'in') # landscape

# spearman test
spear.test.f1 <- cor.test(f1_df$Fitted,f1_df$Obs,method="spearman")
spear.test.f2 <- cor.test(f2_df$Fitted,f2_df$Obs,method="spearman")

spear.test.f1$estimate # rho: 0.943129
spear.test.f1$estimate # rho: 0.9449175
# There are strong positive monotonic relationship between fitted and obs CHl. 
# F2 performs a better perfect poitive monotonic.

##### Obs CHL vs trend estimates
## one
f1_df <- results_df %>%
  filter(Label == "STC")
f2_df <- results_df %>%
  filter(Label == "F2")

p1 <- ggplot(f1_df,aes(x=Obs,y=Trend_Value)) +
  geom_point(color="#66c5b8",alpha=0.8)+
  stat_smooth(method='loess', formula= y~x,color="#FE6500")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(y=expression(paste("Trend ", "(%yr"^" -1",")")),
       x=expression(paste("Observed CHL ", "(mg·m"^"-3",")"))) +
  theme_classic() +
  theme(axis.title = element_text(size = 16, color = "black"),
        axis.text = element_text(size=13, color = "black"))
p1
p2 <- ggplot(f2_df,aes(x=Obs,y=Trend_Value)) +
  geom_point(color="#f6db2b",alpha=0.8)+
  stat_smooth(method='loess', formula= y~x,color="#FE6500")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(y=expression(paste("Trend ", "(%yr"^" -1",")")),
       x=expression(paste("Observed CHL ", "(mg·m"^"-3",")"))) +
  theme_classic() +
  theme(axis.title = element_text(size = 16, color = "black"),
        axis.text = element_text(size=13, color = "black"))
p2
p1+p2
ggsave("trend_obs_models.png",dpi = 300, height = 20, width = 30, unit = 'in') # landscape

## two (facet)
groupcolors<-c("#2e4ba0", "#faa419") # pallette: blue and yellow

p <- ggplot(results_df,aes(x=Obs,y=Trend_Value,group=Label,color=Label)) +
  geom_point(alpha=0.8)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values=groupcolors) +
  labs(y=expression(paste("Trend ", "(%yr"^" -1",")")),
       x=expression(paste("Observed CHL ", "(mg·m"^"-3",")"))) +
  theme_classic() +
  theme(axis.title = element_text(size = 16, color = "black"),
        axis.text = element_text(size=13, color = "black"),
        legend.position = "none") +
  facet_wrap(~Label)
p
ggsave("trend_obs_models_facet.png",dpi = 300, height = 20, width = 20, unit = 'in') # landscape


# spearman test
spear.test.f1 <- cor.test(f1_df$Trend_Value,f1_df$Obs,method="spearman")
spear.test.f2 <- cor.test(f2_df$Trend_Value,f2_df$Obs,method="spearman")
spear.test.f1$estimate # rho: 0.5905171
spear.test.f1$estimate # rho: 0.6898162
# There are strong positive monotonic relationship between trend estimates and obs CHl. 
# F2 performs a better perfect poitive monotonic.


##### trend estimated and obs CHL (R^2)
rsq <- function (x, y) cor(x, y) ^ 2

R2_f1 <- rsq(f1_df$Trend_Value,f1_df$Obs) # 0.3574554
R2_f2 <- rsq(f2_df$Trend_Value,f2_df$Obs) # 0.3972438
# Larger R2 indicates that the model better explains all of the variability in the dependent variable.

##### Part III. Plotting on ocfix according to OC (boxplot) #####
rm(list=ls())
setwd("~/Coding/Dynamic_SPT/dynamic")
load("/Users/doris_zhai/Coding/Dynamic_SPT/dynamic/modeled_chl_deviations.0508.Rdata")

Date <- seq(from = as.Date("1997/09/01",format="%Y/%m/%d"), 
            to = as.Date("2022/12/01",format="%Y/%m/%d"), 
            by = "1 month")
dev_all$date <- rep(Date,times=29)
dev_all$ocfix <- as.character(dev_all$ocfix)
dev_all$group <- as.factor(dev_all$group)
# plot deviation
groupcolors <- c("lightslateblue","goldenrod1") 
ts_df <- dev_all %>%
  group_by(group)

# Facet boxplot: sd
p <- ggplot(data = dev_all,aes(x=group,y=sd,group=group)) + # x="OCfix",y="ci",color="Label",palette = groupcolors
  geom_boxplot(position="dodge",aes(fill=group)) + #,add = "mean_sd",,scale="width"
  scale_fill_manual(values = groupcolors) +
  scale_y_continuous(limits = c(0,0.05), expand = c(0, 0)) +
  # scale_x_discrete(labels = c("STC","DSTC")) +
  labs(y=expression(paste("Standard Deviation")),x=NULL) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_text(face="bold",size=25,colour = "black"),
        axis.line.x = element_line(color="black", linewidth = 0.5),
        axis.line.y = element_line(color="black", linewidth = 0.5),
        axis.text = element_text(face="bold",size=18, color = "black"),
        strip.text = element_text(face="bold", size=18,lineheight=5.0),
        strip.background = element_rect(fill="#C9C5C4", colour="black",linewidth=1),
        axis.text.x=element_text(angle=60, hjust=1),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))+
  facet_wrap(~ocfix)
p
ggsave("30oc_sd_facet_box.png",dpi = 300, height = 20, width = 20, unit = 'in') # landscape

# Facet boxplot: rmse
p <- ggplot(data = dev_all,aes(x=group,y=rmse,group=group)) + # x="OCfix",y="ci",color="Label",palette = groupcolors
  geom_boxplot(position="dodge",aes(fill=group)) + #,add = "mean_sd",,scale="width"
  scale_fill_manual(values = groupcolors) +
  scale_y_continuous(limits = c(0,0.8), expand = c(0, 0)) +
  # scale_x_discrete(labels = c("STC","DSTC")) +
  labs(y=expression(paste("RMSE")),x=NULL) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_text(face="bold",size=25,colour = "black"),
        axis.line.x = element_line(color="black", linewidth = 0.5),
        axis.line.y = element_line(color="black", linewidth = 0.5),
        axis.text = element_text(face="bold",size=18, color = "black"),
        strip.text = element_text(face="bold", size=18,lineheight=5.0),
        strip.background = element_rect(fill="#C9C5C4", colour="black",linewidth=1),
        axis.text.x=element_text(angle=60, hjust=1),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))+
  facet_wrap(~ocfix)
p
ggsave("30oc_rmse_facet_box.png",dpi = 300, height = 20, width = 20, unit = 'in') # landscape


########## Part III. Time series of monthly CHL with fitted LM (Facet OC) 
########## Part IV. Time series of monthly CHL with fitted LM (Facet Longhurst)
#############################################################################################
# Pallette option: c("coral1","lightslateblue","olivedrab3","goldenrod1","lightgray") # red, purple,green, yellow, gray
# groupcolors<-c("#2e4ba0", "#faa419") # pallette: blue and yellow
# groupcolors<-c("#629433", "#FEFF99") # pallette: green and yellow (#629433:pretty green)
# groupcolors<-c("#66c5b8", "#f6db2b") 

# width=7.5, height=3.44, dpi=300 # portriat

# p <- ggplot(daf,aes(x=Region,y=Trend,fill=corr))
# p+geom_hline(yintercept=-0.38,colour="tan2",size=0.9)+
#   geom_hline(yintercept=-0.036,colour="slateblue",size=0.9)+
#   geom_violin(adjust=2,scale="width")+
#   scale_fill_manual(values=c("slateblue","tan2"))+
#   coord_flip()+ 
#   theme_bw()+
#   ylab(expression(paste("Trend  ", "(%yr"^" -1",")"))) + 
#   guides(fill=guide_legend(title=NULL))+
#   theme(axis.ticks.length=unit(-0.1,"cm"),
#         axis.text.x = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")), 
#         axis.text.y = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),
#         text=element_text(family="Times New Roman",size=16))+
#   scale_y_continuous(breaks=seq(-2,2,1),minor_breaks = seq(-2,2,1),limits=c(-2.4,2.4))    

# for(i in c(1:29)){
#   if(i==1){
#     print(i)
#     temp <- results_df %>%
#       filter(OCfix == i) %>%
#       add_significance()
#     tempci <- t_test(temp, ci ~ Label)
#     p_ci <- tempci
#   }else{
#     print(i)
#     temp <- results_df %>%
#       filter(OCfix == i) %>%
#       add_significance()
#     tempci <- t_test(temp, ci ~ Label)
#     p_ci <- rbind(p_ci,tempci)
#   }
# 
# }
# p_ci <- p_ci %>%
#   add_xy_position(x = "OCfix")

p <- p+stat_pvalue_manual(stat.test, label = "p")
stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0.01)

p + 
  add_pvalue(p_ci, label.size = 6,
             bracket.size = 0.01,  # 这里指定显著性标注线的粗细或厚度的
             y.position = c(5.5, 5.3, 5.7, 5.2, 5.4, 5.5),#根据pH值的范围，将显著性标注线放在不重叠的位置
             bracket.shorten = c(0.05, 0, 0.025, 0, 0.025, 0))#调整显著性标注线的长度的
#bracket.shorten 的值为 (0.025, 0, 0.025, 0, 0.025, 0)，表示每条标注线的两端都会缩短 0.025 的长度。这样可以让标注线看起来更加整洁和美观。

