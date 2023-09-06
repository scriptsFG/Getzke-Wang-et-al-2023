#load libraries
library(tidyverse)
library(PMCMRplus)


#clean environment
rm(list = ls())

#set wd
setwd()

#read data
read_tsv(file = "Fig1A_data.txt", skip_empty_rows = T)->FW


log2(FW$avg_SFW)->FW$avg_SFW_norm

#########boxplot
ggplot(FW, aes(x=R401, y=avg_SFW_norm))+
  geom_jitter(aes(x=R401, y=avg_SFW_norm), width = 0.2, alpha=0.3)+
  scale_shape_manual(values = c(1:4))+
  geom_boxplot(outlier.shape = NA, lwd=0.9, fill=NA)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.ticks = element_line(colour = "black", size = 0.5),
        panel.border = element_rect(colour = 'black', size=1, fill = NA),
        axis.text=element_text(size=12, color= "black"),
        axis.text.x =element_text(size=12, color= "black", angle = 90, hjust = 1),
        axis.title=element_text(size=12), 
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.position="none")+
  scale_x_discrete(limits=c("HK", "wt"), labels=c("HK", "WT"))+
  ylab("log2(shoot fresh weight) [mg]")+
  xlab("")+
  NULL


kwAllPairsDunnTest(avg_SFW_norm~as.factor(R401), data=FW, p.adjust.method = "BH")->test
test$p.value
