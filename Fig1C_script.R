#load libraries
library(tidyverse)
library(PMCMRplus)


#clean environment
rm(list = ls())

colors<-c("None"="#8B8C89", "WT" = "#000001")

#set wd
setwd()

#read data
read_tsv(file = "Fig1C_data.txt", skip_empty_rows = T)->FW


log2(FW$Weight)->FW$Weight_norm


str_c(FW$Watering, FW$R401)->FW$Condition
as.factor(FW$Condition)->FW$Condition


FW %>% 
  filter(Watering=="NaCl")->NaCl
FW %>% 
  filter(Watering!="NaCl")->H2O
FW %>% 
  filter(R401=="None")->HK


#########boxplot
ggplot(FW, aes(x=Condition, y=Weight_norm, color=R401))+
  geom_jitter(aes(x=Condition, y=Weight_norm), width = 0.2, alpha=0.3)+
  geom_boxplot(outlier.shape = NA, lwd=0.9, fill=NA)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.ticks = element_line(colour = "black", size = 0.5),
        panel.border = element_rect(colour = 'black', size=1, fill = NA),
        axis.text=element_text(size=12, color= "black"),
        axis.text.x =element_text(size=12, color= "black", angle = 45, hjust = 1),
        axis.title=element_text(size=12), 
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.position="none")+
  scale_color_manual(values=colors)+
  geom_vline(xintercept = 2.5)+
  scale_x_discrete(limits=c("H2ONone", "H2OWT",  "NaClNone", "NaClWT"), labels=c("HK", "WT", "HK", "WT"))+
  ylab("Shoot fresh-weight [mg]")+
  xlab("")+
  NULL



#dunn
kwAllPairsDunnTest(Weight_norm~Condition, data=NaCl, p.adjust.method = "BH")->test
test$p.value

kwAllPairsDunnTest(Weight_norm~Condition, data=H2O, p.adjust.method = "BH")->test
test$p.value

