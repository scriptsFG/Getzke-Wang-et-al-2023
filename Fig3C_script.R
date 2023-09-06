#load libraries
library(tidyverse)
library(PMCMRplus)


#clean environment
rm(list = ls())


colors<-c("WT" = "#000001", "dLP"="#009B7D")


#set wd
setwd()

#read 16s table
read_tsv(file = "Fig3C_data.txt")->data


ggplot(data, aes(x=NaCl, y=Ratio, color=R401, fill=R401))+
  geom_point(aes(color=R401, size=0.1))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.ticks = element_line(colour = "black", size = 0.5),
        panel.border = element_rect(colour = 'black', size=1, fill = NA),
        axis.text=element_text(size=12, color= "black"),
        axis.title=element_text(size=12), 
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.position="none")+
  scale_color_manual(values=colors)+
  scale_fill_manual(values=colors)+
  ylab("Effect on Shoot Freshweight relative to HK")+
  xlab("NaCl concentration [mM]")+
  geom_smooth(method='lm', se=F)


data %>% 
  filter(R401=="WT")->WT

data %>% 
  filter(R401=="dLP")->LP

summary(lm(NaCl ~ Ratio, data=WT)) 
summary(lm(NaCl ~ Ratio, data=LP)) 
