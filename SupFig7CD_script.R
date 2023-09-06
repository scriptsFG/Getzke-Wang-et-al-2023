

## load libraries
library(tidyverse)
library(PMCMRplus)
##clean environment
rm(list = ls())


#colors
colors<-c("R401_HK"="#8B8C89", "R401_WT" = "#000001", "R401_LP"="#009B7D", "R107_WT"="#B22222", "R131_WT"="#006400")

#set wd
setwd()



##load data files
read_tsv("SupFig7CD_data1.txt")->FW
read_tsv("SupFig7CD_data2.txt")->CFU23

pivot_longer(CFU23,  cols = 10:15, names_to ="Dilution", values_to = "CFU")->CFU23

CFU23 %>% 
  filter(CFU != "x") %>% 
  filter(CFU!="no") %>% 
  filter()->CFU23


as.numeric(CFU23$CFU)->CFU23$CFU
as.numeric(CFU23$Dilution)->CFU23$Dilution

replace(CFU23$FM, CFU23$FM<=0,0.1)->CFU23$FM

CFU23$CFU*(CFU23$Dilution_factor^CFU23$Dilution)/CFU23$FM->CFU23$counts


CFU23 %>%
  group_by(Experiment, Rep, Label, Compartment) %>%
  summarise(counts=median(counts))->CFU23


inner_join(CFU23, FW)->data


log2(data$FW)->data$logFW
log2(data$counts)->data$logcounts
str_c(data$Treatment, data$Bacterium, sep = "_")->data$full
na.omit(data)->data




data %>% 
  filter(Treatment!="low PAR") %>% 
  filter(Treatment!="drought") %>% 
  filter(Bacterium!="R131_WT") %>% 
  filter(Bacterium!="R107_WT") %>% 
  filter(Plant!="rbohD") %>% 
  filter(Plant!="Col-0") %>% 
  filter(Plant!="Micro Tom") %>% 
  filter()->Lotus


ggplot(data=Lotus, aes(x=Bacterium, y=logcounts))+
  geom_jitter(aes(color=Bacterium, fill=Bacterium), width = 0.2, alpha=0.3)+
  scale_shape_manual(values = c(1:4))+
  geom_boxplot(outlier.shape = NA, lwd=0.9, fill=NA, aes(color=Bacterium))+
  facet_wrap(Treatment~Compartment)+
  xlab("")+
  ylab("log2(R401 CFU per mg plant)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.ticks = element_line(colour = "black", size = 0.5),
        panel.border = element_rect(colour = 'black', size=1, fill = NA),
        axis.text=element_text(size=12, color= "black"),
        axis.text.x = element_text(size=12, color= "black", angle = 90, hjust=1),
        axis.title=element_text(size=12), 
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.position="none",
        strip.background = element_rect(color="black", fill=NA, size=1, linetype="solid"),
        strip.text.x = element_text(size = 12, color = "black"))+
  scale_x_discrete(limits=c("R401_WT", "R401_LP"), labels=c( "WT","sypc"))+
  scale_color_manual(values=colors)+
  scale_fill_manual(values=colors)+
  ggtitle("")+
  NULL


Lotus %>% 
  #filter(Treatment=="cntrl") %>% 
  filter(Treatment!="cntrl") %>% 
  #filter(Compartment=="Root") %>% 
  filter(Compartment!="Root") %>% 
  filter()->Lotus2


dunn <- kwAllPairsDunnTest(logcounts ~ as.factor(Bacterium), data=Lotus2, p.adjust.method ="BH")
dunn





data %>% 
  filter(Treatment!="low PAR") %>% 
  filter(Treatment!="drought") %>% 
  filter(Bacterium!="R131_WT") %>% 
  filter(Bacterium!="R107_WT") %>% 
  filter(Plant!="rbohD") %>% 
  filter(Plant!="Col-0") %>% 
  filter(Plant!="Lotus japonicus") %>% 
  filter()->Tom

ggplot(data=Tom, aes(x=Bacterium, y=logcounts))+
  geom_jitter(aes(color=Bacterium, fill=Bacterium), width = 0.2, alpha=0.3)+
  scale_shape_manual(values = c(1:4))+
  geom_boxplot(outlier.shape = NA, lwd=0.9, fill=NA, aes(color=Bacterium))+
  facet_wrap(Treatment~Compartment)+
  xlab("")+
  ylab("log2(R401 CFU per mg plant)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.ticks = element_line(colour = "black", size = 0.5),
        panel.border = element_rect(colour = 'black', size=1, fill = NA),
        axis.text=element_text(size=12, color= "black"),
        axis.text.x = element_text(size=12, color= "black", angle = 90, hjust=1),
        axis.title=element_text(size=12), 
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.position="none",
        strip.background = element_rect(color="black", fill=NA, size=1, linetype="solid"),
        strip.text.x = element_text(size = 12, color = "black"))+
  scale_x_discrete(limits=c("R401_WT", "R401_LP"), labels=c( "WT","sypc"))+
  scale_color_manual(values=colors)+
  scale_fill_manual(values=colors)+
  ggtitle("")+
  NULL


Tom %>% 
  #filter(Treatment=="cntrl") %>% 
  filter(Treatment!="cntrl") %>% 
  filter(Compartment=="Root") %>% 
  #filter(Compartment!="Root") %>% 
  filter()->Tom2


dunn <- kwAllPairsDunnTest(logcounts ~ as.factor(Bacterium), data=Tom2, p.adjust.method ="BH")
dunn


