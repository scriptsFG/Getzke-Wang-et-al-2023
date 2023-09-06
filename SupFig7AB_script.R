

## load libraries
library(tidyverse)
library(PMCMRplus)

##clean environment
rm(list = ls())


#colors
colors<-c("R401_HK"="#8B8C89", "R401_WT" = "#000001", "R401_LP"="#009B7D")


#set wd
setwd()


##load data files
read_tsv("SupFig7AB_data.txt")->FW


pivot_longer(FW,  cols = 6:8, names_to ="Shoots", values_to = "FW")->FW
na.omit(FW)->FW
log2(FW$FW)->FW$logFW
str_c(FW$Treatment, FW$Bacterium, sep = "_")->FW$full
filter(FW, logFW!= -Inf)->FW

filter(FW, Plant=="Micro Tom")->Tom
filter(FW, Plant=="Lotus japonicus")->Lotus



##make boxplots
#Tom
ggplot(data=Tom, aes(x=Bacterium, y=logFW))+
  geom_jitter(aes(color=Bacterium, fill=Bacterium), width = 0.2, alpha=0.3)+
  scale_shape_manual(values = c(1:4))+
  geom_boxplot(outlier.shape = NA, lwd=0.9, fill=NA, aes(color=Bacterium))+
  facet_wrap(~Treatment)+
  xlab("")+
  ylab("log2(shoot fresh weight) [mm]")+
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
  scale_color_manual(values=colors)+
  scale_fill_manual(values=colors)+
  geom_hline(yintercept = 6.782671, size=1, color="#8B8C89")+
  ggtitle("")+
  NULL


filter(Tom, Treatment=="cntrl")->cntrl
filter(Tom, Treatment=="NaCl")->NaCl


dunn <- kwAllPairsDunnTest(logFW ~ as.factor(Bacterium), data=cntrl, p.adjust.method ="BH")
dunn
plot(dunn)

dunn <- kwAllPairsDunnTest(logFW ~ as.factor(Bacterium), data=NaCl, p.adjust.method ="BH")
dunn
plot(dunn)


#Tom
ggplot(data=Lotus, aes(x=Bacterium, y=logFW))+
  geom_jitter(aes(color=Bacterium, fill=Bacterium), width = 0.2, alpha=0.3)+
  scale_shape_manual(values = c(1:4))+
  geom_boxplot(outlier.shape = NA, lwd=0.9, fill=NA, aes(color=Bacterium))+
  facet_wrap(~Treatment)+
  xlab("")+
  ylab("log2(shoot fresh weight) [mm]")+
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
  scale_color_manual(values=colors)+
  scale_fill_manual(values=colors)+
  geom_hline(yintercept = 4.035624, size=1, color="#8B8C89")+
  ggtitle("")+
  NULL


filter(Lotus, Treatment=="cntrl")->cntrl
filter(Lotus, Treatment=="NaCl")->NaCl



dunn <- kwAllPairsDunnTest(logFW ~ as.factor(Bacterium), data=cntrl, p.adjust.method ="BH")
dunn
plot(dunn)

dunn <- kwAllPairsDunnTest(logFW ~ as.factor(Bacterium), data=NaCl, p.adjust.method ="BH")
dunn
plot(dunn)
