

## load libraries
library(tidyverse)
library(PMCMRplus)

##clean environment
rm(list = ls())


#colors
colors<-c("R401_HK"="#8B8C89", "R401_WT" = "#000001", "R107_WT"="#B22222", "R131_WT"="#006400")


#set wd
setwd()



##load data files
read_tsv("SupFig2AB_data.txt")->FW


pivot_longer(FW,  cols = 6:10, names_to ="Shoots", values_to = "FW")->FW
str_split(string = FW$Bacterium, pattern = "_", simplify = T)[,1]->FW$Strain
na.omit(FW)->FW
log2(FW$FW)->FW$logFW
str_c(FW$Treatment, FW$Bacterium, sep = "_")->FW$full
filter(FW, logFW!= -Inf)->FW
filter(FW, Plant=="Col-0")->Col
filter(FW, Treatment=="cntrl")->rbohd


FW %>% 
  filter(Bacterium!="R401_LP") %>% 
  group_by(Bacterium, Treatment, Plant) %>% 
  summarise(mlogFW=median(logFW))->test

##make boxplots
ggplot(data=Col, aes(x=Bacterium, y=logFW))+
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
  scale_x_discrete(limits=c("R401_HK", "R401_WT","R107_WT", "R131_WT"), labels=c("HK", "R401","R107", "L131"))+
  scale_color_manual(values=colors)+
  scale_fill_manual(values=colors)+
  geom_hline(yintercept = 2.8073549, size=1, color="#8B8C89")+
  ggtitle("")+
  NULL

dunn <- kwAllPairsDunnTest(logFW ~ as.factor(full), data=Col, p.adjust.method ="BH")
dunn
plot(dunn)
