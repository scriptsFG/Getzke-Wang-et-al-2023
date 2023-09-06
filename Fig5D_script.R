# load libraries
library(tidyverse)
library(PMCMRplus)

##clean environment
rm(list = ls())

colors<-c("O"="#6c757d", "B"="#343a40", "A"="#1F1F1F")


#set wd
setwd()


##load data files
read_tsv("Fig5D_data.txt")->ion


filter(ion, Salt==0)->ion


## boxplot
ggplot(data=ion, aes(x=Molecule, y=Conductivity))+
  geom_jitter(aes(color=Molecule, fill=Molecule), width = 0.2, alpha=0.3)+
  scale_shape_manual(values = c(1:4))+
  geom_boxplot(outlier.shape = NA, lwd=0.9, fill=NA, aes(color=Molecule))+
  xlab("")+
  ylab("Ion leakage [µS/cm]")+
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
  scale_x_discrete(limits=c("O", "B","A"), labels=c("0 µg/ml", "1 ng/ml","1 µg/ml"))+
  scale_color_manual(values=colors)+
  scale_fill_manual(values=colors)+
  ggtitle("")+
  NULL


dunn <- kwAllPairsDunnTest(Conductivity ~ as.factor(Molecule), data=ion, p.adjust.method ="BH")
dunn
plot(dunn)


