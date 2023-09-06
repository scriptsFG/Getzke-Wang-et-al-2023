#load libraries
library(tidyverse)
library(PMCMRplus)


#clean environment
rm(list = ls())


colors<-c("HK"="#8B8C89", "WT" = "#000001", "dLP"="#009B7D")


#set wd
setwd()

#read 
read_tsv(file = "SupFig3_data.txt", skip_empty_rows = T, )->data
data[,1:6]->data
na.omit(data)->data
log2(data$Value)->data$Value_log2
str_c(data$SynCom, data$R401, sep = "_")->data$Condition


filter(data, data$R401!="dD4")->data
filter(data, data$NaCl=="0 mM")->mM0
filter(data, data$NaCl=="50 mM")->mM50
filter(data, data$NaCl=="100 mM")->mM100
filter(data, data$NaCl=="150 mM")->mM150
filter(mM0, mM0$R401=="HK")->HK
filter(HK, HK$SynCom=="no SC")->HK
subset(HK, HK$Value!=0)->HK
median(HK$Value_log2)


dunn <- kwAllPairsDunnTest(Value ~ as.factor(Condition), data=mM0, p.adjust.method ="BH")
plot(dunn)
dunn <- kwAllPairsDunnTest(Value ~ as.factor(Condition), data=mM50, p.adjust.method ="BH")
plot(dunn)
dunn <- kwAllPairsDunnTest(Value ~ as.factor(Condition), data=mM100, p.adjust.method ="BH")
plot(dunn)
dunn <- kwAllPairsDunnTest(Value ~ as.factor(Condition), data=mM150, p.adjust.method ="BH")
plot(dunn)



#########boxplot
ggplot(data,aes(x=Condition, y=Value_log2))+
  geom_jitter(aes(x=Condition, y=Value_log2, color=R401), width = 0.3, alpha=0.6)+
  geom_boxplot(outlier.shape = NA, lwd=1, fill=NA, aes(color=R401))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.ticks = element_line(colour = "black", size = 0.5),
        panel.border = element_rect(colour = 'black', size=1, fill = NA),
        axis.text.y =element_text(size=12, color= "black", vjust = 0.5),
        axis.text.x =element_text(size=12, color= "black", angle = 90, hjust = 1, vjust = 0.5),
        axis.title=element_text(size=12), 
        legend.background=element_blank(),
        legend.key=element_blank(),
        plot.background = element_blank(),
        legend.position="none",
        strip.background = element_rect(color="black", fill=NA, size=1, linetype="solid"),
        strip.text.x = element_text(size = 12, color = "black"))+
  scale_color_manual(values=colors)+
  geom_hline(yintercept = 4.045268, size=1)+
  scale_x_discrete(limits=c("no SC_HK", "no SC_WT", "no SC_dLP", "MSC_HK", "MSC_WT", "MSC_dLP"), labels=c("HK", "WT", "sypc", "HK", "WT", "sypc"))+
  scale_y_continuous(limits=c(-2.5,6.5))+
  geom_vline(xintercept = 3.5, size=0.5)+
  ylab("Normalized Shoot Freshweight")+
  xlab("")+
  facet_wrap(~NaCl)+
  NULL


