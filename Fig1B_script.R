#load libraries
library(tidyverse)
library(PMCMRplus)


#clean environment
rm(list = ls())


colors<-c("HK"="#8B8C89", "WT" = "#000001")


#set wd
setwd()


#read 
read_tsv(file = "Fig1B_data.txt", skip_empty_rows = T, )->data
na.omit(data)->data
log2(data$Value)->data$Value_log2
str_c(data$Salt, data$R401, sep = "_")->data$Condition


data %>% 
  filter(Salt=="MS") %>% 
  filter(R401=="HK")->HK
median(HK$Value_log2)


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
  geom_hline(yintercept = 3.533421, size=1, color="#8B8C89")+
  scale_x_discrete(limits=c("MS_HK", "MS_WT",  "NaCl_HK", "NaCl_WT"), labels=c("HK", "WT", "HK", "WT"))+
  geom_vline(xintercept = 2.5, size=0.5)+
  ylab("log2(shoot fresh weight) [mg]")+
  xlab("")+
  NULL



filter(data, Salt=="MS")->MS
filter(data, Salt!="MS")->NaCl

dunn <- kwAllPairsDunnTest(Value_log2 ~ as.factor(Condition), data=NaCl, p.adjust.method ="BH")
plot(dunn)

dunn <- kwAllPairsDunnTest(Value_log2 ~ as.factor(Condition), data=MS, p.adjust.method ="BH")
plot(dunn)

