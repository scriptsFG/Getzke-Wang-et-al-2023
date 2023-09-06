
## load libraries
library(tidyverse)

#set wd
setwd("")

##clean environment
rm(list = ls())

colors<-c("HK"="#8B8C89", "WT" = "#000001")
colors2<-c("non_sig"="#8B8C89", "sig" = "#e63946")
alpha<-c("non_sig"=0.2, "sig"=1)


#load data
read_tsv("Fig2A_data.txt")->data

data %>% 
  filter(padj<= 0.05) %>% 
  filter(log2FoldChange>1 | log2FoldChange< -1)->data_sig



#vulcano plots
data %>% 
  filter(condition<=10) %>% 
  filter(condition!=4) %>% 
  filter(condition!=5) %>% 
  filter(condition!=9) %>% 
  filter(condition!=10)->data2

ggplot(data2, aes(y=log10_padj, x=log2FoldChange))+
  geom_point(aes(color=sig, fill=sig), size=0.5, alpha=0.3)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.ticks = element_line(colour = "black", size = 0.5),
        panel.border = element_rect(colour = 'black', size=1, fill = NA),
        axis.text=element_text(size=12, color= "black"),
        axis.title=element_text(size=12), 
        panel.background = element_blank(),
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.position="none")+
  scale_color_manual(values=colors2)+
  scale_fill_manual(values=colors2)+
  geom_hline(yintercept =  1.30103, size=0.5, color="black")+
  geom_vline(xintercept = 1, size=0.5, color="black")+
  geom_vline(xintercept = -1, size=0.5, color="black")+
  ylab("-log10(padj)")+
  xlab("log2(fold change)")+
  facet_wrap(~condition, nrow = 2)+
  NULL






