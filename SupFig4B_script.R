

## load libraries
library(tidyverse)
library(ggpubr)
library(rstatix)

##clean environment
rm(list = ls())

colors<-c("WT" = "#1F1F1F", "dphld"="#F9C74F", "dphlddpvdy"="#F8961E", "dpvdydpvdl"="#83677B", "dsypc"="#009B7D", "dsypcdphlddpvdy"="#B8E9D4")
colors2<-c("Agaricomycetes"="#bfbfbf", "Ascomycota"="#8B008B", "Dothideomycetes"="#DB7093", "Eurotiomycetes"="#bfbfbf", "Glomeromycetes"="#bfbfbf", "Lecanoromycetes"="#bfbfbf", "Leotiomycetes"="#bfbfbf", "Microbotryomycetes"="#bfbfbf", "Mucoromycotina"="#bfbfbf", "Pezizomycetes"="#bfbfbf", "Pezizomycotina"="#bfbfbf", "Saccharomycetes"="#bfbfbf", "Sordariomycetes"="#8B008B", "Tremellomycetes"="#bfbfbf", "[Saprospirae]"="#bfbfbf", "Acidimicrobiia"="#bfbfbf",  "Actinobacteria"="#B22222",  "Alphaproteobacteria"="#32CD32", "Bacilli"="#FFA500",  "Bacteroidia"="#bfbfbf",  "Betaproteobacteria"="#228B22",  "Chlamydiia"="#bfbfbf",  "Chloroflexi"="#bfbfbf", 
           "Clostridia"="#bfbfbf",  "Cytophagia"="#bfbfbf",  "DA052"="#bfbfbf",  "Deltaproteobacteria"="#bfbfbf", 
           "Fibrobacteria"="#bfbfbf",  "Flavobacteriia"="#4682B4",  "Gammaproteobacteria"="#006400",  "Gemm-1"="#bfbfbf",  "Nitrospira"="#bfbfbf", 
           "Rubrobacteria"="#bfbfbf",  "SC3"="#bfbfbf",  "Sphingobacteriia"="#bfbfbf",  "Thermoleophilia"="#bfbfbf",  "TK10"="#bfbfbf",  "TM7-3"="#bfbfbf",  "VHS-B5-50"="#bfbfbf")


#set wd
setwd()

##load data file
read_tsv("SupFig4B_data.txt")->data
read_tsv("SupFig4B_tax.txt")->tax
data[is.na(data)] <- "0"
data %>% 
  filter(Halo_size_mm!="x")->data


#load colony information
read_tsv("SupFig4B_setup.txt")->colonies

inner_join(data, colonies)->data

as.numeric(data$Halo_size_mm)->data$Halo_size_mm



##aggregate
data %>% 
  group_by(Lawn, Plate, R401, Date) %>%
  summarise(halo=mean(Halo_size_mm)) %>%
  group_by(Lawn, R401, Date) %>%
  summarise(halo=mean(halo)) %>% 
  filter(., R401!="dphld") %>% 
  filter(., R401!="dphlddpvdl") %>% 
  filter(., Lawn!="R568") %>% 
  filter(., Lawn!="R565") %>% 
  filter(., Lawn!="R329") %>% 
  filter(., Lawn!="R695") %>% 
  filter(., Lawn!="R147") %>% 
  filter()->data2

left_join(data2, tax)->data2

ggballoonplot(data=data2, x="R401", y="Lawn", fill='Class',size="halo", size.range = c(0, 15))+
  ylab("")+
  xlab("")+
  #ggtitle("Halo width of R401(-mutant) on different lawn bacteria")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), 
        axis.ticks = element_line(colour = "black", size = 0.5),
        panel.border = element_rect(colour = 'black', size=1, fill = NA),
        axis.text=element_text(size=12, color= "black"),
        axis.text.x = element_text(size=12, color= "black", angle = 45, hjust=0.99),
        axis.title=element_text(size=12),
        #legend.position = "none", 
        strip.background = element_rect(color="black", fill=NA, size=1, linetype="solid"),
        strip.text.x = element_text(size = 12, color = "black"))+
  scale_y_discrete(limits=rev(c("R16_2", "R9","R431", "R420","F212")), labels=rev(c("R16D2","R9", "R431", "R420", "F212")))+
  scale_x_discrete(limits=c("WT", "dsypc","dphlddpvdy", "dsypcdphlddpvdy"))+
  #scale_x_discrete(limits=c("54","56", "DD", "54DD","56DD"), labels=c("dAcT","dNRPS", "dDAPG", "dAcTdDAPG", "dNRPSdDAPG"))+
  scale_fill_manual(values=colors2)+
  NULL

data %>%
  filter(Halo_size_mm>0) %>% 
  group_by(Lawn) %>%
  wilcox_test(data =., Halo_size_mm ~ R401) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance("p.adj") %>% 
  #filter(., group2=="WT") %>% 
  #filter(., group1=="dphlddpvdy") %>% 
  filter(., p.adj<=0.05) %>% 
  filter()->test2


