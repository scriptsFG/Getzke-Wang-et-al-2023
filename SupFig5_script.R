
library(tidyverse)
library(ggpubr)
library(rstatix)

#######
#######
#set wd
setwd()



rm(list = ls())

colors<-c("Agaricomycetes"="#bfbfbf", "Ascomycota"="#8B008B", "Dothideomycetes"="#DB7093", "Eurotiomycetes"="#bfbfbf", "Glomeromycetes"="#bfbfbf", "Lecanoromycetes"="#bfbfbf", "Leotiomycetes"="#bfbfbf", "Microbotryomycetes"="#bfbfbf", "Mucoromycotina"="#bfbfbf", "Pezizomycetes"="#bfbfbf", "Pezizomycotina"="#bfbfbf", "Saccharomycetes"="#bfbfbf", "Sordariomycetes"="#8B008B", "Tremellomycetes"="#bfbfbf", "[Saprospirae]"="#bfbfbf", "Acidimicrobiia"="#bfbfbf",  "Actinobacteria"="#B22222",  "Alphaproteobacteria"="#32CD32", "Bacilli"="#FFA500",  "Bacteroidia"="#bfbfbf",  "Betaproteobacteria"="#228B22",  "Chlamydiia"="#bfbfbf",  "Chloroflexi"="#bfbfbf", 
           "Clostridia"="#bfbfbf",  "Cytophagia"="#bfbfbf",  "DA052"="#bfbfbf",  "Deltaproteobacteria"="#bfbfbf", 
           "Fibrobacteria"="#bfbfbf",  "Flavobacteriia"="#4682B4",  "Gammaproteobacteria"="#006400",  "Gemm-1"="#bfbfbf",  "Nitrospira"="#bfbfbf", 
           "Rubrobacteria"="#bfbfbf",  "SC3"="#bfbfbf",  "Sphingobacteriia"="#bfbfbf",  "Thermoleophilia"="#bfbfbf",  "TK10"="#bfbfbf",  "TM7-3"="#bfbfbf",  "VHS-B5-50"="#bfbfbf")


colors<-c("Leaf"="#bfbfbf", "Root"="#343a40")

#read meta data
read_tsv("SupFig5_data1.txt")->nBGCs
filter(nBGCs,family=="Pseudomonadaceae")->nBGCs
read_tsv("SupFig5_data2.txt")->Karasov
filter(nBGCs,Genomes!="Root401")->nBGCs

filter(nBGCs, Culture_collection=="Chlamy")->Chlamy
filter(nBGCs, Culture_collection=="AtLeaf")->AtLeaf
nBGCs %>% 
  filter(Culture_collection!="AtLeaf") %>% 
  filter(Culture_collection!="Chlamy")->Roots

rep("Root", length(Roots$Culture_collection))->Roots$Organ
rep("Chlamy", length(Chlamy$Culture_collection))->Chlamy$Organ
rep("Leaf", length(AtLeaf$Culture_collection))->AtLeaf$Organ
rep("Leaf", length(Karasov$Genomes))->Karasov$Organ
rep("Karasov", length(Karasov$Genomes))->Karasov$Culture_collection
bind_rows(Roots, Chlamy, AtLeaf, Karasov)->full
#bind_rows(Roots, AtLeaf, Karasov)->full
full %>% group_by(Culture_collection) %>%tally()->n_entries
#write_tsv(full, "BGC_metabolites.txt")
full %>% 
  select(Genomes, Organ, Culture_collection, class, family, genus, `corpeptin A / corpeptin B`, `nunapeptin / nunamycin`, syringomycin, `syringopeptin 25A`, cichopeptin,`2,4-diacetylphloroglucinol`, pyoverdin)->full2

colSums(is.na(full2))
full2$`2,4-diacetylphloroglucinol`[is.na(full2$`2,4-diacetylphloroglucinol`)]<- 0
full2$peptin = rowSums(full2[,c(7:11)])
replace(full2$peptin,full2$peptin>1, 1)->full2$peptin

full2 %>% 
  group_by(Organ, Culture_collection) %>% 
  summarise("peptin"=mean(peptin), "DAPG"=mean(`2,4-diacetylphloroglucinol`),"pyoverdin"=mean(pyoverdin))->full3



filter(full2,Culture_collection!="Chlamy")->noChlamy
pivot_longer(full3, cols = 3:5, names_to ="BGC", values_to = "n")->full3
filter(full3, Culture_collection!="Chlamy")->full3
filter(full3, Culture_collection!="IT")->full3





ggballoonplot(data=full3, x="BGC", y="Culture_collection", fill='Organ',size="n", size.range = c(0, 15))+
  ylab("")+
  xlab("")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), 
        axis.ticks = element_line(colour = "black", size = 0.5),
        panel.border = element_rect(colour = 'black', size=1, fill = NA),
        axis.text=element_text(size=12, color= "black"),
        axis.text.x = element_text(size=12, color= "black", angle = 45, hjust=0.99),
        axis.title=element_text(size=12),
        strip.background = element_rect(color="black", fill=NA, size=1, linetype="solid"),
        strip.text.x = element_text(size = 12, color = "black"))+
  scale_y_discrete(limits=rev(c("AtLeaf","Karasov", "AtRoot","NC", "LjRoot")))+
  scale_x_discrete(limits=c("DAPG", "pyoverdin", "peptin"), labels=c("DAPG", "pyoverdin", "Pseudopeptin"))+
  scale_fill_manual(values=colors)+
  NULL

