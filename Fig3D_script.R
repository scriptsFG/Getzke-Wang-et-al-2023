library(tidyverse)
#######

rm(list = ls())

#colors for plotting
colors<-c("none"="#8B8C89", "WT" = "#000001", "sypc"="#009B7D")

#set wd
setwd

#read data file for ODS
read_tsv(file = "Fig3D_data.txt")->are
pivot_longer(are, cols = c(3:62), names_to = "Well", values_to = "OD")->are
are$`Time [s]`/60/60->are$Time_h

#read setup
read_tsv(file = "Fig3D_setup.txt")->setup
str_c(setup$Row, setup$Col)->setup$Well
str_split(setup$Condition, pattern = "_", simplify = T)[,1]->setup$NaCl
str_split(setup$Condition, pattern = "_", simplify = T)[,2]->setup$R401


left_join(are, setup)->od



od %>% 
  filter(NaCl!="X") %>% 
  filter(Col!=2) %>% 
  filter(Col!=3) %>% 
  filter(Time_h<48) %>% 
  group_by(Time_h, R401, NaCl) %>%
  summarise(OD_mean=mean(OD), OD_sd=sd(OD)) %>% 
  #filter(`Time [s]`<40) %>% 
  filter()->df_mean


#kinetic
#OD
ggplot(df_mean) + 
  geom_ribbon(aes(x = Time_h, y = OD_mean,  fill=R401, ymin = OD_mean - OD_sd, ymax = OD_mean + OD_sd), alpha=0.5) +
  geom_line(aes(x = Time_h, y = OD_mean,  color=R401)) +
  theme(panel.background = element_rect(F))+
  theme(axis.line = element_line(T))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.ticks = element_line(colour = "black", size = 0.5),
        panel.border = element_rect(colour = 'black', size=1, fill = NA),
        axis.text=element_text(size=12, color= "black"),
        axis.text.x = element_text( size=12, color= "black"),
        axis.title=element_text(size=12), 
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.position="none",
        strip.background = element_rect(color="black", fill=NA, size=1, linetype="solid"),
        strip.text.x = element_text(size = 12, color = "black"))+
  scale_color_manual(values=colors)+
  scale_fill_manual(values=colors)+
  ylab("Abs600")+
  xlab("Time [s]")+
  facet_wrap(~NaCl)+
  NULL
