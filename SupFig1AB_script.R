
## load libraries
library(tidyverse)
library(ggvenn)


#set wd
setwd("")

##clean environment
rm(list = ls())




#load data
read_tsv("SupFig1AB_data.txt")->data


data %>% 
  filter(padj<= 0.05) %>% 
  filter(log2FoldChange>1 | log2FoldChange< -1)->data_sig


data_sig %>% filter(condition==1) %>% .$gene->cond1
data_sig %>% filter(condition==2) %>% .$gene->cond2
data_sig %>% filter(condition==3) %>% .$gene->cond3
data_sig %>% filter(condition==6) %>% .$gene->cond6
data_sig %>% filter(condition==7) %>% .$gene->cond7
data_sig %>% filter(condition==8) %>% .$gene->cond8



#root
vennR <- list(`R401` = cond1,
              `NaCl` = cond2,
              `R401+NaCl` = cond3)


ggvenn(
  vennR, 
  fill_color = c("#f8f9fa", "#d3d3d3", "#495057"),
  stroke_size = 0.5, set_name_size = 4
)


#shoot
vennS <- list(`R401` = cond6,
              `NaCl` = cond7,
              `R401+NaCl` = cond8)

ggvenn(
  vennS, 
  fill_color = c("#f8f9fa", "#d3d3d3", "#495057"),
  stroke_size = 0.5, set_name_size = 4
)



