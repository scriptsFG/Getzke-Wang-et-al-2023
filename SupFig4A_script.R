
## load libraries
library(tidyverse)
library(circlize)


#set wd
setwd("")

##clean environment
rm(list = ls())

#read data frame
read_tsv(file = "SupFig4A_genome.txt")->contig_1
read_tsv(file = "SupFig4A_BGCs.txt")->BGCs

filter(contig_1, contig_1$strand=="fwd")->contig_1_fwd
filter(contig_1, contig_1$strand=="rev")->contig_1_rev


circos.clear()
circos.initializeWithIdeogram(contig_1_fwd)
circos.rect(contig_1_fwd$start, -0.2, contig_1_fwd$end, -0.6, col=c("#000000"), border=c(NA))
circos.rect(contig_1_rev$start, -0.8, contig_1_rev$end, -1.2, col=c("#000000"), border=c(NA))
circos.rect(BGCs$start, -2, BGCs$end, -2.3, col=BGCs$color, border=BGCs$border)
draw.sector(0, 360, rou1 = 0.8190562)
draw.sector(0, 360, rou1 = 0.6983899)
draw.sector(0, 360, rou1 = 0.758723)
draw.sector(0, 360, rou1 = 0.6380568)

