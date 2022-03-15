library(dplyr)
library(ggplot2)
library(ggfortify)
library(vegan)

ABS <- read.csv("./R/ascbassitenew.csv")

ABS_NMDS=metaMDS(ABS, k=2, trymax=100)

ordiplot(ABS_NMDS,type="n", xlab = NULL, ylab = NULL)
treat=c(rep("Gadlas Ascomycota",70),rep("Gadlas Basidiomycota",73),
        rep("Morfa Canol Ascomycota",67), rep("Morfa Canol Basidiomycota",65))

ordihull(ABS_NMDS,groups=treat,draw="polygon",col = 1:2,
         label=TRUE, border = NULL, bg = NULL)

capscale(SITE ~ 1, dist="bray", metaMDS = TRUE)

*****
  
  
ordicluster(ABS_NMDS,prune=4, groups=treat, col = 1:4,
            label=TRUE, border = NULL, bg = NULL)
