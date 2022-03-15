library(dplyr)
library(ggplot2)
library(ggfortify)
library(vegan)

NITR <- read.csv("./R/gadlasnitrogen.csv")

NITR_NMDS=metaMDS(NITR, k=2, trymax=100)

ordiplot(NITR_NMDS,type="n", xlab = NULL, ylab = NULL)
treat=c(rep("Before Nitrogen",24),rep("After Nitrogen",23))

ordihull(NITR_NMDS,groups=treat,draw="polygon",col = 1:2,
         label=TRUE, border = NULL, bg = NULL)
