library(dplyr)
library(ggplot2)
library(ggfortify)
library(vegan)

Date <- read.csv("./R/MCsampledate.csv")

Date_NMDS=metaMDS(Date, k=2, trymax=100)

ordiplot(Date_NMDS,type="n", xlab = NULL, ylab = NULL)
treat=c(rep("July",22),rep("August",53))

ordihull(Date_NMDS,groups=treat,draw="polygon",col = 1:2,
         label=TRUE, border = NULL, bg = NULL)

capscale(SITE ~ 1, dist="bray", metaMDS = TRUE)
