library(dplyr)
library(ggplot2)
library(ggfortify)
library(vegan)

SITE <- read.csv("./R/sitefungidist.csv")

SITE_NMDS=metaMDS(SITE, k=2, trymax=100)

ordiplot(SITE_NMDS,type="n", xlab = NULL, ylab = NULL)
treat=c(rep("Gadlas",81),rep("Morfa Canol",82))
geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
             arrow = arrow(length = unit(0.2, "cm")),color="#4C005C",alpha=0.5)

ordihull(SITE_NMDS,groups=treat,draw="polygon",col = 1:2,
         label=TRUE, border = NULL, bg = NULL)

capscale(SITE ~ 1, dist="bray", metaMDS = TRUE)

