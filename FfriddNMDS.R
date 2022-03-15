library(devtools)
install_github("r-lib/scales")
library(scales)
install_github("vqv/ggbiplot")
library(ggbiplot)

library(dplyr)
library(ggplot2)
library(ggfortify)
library(vegan)

Fridd <- read.csv("./R/Fridd data.csv")

Fridd_NMDS=metaMDS(Fridd, k=2, trymax=100)

ordiplot(Fridd_NMDS,type="n", xlab = NULL, ylab = NULL)
treat=c(rep("No inputs",4),rep("Ferts + rot.grazing",4),rep("Rotational grazing",4),
        rep("Fertilisers only",4))
geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
             arrow = arrow(length = unit(0.2, "cm")),color="#4C005C",alpha=0.5)


points(Fridd_NMDS, display = "sites", cex = 0.8, 
       pch=21, col="red")

points(Fridd_NMDS, display = "sites", cex = 0.8, pch=21, col="red", bg="yellow")

orditorp(Fridd,display="species",col="red",air=0.01)
orditorp(Fridd,display="sites",col=c(rep("green",4),rep("blue",4),
                                     rep("red",4),rep("yellow",4)),
         air=0.01,cex=1, labels = NULL)


ordihull(Fridd_NMDS,groups=treat,draw="polygon",col=1:4,
         label=TRUE, border = FALSE, bg = FALSE)



capscale(Fridd ~ 1, dist="bray", metaMDS = TRUE)



orditorp(Fridd,display="species",col="red",air=0.01)
orditorp(Fridd,display="sites",col=c(rep("green",4),rep("blue",4),
                                          rep("red",4),rep("yellow",4)),
         air=0.01,cex=1)

?ordihull

colors=c(rep("red",4),rep("blue",4), rep("green",4),rep("yellow",4))
ordiplot(Fridd_NMDS,type="n")
for(i in unique(treat)) {
  ordihull(Fridd_NMDS$point[grep(i,treat),],draw="polygon",
           groups=treat[treat==i],col=colors[grep(i,treat)],label=TRUE) } 
orditorp(Fridd_NMDS,display="species",col="red",air=0.01)
orditorp(Fridd_NMDS,display="sites",col=c(rep("green",4),rep("blue",4),
                                              rep("red",4),rep("yellow",4)),air=0.01,cex=1.25)

No_inputs <- Fridd_NMDS[1:4,]
Ferts_and_grazing <- Fridd_NMDS[5:8,]
Rotational_grazing <- Fridd_NMDS[9:12,]
Ferts_only <- Fridd_NMDS[13:16,]


