#pca

library(devtools)
library(scales)
library(ggbiplot)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(vegan)

Fridd <- read.csv("./R/Fridd data.csv")

Fridd.pca <- prcomp(Fridd, center = TRUE,scale. = TRUE)
summary(Fridd.pca)
ggbiplot(Fridd.pca)
Fridd.treat <- c(rep("No input", 4), rep("Fertiliser and grazing", 4), 
                 rep("Rotational grazing", 4), rep("Fertiliser", 4))

ggbiplot(Fridd.pca,ellipse=TRUE, obs.scale = 1, var.scale = 1,
         var.axes=FALSE,
         labels=rownames(Fridd2), groups=Fridd.treat)+
  scale_colour_manual(name="Treatment",
                      values= c("forest green", "red3", 
                                "dark blue", "orange"))+
  ggtitle("PCA of Fridd fungal communities")+
  theme_minimal()+
  theme(legend.position = "bottom")

ggbiplot(Fridd.pca,ellipse=TRUE, choices=c(3,4),
         obs.scale = 1, var.scale = 1,
         var.axes=FALSE,
         labels=rownames(Fridd), groups=Fridd.treat)+
  scale_colour_manual(name="Treatment",
                      values= c("forest green", "red3", 
                                "dark blue", "orange"))+
  ggtitle("PCA of Fridd fungal communities")+
  theme_minimal()+
  theme(legend.position = "bottom")


###Just in case

Fridd <- read.csv("./R/FfriddPCA.csv")
Fridd2 <- Fridd[,-1]
rownames(Fridd2) <- Fridd[,1]
