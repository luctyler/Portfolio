#Fridd Glomermycota

library(ggbiplot)

Fridd <- read.csv("./R/FriddGlo.csv")

Fridd.pca <- prcomp(Fridd, center = TRUE,scale. = TRUE)
summary(Fridd.pca)
ggbiplot(Fridd.pca)
Fridd.treat <- c(rep("No input", 4), rep("Fertiliser and grazing", 4), 
                 rep("Rotational grazing", 4), rep("Fertiliser", 4))

ggbiplot(Fridd.pca,ellipse=TRUE, obs.scale = 1, var.scale = 1,
         var.axes=FALSE,
         labels=rownames(Fridd), groups=Fridd.treat)+
  scale_colour_manual(name="Treatment",
                      values= c("forest green", "red3", 
                                "dark blue", "orange"))+
  ggtitle("PCA of Fridd Glomermycota")+
  theme_minimal()+
  theme(legend.position = "bottom")

