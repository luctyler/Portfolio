#Fridd PCA Simplified

library(ggbiplot)

Fridd <- read.csv("./R/FriddPCAe.csv")
Fridd2 <- Fridd[,-1]
rownames(Fridd2) <- Fridd[,1]


Fridd.pca <- prcomp(Fridd2, center = TRUE,scale. = TRUE)
summary(Fridd.pca)
ggbiplot(Fridd.pca)

ggbiplot(Fridd.pca, obs.scale = 0.1, var.scale = 0.1)+
  ggtitle("PCA of Fridd fungi communities")+
  theme_minimal()

ggbiplot(Fridd.pca,ellipse=TRUE, choices=c(3,4),
         obs.scale = 1, var.scale = 1,
         var.axes=FALSE,
         labels=rownames(Fridd), groups=Fridd.treat)+
  scale_colour_manual(name="Treatment",
                      values= c("forest green", "red3", 
                                "dark blue", "orange"))+
  ggtitle("PCA of Fridd Ecology")+
  theme_minimal()+
  theme(legend.position = "bottom")

#Tim's stuff

species <- read.csv("./R/samples.csv")

sp_NMDS=metaMDS(species, k=20, trymax=175)

SP <- ordiplot(sp_NMDS,type="n", xlab = NULL, ylab = NULL)

treat=c(rep("Chestnut enriched", 4),rep("Chestnut non-enriched", 4), rep("Chestnut sand", 4), rep("Birch enriched", 3),rep("Birch non-enriched", 3), rep("Birch sand", 4))
treat=c(rep("CE", 4),rep("CNE", 4), rep("CS", 4), rep("BE", 3),rep("BNE", 3), rep("BS", 4))

SP + ordihull(sp_NMDS,groups=treat,draw="polygon", col=1:6,
              label=TRUE, cex=0.5, border = FALSE, bg = FALSE)

##All done?

#another attempt

tim <- read.csv("./R/tim.csv")

tim_NMDS=metaMDS(tim, k=2, trymax=100)
tim2_NMDS=metaMDS(tim, k=3, trymax=100)
tim3_NMDS=metaMDS(tim, k=10, trymax=200)

plot(tim_NMDS)
plot(tim2_NMDS)
new <- plot(tim3_NMDS)

tm <- ordiplot(tim3_NMDS,type="n", xlab = NULL, ylab = NULL)

new + ordihull(tim3_NMDS,groups=treat,draw="polygon", col=1:6,
              label=TRUE, cex=0.5, border = TRUE, bg = FALSE)



?metaMDS

ordihull(sp_NMDS,groups=treat,draw="polygon",col=1:6,
         label=TRUE, offset = 0.1, border = TRUE, bg = FALSE)
capscale(species ~ 1, dist="bray", metaMDS = TRUE)

colors=c(rep("red",4), rep("blue",4), rep("green",4),
         rep("yellow",3), rep("forest green",3), rep("orange"4))
ordiplot(sp_NMDS,type="n")
for(i in unique(treat)) {
  ordihull(sp_NMDS$point[grep(i,treat),],draw="polygon",
           groups=treat[treat==i],label=TRUE) } 
orditorp(Fridd_NMDS,display="species",col="red",air=0.01)
orditorp(Fridd_NMDS,display="sites",col=c(rep("green",4),rep("blue",4),
                                          rep("red",4),rep("yellow",4)),air=0.01,cex=1.25)

No_inputs <- Fridd_NMDS[1:4,]
Ferts_and_grazing <- Fridd_NMDS[5:8,]
Rotational_grazing <- Fridd_NMDS[9:12,]
Ferts_only <- Fridd_NMDS[13:16,]






#PCA

Tim.pca <- prcomp(species, center = TRUE,scale. = TRUE)
summary(Tim.pca)
ggbiplot(Tim.pca)

ggbiplot(Tim.pca, obs.scale = 0.1, var.scale = 0.1)+
  ggtitle("PCA of fungal communities")+
  theme_minimal()

ggbiplot(Tim.pca,ellipse=TRUE, choices=c(1,2),
         obs.scale = 1, var.scale = 1,
         var.axes=FALSE,
         labels=rownames(species), groups=treat)+
  scale_colour_manual(name="Treatment",
                      values= c("forest green", "red3", "yellow",
                                "dark blue", "orange", "green"))+
  ggtitle("PCA of Fungal Communities")+
  theme_minimal()+
  theme(legend.position = "bottom")

