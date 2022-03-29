# Fridd DNA

library(ggplot2)
library(hrbrthemes)

SIP <- read.csv("./R/SIPGen.csv")

ggplot(SIP, aes(fill=Genus, y=Percentage, x=Code)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_brewer(palette="Set3")+
  theme_ipsum()

SIP <- read.csv("./R/SIPEco.csv")



ggplot(SIP, aes(fill=Ecology, y=Percentage, x=Code)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_brewer(palette="Set3")+
  theme_ipsum()

#Pathogens

SIPE <- read.csv("./R/SAPSIP.csv")

#for future ref
nb.cols <- 18
mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)

ggplot(SIPE, aes(fill=Genus, y=Percentage, x=Treatment)) + 
  ylab("Percentage %")+
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = mycolors) +
  theme_ipsum()


res.aov <- aov(Percentage ~ Nutrient*Stocking, data = SIPE)
summary(res.aov)
TukeyHSD(res.aov)


Psu <- filter(SIPE, Ecology == "Smut")
res.aov <- aov(Percentage ~ Treatment, data = Psu)
summary(res.aov)
TukeyHSD(res.aov)

Psu <- filter(SIPE, Genus == "Cochliobolus")
res.aov <- aov(Percentage ~ Nutrient*Stocking, data = Psu)
summary(res.aov)

Psu <- filter(SIPE, Genus == "Entyloma")
res.aov <- aov(Percentage ~ Nutrient*Stocking, data = Psu)
summary(res.aov)

Psu <- filter(SIPE, Genus == "Kriegeria")
res.aov <- aov(Percentage ~ Nutrient*Stocking, data = Psu)
summary(res.aov)

Psu <- filter(SIPE, Genus == "Physoderma")
res.aov <- aov(Percentage ~ Nutrient*Stocking, data = Psu)
summary(res.aov)

#AMF

SIPAMF <- read.csv("./R/SIPAMF.csv")

library(RColorBrewer)
# Define the number of colors you want
nb.cols <- 15
mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)

ggplot(SIPAMF, aes(fill=Genus, y=Percentage, x=Treatment)) + 
  ylab("Percentage %")+
  geom_bar(position="stack", stat="identity")+
  scale_fill_brewer(palette="Set3")+
    theme_ipsum()

res.aov <- aov(Percentage ~ Treatment, data = SIPAMF)
summary(res.aov)
TukeyHSD(res.aov)

res.aov <- aov(Percentage ~ Nutrient*Stocking, data = SIPAMF)
summary(res.aov)

Psu <- filter(SIPAMF, Genus == "Acaulospora")
res.aov <- aov(Percentage ~ Nutrient*Stocking, data = Psu)
summary(res.aov)

Psu <- filter(SIPAMF, Genus == "Ambispora")
res.aov <- aov(Percentage ~ Nutrient*Stocking, data = Psu)
summary(res.aov)

Psu <- filter(SIPAMF, Genus == "Claroideoglomus")
res.aov <- aov(Percentage ~ Nutrient*Stocking, data = Psu)
summary(res.aov)

Psu <- filter(SIPAMF, Genus == "Paraglomus")
res.aov <- aov(Percentage ~ Nutrient*Stocking, data = Psu)
summary(res.aov)

#yeast

Y <- read.csv("./R/YeastSIP.csv")

ggplot(Y, aes(fill=Genus, y=Percentage, x=Treatment)) + 
  ylab("Percentage %")+
  geom_bar(position="stack", stat="identity")+
  scale_fill_brewer(palette="Set3")+
  theme_ipsum()

res.aov <- aov(Percentage ~ Nutrient*Stocking, data = Y)
summary(res.aov)

Psu <- filter(Y, Genus == "Solicoccozyma")
res.aov <- aov(Percentage ~ Nutrient*Stocking, data = Psu)
summary(res.aov)
TukeyHSD(res.aov)

Psu <- filter(Y, Genus == "Sporobolomyces")
res.aov <- aov(Percentage ~ Nutrient*Stocking, data = Psu)
summary(res.aov)
TukeyHSD(res.aov)

DSE <- read.csv("./R/DSESIP.csv")

ggplot(DSE, aes(fill=Genus, y=Value, x=Treatment)) + 
  ylab("Percentage %")+
  geom_bar(position="stack", stat="identity")+
  scale_fill_brewer(palette="Set3")+
  theme_ipsum()

res.aov <- aov(Value ~ Nutrient*Stocking, data = DSE)
summary(res.aov)

Psu <- filter(DSE, Genus == "Sorocybe")
res.aov <- aov(Percentage ~ Nutrient*Stocking, data = Psu)
summary(res.aov)

Psu <- filter(DSE, Genus == "Lachnum")
res.aov <- aov(Percentage ~ Nutrient*Stocking, data = Psu)
summary(res.aov)

Psu <- filter(DSE, Genus == "Veronaea")
res.aov <- aov(Percentage ~ Nutrient*Stocking, data = Psu)
summary(res.aov)

Psu <- filter(DSE, Genus == "Tetracladium")
res.aov <- aov(Percentage ~ Nutrient*Stocking, data = Psu)
summary(res.aov)

Psu <- filter(DSE, Genus == "Graddonia")
res.aov <- aov(Percentage ~ Nutrient*Stocking, data = Psu)
summary(res.aov)

#Saprobes
library(vegan)
SAP <- read.csv("./R/SapNMDS.csv")
ar.matrix<-as.matrix(SAP[,4:43])
ar.mat<-sqrt(ar.matrix)#square root transform
ar.dist<-vegdist(ar.mat, method='bray')
set.seed(36) #reproducible results
ar.div<-adonis2(ar.dist~Treatment, data=SAP, 
                permutations = 999, method="bray", strata="PLOT")
ar.div
dispersion2<-betadisper(ar.dist, group=SAP$Treatment)
permutest(dispersion2)
plot(dispersion2, hull=FALSE, ellipse=TRUE, label = FALSE)
#label.cex = 0.75, colour = NA)##sd ellipse
sapMDS<-metaMDS(ar.mat, distance="bray", 
               k=2, trymax=35, autotransform=TRUE) ##k is the number of dimensions
sapMDS ##metaMDS takes eaither a distance matrix or your community matrix (then requires method for 'distance=')

stressplot(sapMDS)

NMDS1 <- sapMDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- sapMDS$points[,2]
sap.plot<-cbind(SAP, NMDS1, NMDS2)

#plot ordination

s.plot <- sap.plot[,-1]
s.plot <- s.plot[,-1]
s.plot <- s.plot[,-1]

p<-ggplot(sap.plot, aes(NMDS1, NMDS2, color=Treatment))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points
  stat_ellipse(type='t',size =1)+ ##draws 95% confidence interval ellipses
  theme_minimal()
p

Sapm <- read.csv("./R/sapm.csv")

res.aov <- aov(Mean ~ Nutrient*Stocking, data = Sapm)
summary(res.aov)
TukeyHSD(res.aov)

Psu <- filter(DSE, Genus == "Sorocybe")
res.aov <- aov(Percentage ~ Nutrient*Stocking, data = Psu)
summary(res.aov)


#General
library(vegan)
SIPG <- read.csv("./R/SIP.csv")
ar.matrix<-as.matrix(SIPG[,4:950])
ar.mat<-sqrt(ar.matrix)#square root transform
ar.dist<-vegdist(ar.mat, method='bray', na.rm = FALSE)
set.seed(36) #reproducible results
ar.div<-adonis2(ar.dist~Treatment, data=SIPG, 
                permutations = 999, method="bray", strata="PLOT")
ar.div
dispersion2<-betadisper(ar.dist, group=SIPG$Stocking)
permutest(dispersion2)
plot(dispersion2, hull=FALSE, ellipse=TRUE, label.cex = 0.75)
#label.cex = 0.75, colour = NA)##sd ellipse
sipMDS<-metaMDS(ar.mat, distance="bray", 
                k=2, trymax=35, autotransform=TRUE) ##k is the number of dimensions
sipMDS ##metaMDS takes eaither a distance matrix or your community matrix (then requires method for 'distance=')

stressplot(sipMDS)

NMDS1 <- sipMDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- sipMDS$points[,2]
sip.plot<-cbind(SIPG, NMDS1, NMDS2)

#plot ordination

s.plot <- sip.plot[,-3]
s.plot <- s.plot[,-1]
s.plot <- s.plot[,-1]

p<-ggplot(sip.plot, aes(NMDS1, NMDS2, color=Treatment))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points
  stat_ellipse(type='t',size =1)+ ##draws 95% confidence interval ellipses
  theme_minimal()
p

#General ecology

frec <- read.csv("./R/frec.csv")

ggplot(frec, aes(fill=Ecology, y=Percentage, x=Treatment)) + 
  ylab("Cumulative Percentage %")+
  geom_bar(position="stack", stat="identity", color = FALSE)+
  scale_fill_brewer(palette="Set3")+
  theme_ipsum()

res.aov <- aov(Value ~ Treatment, data = frec)
summary(res.aov)
TukeyHSD(res.aov)

Psu <- filter(frec, Ecology == "Saprobes")
res.aov <- aov(Value ~ Nutrient*Stocking, data = frec)
summary(res.aov)

res.aov <- aov(Percentage ~ Nutrient*Stocking, data = Psu)
summary(res.aov)

sc <- read.csv("./R/sipcheg.csv")

ggplot(sc, aes(fill=Genus, y=Percentage, x=Treatment)) + 
  ylab("Percentage %")+
  geom_bar(position="stack", stat="identity", color = FALSE)+
  scale_fill_brewer(palette="Set3")+
  theme_ipsum()

res.aov <- aov(Value ~ Treatment, data = sc)
summary(res.aov)
TukeyHSD(res.aov)

Psu <- filter(sc, Genus == "Cuphophyllus CU4")
res.aov <- aov(Value ~ Nutrient*Stocking, data = Psu)
summary(res.aov)

Psu <- filter(sc, Genus == "Clavulinopsis CPLA")
res.aov <- aov(Value ~ Nutrient*Stocking, data = Psu)
summary(res.aov)

##Sterols

st <- read.csv("./R/sterols.csv")

ggplot(st, aes(fill=Sterol, y=Pergentage, x=Treatment)) + 
  ylab("mg/ml of sterol")+
  geom_bar(position="stack", stat="identity", color = FALSE)+
  scale_fill_brewer(palette="Set3")+
  theme_ipsum()


res.aov <- aov(Value ~ Treatment, data = st)
summary(res.aov)
TukeyHSD(res.aov)

Psu <- filter(st, Sterol == "Sistosterol")
res.aov <- aov(Value ~ Nutrient*Stocking, data = Psu)
summary(res.aov)
TukeyHSD(res.aov)

##Mortierella

sm <- read.csv("./R/sipmor.csv")

ggplot(sm, aes(fill=Genus, y=Log, x=Treatment)) + 
  ylab("Mortierella abunance %")+
  geom_bar(position="stack", stat="identity", color = FALSE)+
  scale_fill_brewer(palette="Set3")+
  theme_ipsum()

res.aov <- aov(Log ~ Treatment, data = sm)
summary(res.aov)
TukeyHSD(res.aov)
#
res.aov <- aov(Log ~ Nutrient*Stocking, data = sm)
summary(res.aov)
TukeyHSD(res.aov)

#order

so <- read.csv("./R/sipo.csv")

ggplot(Psu, aes(fill=Order, y=Log, x=Treatment)) + 
  ylab("Percentage abunance %")+
  geom_bar(position="stack", stat="identity", color = FALSE)+
  scale_fill_brewer(palette="Set3")+
  theme_ipsum()

Psu <- filter(so, Order == "Pezizales")

res.aov <- aov(Log ~ Nutrient*Stocking, data = Psu)
summary(res.aov)
TukeyHSD(res.aov)
#
Psu <- filter(so, Order == "Pleosporales")

res.aov <- aov(Value ~ Nutrient*Stocking, data = Psu)
summary(res.aov)
TukeyHSD(res.aov)
