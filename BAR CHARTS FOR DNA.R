# barcharts

library(ggplot2)
library(hrbrthemes)


ArDNA <- read.csv("./R/C4barDNA.csv")

ardata <- ArDNA#[,1:13]

ggplot(ArDNA, aes(fill=Species, y=Percentage, x=Biostimulant)) + 
  geom_bar(position="stack", stat="identity")+
  facet_wrap(vars(Phosphorus,Sterilised), nrow = 1, ncol = 4)+
  scale_fill_brewer(palette="Set3")+
  theme_ipsum()
  
ArE <- read.csv("./R/ArbEcology.csv")

ggplot(ArE, aes(fill=Ecology, y=Percentage, x=Biostimulant)) + 
  geom_bar(position="stack", stat="identity")+
  facet_wrap(vars(Phosphorus,Condition), nrow = 1, ncol = 4)+
  scale_fill_brewer(palette="Set3")+
  theme_ipsum()

BAS <- read.csv("./R/basARB.csv")

ggplot(BAS, aes(fill=Species, y=Percentage, x=Biostimulant)) + 
  geom_bar(position="stack", stat="identity")+
  facet_wrap(vars(Phosphorus,Sterilised), nrow = 1, ncol = 4)+
  scale_fill_brewer(palette="Set3")+
  theme_ipsum()

ggplot(Psu, aes(fill=Species, y=Percentage, x=Biostimulant)) + 
  geom_bar(position="stack", stat="identity")+
  facet_wrap(vars(Phosphorus,Sterilised), nrow = 1, ncol = 4)+
  scale_fill_brewer(palette="Set3")+
  theme_ipsum()

# Graph
ggplot(ArDNA, aes(fill=Species, y=Value, x=Sterilised)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Fungal commmunity structure") +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("")
