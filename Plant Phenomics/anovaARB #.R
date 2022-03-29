ArDNA <- read.csv("./R/C4barDNA.csv")

Mort <- filter(ArDNA, Species == "Mortierella parvispora")
res.aov <- aov(Percentage ~ Biostimulant, data = Mort)
summary(res.aov)

res.aov <- aov(Percentage ~ Phosphorus, data = Mort)
summary(res.aov)

res.aov <- aov(Percentage ~ Sterilised, data = Mort)
summary(res.aov)


Psu <- filter(ArDNA, Species == "Pseudogymnoascus")
res.aov <- aov(Percentage ~ Biostimulant, data = Psu)
summary(res.aov)
TukeyHSD(res.aov)

res.aov <- aov(Percentage ~ Phosphorus, data = Psu)
summary(res.aov)
res.aov <- aov(Percentage ~ Sterilised, data = Psu)
summary(res.aov)

Psu <- filter(ArDNA, Species == "Sphaerosporella")
res.aov <- aov(Percentage ~ Biostimulant, data = Psu)
summary(res.aov)
res.aov <- aov(Percentage ~ Phosphorus, data = Psu)
summary(res.aov)
res.aov <- aov(Percentage ~ Sterilised, data = Psu)
summary(res.aov)
res.aov <- aov(Value ~ Biostimulant*Phosphorus, data = Psu)
summary(res.aov)
TukeyHSD(res.aov)

Psu <- filter(ArDNA, Species == "Mortierella alpina")
res.aov <- aov(Percentage ~ Biostimulant, data = Psu)
summary(res.aov)
res.aov <- aov(Percentage ~ Phosphorus, data = Psu)
summary(res.aov)
res.aov <- aov(Percentage ~ Sterilised, data = Psu)
summary(res.aov)

Psu <- filter(ArDNA, Species == "OTU 5 Penicillium")
res.aov <- aov(Percentage ~ Biostimulant, data = Psu)
summary(res.aov)
TukeyHSD(res.aov)

res.aov <- aov(Percentage ~ Phosphorus, data = Psu)
summary(res.aov)
res.aov <- aov(Percentage ~ Sterilised, data = Psu)
summary(res.aov)

Psu <- filter(ArDNA, Species == "Chrysosporium")
res.aov <- aov(Percentage ~ Biostimulant, data = Psu)
summary(res.aov)
res.aov <- aov(Percentage ~ Phosphorus, data = Psu)
summary(res.aov)
res.aov <- aov(Percentage ~ Sterilised, data = Psu)
summary(res.aov)

Psu <- filter(ArDNA, Species == "Saitozyma")
res.aov <- aov(Percentage ~ Biostimulant, data = Psu)
summary(res.aov)
TukeyHSD(res.aov)

res.aov <- aov(Percentage ~ Phosphorus, data = Psu)
summary(res.aov)
res.aov <- aov(Percentage ~ Sterilised, data = Psu)
summary(res.aov)

#Combined effect
res.aov <- aov(Percentage ~ Sterilised*Biostimulant, data = Psu)
summary(res.aov)

res.aov <- aov(Percentage ~ Phosphorus*Sterilised*Biostimulant,
               data = Psu)
summary(res.aov)

#penicillium

Psu <- filter(ArDNA, Species == "OTU 14 Penicillium")
res.aov <- aov(Percentage ~ Biostimulant, data = Psu)
summary(res.aov)
res.aov <- aov(Percentage ~ Phosphorus, data = Psu)
summary(res.aov)
res.aov <- aov(Percentage ~ Sterilised, data = Psu)
summary(res.aov)

Psu <- filter(ArDNA, Species == "OTU 12 Penicillium")
res.aov <- aov(Percentage ~ Biostimulant, data = Psu)
summary(res.aov)
TukeyHSD(res.aov)

res.aov <- aov(Percentage ~ Phosphorus, data = Psu)
summary(res.aov)

res.aov <- aov(Percentage ~ Sterilised, data = Psu)
summary(res.aov)

Psu <- filter(ArDNA, Species == "Chromocleista")
res.aov <- aov(Percentage ~ Biostimulant, data = Psu)
summary(res.aov)
res.aov <- aov(Percentage ~ Phosphorus, data = Psu)
summary(res.aov)
res.aov <- aov(Percentage ~ Sterilised, data = Psu)
summary(res.aov)

Psu <- filter(ArDNA, Species == "Trichoderma")
res.aov <- aov(Percentage ~ Biostimulant, data = Psu)
summary(res.aov)
res.aov <- aov(Percentage ~ Phosphorus, data = Psu)
summary(res.aov)
res.aov <- aov(Percentage ~ Sterilised, data = Psu)
summary(res.aov)

#Combined effect
res.aov <- aov(Percentage ~ Biostimulant*Phosphorus, data = Psu)
summary(res.aov)
TukeyHSD(res.aov)


Psu <- filter(ArDNA, Phylum == "Fungi incertae sedis")
res.aov <- aov(Percentage ~ Biostimulant, data = Psu)
summary(res.aov)
res.aov <- aov(Percentage ~ Phosphorus, data = Psu)
summary(res.aov)
res.aov <- aov(Percentage ~ Sterilised, data = Psu)
summary(res.aov)

Psu <- filter(ArDNA, Phylum == "Ascomycota")
res.aov <- aov(Percentage ~ Biostimulant, data = Psu)
summary(res.aov)
res.aov <- aov(Percentage ~ Phosphorus, data = Psu)
summary(res.aov)
res.aov <- aov(Percentage ~ Sterilised, data = Psu)
summary(res.aov)
res.aov <- aov(Value ~ Biostimulant*Phosphorus, data = Psu)
summary(res.aov)
TukeyHSD(res.aov)

ggplot(Psu, aes(fill=Species, y=Percentage, x=Biostimulant)) + 
  geom_bar(position="stack", stat="identity")+
  facet_wrap(vars(Phosphorus,Sterilised), nrow = 1, ncol = 4)+
  scale_fill_brewer(palette="Set3")+
  theme_ipsum()

Psu <- read.csv("./R/basARB.csv")
res.aov <- aov(Percentage ~ Biostimulant, data = Psu)
summary(res.aov)
TukeyHSD(res.aov)

res.aov <- aov(Percentage ~ Phosphorus, data = Psu)
summary(res.aov)
res.aov <- aov(Percentage ~ Sterilised, data = Psu)
summary(res.aov)

Div <- read.csv("./R/DiversityARB.csv")

DivA <- filter(Div, Ecology == "Fungi Total")
res.aov <- aov(Value ~ Biostimulant, data = DivA)
summary(res.aov)
res.aov <- aov(Value ~ Phosphorus, data = DivA)
summary(res.aov)
res.aov <- aov(Value ~ Sterilised, data = DivA)
summary(res.aov)
res.aov <- aov(Value ~ Sterilised*Phosphorus, data = DivA)
summary(res.aov)
TukeyHSD(res.aov)

ggplot(DivA, aes(fill=Ecology, y=Value, x=Biostimulant)) + 
  geom_bar(position="stack", stat="identity")+
  facet_wrap(vars(Phosphorus,Sterilised), nrow = 1, ncol = 4)+
  scale_fill_brewer(palette="Set3")+
  theme_ipsum()

#Inverse Simpson Index (Taxa)	

DivA <- filter(Div, Ecology == "Inverse Simpson Index (Taxa)")
res.aov <- aov(Value ~ Biostimulant, data = DivA)
summary(res.aov)
res.aov <- aov(Value ~ Phosphorus, data = DivA)
summary(res.aov)
res.aov <- aov(Value ~ Sterilised, data = DivA)
summary(res.aov)
res.aov <- aov(Value ~ Biostimulant*Phosphorus, data = DivA)
summary(res.aov)
TukeyHSD(res.aov)

ggplot(DivA, aes(fill=Ecology, y=Value, x=Biostimulant)) + 
  geom_bar(position="stack", stat="identity")+
  facet_wrap(vars(Phosphorus,Sterilised), nrow = 1, ncol = 4)+
  scale_fill_brewer(palette="Set3")+
  theme_ipsum()

#Shannon Index (Taxa)	

DivA <- filter(Div, Ecology == "Shannon Index (Taxa)")
res.aov <- aov(Value ~ Biostimulant, data = DivA)
summary(res.aov)
res.aov <- aov(Value ~ Phosphorus, data = DivA)
summary(res.aov)
res.aov <- aov(Value ~ Sterilised, data = DivA)
summary(res.aov)
res.aov <- aov(Value ~ Biostimulant*Phosphorus, data = DivA)
summary(res.aov)
TukeyHSD(res.aov)

ggplot(DivA, aes(fill=Ecology, y=Value, x=Biostimulant)) + 
  geom_bar(position="stack", stat="identity")+
  facet_wrap(vars(Phosphorus,Sterilised), nrow = 1, ncol = 4)+
  scale_fill_brewer(palette="Set3")+
  theme_ipsum()

#Taxa Count

DivA <- filter(Div, Ecology == "Taxa Count")
res.aov <- aov(Value ~ Biostimulant, data = DivA)
summary(res.aov)
res.aov <- aov(Value ~ Phosphorus, data = DivA)
summary(res.aov)
res.aov <- aov(Value ~ Sterilised, data = DivA)
summary(res.aov)
res.aov <- aov(Value ~ Biostimulant*Phosphorus, data = DivA)
summary(res.aov)
TukeyHSD(res.aov)

ggplot(DivA, aes(fill=Ecology, y=Value, x=Biostimulant)) + 
  geom_bar(position="stack", stat="identity")+
  facet_wrap(vars(Phosphorus,Sterilised), nrow = 1, ncol = 4)+
  scale_fill_brewer(palette="Set3")+
  theme_ipsum()

#	OTU Count

DivA <- filter(Div, Ecology == "OTU Count")
res.aov <- aov(Value ~ Biostimulant, data = DivA)
summary(res.aov)
res.aov <- aov(Value ~ Phosphorus, data = DivA)
summary(res.aov)
res.aov <- aov(Value ~ Sterilised, data = DivA)
summary(res.aov)
res.aov <- aov(Value ~ Biostimulant*Phosphorus, data = DivA)
summary(res.aov)
TukeyHSD(res.aov)

ggplot(DivA, aes(fill=Ecology, y=Value, x=Biostimulant)) + 
  geom_bar(position="stack", stat="identity")+
  facet_wrap(vars(Phosphorus,Sterilised), nrow = 1, ncol = 4)+
  scale_fill_brewer(palette="Set3")+
  theme_ipsum()

#ArE
ArEc <- filter(ArE, Ecology == "Saprophyte")
res.aov <- aov(Value ~ Biostimulant, data = ArEc)
summary(res.aov)
res.aov <- aov(Value ~ Phosphorus, data = ArEc)
summary(res.aov)
res.aov <- aov(Value ~ Condition, data = ArEc)
summary(res.aov)
res.aov <- aov(Value ~ Biostimulant*Phosphorus, data = ArEc)
summary(res.aov)
TukeyHSD(res.aov)
