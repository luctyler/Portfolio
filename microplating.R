library(dplyr)
library(ggplot2)
library(ggfortify)
library(vegan)
library(ggbiplot)

MP <- read.csv("./R/Microplate.csv")

sumDat <- MP %>% 
  group_by(Treatment) %>%
  summarise(
    meanConc = mean(Concentration, na.rm=TRUE),
    sdConc = sd(Concentration, na.rm=TRUE),
    seConc = (sdConc/(sqrt(12))))

BP1 <- ggplot(MP, aes(x = Treatment, y = Concentration, 
                      fill = Treatment, alpha = 0.5))+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  ylab(ex)+
  coord_flip()+
  theme_bw()

BP1 + theme(legend.position="none")+ 
  scale_fill_brewer(palette="Set3")

#ylab = expression("P Concentration in dried 0.5 g soil extract µg PO4"^2*"P mL^-1")+  

ex <- expression("P concentration in dried 0.5 g soil extract (µg PO"[4]^-1 *"P mL"^-1*")")

gnoib <- lm(Concentration ~ Treatment, data = MP)

autoplot(gnoib)
anova(gnoib, test = "LRT")
summary(gnoib)

gnoib2_Tukey <- aov(gnoib2)

tukey2_out <- TukeyHSD(gnoib2_Tukey)
tukey2_out