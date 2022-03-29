
library(dplyr)

install.packages("ggplot2", dep=TRUE)
library(ggplot2)
install.packages("ggfortify")
library(ggfortify)
install.packages("ggpubr")
library(ggpubr)

MP <- read.csv("./R/LOL_LEAF.csv")

sumDat <- MP %>% 
  group_by(Treatment) %>%
  summarise(
    meanConc = mean(Concentration, na.rm=TRUE),
    sdConc = sd(Concentration, na.rm=TRUE),
    seConc = (sdConc/(sqrt(14))))

BPM <- ggplot(MP, aes(x = Treatment, y = Concentration, 
                      fill = Treatment, alpha = 0.5))+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  ylab(ex)+
  coord_flip()+
  theme_bw()

BPM + stat_compare_means(label = "p.signif", method = "t.test", 
                         hide.ns = TRUE,
                         ref.group = "Control")+
  theme(legend.position="none")+
  scale_fill_brewer(palette="Paired")



#ylab = expression("P Concentration in dried 0.5 g soil extract µg PO4"^2*"P mL^-1")+  

ex <- expression("P concentration in dried 1 g leaf and root biomass (µg PO"[4]^"3-" *" P ml"^-1*")")

gnoib <- lm(Concentration ~ Treatment, data = MP)

autoplot(gnoib)
anova(gnoib, test = "LRT")
summary(gnoib)

gnoib2_Tukey <- aov(gnoib2)

tukey2_out <- TukeyHSD(gnoib2_Tukey)
tukey2_out
