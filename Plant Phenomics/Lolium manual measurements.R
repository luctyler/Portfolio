library(dplyr)
library(ggplot2)
library(ggfortify)
library("ggpubr")

DMass <- read.csv("./R/Manual measurements.csv")

sumDat <- DMass %>% 
  group_by(Treatment) %>%
  summarise(
    meanMass = mean(Dry_mass, na.rm=TRUE),
    sdMass = sd(Dry_mass, na.rm=TRUE),
    seMass = (sdMass/(sqrt(10))))

BP1 <- ggplot(WMass, aes(x = Treatment, y = Wet_mass, 
                         fill = Treatment, alpha = 0.5))+
  ylab("Mean wet mass (g)")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()

BP1 + theme(legend.position="none")

BP2 <- ggplot(DMass, aes(x = Treatment, y = Height, 
                         fill = Treatment, alpha = 0.5))+
  ylab("Mean height (mm)")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()

BP2 + theme(legend.position="none")

BP3 <- ggplot(DMass, aes(x = Treatment, y = Dry_mass, 
                         fill = Treatment, alpha = 0.5))+
  ylab("Dry mass (g)")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()

BP3 + stat_compare_means(label = "p.signif", method = "t.test", hide.ns = TRUE,
                         ref.group = "Control")+
  theme(legend.position="none")+
  scale_fill_brewer(palette="Paired")

#Height

sumDat <- DMass %>% 
  group_by(Treatment) %>%
  summarise(
    meanH = mean(Height, na.rm=TRUE),
    sdH = sd(Height, na.rm=TRUE),
    seH = (sdH/(sqrt(10))))

BPH <- ggplot(DMass, aes(x = Treatment, y = Height, 
                         fill = Treatment, alpha = 0.5))+
  ylab("Height (mm)")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()

BPH + stat_compare_means(label = "p.signif", method = "t.test", hide.ns = TRUE,
                         ref.group = "Control")+
  theme(legend.position="none")+
  scale_fill_brewer(palette="Paired")


#Stats

gnoib <- lm(Dry_mass ~ Treatment, data = WMass)

autoplot(gnoib)
anova(gnoib, test = "LRT")
summary(gnoib)

gnoib1 <- lm(Wet_mass ~ Treatment, data = WMass)

autoplot(gnoib1)
anova(gnoib1, test = "LRT")
summary(gnoib1)

gnoib2 <- lm(Height ~ Treatment, data = WMass)

autoplot(gnoib2)
anova(gnoib2, test = "LRT")
summary(gnoib2)

gnoib2_Tukey <- aov(gnoib2)

tukey2_out <- TukeyHSD(gnoib2_Tukey)
tukey2_out
