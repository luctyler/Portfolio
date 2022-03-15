library(dplyr)
library(ggplot2)
library(ggfortify)

WMass <- read.csv("./R/Wet mass data.csv")

sumDat <- WMass %>% 
  group_by(Treatment) %>%
  summarise(
    meanMass = mean(Wet_mass, na.rm=TRUE),
    sdMass = sd(Wet_mass, na.rm=TRUE),
    seMass = (sdMass/(sqrt(10))))

BP1 <- ggplot(WMass, aes(x = Treatment, y = Wet_mass, 
                         fill = Treatment, alpha = 0.5))+
  ylab("Mean wet mass (g)")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()

BP1 + theme(legend.position="none")