library(dplyr)
library(ggplot2)
library(ggfortify)

PSImass <- read.csv("./R/PSI mass data.csv")

sumDat <- PSImass %>% 
  group_by(Day, Plant.Info) %>%
  summarise(
    meanDry = mean(Weight, na.rm=TRUE),
    sdDry = sd(Weight, na.rm=TRUE),
    seDry = (sdDry/(sqrt(12))))

April <- sumDat[1:48,]
May <- sumDat[49:408,]
June <- sumDat[409:636,]

BPDRY <- ggplot(sumDat, aes(x = Plant.Info, y = meanDry, 
                         fill = Plant.Info, alpha = 0.5))+
  ylab("Mean plant mass (g)")+
  xlab("Treatment")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()

BPDRY + theme(legend.position="none")+ 
  scale_fill_brewer(palette="Set3")

#April

BPDRYAP <- ggplot(April, aes(x = Plant.Info, y = meanDry, 
                            fill = Plant.Info, alpha = 0.5))+
  ylab("Mean plant mass (g)")+
  xlab("Treatment")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()

BPDRYAP + theme(legend.position="none")+ 
  scale_fill_brewer(palette="Set3")

#May

BPDRYMA <- ggplot(May, aes(x = Plant.Info, y = meanDry, 
                             fill = Plant.Info, alpha = 0.5))+
  ylab("Mean plant mass (g)")+
  xlab("Treatment")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()

BPDRYMA + theme(legend.position="none")+ 
  scale_fill_brewer(palette="Set3")

# June

BPDRYJU <- ggplot(June, aes(x = Plant.Info, y = meanDry, 
                             fill = Plant.Info, alpha = 0.5))+
  ylab("Mean plant mass (g)")+
  xlab("Treatment")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()

BPDRYJU + theme(legend.position="none")+ 
  scale_fill_brewer(palette="Set3")

#LOESS

CRV <- ggplot(PSImass, aes(x = Day, y = Weight,
                          col = Plant.Info))+
  ylab("Mean plant mass (g)")+
  geom_smooth(method = "loess", se = FALSE, size = 1)+
  theme_classic()

CRV + scale_colour_hue(l = 50, h = c(0, 270))

## WETMASS

