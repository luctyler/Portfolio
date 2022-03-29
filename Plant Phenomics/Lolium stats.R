library(dplyr)
library(ggplot2)
library(ggfortify)


Lolium <- read.csv("./R/Lolium.csv")

## the "na.rm=TRUE" code stops R from including missing values in analyses

sumDat <- Lolium %>% 
  group_by(Day, Treatment) %>%
  summarise(
  meanArea = mean(Leaf_area_.pixels., na.rm=TRUE),
  sdArea = sd(Leaf_area_.pixels., na.rm=TRUE),
  seArea = (sdArea/(sqrt(12))))


day1 <- sumDat[1:12,]
day5 <- sumDat[13:24,]
day6 <- sumDat[25:36,]
day7 <- sumDat[37:48,]
day8 <- sumDat[49:60,]
day9 <- sumDat[61:72,]
day11 <- sumDat[73:84,]
day13 <- sumDat[85:96,]
day14 <- sumDat[97:108,]
day15 <- sumDat[109:120,]
day16 <- sumDat[121:132,]
day17 <- sumDat[133:144,]
day18 <- sumDat[145:156,]
day19 <- sumDat[157:168,]
day20 <- sumDat[169:180,]
day21 <- sumDat[181:192,]

week1 <- sumDat[1:24,]
week2 <- sumDat[13:84,]
week3 <- sumDat[73:156,]
week4 <- sumDat[145:192,]

#Boxplots

ggplot(week1, aes(x = Treatment, y = meanArea, fill = Treatment, alpha = 0.5))+
  ylab("Mean leaf area (pixels)")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()


ggplot(week2, aes(x = Treatment, y = meanArea, fill = Treatment, alpha = 0.5))+
  ylab("Mean leaf area (pixels)")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()

ggplot(week3, aes(x = Treatment, y = meanArea, fill = Treatment, alpha = 0.5))+
  ylab("Mean leaf area (pixels)")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()

ggplot(week4, aes(x = Treatment, y = meanArea, fill = Treatment, alpha = 0.5))+
  ylab("Mean leaf area (pixels)")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()

#Statistical analysis

gnoib <- lm(meanArea ~ Treatment, data = week3)

autoplot(gnoib)
anova(gnoib, test = "LRT")
summary(gnoib)

