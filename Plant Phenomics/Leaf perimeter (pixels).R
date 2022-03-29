library(dplyr)
library(ggplot2)
library(ggfortify)

Lolium <- read.csv("./R/Lolium.csv")

## the "na.rm=TRUE" code stops R from including missing values in analyses

sumDat <- Lolium %>% 
  group_by(Day, Treatment) %>%
  summarise(
    meanArea = mean(Perimeter_.pixels., na.rm=TRUE),
    sdArea = sd(Perimeter_.pixels., na.rm=TRUE),
    seArea = (sdArea/(sqrt(12))))

week1 <- sumDat[1:24,]
week2 <- sumDat[13:84,]
week3 <- sumDat[73:156,]
week4 <- sumDat[145:192,]

#Boxplots

#WEEK 1

BP1 <- ggplot(week1, aes(x = Treatment, y = meanArea, 
                         fill = Treatment, alpha = 0.5))+
  ylab("Mean plant perimeter (pixels)")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()

BP1 + theme(legend.position="none")+ 
  scale_fill_brewer(palette="Set3")

#WEEK2

BP2 <- ggplot(week2, aes(x = Treatment, y = meanArea, 
                         fill = Treatment, alpha = 0.5))+
  ylab("Mean plant perimeter (pixels)")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()

BP2 + theme(legend.position="none")+
  scale_fill_brewer(palette="Set3")


#WEEK3

BP3 <- ggplot(week3, aes(x = Treatment, y = meanArea, fill = Treatment, alpha = 0.5))+
  ylab("Mean plant perimeter (pixels)")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()

BP3 + theme(legend.position="none")+
  scale_fill_brewer(palette="Set3")


#WEEK4

BP4 <- ggplot(week4, aes(x = Treatment, y = meanArea, fill = Treatment, alpha = 0.5))+
  ylab("Mean plant perimeter (pixels)")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()

BP4 + theme(legend.position="none")+
  scale_fill_brewer(palette="Set3")



#Statistical analysis

gnoib <- lm(meanArea ~ Treatment, data = week3)

autoplot(gnoib)
anova(gnoib, test = "LRT")
summary(gnoib)

