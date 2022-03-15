library(dplyr)
library(ggplot2)
library(ggfortify)

Lolium <- read.csv("./R/LoliumNew.csv")

## the "na.rm=TRUE" code stops R from including missing values in analyses

sumDat <- Lolium %>% 
  group_by(Day, Treatment) %>%
  summarise(
    meanArea = mean(Perimeter_.mm., na.rm=TRUE),
    sdArea = sd(Perimeter_.mm., na.rm=TRUE),
    seArea = (sdArea/(sqrt(12))))

week1 <- sumDat[1:48,]
week2 <- sumDat[49:108,]
week3 <- sumDat[109:180,]

#Boxplots

#WEEK 1

BP1 <- ggplot(week1, aes(x = Treatment, y = meanArea, 
                         fill = Treatment, alpha = 0.5))+
  ylab("Mean plant perimeter (mm)")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()

BP1 + theme(legend.position="none")+ 
  scale_fill_brewer(palette="Paired")

#WEEK2

BP2 <- ggplot(week2, aes(x = Treatment, y = meanArea, 
                         fill = Treatment, alpha = 0.5))+
  ylab("Mean plant perimeter (mm)")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()

BP2 + theme(legend.position="none")+
  scale_fill_brewer(palette="Paired")


#WEEK3

BP3 <- ggplot(week3, 
              aes(x = Treatment, y = meanArea, fill = Treatment, alpha = 0.5))+
  ylab("Mean plant perimeter (mm)")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()

BP3 + stat_compare_means(label = "p.signif", method = "t.test", hide.ns = TRUE,
                         ref.group = "Control")+
  theme(legend.position="none")+
  scale_fill_brewer(palette="Paired")

#Statistical analysis

gnoib <- lm(meanArea ~ Treatment, data = sumDat)

autoplot(gnoib)
anova(gnoib, test = "LRT")
summary(gnoib)

#Week1 

gnoib1 <- lm(meanArea ~ Treatment, data = week1)

autoplot(gnoib1)
anova(gnoib1, test = "LRT")
summary(gnoib1)

#Week 2

gnoib2 <- lm(meanArea ~ Treatment, data = week2)

autoplot(gnoib2)
anova(gnoib2, test = "LRT")
summary(gnoib2)

#Week3

gnoib3 <- lm(meanArea ~ Treatment, data = week3)

autoplot(gnoib3)
anova(gnoib3, test = "LRT")
summary(gnoib3)

#Loess

CRV <- ggplot(Lolium, aes(x = Day, y = Perimeter_.mm.,
                          col = Treatment))+
  ylab("Mean plant perimeter (mm)")+
  geom_smooth(method = "loess", se = FALSE, span = 2)+
  theme_classic()

CRV + scale_colour_hue(l = 55, h = c(0, 270))
