library(dplyr)
library(ggplot2)
library(ggfortify)

Lolium <- read.csv("./R/LoliumNew.csv")

## the "na.rm=TRUE" code stops R from including missing values in analyses

sumDat <- Lolium %>% 
  group_by(Day, Treatment) %>%
  summarise(
    meanComp = mean(Compactness, na.rm=TRUE),
    sdComp = sd(Compactness, na.rm=TRUE),
    seComp = (sdComp/(sqrt(12))))

week1 <- sumDat[1:48,]
week2 <- sumDat[49:108,]
week3 <- sumDat[109:180,]

#Boxplots

#WEEK 1

BP1 <- ggplot(week1, aes(x = Treatment, y = meanComp, 
                         fill = Treatment, alpha = 0.5))+
  ylab("Mean compactness score")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()

BP1 + theme(legend.position="none")+ 
  scale_fill_brewer(palette="Set3")

#WEEK2

BP2 <- ggplot(week2, aes(x = Treatment, y = meanComp, 
                         fill = Treatment, alpha = 0.5))+
  ylab("Mean compactness score")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()

BP2 + theme(legend.position="none")+
  scale_fill_brewer(palette="Set3")


#WEEK3

BP3 <- ggplot(week3, aes(x = Treatment, y = meanComp, 
                         fill = Treatment, alpha = 0.5))+
  ylab("Mean compactness score")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+
  theme_bw()

BP3 + stat_compare_means(label = "p.signif", method = "t.test", hide.ns = TRUE,
                         ref.group = "Control")+
  theme(legend.position="none")+
  scale_fill_brewer(palette="Paired")


#Statistical analysis

# gnoib <- lm(Compactness ~ Treatment, data = Lolium)

gnoib <- glm(Compactness ~ Treatment, data = Lolium, 
             family = poisson(link = log))

autoplot(gnoib)
anova(gnoib, test = "LRT")
summary(gnoib)

gnoib_Tukey <- aov(gnoib)

tukey_out <- TukeyHSD(gnoib_Tukey)
tukey_out

#Week1 

gnoib1 <- glm(meanComp ~ Treatment, data = week1, 
              family = poisson(link = log))

autoplot(gnoib1)
anova(gnoib1, test = "LRT")
summary(gnoib1)

gnoib1_Tukey <- aov(gnoib1)

tukey_out1 <- TukeyHSD(gnoib1_Tukey)
tukey_out1


#Week 2

gnoib2 <- glm(meanComp ~ Treatment, data = week2, 
              family = poisson(link = log))

autoplot(gnoib2)
anova(gnoib2, test = "LRT")
summary(gnoib2)

gnoib2_Tukey <- aov(gnoib2)

tukey_out2 <- TukeyHSD(gnoib2_Tukey)
tukey_out2

#Week3

gnoib3 <- glm(meanComp ~ Treatment, data = week3, 
              family = poisson(link = log))

autoplot(gnoib3)
anova(gnoib3, test = "LRT")
summary(gnoib3)

gnoib3_Tukey <- aov(gnoib3)

tukey_out3 <- TukeyHSD(gnoib3_Tukey)
tukey_out3


# GLM Graph

CRV <- ggplot(Lolium, aes(x = Day, y = Compactness,
                          col = Treatment))+
  ylab("Mean plant compactness")+
  geom_smooth(method = "loess", se = FALSE, span = 2, size =1.1)+
  theme_classic()

CRV + scale_colour_hue(l = 55, h = c(0, 270))


#All sorts of other stuff

CRV <- ggplot(Lolium, aes(x = Day, y = Compactness,
                          col = Treatment))+
  ylab("Mean compactness")+
  geom_jitter(alpha = 0.5, size = 0.7)+
  stat_smooth(method = "glm", se = FALSE,
              method.args = list(family = "poisson"))+
  theme_classic()

CRV + scale_colour_hue(l = 50, h = c(0, 270))

CRVS <- ggplot(sumDat, aes(x = Day, y = meanComp,
                           col = Treatment))+
  ylab("Mean compactness")+
  geom_smooth(method = "glm", se = FALSE,
              method.args = list(family = "poisson"), 
              size = 1.3)+
  theme_classic()

CRVS + scale_colour_hue(l = 50, h = c(0, 270))
