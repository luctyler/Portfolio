library(dplyr)
library(ggplot2)
library(ggfortify)
library("ggpubr")
library(dichromat)
library(viridis)

ARB <- read.csv("./R/Arb.csv")



#Roundness

sumDat <- ARB %>% 
  group_by(Day, Treatment) %>%
  summarise(
    meanA = mean(AREA_MM, na.rm=TRUE),
    sdA = sd(AREA_MM, na.rm=TRUE),
    seA = (sdA/(sqrt(14))))

week3 <- sumDat[157:248,]

BPA <- ggplot(week3, aes(x = Treatment, y = meanA, 
                         fill = Treatment, alpha = 0.5))+
  ylab("Mean rosetta area (mm^2)")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+ 
  theme_bw()
  
BPA + stat_compare_means(label = "p.signif", method = "t.test", hide.ns = TRUE,
                         ref.group = "Control+P")+
  theme(legend.position="none")

#Roundness

sumDat <- ARB %>% 
  group_by(Day, Treatment) %>%
  summarise(
    meanR = mean(ROUNDNESS, na.rm=TRUE),
    sdR = sd(ROUNDNESS, na.rm=TRUE),
    seR = (sdR/(sqrt(14))))



week3 <- sumDat[157:248,]

BP3 <- ggplot(week3, aes(x = Treatment, y = meanR, 
                         fill = Treatment, alpha = 0.5))+
  ylab("Mean roundness score")+
  stat_boxplot(geom ="errorbar", size=0.1, width=0.4)+
  geom_boxplot(lwd=0.2)+
  coord_flip()+ 
  theme_bw()

BP3 + stat_compare_means(label = "p.signif", method = "t.test", hide.ns = TRUE,
                         ref.group = "Control+P")+
  theme(legend.position="none")


CRV <- ggplot(ARB, aes(x = Day, y = AREA_MM,
                           col = Treatment))+
  ylab("Mean plant area (mm)")+
  geom_smooth(method = "loess", se = FALSE, size = 1)+
  theme_classic()

CRV + scale_colour_hue(l = 50, h = c(0, 270))

MCV <- ggplot(ARB, aes(x = Day, y = AREA_MM,
                       col = Treatment))+
                        geom_point() + 
                        geom_smooth(span = 0.15, method.args = list(degree=1))+
                        facet_wrap(~ Treatment)

MCV + scale_colour_hue(l = 50, h = c(0, 270))

sumDat <- ARB %>% 
  group_by(Day, Treatment) %>%
  summarise(
    meanA = mean(AREA_MM, na.rm=TRUE),
    sdA= sd(AREA_MM, na.rm=TRUE),
    seA = (sdA/(sqrt(14))))

ARD <- group_by()

y = factor(rep(c("Control+P", "Control-P"), each  = 5), 
           levels = c("a", "b"))


#YOU ALSO HAVE MORE VARIABLES TO CONSIDER (STERILISED/PHOSPHORUS)

ex3 <- expression("Mean rosette area (mm"^2*")")

df$AREA <- with(ARB, ifelse(name %in% Treatment, "Control-P"))
                            
aggregate(value ~ group, sum, data=df)

ABD <- read.csv("./R/ABD.csv")

sumDat <- dfps %>% 
  group_by(Day, Treatment) %>%
  summarise(
    meanA = mean(AREA_MM, na.rm=TRUE),
    sdA= sd(AREA_MM, na.rm=TRUE),
    seA = (sdA/(sqrt(7))))

?facet_wrap

MCB <- ggplot(ABD, aes(x = Day, y = AREA_MM,
                       col = Treatment))+
                        geom_point() + 
                        geom_smooth(span = 0.15, method.args = list(degree=1))+
                         facet_wrap(vars(Treatment))

MCB + scale_colour_hue(l = 50, h = c(0, 270))

?slice
?facet_grid

df <- filter(ABD, BS == "2")
dfc <- filter(ABD, BS == "1")
dfp <- filter(ABD, Phos == "1")
dfps <- filter(dfp, BS == "2")

MCB <- ggplot(df, aes(x = Day, y = AREA_MM,
                       col = Treatment))+
  ylab(ex3)+
  geom_point(data = transform(dfc)) +
  geom_point() + 
  geom_smooth(span = 0.15, method.args = list(degree=1))+
  facet_wrap(vars(Treatment))
MCB + scale_colour_hue(l = 50, h = c(0, 270))+
  theme_bw()

MCP <- ggplot(dfp, aes(x = Day, y = AREA_MM,
                      col = Treatment))+
  ylab(ex3)+
  geom_point() + 
  geom_smooth(span = 0.15, method.args = list(degree=1))+
  facet_wrap(vars(Treatment ~ BI))
MCP + scale_colour_hue(l = 50, h = c(0, 270))+
  theme_bw()
#11pm brainwave

MCP <- ggplot(dfps, aes(x = Day, y = AREA_MM,
                       col = Treatment))+
      ylab(ex3)+
       stat_smooth(geom = "smooth",
                   position = "identity",
                   fullrange = FALSE,
                   se = FALSE, na.rm = FALSE,
                   span = 0.3, method.args = list(degree=1)+
                   fill = NA, alpha = 0.1)
        
        
MCP + scale_colour_hue(l = 50, h = c(0, 270))+
  theme_bw()

MCP <- ggplot(dfps, aes(x = Day, y = AREA_MM,col = Treatment))+
  ylab(ex3)+
  geom_smooth(show.legend = TRUE, 
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
  fill = factor(20), alpha = 0.05)

MCP + scale_colour_hue(l = 50, h = c(0, 270))+ theme_bw()
MCP + scale_y_continuous(limits = c(0, 800))
MCP + coord_cartesian(ylim = c(0, 600), expand = FALSE)+
  theme_bw()
  
CP <- filter(ABD, Treatment == "Control+P")
CN <- filter(ABD, Treatment == "Control-P")
KSP <- filter(ABD, Treatment == "KB(S)+P")
KSN <- filter(ABD, Treatment == "KB(S)-P")
KP <- filter(ABD, Treatment == "KB+P")
KN <- filter(ABD, Treatment == "KB-P")
URSP <- filter(ABD, Treatment == "UR(S)+P")
URSN <- filter(ABD, Treatment == "UR(S)-P")
URP <- filter(ABD, Treatment == "UR+P")
URN <- filter(ABD, Treatment == "UR-P")
UMSP <- filter(ABD, Treatment == "UM(S)+P")
UMSN <- filter(ABD, Treatment == "UM(S)-P")
UMP <- filter(ABD, Treatment == "UM+P")
UMN <- filter(ABD, Treatment == "UM-P")


MCP <- ggplot(dfps, aes(x = Day, y = AREA_MM,col = Treatment))+
  ylab(ex3)+
  geom_smooth(data = UMSP, linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = "light blue", alpha = 0.3, show.legend = FALSE)+
  geom_smooth(data = KSP, linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = "green", alpha = 0.1, show.legend = FALSE)+
  geom_smooth(data = URSP, linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = "purple", alpha = 0.1, show.legend = FALSE)+
  geom_smooth(data = CP, linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = "red", alpha = 0.1, show.legend = FALSE)+
  geom_smooth(data = CP, linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = NA, alpha = 0.1)

MCP + scale_colour_hue(l = 50, h = c(0, 270))+ theme_bw()
MCP + coord_cartesian(ylim = c(-100, 1000), expand = FALSE)+
  theme_bw()

MCP1 <- ggplot(dfps, aes(x = Day, y = AREA_MM,col = Treatment))+
  ylab(ex3)+
  geom_smooth(data = UMP, linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = "light blue", alpha = 0.3, show.legend = FALSE)+
  geom_smooth(data = KP, linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = "green", alpha = 0.1, show.legend = FALSE)+
  geom_smooth(data = URP, linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = "purple", alpha = 0.1, show.legend = FALSE)+
  geom_smooth(data = CP, linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = "red", alpha = 0.1, show.legend = FALSE)+
  geom_smooth(data = CP, linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = NA, alpha = 0.1)
MCP1 + scale_colour_hue(l = 50, h = c(0, 270))+ theme_bw()
MCP1 + coord_cartesian(ylim = c(-100, 1000), expand = FALSE)+
  theme_bw()


MCP2 <- ggplot(dfps, aes(x = Day, y = AREA_MM,col = Treatment, show.legend = FALSE))+
  ylab(ex3)+
  geom_smooth(data = UMSN, linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = "light blue", alpha = 0.3, show.legend = FALSE)+
  geom_smooth(data = KSN, linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = "green", alpha = 0.1, show.legend = FALSE)+
  geom_smooth(data = URSN, linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = "purple", alpha = 0.1, show.legend = FALSE)+
  geom_smooth(data = CP, linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = "red", alpha = 0.1, show.legend = FALSE)+
  geom_smooth(data = CP, linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = NA, alpha = 0.1)
MCP2 + scale_colour_hue(l = 50, h = c(0, 270))+ theme_bw() + show.legend = FALSE
MCP2 + coord_cartesian(ylim = c(-375, 2250), expand = FALSE)+
  theme_bw()

MCP3 <- ggplot(dfps, aes(x = Day, y = AREA_MM,col = Treatment))+
  ylab(ex3)+
  geom_smooth(data = UMN, linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = "light blue", alpha = 0.3, show.legend = FALSE)+
  geom_smooth(data = KN, linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = "green", alpha = 0.1, show.legend = FALSE)+
  geom_smooth(data = URN, linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = "purple", alpha = 0.1, show.legend = FALSE)+
  geom_smooth(data = CP, linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = "red", alpha = 0.1, show.legend = FALSE)+
  geom_smooth(data = CP, linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = NA, alpha = 0.1)
MCP3 + scale_colour_hue(l = 50, h = c(0, 270))+ theme_bw()
MCP3 + coord_cartesian(ylim = c(-375, 2250), expand = FALSE)+
  theme_bw()

?legend

?fill
?ylim

#worth a shot?

dsa <- glm(ROUNDNESS ~ BS+Treatment, 
           data = dfps, family = "binomial")

autoplot(dsa)

anova(dsa, test = "F")

summary(dsa)

my_area_vals <- seq(from = min(dfps$ROUNDNESS, na.rm=TRUE),  
                    to = max(dfps$ROUNDNESS, na.rm=TRUE),
                    length = 100)
my_BS_vals <- levels(dfps$BS)

newX <- expand.grid(ROUNDNESS = my_area_vals,
                    BS = my_BS_vals)

newY <- predict(dsa,
                type = "link", newdata = newX, se.fit = TRUE,
                interval = "confidence")
?predict

add_these <- data.frame(newX, newY)

add_those <- mutate(sumDat, 
                    AREA = (meanR),
                    ll = (meanR - seR),
                    ul = (meanR + seR))

ggplot(add_those, aes(x = Day, y = meanR, col = Treatment, na.rm=TRUE))+
  geom_smooth(ribbon)+
  geom_ribbon(alpha = 0.25, colour = NA,
              aes(ymin = ul, ymax = ll, fill = Treatment))+
  geom_point(data = dfps)+
  labs(x = "Day", y = "Roundness",
       colour = "Treatment",
       fill = "Treatment")+
  theme_classic()

?geom_smooth()

## no idea
install.packages("hexbin")
library(hexbin)
ggplot(data.frame(x = rnorm(10000), y = rnorm(10000)), aes(x = x, y = y)) +
  geom_hex() + coord_fixed() +
  scale_fill_viridis() + theme_bw()

BSA <- ABD$BS
PHOSA <- ABD$Phos
# MANOVA test
res.man <- manova(cbind(BS, Phos) ~ Treatment, data = ABD)
summary(res.man)
summary.aov(res.man)

