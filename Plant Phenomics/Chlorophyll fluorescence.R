#Chlorophyll

CHL <- read.csv("./R/Chlorophyll.csv")

ex3 <- expression("Area (mm"^2*")")#  ylab(ex3)+
ex <- expression("PPFD (?? mol m"^-2*" s"^-1*")")
#??PSII

CP <- ggplot(CHL, aes(x = Time, y = FqFm), 
             na.rm = FALSE)+
  ylab("??PSII")+
  geom_point(aes(colour = factor(Treatment)), show.legend = FALSE,
             alpha = 0.7, shape = 18) +
  stat_smooth(aes(colour = factor(Treatment)), show.legend = FALSE,
              method = lm, formula = y ~ poly(x, 3, raw = TRUE),
              se = FALSE)+
  facet_wrap(vars(Treatment), nrow = 2, ncol = 4)

CP +  scale_x_continuous(breaks = seq(750, 1250, by=250))+
  theme_bw()

####

CP <- ggplot(CHL, aes(x = PPFD, y = FqFm), 
             na.rm = FALSE)+
  ylab("??PSII")+
  geom_point(aes(colour = factor(Treatment)), show.legend = FALSE,
             alpha = 0.7, shape = 18, size = 2) +
  stat_smooth(aes(colour = factor(Treatment)), show.legend = FALSE,
              method = lm, formula = y ~ poly(x, 2, raw = TRUE),
              se = FALSE)+
  xlim(-50, 2100)+
  facet_wrap(vars(Treatment), nrow = 2, ncol = 4)

CP + theme_bw()+
  labs(x = ex,
       title = "PSII operating efficiency",
       subtitle = "The efficiency of PSII electron transport in the light")
##

CP <- ggplot(CHL, aes(x = PPFD, y = FvFm), 
             na.rm = FALSE)+
  ylab("Fv'/Fm'")+
  geom_point(aes(colour = factor(Treatment)), show.legend = FALSE,
             alpha = 0.7, shape = 18, size = 2) +
  stat_smooth(aes(colour = factor(Treatment)), show.legend = FALSE,
              method = lm, formula = y ~ poly(x, 3, raw = TRUE),
              se = FALSE)+
  xlim(-50, 2100)+
  facet_wrap(vars(Treatment), nrow = 2, ncol = 4)

CP + theme_bw()+
  labs(x = ex,
       title = "Maximum projected PSII operating efficiency",
       subtitle = "Maximum efficiency of PSII photochemistry in the light if all centres were open")

###

#Photochemical quenching

CP <- ggplot(CHL, aes(x = PPFD, y = FqFv), 
             na.rm = FALSE)+
  ylab("Fq'/Fv'")+
  xlim(-50, 2100)+
  geom_point(aes(colour = factor(Treatment)), show.legend = FALSE,
             alpha = 0.7, shape = 18, size = 2) +
  stat_smooth(aes(colour = factor(Treatment)), show.legend = FALSE,
              method = lm, formula = y ~ poly(x, 3, raw = TRUE),
              se = FALSE)+
  facet_wrap(vars(Treatment), nrow = 2, ncol = 4)

CP +  theme_bw()+
  labs(x = ex,
       title = "PSII maximum efficiency to operating efficiency",
       subtitle = "A function of the proportion of PSII centres that are open")


#  scale_x_continuous(breaks = seq(750, 1250, by=250))+


NPQ <- read.csv("./R/NPQ.csv")

NP <- ggplot(NPQ, aes(x = PPFD, y = NPQ), 
          na.rm = FALSE)+
  ylab("NPQ")+
  xlim(-50, 2100)+
  geom_point(aes(colour = factor(Treatment)), show.legend = FALSE,
             alpha = 0.7, shape = 18, size = 2) +
  stat_smooth(aes(colour = factor(Treatment)), show.legend = FALSE,
              method = lm, formula = y ~ poly(x, 4, raw = TRUE),
              se = FALSE)+
  facet_wrap(vars(Treatment), nrow = 2, ncol = 4)

NP +  theme_bw()+
  labs(x = ex,
       title = "Non-photochemical quenching",
       subtitle = "An estimate of the rate constant for heat loss from PSII")

LS <- read.csv("./R/lstat.csv")

mod <- lm(FvFm ~ poly(PPFD, 4, raw = TRUE), data = LS)
mod <- lm(FvFm ~ poly(PPFD, 4, raw = TRUE), data = LS)

install.packages("kmlShape")
library(kmlShape)
install.packages("dgof")
library(dgof)
install.packages("Matching")
library(Matching)
set.seed(1)

# Do x and y come from the same distribution?
Psu <- filter(LS, Treatment == "Control+P")
Pyr <- as.numeric(as.character(Psu$FvFm)) 
Psu <- filter(LS, Treatment == "UM")
Pym <- as.numeric(as.character(Psu$FvFm)) 

ks.boot(Pyr, Pym)

Psu <- filter(LS, Treatment == "UR")
Pym <- as.numeric(as.character(Psu$FvFm)) 

ks.boot(Pyr, Pym)

Psu <- filter(LS, Treatment == "UM(S)")
Pym <- as.numeric(as.character(Psu$FvFm)) 

ks.boot(Pyr, Pym)

Psu <- filter(LS, Treatment == "Control-P")
Pym <- as.numeric(as.character(Psu$FvFm)) 

ks.boot(Pyr, Pym)

Psu <- filter(LS, Treatment == "BGMP")
Pym <- as.numeric(as.character(Psu$FvFm)) 

ks.boot(Pyr, Pym)

Psu <- filter(LS, Treatment == "SMFT")
Pym <- as.numeric(as.character(Psu$FvFm)) 

ks.boot(Pyr, Pym)

# PsII max
Psu <- filter(CHL, Treatment == "Control+P")
Pyr <- as.numeric(as.character(Psu$FqFv)) 
Psu <- filter(CHL, Treatment == "UM")
Pym <- as.numeric(as.character(Psu$FqFv)) 

ks.boot(Pyr, Pym)

Psu <- filter(CHL, Treatment == "UR")
Pym <- as.numeric(as.character(Psu$FqFv)) 

ks.boot(Pyr, Pym)

Psu <- filter(CHL, Treatment == "UM(S)")
Pym <- as.numeric(as.character(Psu$FqFv)) 

ks.boot(Pyr, Pym)

Psu <- filter(CHL, Treatment == "Control-P")
Pym <- as.numeric(as.character(Psu$FqFv)) 

ks.boot(Pyr, Pym)

Psu <- filter(CHL, Treatment == "BGMP")
Pym <- as.numeric(as.character(Psu$FqFv)) 

ks.boot(Pyr, Pym)

Psu <- filter(CHL, Treatment == "SMFT")
Pym <- as.numeric(as.character(Psu$FqFv)) 

ks.boot(Pyr, Pym)

# NPQ
Psu <- filter(CHL, Treatment == "Control+P")
Pyr <- as.numeric(as.character(Psu$NPQ)) 
Psu <- filter(CHL, Treatment == "UM")
Pym <- as.numeric(as.character(Psu$NPQ)) 

ks.boot(Pyr, Pym)

Psu <- filter(CHL, Treatment == "UR")
Pym <- as.numeric(as.character(Psu$NPQ)) 

ks.boot(Pyr, Pym)

Psu <- filter(CHL, Treatment == "UM(S)")
Pym <- as.numeric(as.character(Psu$NPQ)) 

ks.boot(Pyr, Pym)

Psu <- filter(CHL, Treatment == "Control-P")
Pym <- as.numeric(as.character(Psu$NPQ)) 

ks.boot(Pyr, Pym)

Psu <- filter(CHL, Treatment == "BGMP")
Pym <- as.numeric(as.character(Psu$NPQ)) 

ks.boot(Pyr, Pym)

Psu <- filter(CHL, Treatment == "SMFT")
Pym <- as.numeric(as.character(Psu$NPQ)) 

ks.boot(Pyr, Pym)

## Frechet dist

Psu <- filter(LS, Treatment == "UR")
Px <- as.vector(Psu['PPFD'])
Py <- as.vector(Psu['FvFm'])
Psu <- filter(LS, Treatment == "UM")
Qx <- as.vector(Psu['PPFD'])
Qy <- as.vector(Psu['PPFD'])

distFrechetR(Px,Py,Qx, Qy, 
            timeScale=0.1, FrechetSumOrMax = "sum")

??Frechet




URmod <- lm(FvFm ~ poly(PPFD, 4, raw = TRUE), data = Psu)

Psu <- filter(CHL, Treatment == "UM(S)")
Psu <- slice(Psu, (1:41))

UMmod <- lm(FvFm ~ poly(PPFD, 4, raw = TRUE), data = Psu)

Psu <- filter(CHL, Treatment == "Control-P")
Psu <- slice(Psu, (1:45))
CNmod <- lm(CHL ~ poly(PPFD, 4, raw = TRUE), data = Psu)

anova(UMmod, Bmod)

UMmod <- lm(NPQ ~ poly(PPFD, 4, raw = TRUE), data = Psu)

Psu <- filter(NPQ, Treatment == "Control+P")
Psu <- slice(Psu, (1:41))
CPmod <- lm(NPQ ~ poly(PPFD, 4, raw = TRUE), data = Psu)

anova(CPmod, UMmod)

UMmod <- lm(NPQ ~ poly(PPFD, 4, raw = TRUE), data = Psu)

Psu <- filter(CHL, Treatment == "BGMP")

Bmod <- lm(FvFm ~ poly(PPFD, 4, raw = TRUE), data = Psu)
Psu <- filter(NPQ, Treatment == "Control-P")
Psu <- slice(Psu, (1:45))
CNmod <- lm(NPQ ~ poly(PPFD, 4, raw = TRUE), data = Psu)
anova(CNmod, Bmod)

Psu <- filter(NPQ, Treatment == "SMFT")
Sod <- lm(NPQ ~ poly(PPFD, 4, raw = TRUE), data = Psu)
anova(CNmod, Sod)

