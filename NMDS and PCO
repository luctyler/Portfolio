library(dplyr)
library(ggplot2)
library(ggfortify)
library(vegan)

AAR <- read.csv("./R/ANOVAARB.csv")

res.aov <- AAR %>% anova_test(score ~ gender * education_level)
res.aov

sumDat <- AAR %>% 
  group_by(Phos) %>%
  summarise(
    mean = mean(Compactness, na.rm=TRUE),
    sdComp = sd(Compactness, na.rm=TRUE),
    seComp = (sdComp/(sqrt(12))))

birds<-read.csv('https://raw.githubusercontent.com/collnell/lab-demo/master/bird_by_fg.csv')
trees<-read.csv('https://raw.githubusercontent.com/collnell/lab-demo/master/tree_comp.csv')

bird.matrix<-as.matrix(birds[,3:9])##response variables in a sample x species matrix
trees$B<-as.factor(trees$B)

bird.manova<-manova(bird.matrix~as.factor(B), data=trees) ##manova test
summary(bird.manova) 

summary.aov(bird.manova)

#permanova#

bird.mat<-sqrt(bird.matrix)#square root transform

bird.dist<-vegdist(bird.mat, method='bray')

set.seed(36) #reproducible results

bird.div<-adonis2(bird.dist~DIVERSITY, data=birds, 
                  permutations = 999, method="bray", strata="PLOT")
bird.div

dispersion<-betadisper(bird.dist, group=birds$DIVERSITY)
permutest(dispersion)

plot(dispersion, hull=FALSE, ellipse=TRUE) ##sd ellipse

birdMDS<-metaMDS(bird.mat, distance="bray", 
                 k=2, trymax=35, autotransform=TRUE) ##k is the number of dimensions
birdMDS ##metaMDS takes eaither a distance matrix or your community matrix (then requires method for 'distance=')

stressplot(birdMDS)

NMDS1 <- birdMDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- birdMDS$points[,2]
bird.plot<-cbind(birds, NMDS1, NMDS2, trees)

#plot ordination

birdplot <- bird.plot[,-2]


p<-ggplot(birdplot, aes(NMDS1, NMDS2, color=DIVERSITY))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points
  stat_ellipse(type='t',size =1)+ ##draws 95% confidence interval ellipses
  theme_minimal()
p

p<-ggplot(birdplot, aes(NMDS1, NMDS2, color=DIVERSITY))+
  stat_ellipse(type='t',size =1)+
  theme_minimal()+geom_text(data=birdplot,aes(NMDS1, NMDS2, label=comp), position=position_jitter(.35))+
  annotate("text", x=min(NMDS1), y=min(NMDS2), label=paste('Stress =',round(birdMDS$stress,3))) #add stress to plot
p

rlang::last_error()

fit<-envfit(birdMDS, bird.mat)
arrow<-data.frame(fit$vectors$arrows,R = fit$vectors$r, P = fit$vectors$pvals)
arrow$FG <- rownames(arrow)
arrow.p<-filter(arrow, P <= 0.05)


p<-ggplot(data=birdplot, aes(NMDS1, NMDS2))+
  geom_point(data=birdplot, aes(NMDS1, NMDS2, color=DIVERSITY),position=position_jitter(.1))+##separates overlapping points
  stat_ellipse(aes(fill=DIVERSITY), alpha=.2,type='t',size =1, geom="polygon")+ ##changes shading on ellipses
  theme_minimal()+
  geom_segment(data=arrow.p, aes(x=0, y=0, xend=NMDS1, yend=NMDS2, label=FG, lty=FG), arrow=arrow(length=unit(.2, "cm")*arrow.p$R)) ##add arrows (scaled by R-squared value)

p

## fin 

ar.matrix<-as.matrix(AAR[,4:42])##response variables in a sample x species matrix
#trees$B<-as.factor(trees$B)

#bird.manova<-manova(bird.matrix~as.factor(B), data=trees) ##manova test
#summary(bird.manova) 

#summary.aov(bird.manova)
ar.mat<-sqrt(ar.matrix)#square root transform

ar.dist<-vegdist(ar.mat, method='bray')

set.seed(36) #reproducible results

ar.div<-adonis2(ar.dist~Treat, data=AAR, 
                  permutations = 999, method="bray", strata="PLOT")
ar.div

dispersion2<-betadisper(ar.dist, group=AAR$Treat)
permutest(dispersion2)

plot(dispersion2, hull=FALSE, ellipse=TRUE) ##sd ellipse

arMDS<-metaMDS(ar.mat, distance="bray", 
                 k=2, trymax=35, autotransform=TRUE) ##k is the number of dimensions
arMDS ##metaMDS takes eaither a distance matrix or your community matrix (then requires method for 'distance=')

stressplot(arMDS)

NMDS1 <- arMDS$points[,1] ##also found using scores(birdMDS)
NMDS2 <- arMDS$points[,2]
ar.plot<-cbind(AAR, NMDS1, NMDS2)

#plot ordination

arplot <- ar.plot[,-2]


p<-ggplot(ar.plot, aes(NMDS1, NMDS2, color=Treat))+
  geom_point(position=position_jitter(.1), shape=3)+##separates overlapping points
  stat_ellipse(type='t',size =1)+ ##draws 95% confidence interval ellipses
  theme_minimal()
p

p<-ggplot(ar.plot, aes(NMDS1, NMDS2, color=Treat))+
  stat_ellipse(type='t',size =1)+
  theme_minimal()+geom_text(data=ar.plot,aes(NMDS1, NMDS2, label=BI), position=position_jitter(.35))+
  annotate("text", x=min(NMDS1), y=min(NMDS2), label=paste('Stress =',round(arMDS$stress,3))) #add stress to plot
p

rlang::last_error()

fit<-envfit(arMDS, ar.mat)
arrow<-data.frame(fit$vectors$arrows,R = fit$vectors$r, P = fit$vectors$pvals)
arrow$FG <- rownames(arrow)
arrow.p<-filter(arrow, P <= 0.05)


p<-ggplot(data=ar.plot, aes(NMDS1, NMDS2))+
  geom_point(data=ar.plot, aes(NMDS1, NMDS2, color=Phos),position=position_jitter(.1))+##separates overlapping points
  stat_ellipse(aes(fill=Phos), alpha=.2,type='t',size =1, geom="polygon")+ ##changes shading on ellipses
  theme_minimal()+
  geom_segment(data=arrow.p, aes(x=0, y=0, xend=NMDS1, yend=NMDS2, label=FG, lty=FG), arrow=arrow(length=unit(.2, "cm")*arrow.p$R)) ##add arrows (scaled by R-squared value)

p

#ordisurf(arMDS, ar.mat[,'IN'], bubble=TRUE)

