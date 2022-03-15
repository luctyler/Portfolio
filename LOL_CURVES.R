library(dplyr)
library(ggplot2)
library(ggfortify)

LOL <- read.csv("./R/LoliumNew.csv")
LOLC <- read.csv("./R/lol_cont.csv")

CP <- filter(LOL, Treatment == "Control")
CN <- filter(LOL, Treatment == "Control-P")
MP <- filter(LOL, Treatment == "MP")
MPS <- filter(LOL, Treatment == "MP(S)")
SMMTS <- filter(LOL, Treatment == "SMMT(S)")
SMMT <- filter(LOL, Treatment == "SMMT")
SMGS <- filter(LOL, Treatment == "SMMT")
SMFT <- filter(LOL, Treatment == "SMMT")
UMS <- filter(LOL, Treatment == "UM(S)")
UM <- filter(LOL, Treatment == "UM")
UR <- filter(LOL, Treatment == "UR")
KGB <- filter(LOL, Treatment == "KGB")

DF <- filter(LOL, Treatment != "Control")


#Area

ex3 <- expression("Mean leaf area (mm"^2*")")

MCP <- ggplot(LOL, aes(x = Day, y = AREA_MM,col = Treatment))+
  ylab(ex3)+
  geom_smooth(data = CP, method = "loess", linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = "light blue", alpha = 0.3, show.legend = FALSE)
MCP + scale_colour_hue(l = 50, h = c(0, 270))+ theme_bw()

MCP <- ggplot(DF, aes(x = Day, y = AREA_MM,col = Treatment))+
  ylab(ex3)+
  geom_smooth(data = DF, method = "loess", linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = "light blue", alpha = 0.3, show.legend = FALSE)+
  facet_wrap(vars(Treatment), nrow = 4)
  
MCP + scale_colour_hue(l = 50, h = c(0, 270))+ theme_bw()

geom_smooth(data = transform(CP,  Treatment = NULL), method = "loess", 
            linetype="dashed", size = 0.75, fullrange = FALSE, span = 0.3, 
            method.args = list(degree=1), fill = "red", 
            alpha = 0.3, show.legend = FALSE)+

MCP + coord_cartesian(ylim = c(-100, 1000), expand = FALSE)+
  theme_bw()
?facet_wrap

every_facet_data = subset(LOL, BS == 1)
individual_facet_data = subset(LOL, BS != 1)
individual_facet_data$facet = individual_facet_data$BS

every_facet_data = merge(every_facet_data,
                         data.frame(BS = 1, 
                                    facet = unique(individual_facet_data$facet)))

plot_data = rbind(every_facet_data, individual_facet_data)


geom_smooth(data = CN, method = "loess", linetype="dashed", size = 0.75,
            fullrange = FALSE, span = 0.3, method.args = list(degree=1),
            fill = "green", alpha = 0.1, show.legend = FALSE)+
  geom_smooth(data = UR, method = "loess", linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = "purple", alpha = 0.1, show.legend = FALSE)+
  geom_smooth(data = UM, method = "loess", linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = "red", alpha = 0.1, show.legend = FALSE)+
  geom_smooth(data = UMS, method = "loess", linetype="dashed", size = 0.75,
              fullrange = FALSE, span = 0.3, method.args = list(degree=1),
              fill = NA, alpha = 0.1)