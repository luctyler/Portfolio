stations <- read.csv(https://github.com/EDMANSolar/downscaling/blob/master/Stations.RData
R> load('Stations.RData')

install.packages("sf")
install.packages("qmap")
library(sp)
library(sf)
library(raster)
library(rgdal)
library(rgeos)
library(dplyr)
library(ggplot2)
library(qmap)

HS <- st_read("./R/hotspots_2016_1.shp")

st_geometry_type(HS) 
st_crs(HS)
st_is_valid(HS)
summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")
head(HS)
HTS <- filter(HS, Type == "hotspot area")




ggplot() + 
  geom_sf(data = HS, size = 1, 
          color = "black", fill = "cyan1") + 
  ggtitle("Hotspots") + 
  coord_sf()


download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="./R//world_shape_file.zip")
system("unzip ./R/world_shape_file.zip")

library(rgdal)
my_spdf <- readOGR( 
  dsn= paste0(getwd(),"./R/world_shape_file") ,
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

par(mar=c(0,0,0,0))
plot(my_spdf, col="#f2f2f2", bg="skyblue", lwd=0.25, border=0 )

# 'fortify' the data to get a dataframe format required by ggplot2
library(broom)
spdf_fortified <- tidy(my_spdf, region = "NAME")

# Plot it
library(ggplot2)
HP <- ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  theme_void() 

HP + geom_sf(data = HTS,size = 0.2, 
             color = "black", fill = "red", alpha = 0.2) + 
  ggtitle("Luke's progress on the hotspot map") + 
  coord_sf()

##

HP + geom_sf(data = HTS,size = 0.2, 
             color = NA, fill = "red", alpha = 0.4) + 
  ggtitle("Hotspot map") + 
  coord_sf()

#Chloropleth
install.packages("cartography")
library(cartography)

# Load data
data(nuts2006)

# Build a choropleth
choroLayer(spdf = nuts2.spdf, df = nuts2.df, var = "pop2008" , 
           legend.pos = "right")
title("Population in 2008")


####

HP <- ggplot() +
  geom_polygon(data = spdf_fortified, 
               aes( x = long, y = lat, group = group),
               fill="#69b3a2", color=NA) +
  theme_void() 

HP + geom_sf(data = HTS,size = 0.2, 
             color = NA, fill = "red", alpha = 0.4) + 
  ggtitle("Hotspot map") + 
  coord_sf()

MP <- read.csv("./R/medplants.csv", stringsAsFactors=F)

HP + geom_sf(data = HTS,size = 0.3, 
             color = NA, fill = "red", alpha = 0.4) + 
  geom_sf(data = HTS, size = 0.3, color = NA, fill = NA)+
  ggtitle("Hotspot map") + 
  coord_sf()+
  geom_point(data=MP, aes(x=lon, y=lat), fill="yellow", 
             colour="orange",pch=21, alpha = 0.8, size = 8)

MEM <- read.csv("./R/memplants.csv", stringsAsFactors=F)

HP + geom_sf(data = HTS,size = 0.3, 
             color = NA, fill = "red", alpha = 0.4) + 
  geom_sf(data = HTS, size = 0.3, color = NA, fill = NA)+
  ggtitle("Memory improvement map") + 
  coord_sf()+
  geom_point(data=MEM, aes(x=lon, y=lat), fill="yellow", 
             colour="orange",pch=23, alpha = 0.8, size = 8)

########

HP1 <- ggplot() +
  geom_polygon(data = spdf_fortified, 
               aes(x = long, y = lat, group = group),
               fill=NA, color="black") +
  theme_void() 

HP1 + geom_sf(data = HTS,size = 0.3, 
             color = NA, fill = "green", alpha = 0.5) + 
  geom_sf(data = HTS, size = 0.3, color = NA, fill = NA)+
  ggtitle("Memory improvement map") + 
  coord_sf()+
  geom_point(data=MP, aes(x=lon, y=lat), fill="orange", 
             colour="red",pch=21, alpha = 0.8, size = 8)+
  geom_point(data=MEM, aes(x=lon, y=lat), fill="yellow", 
             colour="red",pch=21, alpha = 0.8, size = 8)

#Main map

AP <- read.csv("./R/antipara.csv", stringsAsFactors=F)

HP1 <- ggplot() +
  geom_polygon(data = spdf_fortified, 
               aes(x = long, y = lat, group = group),
               fill=NA, color="black") +
  theme_void() 

HP1 + geom_sf(data = HTS,size = 0.3, 
              color = NA, fill = "green", alpha = 0.5) + 
  geom_sf(data = HTS, size = 0.3, color = NA, fill = NA)+
  ggtitle("Main map") + 
  coord_sf()+
  geom_point(data=MP, aes(x=lon, y=lat), fill="orange", 
             colour="red",pch=21, alpha = 0.8, size = 8)+
  geom_point(data=MEM, aes(x=lon, y=lat), fill="yellow", 
             colour="red",pch=21, alpha = 0.8, size = 8)+
  geom_point(data=AP, aes(x=lon, y=lat), fill="yellow", 
             colour="red",pch=21, alpha = 0.8, size = 8)

#Mem only

MEMC <- read.csv("./R/memplantscomp.csv", stringsAsFactors=F)
MEMD <- read.csv("./R/memdatabase.csv", stringsAsFactors=F)
pd_spdf <- readOGR( 
  dsn= paste0(getwd(),"./R/Centres_of_Plant_Diversity_2013") ,
  layer="CPD_2013",
  verbose=FALSE
)

library(broom)
pd_fortified <- tidy(pd_spdf)

#par(mar=c(0,0,0,0))
#plot(pd_spdf, col="#f2f2f2", bg="skyblue", lwd=0.25, border=0 )

HP2 <- ggplot() +
  geom_polygon(data = spdf_fortified, 
               aes(x = long, y = lat, group = group),
               fill=NA, color="black") +
  theme_void() 

HP2 + geom_sf(data = HTS,size = 0.3, 
              color = NA, fill = "green", alpha = 0.5) + 
  geom_sf(data = HTS, size = 0.3, color = NA, fill = NA)+
  ggtitle("Memory improvement map") + 
  coord_sf()+
  geom_point(data=MEM, aes(x=lon, y=lat), fill="yellow", 
             colour="red",pch=21, alpha = 0.8, size = 8)+
  geom_point(data=MEMC, aes(x=lon, y=lat), fill="yellow", 
             colour="red",pch=21, alpha = 0.8, size = 8)+
  geom_point(data=MEMD, aes(x=lon, y=lat), fill="yellow", 
             colour="red",pch=21, alpha = 0.8, size = 8)

HP2+   geom_polygon(data = pd_fortified, 
         aes(x = long, y = lat, group = group),
          fill="red", alpha = 0.7)+
  geom_sf(data = HTS,size = 0.3, 
          color = NA, fill = "green", alpha = 0.5) + 
  geom_sf(data = HTS, size = 0.3, color = NA, fill = NA)+
  ggtitle("Memory improvement map") + 
  coord_sf()+
  geom_point(data=MEM, aes(x=lon, y=lat), fill="yellow", 
             colour="red",pch=21, alpha = 0.8, size = 8)+
  geom_point(data=MEMC, aes(x=lon, y=lat), fill="yellow", 
             colour="red",pch=21, alpha = 0.8, size = 8)+
  geom_point(data=MEMD, aes(x=lon, y=lat), fill="yellow", 
             colour="red",pch=21, alpha = 0.8, size = 8)
  
  ## Antip only

HP2 <- ggplot() +
  geom_polygon(data = spdf_fortified, 
               aes(x = long, y = lat, group = group),
               fill="lightgoldenrod1", color="black") +
  theme_void() 

HP2+   geom_sf(data = HTS,size = 0.3, 
          color = NA, fill = "green", alpha = 0.5) + 
    ggtitle("Anti-paralytics map") + 
  coord_sf()+
  geom_point(data=AP, aes(x=lon, y=lat), fill="yellow", 
             colour="red",pch=21, alpha = 0.8, size = 8)+
  theme(plot.background = element_rect(fill = "skyblue1"))

## 18th sep main map

HP2 <- ggplot() +
  geom_polygon(data = spdf_fortified, 
               aes(x = long, y = lat, group = group),
               fill="lightgoldenrod1", color="black") +
  theme_void() 

HP2+   geom_sf(data = HTS,size = 0.3, 
               color = NA, fill = "green", alpha = 0.6) + 
  coord_sf()+
  geom_point(data=AP, aes(x=lon, y=lat), 
             fill="orange", 
             colour="black",pch=24, alpha = 0.8, size = 12,
             stroke = 2)+
  geom_point(data=MP, aes(x=lon, y=lat), fill="orange", 
             colour="black",pch=21, alpha = 0.8, size = 12, 
             stroke = 2)+
  geom_point(data=MEM, aes(x=lon, y=lat), fill="orange", 
             colour="black",pch=24, alpha = 0.8, size = 12, 
             stroke = 2)+
  geom_point(data=HL, aes(x=lon, y=lat), fill="orange", 
             colour="black",pch=23, alpha = 0.8, size = 12, 
             stroke = 2)+
  theme(plot.background = element_rect(fill = "skyblue1"))+
  annotate("rect", bg = "white",
           xmin = 51, xmax = 113, 
           ymin = -9.5, ymax = -42)+
  annotate("text", x = 57, y = -12,
           label = "Key", size = 12) +
  annotate("point", x = 54, y = -18, fill="orange", 
           colour="black",pch=21, alpha = 0.8, size = 11, 
           stroke = 2) +
  annotate("point", x = 54, y = -25, fill="orange", 
             colour="black",pch=24, alpha = 0.8, size = 11,
             stroke = 2) +
  annotate("point", x = 54, y = -33, fill="orange", 
           colour="black",pch=23, alpha = 0.8, size = 11, 
           stroke = 2) +
  annotate("point", x = 54, y = -39, fill="green", 
           pch=22, alpha = 0.5, size = 11, 
           stroke = 2) +
  annotate("text", x = 69, y = -18, 
            label = "Survey location", size = 11) +
  annotate("text", x = 79, y = -23.5, 
           label = "Survey assessed for memory",
           size = 11) +
  annotate("text", x = 85, y = -27.5, 
           label = "improvement/anti-paralytic uses only",
           size = 11) +
  annotate("text", x = 83, y = -33, 
            label = "Several surveys in close proximity", size = 11) +
  annotate("text", x = 73, y = -39, 
           label = "Biodiversity hostspot", size = 11)
           
           
           
?annotate
               
?pch
??legend

install.packages("ggsn")
library(ggsn)
##  

MP <- read.csv("./R/medplantsFINAL.csv", stringsAsFactors=F)
MEM <- read.csv("./R/memplantsFINAL.csv", stringsAsFactors=F)
AP <- read.csv("./R/antiparaFINAL.csv", stringsAsFactors=F)
HL <- read.csv("./R/memplantsH.csv", stringsAsFactors=F)

HP3 <- ggplot() +
  geom_polygon(data = spdf_fortified, 
               aes(x = long, y = lat, group = group),
               fill="lightgoldenrod1", color="black") +
  theme_void() 

HP3+   geom_sf(data = HTS,size = 0.3, 
               color = NA, fill = "green", alpha = 0.6) + 
  coord_sf()+
  geom_point(data=MEM, aes(x=lon, y=lat), fill="orange", 
             colour="red",pch=24, alpha = 0.8, size = 8)+
  geom_point(data=MEMC, aes(x=lon, y=lat), fill="orange", 
             colour="red",pch=24, alpha = 0.8, size = 8)+
  geom_point(data=MEMD, aes(x=lon, y=lat), fill="orange", 
             colour="red",pch=24, alpha = 0.8, size = 8)+
  theme(plot.background = element_rect(fill = "skyblue1"))

########


HP2 <- ggplot() +
  geom_polygon(data = spdf_fortified, 
               aes(x = long, y = lat, group = group),
               fill="lightgoldenrod1", color="black") +
  theme_void() 

HP2+   geom_sf(data = HTS,size = 0.3, 
               color = NA, fill = "green", alpha = 0.6) + 
  coord_sf()+
  geom_point(data=AP, aes(x=lon, y=lat), 
             fill="orange", 
             colour="black",pch=21, alpha = 0.8, size = 20,
             stroke = 2)+
  geom_point(data=MP, aes(x=lon, y=lat), fill="orange", 
             colour="black",pch=21, alpha = 0.8, size = 20, 
             stroke = 2)+
  geom_point(data=MEM, aes(x=lon, y=lat), fill="orange", 
             colour="black",pch=21, alpha = 0.8, size = 20, 
             stroke = 2)+
  geom_point(data=HL, aes(x=lon, y=lat), fill="orange", 
             colour="black",pch=21, alpha = 0.8, size = 20, 
             stroke = 2)+
  theme(plot.background = element_rect(fill = "skyblue1"))
  

##

HP2 <- ggplot() +
  geom_polygon(data = spdf_fortified, 
               aes(x = long, y = lat, group = group),
               fill="lightgoldenrod1", color="black") +
  theme_void() 

HP2+   geom_sf(data = HTS,size = 0.3, 
               color = NA, fill = "green", alpha = 0.6) + 
  coord_sf()+
  geom_point(data=MEM, aes(x=lon, y=lat), fill="orange", 
             colour="black",pch=21, alpha = 0.8, size = 20, 
             stroke = 2)+
  theme(plot.background = element_rect(fill = "skyblue1"))
