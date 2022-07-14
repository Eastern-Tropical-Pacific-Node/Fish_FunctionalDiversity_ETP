## --------------------------------------------------------------------------------------------------------- ##
##  Authors:       Dubuc A., Quimbayo J.P et al.                                                             ##
##  Date:          2022-07-14                                                                                ##
##  Script created by: Quimbayo J.P                                                                                                         ##
##  Notes:         1. This file is intended to provide a guide to the basic                                  ##
##                    project workflow, attempting to 'integrate_analysis' the steps                         ##
##                    necessary to conduct the analyses & visual outputs.                                    ##
## --------------------------------------------------------------------------------------------------------- ##

rm (list=ls())
library (dplyr)

# 1. Call database ------------------------------------
FD_Index <- read.csv("Data/FD_Indices.csv", header = T)
coord_locations <- read.csv("Data/coord_locations.csv", header = T, sep=";", dec=",")
FD_Index <- left_join(FD_Index, coord_locations)

# 2. Maps ---------------------------------------------
library (maps)
library (ggplot2)

worldmap <- map_data('world')

Fig_1a <- ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, group = group), 
               fill = 'gray', 
               color = 'gray',
               size=0.2) +
  coord_fixed(xlim = c(-115,-69),ylim = c(-25, 30))+
  geom_point(data=FD_Index, aes(x=Long, y=Lat, fill=richness), size=3, shape=21)+
  scale_fill_distiller(palette = "RdYlGn")+
  theme (axis.text.x=element_blank(),
         axis.text.y = element_text(size=10, angle=0, family = "sans"),
         axis.title.y = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks = element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, size=1),
         panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.title=element_blank(),
         legend.position="bottom",
         legend.key=element_rect(fill = "white"),
         legend.key.height = unit(0.5,"cm"), # Size of the choropleth
         legend.key.size = unit(1, "cm"),
         legend.text=element_text(colour = "black", size = 10, family = "sans"), 
         plot.title =element_text(colour = "black", size = 15, family = "sans", hjust = 0.5))

Fig_1b <- ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, group = group), 
               fill = 'gray', 
               color = 'gray',
               size=0.2) +
  coord_fixed(xlim = c(-115,-69),ylim = c(-25, 30))+
  geom_point(data=FD_Index, aes(x=Long, y=Lat, fill=fric), size=3, shape=21)+
  scale_fill_distiller(palette = "RdYlGn")+
  theme (axis.text.x=element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks = element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, size=1),
         panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.title=element_blank(),
         legend.position="bottom",
         legend.key=element_rect(fill = "white"),
         legend.key.height = unit(0.5,"cm"), # Size of the choropleth
         legend.key.size = unit(1, "cm"),
         legend.text=element_text(colour = "black", size = 10, family = "sans"), 
         plot.title =element_text(colour = "black", size = 15, family = "sans", hjust = 0.5))

Fig_1c <- ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, group = group), 
               fill = 'gray', 
               color = 'gray',
               size=0.2) +
  coord_fixed(xlim = c(-115,-69),ylim = c(-25, 30))+
  geom_point(data=FD_Index, aes(x=Long, y=Lat, fill=feve), size=3, shape=21)+
  scale_fill_distiller(palette = "RdYlGn")+
  theme (axis.text.x= element_text(size=10, angle=0, family = "sans"),
         axis.text.y = element_text(size=10, angle=0, family = "sans"),
         axis.title.y = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks = element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, size=1),
         panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.title=element_blank(),
         legend.position="bottom",
         legend.key=element_rect(fill = "white"),
         legend.key.height = unit(0.5,"cm"), # Size of the choropleth
         legend.key.size = unit(1, "cm"),
         legend.text=element_text(colour = "black", size = 10, family = "sans"), 
         plot.title =element_text(colour = "black", size = 15, family = "sans", hjust = 0.5))

Fig_1d <- ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, group = group), 
               fill = 'gray', 
               color = 'gray',
               size=0.2) +
  coord_fixed(xlim = c(-115,-69),ylim = c(-25, 30))+
  geom_point(data=FD_Index, aes(x=Long, y=Lat, fill=fdis), size=3, shape=21)+
  scale_fill_distiller(palette = "RdYlGn")+
  theme (axis.text.x=element_text(size=10, angle=0, family = "sans"),
         axis.text.y = element_blank(),
         axis.title.y = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks = element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, size=1),
         panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.title=element_blank(),
         legend.position="bottom",
         legend.key=element_rect(fill = "white"),
         legend.key.height = unit(0.5,"cm"), # Size of the choropleth
         legend.key.size = unit(1, "cm"),
         legend.text=element_text(colour = "black", size = 10, family = "sans"), 
         plot.title =element_text(colour = "black", size = 15, family = "sans", hjust = 0.5))

