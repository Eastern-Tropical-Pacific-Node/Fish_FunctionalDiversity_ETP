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

library(ggpubr)
ggarrange(Fig_1a, Fig_1b, Fig_1c, Fig_1d, align="hv", ncol=2, nrow=2,
          font.label = list(size=13, color="black", face="bold", family = "sans"))

# 3. Model figures ----------------------------
library (ggmcmc)
library(tidybayes)
library (ggplot2)

# Figure species richness model 
model1tranformed_fmm_sppRic <- ggs(full_sppRic) 
model1tranformed_fmm_sppRic <- droplevels (subset(model1tranformed_fmm_sppRic, Parameter=="b_shelf_area_scale"|
                                                    Parameter=="b_distland_scale"|
                                                    Parameter=="b_sstmean_scale"|
                                                    Parameter=="b_protection_scale"|
                                                    Parameter=="b_quaternary_scale"|
                                                    Parameter=="b_dailypp_scale"|
                                                    Parameter=="b_gravity_scale"))

Fig2a <-model1tranformed_fmm_sppRic %>% 
  ggplot(aes(y = factor(Parameter,levels = c("b_quaternary_scale",
                                             "b_shelf_area_scale",
                                             "b_distland_scale",
                                             "b_sstmean_scale",
                                             "b_dailypp_scale",
                                             "b_protection_scale",
                                             "b_gravity_scale")), 
             x = value, fill=Parameter)) +
  stat_halfeye()+
  scale_fill_manual(values = c("grey90","grey90",
                               "grey90","#009B9F","grey90",
                               "#D99BC5","#009B9F"))+
  scale_y_discrete(breaks=c("b_quaternary_scale",
                            "b_shelf_area_scale",
                            "b_distland_scale",
                            "b_sstmean_scale",
                            "b_dailypp_scale",
                            "b_protection_scale",
                            "b_gravity_scale"),
                   labels=c("Dis.Ref","Shelf.Area","Dist.Land",
                            "SST","PP","Pro.Level","Gravity"))+
  geom_vline(xintercept = 0, linetype = "dashed", cex=.25)+
  theme_bw() +
  #lims(x=c(-.5,.5))+
  theme(axis.text = element_text(size = 11),
        axis.title = element_blank(),
        legend.position = "none")

# Figure functional richness model 
model1tranformed_FRic <- ggs(fmmFRic) 
model1tranformed_FRic <- droplevels (subset(model1tranformed_FRic, Parameter=="b_shelf_area_scale"|
                                              Parameter=="b_distland_scale"|
                                              Parameter=="b_sstmean_scale"|
                                              Parameter=="b_protection_scale"|
                                              Parameter=="b_quaternary_scale"|
                                              Parameter=="b_dailypp_scale"|
                                              Parameter=="b_gravity_scale"))

Fig2b <- model1tranformed_FRic %>% 
  ggplot(aes(y = factor(Parameter,levels = c("b_quaternary_scale",
                                             "b_shelf_area_scale",
                                             "b_distland_scale",
                                             "b_sstmean_scale",
                                             "b_dailypp_scale",
                                             "b_protection_scale",
                                             "b_gravity_scale")), 
             x = value, fill=Parameter)) +
  stat_halfeye()+
  scale_fill_manual(values = c("grey90","grey90",
                               "grey90","#009B9F","grey90",
                               "#D99BC5","#009B9F"))+
  scale_y_discrete(breaks=c("b_quaternary_scale",
                            "b_shelf_area_scale",
                            "b_distland_scale",
                            "b_sstmean_scale",
                            "b_dailypp_scale",
                            "b_protection_scale",
                            "b_gravity_scale"),
                   labels=c("Dis.Ref","Shelf.Area","Dist.Land",
                            "SST","PP","Pro.Level","Gravity"))+
  geom_vline(xintercept = 0, linetype = "dashed", cex=.25)+
  theme_bw() +
  lims(x=c(-1,1))+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")


# Figure functional evenness model
model1tranformed_fmmFeve <- ggs(fmmFeve)
model1tranformed_fmmFeve <- droplevels (subset(model1tranformed_fmmFeve, Parameter=="b_shelf_area_scale"|
                                                 Parameter=="b_distland_scale"|
                                                 Parameter=="b_sstmean_scale"|
                                                 Parameter=="b_protection_scale"|
                                                 Parameter=="b_quaternary_scale"|
                                                 Parameter=="b_dailypp_scale"|
                                                 Parameter=="b_gravity_scale"))
Fig2c <-model1tranformed_fmmFeve %>% 
  ggplot(aes(y = factor(Parameter,levels = c("b_quaternary_scale",
                                             "b_shelf_area_scale",
                                             "b_distland_scale",
                                             "b_sstmean_scale",
                                             "b_dailypp_scale",
                                             "b_protection_scale",
                                             "b_gravity_scale")), 
             x = value, fill=Parameter)) +
  stat_halfeye()+
  scale_fill_manual(values = c("grey90","grey90",
                               "#009B9F","grey90","grey90",
                               "grey90","grey90"))+
  scale_y_discrete(breaks=c("b_quaternary_scale",
                            "b_shelf_area_scale",
                            "b_distland_scale",
                            "b_sstmean_scale",
                            "b_dailypp_scale",
                            "b_protection_scale",
                            "b_gravity_scale"),
                   labels=c("Dis.Ref","Shelf.Area","Dist.Land",
                            "SST","PP","Pro.Level","Gravity"))+
  geom_vline(xintercept = 0, linetype = "dashed", cex=.25)+
  theme_bw() +
  lims(x=c(-0.25,0.25))+
  theme(axis.text = element_text(size = 11),
        axis.title = element_blank(),
        legend.position = "none")

# Figure functional dispersion model
model1tranformed_fmmFdis <- ggs(fmmFdis)
model1tranformed_fmmFdis <- droplevels (subset(model1tranformed_fmmFdis, Parameter=="b_shelf_area_scale"|
                                                 Parameter=="b_distland_scale"|
                                                 Parameter=="b_sstmean_scale"|
                                                 Parameter=="b_protection_scale"|
                                                 Parameter=="b_quaternary_scale"|
                                                 Parameter=="b_dailypp_scale"|
                                                 Parameter=="b_gravity_scale"))
Fig2d <- model1tranformed_fmmFdis %>% 
  ggplot(aes(y = factor(Parameter,levels = c("b_quaternary_scale",
                                             "b_shelf_area_scale",
                                             "b_distland_scale",
                                             "b_sstmean_scale",
                                             "b_dailypp_scale",
                                             "b_protection_scale",
                                             "b_gravity_scale")), 
             x = value, fill=Parameter)) +
  stat_halfeye()+
  scale_fill_manual(values = c("grey90","grey90",
                               "grey90","grey90","grey90",
                               "#D99BC5","grey90"))+
  scale_y_discrete(breaks=c("b_quaternary_scale",
                            "b_shelf_area_scale",
                            "b_distland_scale",
                            "b_sstmean_scale",
                            "b_dailypp_scale",
                            "b_protection_scale",
                            "b_gravity_scale"),
                   labels=c("Dis.Ref","Shelf.Area","Dist.Land",
                            "SST","PP","Pro.Level","Gravity"))+
  geom_vline(xintercept = 0, linetype = "dashed", cex=.25)+
  theme_bw() +
  lims(x=c(-0.5,0.5))+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")

ggarrange(Fig2a, Fig2b, Fig2c, Fig2d, align="hv", ncol=2, nrow=2,
          font.label = list(size=13, color="black", face="bold", family = "sans"))

# 4. Posterior model figures ---------

QFit_MsppR <- pp_check(full_sppRic, ndraws = 100, type = 'ecdf_overlay')
QFit_MFric <- pp_check(fmmFRic, ndraws = 100, type = 'ecdf_overlay')
QFit_MFEv <- pp_check(fmmFeve, ndraws = 100, type = 'ecdf_overlay')
QFit_MFDis <- pp_check(fmmFdis, ndraws = 100, type = 'ecdf_overlay')

ggarrange(QFit_MsppR, QFit_MFric, fmmFeve, fmmFdis, align="hv", ncol=2, nrow=2,
          font.label = list(size=13, color="black", face="bold", family = "sans"))

