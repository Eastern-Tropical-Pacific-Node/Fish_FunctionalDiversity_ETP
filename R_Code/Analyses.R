## --------------------------------------------------------------------------------------------------------- ##
##  Authors:       Dubuc A., Quimbayo J.P et al.                                                             ##
##  Date:          2022-07-14                                                                                ##
##  Script created by: Quimbayo J.P                                                                                                         ##
##  Notes:         1. This file is intended to provide a guide to the basic                                  ##
##                    project workflow, attempting to 'integrate_analysis' the steps                         ##
##                    necessary to conduct the analyses & visual outputs.                                    ##
## --------------------------------------------------------------------------------------------------------- ##

rm (list=ls())
library(readxl)
library (dplyr)

# 1. Databases --------------------------------
coord_locations <- read.csv("Data/coord_locations.csv", header = T, sep=";", dec=",")
traits   <- read_excel("Data/Trait_DB.xlsx", sheet = 1)
traits   <- as.data.frame(lapply(traits, unlist))
FD_Index <- read.csv("Data/FD_Indices.csv", header = T)

# 2. Models ------------------------------------



