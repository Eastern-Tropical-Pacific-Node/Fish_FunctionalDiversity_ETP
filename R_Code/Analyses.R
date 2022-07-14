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
coord_locations <- read.csv("Data/Factors_Locations.csv", header = T)
traits   <- read_excel("Data/Trait_DB.xlsx", sheet = 1)
traits   <- as.data.frame(lapply(traits, unlist))
FD_Index <- read.csv("Data/FD_Indices.csv", header = T)


FD_Index <- left_join(FD_Index, coord_locations)
# 2. Models ------------------------------------
# 2.1 Re-scale variables 

rescale_variables <- function (x) {(x - mean(x))/(1*sd (x))}
FD_Index$sstmean_scale <- rescale_variables (FD_Index$sstmean)
FD_Index$shelf_area_scale <- rescale_variables (FD_Index$shelf_area)
FD_Index$distland_scale <- rescale_variables (FD_Index$distland)
FD_Index$quaternary_scale <- rescale_variables(log(FD_Index$quaternary)+1)
FD_Index$dailypp_scale <- rescale_variables(FD_Index$dailypp)
FD_Index$gravity_scale <- rescale_variables(log(FD_Index$gravity)+1)

FD_Index$protection_scale <- NA
FD_Index$protection_scale <- ifelse(FD_Index$protection=="non-protected", 1, FD_Index$protection_scale)
FD_Index$protection_scale <- ifelse(FD_Index$protection=="medium-size-managed", 2, FD_Index$protection_scale)
FD_Index$protection_scale <- ifelse(FD_Index$protection=="medium-size-highly-protected", 3, FD_Index$protection_scale)
FD_Index$protection_scale <- ifelse(FD_Index$protection=="large-size-highly-protected", 4, FD_Index$protection_scale)

FD_Index$protection_scale <- rescale_variables(FD_Index$protection_scale)

# Evaluating correlation among predictors 
# Add Histograms in function Pairs
panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

# Add Coeficients of correlation
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs (FD_Index[,18:length(FD_Index)], pch=13, 
       diag.panel=panel.hist, lower.panel=panel.cor, 
       panel=panel.smooth)

# 2.2. Variation inflation factor 
library (car)
vif (lm (richness ~ shelf_area_scale+
           distland_scale+sstmean_scale+
           protection_scale+
           quaternary_scale+dailypp_scale+
           gravity_scale, data=FD_Index))

# 2.3. Adding marine sub-province according to Robertson and Cramer 2009
FD_Index$MarineSubPro <- NA
FD_Index$MarineSubPro <- ifelse (FD_Index$country=="mexico_gulf", "CortezP", FD_Index$MarineSubPro)
FD_Index$MarineSubPro <- ifelse (FD_Index$country=="costa_rica", "PanamaP", FD_Index$MarineSubPro)
FD_Index$MarineSubPro <- ifelse (FD_Index$country=="colombia", "PanamaP", FD_Index$MarineSubPro)
FD_Index$MarineSubPro <- ifelse (FD_Index$country=="mexico", "PanamaP", FD_Index$MarineSubPro)
FD_Index$MarineSubPro <- ifelse (FD_Index$country=="panama", "PanamaP", FD_Index$MarineSubPro)
FD_Index$MarineSubPro <- ifelse (FD_Index$country=="el_salvador", "PanamaP", FD_Index$MarineSubPro)
FD_Index$MarineSubPro <- ifelse (FD_Index$country=="nicaragua", "PanamaP", FD_Index$MarineSubPro)
FD_Index$MarineSubPro <- ifelse (FD_Index$country=="ecuador", "PanamaP", FD_Index$MarineSubPro)

FD_Index$MarineSubPro <- ifelse (FD_Index$country=="france", "Island", FD_Index$MarineSubPro)
FD_Index$MarineSubPro <- ifelse (FD_Index$unique_locality=="cocos_Costa_Rica", "Island", FD_Index$MarineSubPro)
FD_Index$MarineSubPro <- ifelse (FD_Index$unique_locality=="galapagos_1_Ecuador", "Island", FD_Index$MarineSubPro)
FD_Index$MarineSubPro <- ifelse (FD_Index$unique_locality=="galapagos_2_Ecuador", "Island", FD_Index$MarineSubPro)
FD_Index$MarineSubPro <- ifelse (FD_Index$unique_locality=="galapagos_3_Ecuador", "Island", FD_Index$MarineSubPro)
FD_Index$MarineSubPro <- ifelse (FD_Index$unique_locality=="galapagos_4_Ecuador", "Island", FD_Index$MarineSubPro)
FD_Index$MarineSubPro <- ifelse (FD_Index$unique_locality=="galapagos_5_Ecuador", "Island", FD_Index$MarineSubPro)
FD_Index$MarineSubPro <- ifelse (FD_Index$unique_locality=="galapagos_6_Ecuador", "Island", FD_Index$MarineSubPro)
FD_Index$MarineSubPro <- ifelse (FD_Index$unique_locality=="malpelo_Colombia", "Island", FD_Index$MarineSubPro)
FD_Index$MarineSubPro <- ifelse (FD_Index$unique_locality=="revillagigedos_clarion_Mexico", "Island", FD_Index$MarineSubPro)
FD_Index$MarineSubPro <- ifelse (FD_Index$unique_locality=="revillagigedos_roca_partida_Mexico", "Island", FD_Index$MarineSubPro)
FD_Index$MarineSubPro <- ifelse (FD_Index$unique_locality=="revillagigedos_san_benedicto_Mexico", "Island", FD_Index$MarineSubPro)
FD_Index$MarineSubPro <- ifelse (FD_Index$unique_locality=="revillagigedos_socorro_Mexico", "Island", FD_Index$MarineSubPro)
unique (FD_Index$MarineSubPro)

# 2.4. Bayesian models
library (brms)

bcpriors <- prior_string("normal (0,1)","b")+
  prior_string("normal(0, 1)", "Intercept") +
  prior_string("gamma(2, 0.1)", "sd") +
  prior_string("gamma(2, 0.1)", "sigma")

# Species richness model
full_sppRic <- brm(log(richness+1)~quaternary_scale+
                     distland_scale+
                     shelf_area_scale+
                     sstmean_scale+
                     dailypp_scale+
                     protection_scale+
                     gravity_scale + 
                     (1|country/MarineSubPro),
                   prior = bcpriors,
                   data = FD_Index,
                   family = gaussian(), sample_prior = TRUE, chains = 4,
                   cores = 4, iter = 5e3, warmup = 2.5e3, 
                   control = list(adapt_delta = 0.99,
                                  max_treedepth = 18))
full_sppRic <- add_criterion(full_sppRic, "loo")
bayes_R2(full_sppRic)

QFit_MFric <- pp_check(full_sppRic, ndraws = 100, type = 'ecdf_overlay')

moransTest  <-  function (data, model_considered) {
  # Moran's I test of spatial autocorrelation
  coordinates       <-  unique(data[, c('unique_locality', 'Long', 'Lat')])
  scaleData         <-  data
  scaleData$lon     <-  coordinates$Long[match(scaleData$unique_locality, coordinates$unique_locality)]
  scaleData$lat     <-  coordinates$Lat[match(scaleData$unique_locality, coordinates$unique_locality)]
  distMat           <-  as.matrix(dist(cbind(scaleData$Long, scaleData$Lat)))
  invDistMat        <-  1 / distMat
  diag(invDistMat)  <-  0 
  set.seed(10)
  ape::Moran.I(residuals(model_considered)[, 'Estimate'], invDistMat)
}
moransTest (data = FD_Index, full_sppRic)

# Functional richness model
bcpriors <- get_prior (fric~quaternary_scale+
                         distland_scale+
                         shelf_area_scale+
                         sstmean_scale+
                         dailypp_scale+
                         protection_scale+
                         gravity_scale +
                         (1|country:MarineSubPro),
                       data=FD_Index, family="beta")

# Functional richness model 
fmmFRic <- brm(fric~quaternary_scale+
                 distland_scale+
                 shelf_area_scale+
                 sstmean_scale+
                 dailypp_scale+
                 protection_scale+
                 gravity_scale +  
                 (1|country:MarineSubPro), 
               data=FD_Index, family="beta",
               prior = bcpriors, sample_prior = TRUE, chains = 4,
               cores = 4, iter = 5e3, warmup = 2.5e3, 
               control = list(adapt_delta = 0.99,
                              max_treedepth = 18))

fmmFRic <- add_criterion(fmmFRic, "loo")
summary (fmmFRic)
bayes_R2(fmmFRic)
moransTest (data = FD_Index, fmmFRic)

# Functional evenness model

bcpriors <- get_prior(feve~quaternary_scale+
                        distland_scale+
                        shelf_area_scale+
                        sstmean_scale+
                        dailypp_scale+
                        protection_scale+
                        gravity_scale +
                        (1|country:MarineSubPro),
                      data=FD_Index, family="beta")

fmmFeve <- brm(feve~quaternary_scale+
                 distland_scale+
                 shelf_area_scale+
                 sstmean_scale+
                 dailypp_scale+
                 protection_scale+
                 gravity_scale +
                 (1|country:MarineSubPro),
               data=FD_Index, family="beta",
               prior = bcpriors, sample_prior = TRUE, chains = 4,
               cores = 4, iter = 5e3, warmup = 2.5e3, 
               control = list(adapt_delta = 0.99,
                              max_treedepth = 18))

fmmFeve <- add_criterion(fmmFeve, "loo")
summary (fmmFeve)
bayes_R2(fmmFeve)
moransTest (data = FD_Index, fmmFeve)

# Functional dispersion model
bcpriors <- get_prior(fdis~quaternary_scale+
                        distland_scale+
                        shelf_area_scale+
                        sstmean_scale+
                        dailypp_scale+
                        protection_scale+
                        gravity_scale +
                        (1|country:MarineSubPro),
                      data=FD_Index, family="beta")

fmmFdis <- brm(fdis~quaternary_scale+
                 distland_scale+
                 shelf_area_scale+
                 sstmean_scale+
                 dailypp_scale+
                 protection_scale+
                 gravity_scale +
                 (1|country:MarineSubPro),
               data=FD_Index2, family="beta",
               prior = bcpriors)

fmmFdis <- add_criterion(fmmFdis, "loo")
summary (fmmFdis)
bayes_R2(fmmFdis)
moransTest (data = FD_Index, fmmFdis)


