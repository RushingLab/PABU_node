###################################################################################
## Diane Klement
## May 8 2024
##
## Code to combine used/avail RSF locations created by the "13_rsf_used_avail.R" script with spatial layers
##
###################################################################################

# Loading necessary packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(sp)
library(adehabitatLT)
library(tidyverse)
library(hablar)
library(rlang)
library(ggmap)
library(MASS)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(raster)
library(data.table)
library(Rcpp)
library(terra)

# Reset R's brain - removes all previous objects
rm(list=ls())

################################################################################
# BRINGING IN USED/AVAIL DATA

# Reading in used and available data
df <- readRDS("data/rsf/used_avail.rds")

################################################################################
# BRINGING IN RASTER LAYERS

# For spatial layers, .tif or .grid files are best

## LSSI VEG DATA FROM arcGIS
veg.data <- raster("data/spatial_layers/lssi-veg2.tif")
#check classes and number of cells per class
freq(veg.data)
# plot
plot(veg.data)

##trying to rename veg classes
# Reclass structure for burn
veg_rclass_df <- data.frame(value = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                            name = c('Southern_Atlantic_Coastal_Plain_Dune_and_Maritime_Grassland', 
                                     'Southern_Atlantic_Coastal_Plain_Fresh_and_Oligohaline_Tidal_Marsh', 
                                     'Southern_Atlantic_Coastal_Plain_Salt_and_Brackish_Tidal_Marsh', 
                                     'Southern_Atlantic_Coastal_Plain_Tidal_Wooded_Swamp',
                                     'Southern_Atlantic_Coastal_Plain_Maritime_Forest',
                                     'Open-Water_Ponds_and_Lakes', 
                                     'Southern_Coastal_Plain_Interdunal_Wetland',
                                     'Southern_Atlantic_Coastal_Plain_Sea_Island_Beach',
                                     'Development',
                                     'Early_Successional_Vegetation'))
veg_rclass <- subs(veg.data, veg_rclass_df[,1:2], subsWithNA = T)
names(veg_rclass) <- 'veg_reclass'

windows()
plot(veg_rclass)
levels(veg_rclass)

## LSSI burn DATA FROM arcGIS (as Tracts)
burn.data <- raster("data/spatial_layers/lssiburn3.tif")
#check classes and number of cells per class
freq(burn.data)
# plot
plot(burn.data)

##trying to rename burn classes
# Reclass structure for burn
burn_rclass_df <- data.frame(value = c(1, 2, 3, 4, 5, 6, 7),
                            name = c('Year_4-5', 
                                        'Year_2-3', 
                                        'Year_0-1', 
                                        'Year_3-4', 
                                        'Year_1-2_Mow', 
                                        'Year_1-2', 
                                        'Unburned'))
burn_rclass <- subs(burn.data, burn_rclass_df[,1:2], subsWithNA = T)
names(burn_rclass) <- 'burn_reclass'
as.factor(burn_rclass)

burn <- readRDS("data/veg_data.rds")

## LSSI burn DATA FROM arcGIS (as time since management)
burn.data.time <- raster("data/spatial_layers/lssibtime_1.tif")

#check classes and number of cells per class
freq(burn.data.time)
# plot
plot(burn.data.time)



# Ensuring that rasters are the same extent
rsts <- list(burn_rclass, veg_rclass)
for (i in 2:length(rsts)) {
  rsts[[i]] <- resample(rsts[[i]], burn_rclass)
} 
# Stacking the rasters
rastStack <- stack(burn_rclass)


# Recall our used/avail combined data.frames
coordinates(df) <- df[, c('UTMx', 'UTMy')]
proj4string(df) <- CRS('+proj=utm +zone=17 +datum=WGS84') # ensuring that they both have UTM 17 designations

# Extracting  data from rastStack
predictors <- extract(rastStack, df, na.rm=F)
str(predictors)

# Combining point data with predictors 
outDF <- cbind(df@data, predictors)
str(outDF)
names(outDF)

na.omit
outDF1 <- outDF[!is.na(outDF$burn_reclass),]

# Identify columns with continuous variables to standardize continuous variables
names(outDF) #lssibtime_1

str(outDF)
outDF$lssibtime_1_std <- outDF$lssibtime_1

# Standardizes all continuous predictors
mean(outDF$lssibtime_1, na.rm=T) #1.4835
sd(outDF$lssibtime_1, na.rm=T) #0.8786342

outDF$lssibtime_1_std <- scale(outDF$lssibtime_1)


d <- outDF1

# Build dummy codes for veg
# 1 if of that type
# 0 if not of that type
levels(d$burn_reclass)
d$Year2_3 <- ifelse(d$burn_reclass=='2', 1, 0)
d$Year0_1 <- ifelse(d$burn_reclass=='3', 1, 0)
d$Year3_4 <- ifelse(d$burn_reclass=='4', 1, 0)
d$Year1_2 <- ifelse(d$burn_reclass=='6', 1, 0)
d$Unburned <- ifelse(d$burn_reclass=='7', 1, 0)
str(d)

levels(d$veg_reclass)
d$Dune_Mar_Grassland <- ifelse(grepl('^1', d$veg_reclass), 1, 0)
d$Fresh_Tidal_Marsh <- ifelse(grepl('^2', d$veg_reclass), 1, 0)
d$Salt_Tidal_Marsh <- ifelse(grepl('^3', d$veg_reclass), 1, 0)
d$Tidal_Wooded_Swamp <- ifelse(grepl('^4', d$veg_reclass), 1, 0)
d$Mar_Forest <- ifelse(grepl('^5', d$veg_reclass), 1, 0)
d$Ponds_Lakes <- ifelse(grepl('^6', d$veg_reclass), 1, 0)
d$Interdune_Wetland <- ifelse(grepl('^7', d$veg_reclass), 1, 0)
d$Beach <- ifelse(grepl('^8', d$veg_reclass), 1, 0)
head(d)
tail(d)
summary(d)

# Saving RSF data as an RDS
saveRDS(d, "data/rsf/rsf_data.rds")

saveRDS(d, "data/rsf/rsf_data_burn.rds")
