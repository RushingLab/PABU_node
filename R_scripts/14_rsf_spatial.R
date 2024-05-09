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
elev2 <- projectRaster(elev, crs = "+proj=longlat +datum=WGS84") ##raster with correct CRS

# Assign a CRS object
proj4string(elev2) <- CRS('+proj=longlat +datum=WGS84')

#Importing LSSI Boundary
bound <- readOGR('C:/Users/dklem/Documents/ArcGIS/Projects', 'LittleSSI')
bound@proj4string
class(bound)
windows()
plot(bound)


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
as.factor(veg_rclass)

windows()
plot(veg.data)
levels(veg.data)

## LSSI burn DATA FROM arcGIS
burn.data <- raster("data/spatial_layers/lssi_burn1.tif")
#check classes and number of cells per class
freq(burn.data)
# plot
plot(burn.data)

# Extending the size of the raster to match that of the veg
burn.extend <- extend(burn.data, veg.data)
windows()
plot(burn.extend)
  

##trying to rename burn classes
# Reclass structure for burn
burn_rclass_df <- data.frame(value = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17),
                            name = c('Tract1', 
                                        'Tract2', 
                                        'Tract3', 
                                        'Tract4', 
                                        'Tract5', 
                                        'Tract6', 
                                        'Tract7', 
                                        'Tract8', 
                                        'Tract9', 
                                        'Tract10', 
                                        'Tract11', 
                                        'Tract12', 
                                        'Tract13', 
                                        'Tract14', 
                                        'Tract15', 
                                        'Tract16', 
                                        'Tract17'))
burn_rclass <- subs(burn.extend, burn_rclass_df[,1:2], subsWithNA = T)
names(burn_rclass) <- 'burn_reclass'
as.factor(burn_rclass)


##creating a raster stack with reclassified veg data
rastStack2 <- stack(list(burn_rclass, veg_rclass))
#but didn't work so...


# Ensuring that rasters are the same extent
rsts <- list(burn_rclass, veg_rclass)
for (i in 2:length(rsts)) {
  rsts[[i]] <- resample(rsts[[i]], burn_rclass)
} 
# Stacking the rasters
rastStack4 <- stack(rsts)


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


##WEEK 7 pt 2
head(outDF.1)
str(outDF.1)

# 'veg_reclass' is stored as a numeric but should
# be a factor.  Less reclass as a factor first.
outDF.1$veg_reclass <- as.factor(outDF.1$veg_reclass)
class(outDF.1$veg_reclass)
levels(outDF.1$veg_reclass)
d <- outDF.1

# Build dummy codes, 
# 1 if of that type
# 0 if not of that type
levels(d$veg_reclass)
d$Non_veg <- ifelse(d$veg_reclass=='1', 1, 0)
d$Closed_Tree_Canopy <- ifelse(d$veg_reclass=='3', 1, 0)
d$Herb_Grass <- ifelse(d$veg_reclass=='4', 1, 0)
d$Open_Tree_Canopy <- ifelse(d$veg_reclass=='5', 1, 0)
head(d)
tail(d)
summary(d)


# Summary...output not shown

##WEEK 10 pt 1
## creating a GLMM
##A. data import

d <- read.csv('C:/Users/dklem/Documents/Classes/WILD 8321/RSFdata_std_3rdOrder_Kernel_rand10x.csv')
# Change veg_reclass to a factor!!
d$veg_reclass <- as.factor(d$veg_reclass)
# Data inspection...
head(d)
# Variable names
names(d)
# Data import
d1 <- d[,c('ID', 'Used', 'veg_reclass', 'EucDist_feed5')]
unique(d1$veg_reclass)

# Load the necessary packages
library(lme4)

# run a null model
mNull <- glmer(Used ~ 1 + (1|ID), data = d, family = 'binomial')
# call model output
summary(mNull)
mNull

##running univariate models
#model with veg type
mVeg <- glmer(Used ~ veg_reclass + (1|ID), data = d, family = 'binomial')
#model with distance to feeder
mFeeder <- glmer(Used ~ EucDist_feed5 + (1|ID), data = d, family = 'binomial')
summary(mVeg)
summary(mFeeder)
# model with distance polynomial
mFeeder2 <- glmer(Used ~ EucDist_feed5 + I(EucDist_feed5^2) + (1|ID), data = d, family = 'binomial')
summary(mFeeder2)
summary(mNull)

#running the categorical model
mVEG <- glmer(Used ~ veg_reclass + (1|ID), data = d, family = 'binomial')
summary(mVEG)


##running a global model
library(lme4)
mGlobal <- glmer(Used ~ veg_reclass + EucDist_feed5 + (1|ID) , data = d, family = 'binomial')
mGlobal1 <- glmer(Used ~ veg_reclass + EucDist_feed5 + 
                    I(EucDist_feed5^2) + (1|ID), data = d, family = 'binomial')
summary(mGlobal1)
summary(d)

#including all dummy codes except for most common one - herb_grass
m1 <- glmer(Used ~  Non_veg + Closed_Tree_Canopy + 
              Open_Tree_Canopy + (1|ID), data = d, family = 'binomial')
summary(m1)


library(AICcmodavg)
aictab(list(mNull, mFeeder, mVeg, mFeeder2,
            mGlobal, mGlobal1),
       modnames = c('mNull', 'mFeeder', 'mVeg', 'mFeeder2',
                    'mGlobal', 'mGlobal1'),
       sort = T, nobs = 7)

##likelihood ratio test
mTop <- mNull
mNull_glm <- glm(Used~ 1, data = d, family = 'binomial')
# LRT using anova()
anova(mNull, mNull_glm, test = 'Chisq')

mTop <- mFeeder
library(lme4)

# Random effects and CI estimates
(fixed <- fixef(mTop))
(fixed.1 <- fixef(mTop1))

parms <- names(fixed) 
(fixedCI <- confint(mTop, level = 0.95, parm=parms, 
                    method='profile'))  # Change method to 'Wald' for speed
# Pull the coefficient estimates from the summary table
loTable <- as.data.frame(coef(summary(mTop)))[,1:2]
# Add the CIs
(loTable <- cbind(loTable, fixedCI))
(oTable <- exp(loTable))
#export table
write.csv(oTable, 'C:/Users/dklem/Documents/Classes/WILD 8321/TopModel_OddsRatio_PABURSF1.csv')

#coefficient plots
# Load ggplot2
library(ggplot2)

# Plot function
pTable <- loTable
pTable$params <- factor(rownames(loTable), levels = rownames(loTable))
names(pTable)[3:4] <- c('ll', 'ul')

# W/o intercept
windows()
qplot(params, Estimate, ymin=ll, ymax=ul, data=pTable[-1,],
      geom='pointrange') + 
  geom_hline(aes(yintercept = 0)) +
  coord_flip() +
  theme_bw()

# For random effects modes and CI estimates
(cV <- ranef(mTop, condVar=T))

d2 <- read.csv('C:/Users/dklem/Documents/Classes/WILD 8321/RSFdata_std_3rdOrder_Kernel_rand10x.csv')
knitr::kable(table(d2[d2$Used==1, 'ID'], d2[d2$Used==1, 'Year']), caption = "Table 3: Data counts by ID by Year")
# Load the lattice (plotting) library
library(lattice)

# Random effects modes and CI estimates
windows()
dotplot(cV)

1- exp(-0.539223)
1- exp(-0.277)
mFeeder
1- exp(-0.003731)
mFeeder

