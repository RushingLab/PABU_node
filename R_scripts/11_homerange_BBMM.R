###################################################################################
## Diane Klement
## March 22 2024
##
## Code to create Brownian Bridge Movement Model (BBMM) Home Ranges using the mean location points generated from CTT Nodes
##
## A BBMM, a continuous-time stochastic model of movement in which the probability of being in an area is conditioned on starting 
## and ending locations, the elapsed time between those points, and the mobility or speed of movement (Horne et al. 2007).
## Unlike a Kernel Density Estimator, it assumes that locations are not independent.
## We then can use this movement model to extract home range estimations.
## 
## The following articles were helpful in working through this code:
##      - https://www.rdocumentation.org/packages/adehabitatHR/versions/0.4.21/topics/kernelbb
##      - https://cran.r-project.org/web/packages/adehabitatLT/vignettes/adehabitatLT.pdf
##      - https://bartk.gitlab.io/move/articles/move.html#dynamic-brownian-bridge-movement-model-dbbmm
##
##    Input:
##      - Estimated.Locations-feb20-90p-90_2023-05-10_2023-11-20.rds
##            - Rds file that contains the estimated locations along with the sd and error estimates associated with locations.
##            - Generated from the 'trilateration' function in "05_functions_rss.based.locations.R" script
##            - Columns:
##                - TagId: CTT tag ID of the bird detected at the location point, factor
##                - Time.group: yyyy-mm-dd hh:mm:ss, POSIXct, time when the location point was detected
##                - Hour: hour when the location point was detected
##                - No.Nodes: number of nodes that provided RSSI values for a location point
##                - UTMx_est: estimated location (mean) for UTMx for a location point
##                - UTMy_est: estimated location (mean) for UTMy for a location point
##                - x.LCI: lower confidence interval value for UTMx for a location point
##                - x.UCI: upper confidence interval value for UTMx for a location point
##                - y.LCI: lower confidence interval value for UTMy for a location point
##                - y.UCI: upper confidence interval value for UTMy for a location point
##
##    Outputs:
##      - 
###################################################################################

# Loading packages
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(raster)
library(tidyverse)
library(adehabitatLT)
library(remotes)
install_github("cran/adehabitatHR")
library(adehabitatHR)
library(move)
library(circular)
library(tidyverse)
library(terra)

# Loading in mean movement locations from CTT Nodes for all PABU from the 2023 field season
locations <- readRDS("data/trilateration/Error.sd.cov.Estimated.Locations90p-90_2023-05-10_2023-11-20.rds")
str(locations)

## PREPARING DATA FOR BBMM
#Subsetting TagIds with counts greater than 45 and removing the test tag
locations_tags <- locations %>%
  group_by(TagId) %>%
  summarize(count=n()) %>% #counting up the number of records per TagId
  filter(count > 45, TagId != "6166071E") %>% #keeping tags where beeps >45 and removing the test tag
  pull(TagId) #subsetting tagids with counts greater than 45
droplevels(locations_tags)

# Filtering the original location data with counts > 45
subset_locations <- locations %>%
  filter(TagId %in% locations_tags)
locations_subset <- droplevels(subset_locations) #dropping low tag levels
str(locations_subset)

# Identifying all unique individuals
id <- levels(locations_subset$TagId)

# Sorting the data for the move function (otherwise it will not run)
locations_subset <- locations_subset[order(locations_subset$TagId,locations_subset$Time.group),]

# Creating a move object for all PABU using the Move package
loc <- move(x=locations_subset$UTMx_est, y=locations_subset$UTMy_est, time=as.POSIXct(locations_subset$Time.group, format= "%Y-%m-%d %H:%M:%S", tz="UTC"),
            proj=CRS("+proj=utm +zone=17 +datum=WGS84"), data=locations_subset, 
            animal=locations_subset$TagId)

loc2<- loc[[1]]

x <- locations_subset$x.sd
y <- locations_subset$y.sd
xy <- c(x,y)


bgbhr <- dynBGB(loc)
dynBBMM

all_dbbmm <- brownian.bridge.dyn(object=loc, location.error= x, window.size = 31, margin = 15, raster = 100)

for (i in id){
  #subset data based on current tag id
  subset_data <- locations_subset[locations_subset$TagId == i,]
  
  for (j in length(subset_data)){ # having the function read each point for the current tag id
  
  # making subset data a move object
  move_data <- move(x=subset_data$UTMx_est, y=subset_data$UTMy_est, time=subset_data$Time.group, 
                    proj=CRS("+proj=utm +zone=17 +datum=WGS84"), data=subset_data, animal=subset_data$TagId)
  
  # creating browninan bridge
  d_dbbmm <- brownian.bridge.dyn(object=move_data, location.error = 42, ext = c(subset_data$x.LCI[j], subset_data$x.UCI[j], subset_data$y.LCI[j], subset_data$y.UCI[j]),
                                 margin=7, dimsize=100)
  
  bgbhr <- dynBGB(loc, window=extent(loc))
  
  plot(d_dbbmm)
}

}

for (i in id){
  #subset data based on current tag id
  subset_data <- locations_subset[locations_subset$TagId == "072A0766",]
  
    # making subset data a move object
    move_data <- move(x=subset_data$UTMx_est, y=subset_data$UTMy_est, time=subset_data$Time.group, 
                      proj=CRS("+proj=utm +zone=17 +datum=WGS84"), data=subset_data, animal=subset_data$TagId)

    
    move.prj <- spTransform(move_data, center=TRUE)
    
    # creating browninan bridge
    d_dbbmm <- brownian.bridge.dyn(object=move.prj, location.error = 42, raster = 100)
    
    plot(d_dbbmm)

  
}

# Making the dataframe an ltraj to store trajectories of animals
lt <- as.ltraj(locations_subset[,c("UTMx_est", "UTMy_est")], date = locations_subset$Time.group, id = locations_subset$TagId,
               burst = locations_subset$TagId, typeII = T, slsp = c("remove", "missing"))

##WORKING THROUGH PARAMETERS FOR BBMM
# sig1 = parameter that controls the width of the "bridge" connecting successive relocations; the larger it is, the larger the bridge is; liker function; speed of animal
# sig2 = parameter that controls the width of the bumps added over the relocations (Bullard, 1999;Horne et al 2007); similar to smoothing parameter of kernel; error of location estimate

# Using the liker function to find the maximum likelihood estimation of the parameter sig1
# 42.43 = median localization error of trilateration estimates when -90 RSSI filter was applied
sig2 <- 42.43


# Finding the maximum likelihood estimation of the parameter sig1
liker(lt, sig2 = 42.43, rangesig1= c(1,10))
tata <- kernelbb(lt[1], sig1=10, sig2=42.43, grid=100)
hr <- getverticeshr(tata, percent = 75, unim = 'm')
proj4string(hr) <- CRS('+proj=utm +zone=17 +datum=WGS84')
image(hr)

