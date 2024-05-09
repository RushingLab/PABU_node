## March 12, 2024
#AKDE with ctmm package
#another brain damaged tort production

#so this code in two steps for you
#first make akde
#then do home ranges
#then some extra resource selection stuff that not important

## The following links can be very helpful: 
##      -https://ecoisilva.github.io/AKDE_minireview/code/AKDE_R-tutorial.html
##      -https://cran.r-project.org/web/packages/ctmm/vignettes/akde.html
##

###################################################################################

# Loading packages
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(raster)
library(sf)
library(ctmm)
library(tidyverse)

###################################################################################

# Loading in mean movement locations from CTT Nodes for all PABU from the 2023 field season
locations <- readRDS("data/trilateration/Error.sd.cov.Estimated.Locations90p-90_2023-05-10_2023-11-20.rds")
str(locations)

# To use the ctmm package to make aKDEs will have to format data according to MoveBank conventions
locs1 <- locations %>%
              rename(ID = TagId) %>% # changing the TagId name to ID
              rename(timestamp = Time.group) %>% # changing the Time.group column name to timestamp
              rename(utm.easting = UTMx_est) %>% # changing simulated UTMx column name to utm.easting
              rename(utm.northing = UTMy_est) %>% # changing simulated UTMy column name to utm.northing
              mutate(UTM.zone = "17 +north") # creating a column for the UTM zone

# Create column 't' (time in seconds from 1970), this is the time interval in seconds/minutes/hours between detection pings
locs1[['t']] <- time_length(interval(ymd("1970-01-01"), locs1$timestamp), "hour")

# Removing the seconds from the POSIXct time format
locs1[['timestamp']] <- as.POSIXct(locs1[['timestamp']],
                                       format = "%m/%d/%Y %H:%M")

# Checking the data structure
str(locs1)

# Remove unnecessary columns (any thing that isn't id, timestamp, t, utm coords, utm zone)
locs.clean <- locs1 %>%
                    dplyr::select(c(ID, timestamp, t, utm.easting, utm.northing, UTM.zone))

str(locs.clean) # checking the data structure -- ensure the column order is ID, timestamp, t, utm.easting, utm.northing, UTM.zone

rm(locs1) # to free up RAM
rm(locations) # to free up RAM

# Coercing movement data.frame into a telemetry object to use in ctmm
locs.tel <- as.telemetry(locs.clean, timezone = 'UTC') 
locs.tel <- locs.tel[-16] # removing the test tag from the list

# Looking for outliers
OUT <- lapply(locs.tel,outlie)

# Looking for asymptote
SVF.locs.tel <- lapply(locs.tel, variogram)
plot(SVF.locs.tel[[1]])
plot(SVF.locs.tel[[7]])

# Automated guesstimate for calibrated data
GUESS.locs.tel <- ctmm.guess(locs.tel[[1]], interactive = FALSE)
GUESS.locs.tel_list <- list()  # Create an empty list to store the results

for (i in 1:length(locs.tel)){
  GUESS.locs.tel <- ctmm.guess(locs.tel[[i]], interactive = FALSE)
  GUESS.locs.tel_list[[paste0(i)]] <- GUESS.locs.tel
  }


M.IID <- ctmm.fit(locs.tel[[1]]) # no autocorrelation timescales, KDE
GUESS <- ctmm.guess(locs.tel[[1]], interactive=FALSE) # automated model guess
M.OUF <- ctmm.select(locs.tel[[1]], GUESS)

KDE <- akde(locs.tel[[1]], M.IID) # KDE
AKDE <- akde(locs.tel[[1]], M.OUF) #AKDE
wAKDE <- akde(locs.tel[[1]], M.OUF, weights=TRUE) # weighted AKDE

plot(KDE)
title(expression("IID KDE"["C"]))
plot(AKDE)
title(expression("OUF AKDE"["C"]))
plot(wAKDE)
title(expression("weighted OUF AKDE"["C"]))


# calculate one extent for all UDs
EXT <- extent(list(KDE,AKDE,wAKDE),level=0.95)

plot(locs.tel[[7]],UD=KDE)
title(expression("IID KDE"["C"]))
plot(locs.tel[[7]],UD=AKDE)
title(expression("OUF AKDE"["C"]))
plot(locs.tel[[7]],UD=wAKDE)
title(expression("weighted OUF AKDE"["C"]))

# comparing the area estimates and effective sample sizes
summary(KDE)$CI
summary(AKDE)$CI
summary(wAKDE)$CI

carlosw1_pHREML <- ctmm.select(locs.tel[[1]], GUESS.locs.tel, verbose = TRUE, method = 'pHREML') #perturbed hybrid REML

summary(carlosw1_pHREML) #get model selection table
UD1_carlos_w1 <- akde(locs.tel[[1]], carlosw1_pHREML, weights = TRUE)

summary(UD1_carlos_w1) #info about akde area
plot(UD1_carlos_w1)
class(UD1_carlos_w1)

#to change percentage use level.UD function (what size akde)
summary(UD1_carlos_w1, level.UD = 0.50)
plot(locs.tel[[1]],UD=UD1_carlos_w1)

plot(locs.tel, col=rainbow(length(locs.tel)))

writeShapefile(UD1_carlos_w1,"E:/Final Location Estimates, Shapefiles, MCPs and AKDE",file="PT2A2A6166_LT4C333366_OnlyFLGData_95akde",convex=FALSE,level.UD=0.95,level=0.95)

#convert to usable format for ARCGIS
shapefile_polygons_carlosw1 <- as.sf(UD1_carlos_w1, level.UD=0.50, level=0.50)
middle_polygon <- shapefile_polygons_carlosw1[2,]  #selects the second row which is the 95% est middle polygon
middle_polygon2 <- shapefile_polygons_carlosw1[2,]  #selects the second row which is the 95% est middle polygon
z <- crs('+proj=longlat +datum=WGS84')
final_polygon_carlosw1 <- st_transform(middle_polygon, crs=z) # match crs to base data so everything is aligned 

plot(final_polygon_carlosw1)
nrow(carlos.w1)


#don't worry about this here, this is making random points in home range for resource selection
#1x
randAvail_carlos1.1x <- spsample(as_Spatial(final_polygon_carlosw1),n=nrow(carlos.w1),"random", pretty=F)
nrow(randAvail_carlos1.1x@coords)

#save points csv
write.csv(randAvail_carlos1.1x, "./Available_pts/carlos_winter1.csv")

plot(as_Spatial(final_polygon_carlosw1))
#points(tt.w1[4:5], pch=20) #bird locations
points(randAvail_carlos1.1x, pch=20)

####################################