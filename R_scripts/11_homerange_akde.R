#AKDE with ctmm package
#another brain damaged tort production

#so this code in two steps for you
#first make akde
#then do home ranges
#then some extra resource selection stuff that not important

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

# Loading in simulated movement data for all PABU from the 2023 field season
sim.locs <- readRDS("data/homeranges/Simulated.Locations500.HomeRange.90p-90.locs.filtered_2023-05-10_2023-11-20.rds")
str(sim.locs) # checking format/columns

# To use the ctmm package to make aKDEs will have to format data according to MoveBank conventions
sim.locs1 <- sim.locs %>%
              rename(ID = TagId) %>% # changing the TagId name to ID
              rename(timestamp = Time.group) %>% # changing the Time.group column name to timestamp
              rename(utm.easting = UTMx_sim) %>% # changing simulated UTMx column name to utm.easting
              rename(utm.northing = UTMy_sim) %>% # changing simulated UTMy column name to utm.northing
              mutate(UTM.zone = "17 +north") # creating a column for the UTM zone

# Create column 't' (time in seconds from 1970), this is the time interval in seconds/minutes/hours between detection pings
sim.locs1[['t']] <- time_length(interval(ymd("1970-01-01"), sim.locs1$timestamp), "hour")

# Removing the seconds from the POSIXct time format
sim.locs1[['timestamp']] <- as.POSIXct(sim.locs1[['timestamp']],
                                       format = "%m/%d/%Y %H:%M")

# Checking the data structure
str(sim.locs1)

# Remove unnecessary columns (any thing that isn't id, timestamp, t, utm coords, utm zone)
sim.locs.clean <- sim.locs1 %>%
                    dplyr::select(c(ID, timestamp, t, utm.easting, utm.northing, UTM.zone))

str(sim.locs.clean) # checking the data structure -- ensure the column order is ID, timestamp, t, utm.easting, utm.northing, UTM.zone

# Coercing movement data.frame into a telemetry object to use in ctmm
sim.locs.tel <- as.telemetry(sim.locs.clean, timezone = 'UTC') 

SVF.carlos <- variogram(Tag.carlos.w1) #to look for asymptote
plot(SVF.carlos)

GUESS.carlos.w1 <- ctmm.guess(Tag.carlos.w1, interactive = FALSE)

carlosw1_pHREML <- ctmm.select(Tag.carlos.w1, GUESS.carlos.w1, verbose = TRUE, method = 'pHREML') #perturbed hybrid REML

summary(carlosw1_pHREML) #get model selection table
UD1_carlos_w1 <- akde(Tag.carlos.w1, carlosw1_pHREML, weights = TRUE)

summary(UD1_carlos_w1) #info about akde area
plot(UD1_carlos_w1)
class(UD1_carlos_w1)

#to change percentage use level.UD function (what size akde)
summary(UD1_carlos_w1, level.UD = 0.50)
plot(Tag.carlos.w1,UD=UD1_carlos_w1)

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