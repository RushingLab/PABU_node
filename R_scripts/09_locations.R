###################################################################################
## Diane Klement
## February 20 2024
##
## Code to map locations of birds from data produced by the "08_trilateration.from.beep.data.R" script
##
###################################################################################

## Loading packages
library(tidyverse)
library(ggplot2)
library(adehabitatLT)
library(sp)
library(move)
library(remotes)
install_github("cran/moveVis")
library(moveVis)

## Bringing in trilateration location estimates
locations <- readRDS("data/trilateration/Estimated.Locations-feb20-90p-90_2023-05-10_2023-11-20.rds") #53565 obs. of  10 variables
str(locations) # checking the data structure


# Visualizing the data

## Plotting the entire population over the field season
entire.pop <- ggplot(aes(x = UTMx_est, y = UTMy_est, color=TagId), data=locations) + 
  geom_point() + 
  ggtitle(paste("2023 Breeding Painted Bunting Population (Little Saint Simons Island, Georgia)"))+
  xlab("Estimated UTM Easting")+
  ylab("Estimated UTM Northing")+
  theme(panel.border =element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.line=element_line(colour="black"),
        axis.text=element_text(size=12, color="black"),
        axis.title.y=element_text(vjust=2),
        axis.title.x=element_text(vjust=1),
        plot.margin = unit(c(1,1,1,1),"lines"))
entire.pop

## create an ID specific file name each time it cycles to a new individual
PlotName <- paste("data/figures/territories_no_map/PABU_population_entire.jpeg")
PlotName

## use a ggplot2 function "ggsave" to save the plot, and then cycle to the next one.
ggsave(plot=entire.pop, filename = PlotName, width=5, height=3, units="in")

## Plotting for each individual with error bars
id <- unique(locations$TagId)
for(i in seq_along(id)){
  
pop.by.id <- ggplot(aes(x = UTMx_est, y = UTMy_est, color=TagId), data= subset(locations, locations$TagId==id[i])) + 
  geom_point() + 
  geom_errorbar(aes(ymin=y.LCI, ymax=y.UCI), width=0.2)+ #added for error bars on plots
  geom_errorbarh(aes(xmin=x.LCI, xmax=x.UCI, height=0.2))+ #added for error bars on plots
  ggtitle(paste("2023 Breeding Painted Bunting Population (Little Saint Simons Island, Georgia)"))+
  xlab("Estimated UTM Easting")+
  ylab("Estimated UTM Northing")+
  theme(panel.border =element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.line=element_line(colour="black"),
        axis.text=element_text(size=12, color="black"),
        axis.title.y=element_text(vjust=2),
        axis.title.x=element_text(vjust=1),
        plot.margin = unit(c(1,1,1,1),"lines"))

## create an ID specific file name each time it cycles to a new individual
PlotName <- paste("data/figures/territories_no_map/", "PABU_", i, "_error.jpeg", sep="")
PlotName

## use a ggplot2 function "ggsave" to save the plot, and then cycle to the next one.
ggsave(plot=pop.by.id, filename = PlotName, width=5, height=3, units="in")
}

## Plotting for each individual without error bars
id <- unique(locations$TagId)
for(i in seq_along(id)){
  
  pop.by.id <- ggplot(aes(x = UTMx_est, y = UTMy_est, color=TagId), data= subset(locations, locations$TagId==id[i])) + 
    geom_point() + 
    ggtitle(paste("2023 Breeding Painted Bunting Population (Little Saint Simons Island, Georgia)"))+
    xlab("Estimated UTM Easting")+
    ylab("Estimated UTM Northing")+
    theme(panel.border =element_blank(),panel.grid.major = element_blank(),
          panel.grid.minor=element_blank(),
          panel.background=element_blank(),
          axis.line=element_line(colour="black"),
          axis.text=element_text(size=12, color="black"),
          axis.title.y=element_text(vjust=2),
          axis.title.x=element_text(vjust=1),
          plot.margin = unit(c(1,1,1,1),"lines"))
  
  ## create an ID specific file name each time it cycles to a new individual
  PlotName <- paste("data/figures/territories_no_map/", "PABU_", id[i], "_no_error.jpeg", sep="")
  PlotName
  
  ## use a ggplot2 function "ggsave" to save the plot, and then cycle to the next one.
  ggsave(plot=pop.by.id, filename = PlotName, width=5, height=3, units="in")
}

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

# Making the dataframe an ltraj to store trajectories of animals
lt <- as.ltraj(locations_subset[,c("UTMx_est", "UTMy_est")], date = locations_subset$Time.group, id = locations_subset$TagId)
head(lt[[1]])
plot(lt)
lt2 <- ld(lt) #convert back to a dataframe
str(lt2)

### reorder data.frame with new movement parameters
lt3 <- lt2[,c(11,1:3, 6,7)]
colnames(lt3) <- c("ID", "X", "Y", "DT", "dist", "dt")



# Working with MoveVis to create cool visuals/ animations of PABU movement

coordinates(lt3) <- lt3[, c('X', 'Y')]
proj4string(lt3) <- CRS('+proj=utm +zone=17 +datum=WGS84')
class(lt3)

lt4 <- spTransform(lt3, CRSobj = CRS('+proj=longlat +datum=WGS84'))
lt4 <- as.data.frame(lt4)
head(lt4)

# changing the coordinate names to latitude and longitude
names(lt4)[names(lt4)=="coords.x1"] <- "Longitude"
names(lt4)[names(lt4)=="coords.x2"] <- "Latitude"
head(lt4)


lt4$ID_Name <- paste("PABU_", lt4$ID, sep="") #concatenate PABU to tag number
table(lt4$ID_Name, month(lt4$DT)) # looking at the table to see where there is alot of data for many birds
lt5 <- subset(lt4, month(DT)==6) # subsetting for 6th month, June
lt6 <- subset(lt5, hour(DT)==6) # subsetting to 6 am
table(lt6$ID, hour(lt6$DT), month(lt6$DT), year(lt6$DT) ) #fair amount of data in hour 6

# looking at the amount of hour 6 data from may to september
table(lt6$ID) #checking if all animals have at least 2 locations

lt7 <- lt6[lt6$ID %in% c("072A0766", "072D4B55", "2D2D2D33", "2D4B662D"),] #selecting only certain bird names
lt7 <- lt6[lt6$ID %in% c("072A0766", "072D4B55", "2D2D2D33"),]
lt8 <- subset(lt7, DT >= ymd("2023-06-04") & DT <= ymd("2023-06-15"))

table(lt8$ID, hour(lt8$DT), month(lt8$DT), year(lt8$DT)) #looking to make sure that there is enough data

#converting data.frame into a move object
move1 <- df2move(lt8, 
                 proj = '+proj=longlat +datum=WGS84', 
                 time= "DT", 
                 x = "Longitude", y="Latitude", 
                 track_id = "ID_Name")

#aligning movement data to a uniform time scale with a uniform temporal resolution to create an animation
#stating that the data resolution is every 44 minutes
#had to keep trying different data resolutions to get enough data to create a movement object
m <- align_move(m= move1, res=44, unit="mins")
#5,12, 18, 27, 36, 44
get_maptypes()

frames1 <- frames_spatial(m, 
                         map_service = "mapbox", map_type = "satellite",   
                         map_token = "INSERT-TOKEN-HERE" ) %>%
  add_labels(x = "Estimated Longitude", y = "Estimated Latitude",
             title = "Painted Bunting Movement (LSSI, GA)") %>% # add some customization, such as axis labels
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(m, type = "label") %>% 
  add_progress()

# take a look at one of the frames, e.g. the 100th
frames[[100]]

animate_frames(frames1, out_file = "data/animations/moveVis_PABU_June_6am_shortless.mp4", overwrite=T)
animate_frames(frames1, out_file = "data/animations/moveVis_PABU_June_shorter.gif")


# Loads the necessary spatial packages
library(sp)
library(remotes)
install_github("cran/rgdal")
library(rgdal)
install_github("cran/rgeos")
library(rgeos)
install_github("cran/maptools")
library(maptools)
install.packages("raster")
library(raster)
library(tidyverse)

## Bringing in trilateration location estimates
locations <- readRDS("data/trilateration/Estimated.Locations-feb20-90p-90_2023-05-10_2023-11-20.rds") #53565 obs. of  10 variables
str(locations) # checking the data structure

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
utm_points <- SpatialPoints(coords = locations_subset[, c("UTMx_est, UTMy_est")], proj4string = CRS("+proj=utm +zone=17 +datum=WGS84"))
install.packages("oce")
library(oce)
locations_subset_longlat <- as.data.frame(utm2lonlat(locations_subset$UTMx_est, locations_subset$UTMy_est, zone=17, hemisphere="N")) #convert easting and northing to long lat
locations_subset_longlat_lower <- as.data.frame(utm2lonlat(locations_subset$x.LCI, locations_subset$y.LCI, zone=17, hemisphere="N"))
locations_subset_longlat_upper  <- as.data.frame(utm2lonlat(locations_subset$x.UCI, locations_subset$y.UCI, zone=17, hemisphere="N"))

locs_longlat <- cbind(locations_subset_longlat,locations_subset_longlat_lower,locations_subset_longlat_upper)
colnames(locs_longlat) <- c('long.est', 'lat.est', 'long.LCI', 'lat.LCI', 'long.UCI', 'lat.UCI')
locations_longlat <- cbind(locations_subset, locs_longlat) #includes all UTM values converted to long lats

#Get the latest Install
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)
library(ggmap)
ggmap::register_google(key = "INSERT-KEY-HERE")


# Define the map center and pull from only
mapcenter <- c(mean(locations_longlat$long.est), mean(locations_longlat$lat.est))

# Download a static version of the google map
basic_map <- ggmap(get_googlemap(center = c(lon = mapcenter[1], lat = mapcenter[2]),
                                 zoom = 16, scale = 2,
                                 maptype ='hybrid'))

basic_map

# Plot map with paths
total.pop <-basic_map + geom_path(aes(x = long.est, y = lat.est, col=TagId), alpha=0.75,
                      data = locations_longlat)+
                ggtitle(paste("2023 Painted Bunting Population (LSSI,GA)"))+
                xlab("Estimated Longitude")+
                ylab("Estimated Latitude")
PlotName <- paste("data/figures/territories_map/", "PABU_total.pop.jpeg", sep="")
ggsave(plot=total.pop, filename = PlotName, width=6, height=4, units="in")

#for loop to cycle through each bird
id <- levels(locations_longlat$TagId)
for(i in seq_along(id)){
  # Plot map with paths
  map.by.id <- basic_map + 
                geom_path(aes(x = long.est, y = lat.est, col=TagId), alpha=0.75,
                        data = subset(locations_longlat, locations_longlat$TagId==id[i])) +
                ggtitle(paste(id[i], "Movements"))+
                xlab("Estimated Longitude")+
                ylab("Estimated Latitude")
    
  
  ## create an ID specific file name each time it cycles to a new individual
  PlotName <- paste("data/figures/territories_map/", "PABU_", id[i], ".jpeg", sep="")
  PlotName
  
  ## use a ggplot2 function "ggsave" to save the plot, and then cycle to the next one.
  ggsave(plot=map.by.id, filename = PlotName, width=6, height=4, units="in")
}
