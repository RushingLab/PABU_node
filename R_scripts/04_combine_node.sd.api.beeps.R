###################################################################################
## Diane Klement
## January 30 2024
##
## Code for combining CTT Node beep data retrieved from physical Node microSD cards and the CTT cellular API
##
###################################################################################
##
## R Script for combining CTT Node beep data retrieved from physical Node microSD cards and the CTT cellular API
##
##    Files Needed:
##      To use this script, you will need two rds files created using previous scripts:
##        - beep_api : this rds will contain all beep data that was cleaned and downloaded using using "02_import_node.api.beeps.R" and "02_functions_node.api.beeps.R" 
##        - beep_sd : this rds will contain all beep data that was cleaned and downloaded using using "03_import_node.sd.beeps.R"
##
##    Woirkflow:
##        First step: Data preparation -- clean up node sd and api beeps so that they match the same format
##        Second step: Data combination -- combine node sd and api data into one rds file
##
##    Output:
##        - BeepMerge.rds: merged beep data from api and node sd cards as a rds (R Data Serialization) file
##        - BeepMerge.csv: merged beep data from api and node sd cards as a csv  (comma-separated values) file
##            - Columns
##                - SensorId: Factor identifying the SensorStation name for the beep detection
##                - Time: POSIXct value in the format: "2021-12-29 09:02:51" that identifies the unique datetime stamp in UTC
##                - RadioId: Integer value of the RadioId from the tower (ie. 1, 2, or 3)
##                - TagId: Factor identifying the unique code of a tag
##                - TagRSSI: Integer indicating the RSS value (in dB) of the signal of the specified tag received by the specified node
##                - NodeId: Factor identifying the node in the network that detected the specified tag
##                - Time.local: POSIXct value in the format: "2021-12-29 09:02:51" that identifies the unique datetime stamp (IN THE LOCAL TIME ZONE OF THE STUDY) when the tag was detected by the specified node
##                - Date: Date when the specified beep was received
##                - Hour: Hour when the specified beep was received
##                - Min: Minute the specified beep was received
##
###################################################################################

# uploading packages needed
library(lubridate)
library(dplyr)
library(arsenal)


# Reset R's brain - removes all previous objects
rm(list=ls())

###################################################################################
#Bringing in and cleaning up the node Api beep data output from "Import_beep.data.Github.R"


# Bring in RDS of node API data as generated from "Import_beep.data.Github.R"
beep_api <- readRDS("data/beep_api.rds")

# Creating a Date column
beep_api$Date <- as.Date(beep_api$Time.local, tz="America/New_York" ) # for New York timezone

# Creating a hour and minute column
beep_api$Hour <- hour(beep_api$Time.local)
beep_api$Min <- minute(beep_api$Time.local)
str(beep_api) #3301942 obs.

# Removing Validated and V columns from beep_api df
beep_api_clean <- subset(beep_api, select = -c(Validated, v))
str(beep_api_clean) 
nrow(beep_api_clean) # 3301942
#beep_api_clean = all clean api beep data that we will use for next steps

#################################################################################

# Bringing in the node microSD card beep data output from "04_import_node.sd.beeps.R"

beep_sd_clean <- readRDS(file = "data/beep_sd.rds")

#################################################################################

# Combining SD card and API databases then cleaning them

## We want to combine the beep_api_clean and beep_sd_clean into a single database then remove duplicate records.
#3301942 + 5456587 = 8758529 rows should be in combined database

BeepMerge <- rbind(beep_api_clean, beep_sd_clean)
str(BeepMerge)
nrow(BeepMerge) #8758529

# remove duplicate data (e.g. detected at multiple Sensor Stations)
BeepMerge1 <- BeepMerge %>%
  dplyr::distinct(Time, TagId, NodeId, TagRSSI, .keep_all = T)
str(BeepMerge1) # BeepMerge1 = removing duplicates
nrow(BeepMerge1) #5687368
nrow(BeepMerge)-nrow(BeepMerge1) #3071161 rows lost

# comparing this record of removing duplicate data to just removing duplicate rows
BeepMerge2 <- BeepMerge %>% distinct() #BeepMerge2 = combined data after removing duplicate rows
nrow(BeepMerge2) #5687368
# BeepMerge2 = removing duplicates in a different way to see if there was a difference

#seeing if each method produced the same number of rows
summary(comparedf(BeepMerge1, BeepMerge2)) #summary showed that dataframes are the same!

# Saving BeepMerge1 
saveRDS(BeepMerge1, file= "data/BeepMerge.rds ")
write.csv(BeepMerge1, file= "data/BeepMerge.csv")
