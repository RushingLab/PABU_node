###################################################################################
## Diane Klement
## January 30 2023
##
## Code for combining CTT Node beep data retrieved from physical Node microSD cards and the CTT cellular API
##
###################################################################################
##
## R Script for combining CTT Node beep data retrieved from physical Node microSD cards and the CTT cellular API
##
## To use this script, you will need two rds files created using previous scripts:
##        - beep_api : this rds will contain all beep data that was cleaned and downloaded using using "02_import_node.api.beeps.R" and "02_functions_node.api.beeps.R" 
##        - beep_sd : this rds will contain all beep data that was cleaned and downloaded using using "03_import_node.sd.beeps.R"
##
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
