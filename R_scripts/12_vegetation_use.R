###################################################################################
## Diane Klement
## May 10 2024
##
## Code to clean veg data for later use in a RSF
##
## Output:
##    - data/rsf/used_avail.rds
##        - Combined and cleaned used and available locations of PABU for further use
##
###################################################################################

library(tidyverse)
library(ggplot2)
library(dplyr)

# Reading in veg survey data
veg <- read.csv(file = "data-raw/veg_surveys.csv")

# Reading in burn data
burn <- read.csv(file = "data-raw/LSSI Burn Records Updated.csv")
burn <- burn %>%
          rename(BURN_TRACT_ID = Tract_ID)

# Looking at data structure
str(veg)
str(burn)

# Cleaning data
# Adding a 0 or 1 depending on whether the plot was used
veg$used <- ifelse(veg$TYPE == "available", 0, ifelse(veg$TYPE == "used", 1, NA))
str(veg)

# Combine vegetation and burn metadata
veg_burn <- merge(veg, burn, by = "BURN_TRACT_ID")
str(veg_burn)

# Filtering down columns
veg_burn_clean <- veg_burn %>%
                    dplyr::select(-FID, -TYPE, -Proofed, -PLANT_PRESSED, -PHOTO_NO, -SEEDS_COLLECTED, -OBSERVER.INITIALS, -NOTES, -Acreage_Managed, -Total_Acreage, -Comments)

# converting to factors
veg_burn_clean$BURN_TRACT_ID <- as.factor(veg_burn_clean$BURN_TRACT_ID)
veg_burn_clean$POINT <- as.factor(veg_burn_clean$POINT)
veg_burn_clean$SPECIES <- as.factor(veg_burn_clean$SPECIES)
veg_burn_clean$NATIVE_STATUS <- as.factor(veg_burn_clean$NATIVE_STATUS)
veg_burn_clean$LOCATION <- as.factor(veg_burn_clean$LOCATION)
veg_burn_clean$Management <- as.factor(veg_burn_clean$Management)

veg_burn_clean$Date.for <- as.Date(veg_burn_clean$Date, format = "%m/%d/%Y")
veg_burn_clean$Current.date <- as.Date("2023-05-01")
veg_burn_clean$Date.diff <- veg_burn_clean$Current.date - veg_burn_clean$Date.for # Difference in days since management
veg_burn_clean$Date.year <- veg_burn_clean$Date.diff / 360 # Years since management
str(veg_burn_clean)

saveRDS(veg_burn_clean, "data/vegetation/veg_data.rds")
