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

veg_burn_clean$Date.for <- as.Date(paste0("01-", veg_burn_clean$Date), format = "%d-%b-%y")
veg_burn_clean$Current.date <- as.Date("2023-05-01")
veg_burn_clean$Date.diff <- veg_burn_clean$Current.date - veg_burn_clean$Date.for # Difference in days since management
veg_burn_clean$Date.year <- veg_burn_clean$Date.diff / 360 # Years since management
str(veg_burn_clean)

saveRDS(veg_burn_clean, "data/veg_data.rds")
########################################################################################

# ANALYSIS OF VEG AS RSF

veg_burn_clean <- readRDS("data/vegetation/veg_data.rds")

fm1 <- glm(used ~ Date.year + BURN_TRACT_ID, family = binomial(link = "logit"), data = veg_burn_clean)
summary(fm1)


# Raw data
ggplot(veg_burn_clean, aes(x = Date.year, y = used)) +
  geom_point() +
  scale_y_continuous("Foraging Occurrence") + scale_x_continuous("Years Since Burn")


veg_burn_clean %>% group_by(SPECIES) %>% summarise(group.prob = mean(used)) %>%
  ggplot(., aes(x = SPECIES, y = group.prob)) +
  geom_col(fill = "grey70", color = "black") +
  scale_y_continuous("Proportion of sites with orchids") + scale_x_discrete("Habitat")

fm1 <- glm(used ~ habitat + elevation, 
           family=binomial(link="logit"), 
           data = veg_burn_clean)
broom::tidy(fm1)

