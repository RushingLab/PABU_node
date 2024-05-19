###################################################################################
## Diane Klement
## May 10 2024
##
## Code to run RSF for veg survey used and available points
##
## Output:
##    - data/rsf/used_avail.rds
##        - Combined and cleaned used and available locations of PABU for further use
##
###################################################################################

# Loading necessary packages
library(tidyverse)
library(ggplot2)
library(dplyr)

# Reset R's brain - removes all previous objects
rm(list=ls())

###################################################################################

# Read in veg survey data
veg_burn_clean <- readRDS("data/vegetation/veg_data.rds")

###################################################################################

# Looking at the number of points per burn tract
unique(veg_burn_clean$BURN_TRACT_ID)
ids <- c(9, 10, 11, 13, 14, 15, 16)
num_points_all <- veg_burn_clean %>%
              filter(BURN_TRACT_ID %in% ids) %>%
              group_by(BURN_TRACT_ID) %>%
              filter(used == 0) %>%
              summarize(num_points = list(unique(POINT)))
print(num_points_all)
###################################################################################
# Filtering out veg survey data to just columns of interest
veg_fil <- veg_burn_clean %>%
              dplyr::select(POINT, BURN_TRACT_ID, SPECIES, NATIVE_STATUS, STEM_DENSITY, PERCENT_VEG, used, Diff.Years)
veg_fil1 <- na.omit(veg_fil[veg_fil$NATIVE_STATUS != "", ])

# Adding a column for plant type
veg_fil2 <- veg_fil1 %>%
                    mutate(FAMILY_TYPE = case_when(
                        SPECIES %in% c("Muhlenbergia sericea", "Setaria parviflora", "Cenchrus tribuloides", "Dichanthelium oligosanthes", "Fimbristylis castanea",
                                                      "marsh-like grass", "Andropogon longiberbis", "Fimbristylis caroliniana", "Cyperus retrorsus", "Rhynchospora latifolia", "Cyperus polystachyos", 
                                                      "Cyperus strigosus", "Cyperus brevifolius",  "Cyperus pseudovegetus", "Juncus marginatus"
                                                 ) ~ "Native_Poales",
                        SPECIES %in% c("Paspalum notatum", "crabgrass", "Cynodon dactylon", "Eleusine indica") ~ "Nonnative_Poales",  
                        SPECIES %in% c("Euthamia caroliniana", "Eupatorium capillifolium", "Cirsium nuttallii", "Solidago sempervirens",
                                                 "Mikania scandens", "Heterotheca subaxillaris") ~ "Asteraceae", 
                        SPECIES %in% c("Rubus trivialis", "Parietaria floridana") ~ "Rosales",
                        SPECIES %in% c("Hypericum hypericoides", "Myrica cerifera", "Baccharis halimifolia") ~ "Shrub", 
                        SPECIES %in% c("Ampelopsis arborea", "Houstonia procumbens", "Hydrocotyle bonariensis", "Parthenocissus quinquefolia",
                                                   "Boehmeria cylindrica", "Polypremum procumbens", "Justicia ovata", "Trichostema dichotomum", "Oxalis stricta", "Phyla nodiflora",
                                                   "Solanum americanum", "Hypericum gentianoides", "Physalis walteri", "Oenothera humifusa", "Sabatia stellaris", "Croton punctatus", "Arenaria serpyllifolia",
                                                    "Ludwigia alternifolia", "Lythrum alatum", "Euphorbia maculata", "Euphorbia maculata ", "Diodia teres", "Polygonum setaceum"
                                                   ) ~ "Native_Forbs",
                        SPECIES %in% c("Stellaria media") ~ "Nonnative_Forb"
                               ))

# Checking for NAs
veg_fil2[is.na(veg_fil2$FAMILY_TYPE),] #yay! 0

# Adding a column for plant type
veg_fil3 <- veg_fil2 %>%
                    mutate(TYPE = case_when(
                    SPECIES %in% c("Muhlenbergia sericea", "Setaria parviflora", "Cenchrus tribuloides", "Cyperus retrorsus", "Rhynchospora latifolia", 
                                                 "Cyperus polystachyos", "Cyperus strigosus", "Juncus marginatus", "Dichanthelium oligosanthes", "Fimbristylis castanea",
                                                 "marsh-like grass", "Cyperus brevifolius", "Andropogon longiberbis", "Cyperus pseudovegetus", "Fimbristylis caroliniana"
                                                 ) ~ "Native_Poales",
                    SPECIES %in% c("Paspalum notatum", "crabgrass", "Cynodon dactylon", "Eleusine indica") ~ "Nonnative_Poales", 
                    SPECIES %in% c("Hypericum hypericoides", "Myrica cerifera", "Baccharis halimifolia") ~ "Shrub", 
                    SPECIES %in% c("Ampelopsis arborea", "Houstonia procumbens", "Hydrocotyle bonariensis", "Rubus trivialis",
                                                   "Boehmeria cylindrica", "Euthamia caroliniana", "Eupatorium capillifolium", "Cirsium nuttallii", "Solidago sempervirens",
                                                   "Mikania scandens", "Polypremum procumbens", "Parthenocissus quinquefolia", "Justicia ovata", "Heterotheca subaxillaris",
                                                  "Trichostema dichotomum", "Oxalis stricta", "Phyla nodiflora", "Solanum americanum", "Hypericum gentianoides", "Physalis walteri",
                                                  "Diodia teres", "Arenaria serpyllifolia", "Ludwigia alternifolia", "Lythrum alatum", "Polygonum setaceum",
                                                  "Oenothera humifusa", "Sabatia stellaris", "Croton punctatus", "Parietaria floridana", "Euphorbia maculata ", "Stellaria media"
                                                  ) ~ "Forbs"
                         ))                    
  
# Checking for NAs
veg_fil3[is.na(veg_fil3$TYPE),] # 0 yay!

###################################################################################
# ANALYSIS OF VEG AS RSF

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

