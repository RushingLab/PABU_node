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

# Read in data
veg_burn_clean <- readRDS("data/vegetation/veg_data.rds")

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

