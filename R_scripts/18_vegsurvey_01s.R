###################################################################################
## Diane Klement
## May 24 2024
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

# Read in veg survey data from the 16_rsf_vegsurvey.R script
veg <- readRDS("data/vegetation/veg_for_rsf.rds")

###################################################################################

# Organizing the veg data

# Removing the species column
veg1 <- veg %>%
          select(-c(SPECIES, BURN_TRACT_ID, Diff.Years))

###################################################################################

# Need to add 0s back into the df and then fit as a multiple regression model
# to add 0s back to the df use full_join: this will add NAs that you can then replace with 0s
# Columns: POINT, BURN_TRACT_ID, SPECIES, NATIVE_STATUS, STEM_DENSITY, PERCENT_VEG, used, Diff.Years, FAMILY_TYPE, TYPE, time_since_burn, GRASS

# make a dataframe with all unique points and all the different types for each point

# For the GRASS category (Cyperaceae, Nonnative_Poales, Not_grass, Poaceae): 4
# Just TYPE category (Forbs, Native_Poales, Nonnative_Poales, Shrub): 4
# Just FAMILY_TYPE category(Asteraceae, Forbs, Native_Poales, Nonnative_Poales, Rosales, Shrub): 6

###########################################
# For the GRASS category (Cyperaceae, Nonnative_Poales, Not_grass, Poaceae): 4

veg.1 <- veg1 %>%
  select(-c(FAMILY_TYPE, TYPE, NATIVE_STATUS)) %>%
  group_by(POINT, GRASS) %>%
  summarize(
    STEM_DENSITY = sum(STEM_DENSITY),
    PERCENT_VEG = first(PERCENT_VEG),
    used = first(used),
    time_since_burn = first(time_since_burn)) %>%
  ungroup()
head(veg.1)
points <- unique(veg.1$POINT) # all of the unique points
grass_types <- unique(veg.1$GRASS) # all of the unique grass types

# Create a dataframe with the expanded points and grass types
points_expanded <- rep(points, each = length(grass_types)) #4 types of grass for each point
grass_expanded <- rep(grass_types, times = length(points)) # repeating grass types for the length of points
df <- data.frame(POINT = points_expanded, GRASS = grass_expanded)

veg.NA <- full_join(df, veg.1)
veg.NA$STEM_DENSITY <- ifelse(is.na(veg.NA$STEM_DENSITY), 0, veg.NA$STEM_DENSITY)

replace_na_within_group <- function(x) {
  if (all(is.na(x))) {
    return(x)
  } else {
    first_non_na <- na.omit(x)[1]
    return(replace_na(x, first_non_na))
  }
}

# Replace NA values with the first non-NA value within each group
veg.grass <- veg.NA %>%
  group_by(POINT) %>%
  mutate(
    PERCENT_VEG = replace_na_within_group(PERCENT_VEG),
    used = replace_na_within_group(used),
    time_since_burn = replace_na_within_group(time_since_burn)
  ) %>%
  ungroup()  # Remove grouping
head(veg.grass)

veg.grass$time_since_burn <- factor(veg.grass$time_since_burn, levels = c("unmanaged", "0-1_years", "1-2_years", "2-3_years"))
veg.grass$GRASS <- factor(veg.grass$GRASS, levels = c("Not_Grass", "Cyperaceae", "Poaceae", "Nonnative_Poales"))
head(veg.grass)

veg.grass2 <- veg.grass %>%
  group_by(POINT) %>%
  summarize(
    PERCENT_VEG = first(PERCENT_VEG),
    used = first(used),
    time_since_burn = first(time_since_burn),
    SD_Not_Grass = sum(ifelse(GRASS == "Not_Grass", STEM_DENSITY, 0)),
    SD_Cyperaceae = sum(ifelse(GRASS == "Cyperaceae", STEM_DENSITY, 0)),
    SD_Poaceae = sum(ifelse(GRASS == "Poaceae", STEM_DENSITY, 0)),
    SD_Nonnative_Poales = sum(ifelse(GRASS == "Nonnative_Poales", STEM_DENSITY, 0))
  )

head(veg.grass2)

# Standardizing Continuous Variable
veg.grass2$PERCENT_VEG.c <- veg.grass2$PERCENT_VEG - mean(veg.grass2$PERCENT_VEG)
veg.grass2$SD_Not_Grass.c <- veg.grass2$SD_Not_Grass - mean(veg.grass2$SD_Not_Grass)
veg.grass2$SD_Cyperaceae.c <- veg.grass2$SD_Cyperaceae - mean(veg.grass2$SD_Cyperaceae)
veg.grass2$SD_Poaceae.c <- veg.grass2$SD_Poaceae - mean(veg.grass2$SD_Poaceae)
veg.grass2$SD_Nonnative_Poales.c <- veg.grass2$SD_Nonnative_Poales - mean(veg.grass2$SD_Nonnative_Poales)

head(veg.grass2)

saveRDS(veg.grass2, "data/veg_sample_rsf_01/veg_grass.rds")
###########################################

# Just TYPE category (Forbs, Native_Poales, Nonnative_Poales, Shrub): 4

veg.1 <- veg1 %>%
  select(-c(FAMILY_TYPE, GRASS, NATIVE_STATUS)) %>%
  group_by(POINT, TYPE) %>%
  summarize(
    STEM_DENSITY = sum(STEM_DENSITY),
    PERCENT_VEG = first(PERCENT_VEG),
    used = first(used),
    time_since_burn = first(time_since_burn)) %>%
  ungroup()

points <- unique(veg.1$POINT) # all of the unique points
types <- unique(veg.1$TYPE) # all of the unique veg types

# Create a dataframe with the expanded points and grass types
points_expanded <- rep(points, each = length(types)) #4 types of veg for each point
type_expanded <- rep(types, times = length(points)) # repeating veg types for the length of points
df <- data.frame(POINT = points_expanded, TYPE = type_expanded)
head(df)

veg.NA <- full_join(df, veg.1)
veg.NA$STEM_DENSITY <- ifelse(is.na(veg.NA$STEM_DENSITY), 0, veg.NA$STEM_DENSITY)
head(veg.NA)

replace_na_within_group <- function(x) {
  if (all(is.na(x))) {
    return(x)
  } else {
    first_non_na <- na.omit(x)[1]
    return(replace_na(x, first_non_na))
  }
}

# Replace NA values with the first non-NA value within each group
veg.type <- veg.NA %>%
  group_by(POINT) %>%
  mutate(
    PERCENT_VEG = replace_na_within_group(PERCENT_VEG),
    used = replace_na_within_group(used),
    time_since_burn = replace_na_within_group(time_since_burn)
  ) %>%
  ungroup()  # Remove grouping
head(veg.type)

veg.type$time_since_burn <- factor(veg.type$time_since_burn, levels = c("unmanaged", "0-1_years", "1-2_years", "2-3_years"))
veg.type$TYPE <- factor(veg.type$TYPE, levels = c("Shrub", "Forbs", "Native_Poales", "Nonnative_Poales"))
head(veg.type)

veg.type2 <- veg.type %>%
  group_by(POINT) %>%
  summarize(
    PERCENT_VEG = first(PERCENT_VEG),
    used = first(used),
    time_since_burn = first(time_since_burn),
    SD_Shrub = sum(ifelse(TYPE == "Shrub", STEM_DENSITY, 0)),
    SD_Forbs = sum(ifelse(TYPE == "Forbs", STEM_DENSITY, 0)),
    SD_Native_Poales = sum(ifelse(TYPE == "Native_Poales", STEM_DENSITY, 0)),
    SD_Nonnative_Poales = sum(ifelse(TYPE == "Nonnative_Poales", STEM_DENSITY, 0))
  )
head(veg.type2)

# Standardizing Continuous Variable
veg.type2$PERCENT_VEG.c <- veg.type2$PERCENT_VEG - mean(veg.type2$PERCENT_VEG)
veg.type2$SD_Shrub.c <- veg.type2$SD_Shrub - mean(veg.type2$SD_Shrub)
veg.type2$SD_Forbs.c <- veg.type2$SD_Forbs - mean(veg.type2$SD_Forbs)
veg.type2$SD_Native_Poales.c <- veg.type2$SD_Native_Poales - mean(veg.type2$SD_Native_Poales)
veg.type2$SD_Nonnative_Poales.c <- veg.type2$SD_Nonnative_Poales - mean(veg.type2$SD_Nonnative_Poales)
head(veg.type2)

saveRDS(veg.type2, "data/veg_sample_rsf_01/veg_type.rds")
###########################################

# Just FAMILY_TYPE category(Asteraceae, Forbs, Native_Poales, Nonnative_Poales, Rosales, Shrub): 6

veg.1 <- veg1 %>%
  select(-c(TYPE, GRASS, NATIVE_STATUS)) %>%
  group_by(POINT, FAMILY_TYPE) %>%
  summarize(
    STEM_DENSITY = sum(STEM_DENSITY),
    PERCENT_VEG = first(PERCENT_VEG),
    used = first(used),
    time_since_burn = first(time_since_burn)) %>%
  ungroup()

points <- unique(veg.1$POINT) # all of the unique points
types <- unique(veg.1$FAMILY_TYPE) # all of the unique veg family types

# Create a dataframe with the expanded points and grass types
points_expanded <- rep(points, each = length(types)) #6 types of veg for each point
type_expanded <- rep(types, times = length(points)) # repeating veg types for the length of points
df <- data.frame(POINT = points_expanded, FAMILY_TYPE = type_expanded)
head(df)
head(veg.1) # checking that both dfs are the same

veg.NA <- full_join(df, veg.1)
veg.NA$STEM_DENSITY <- ifelse(is.na(veg.NA$STEM_DENSITY), 0, veg.NA$STEM_DENSITY)
head(veg.NA)

replace_na_within_group <- function(x) {
  if (all(is.na(x))) {
    return(x)
  } else {
    first_non_na <- na.omit(x)[1]
    return(replace_na(x, first_non_na))
  }
}

# Replace NA values with the first non-NA value within each group
veg.family <- veg.NA %>%
  group_by(POINT) %>%
  mutate(
    PERCENT_VEG = replace_na_within_group(PERCENT_VEG),
    used = replace_na_within_group(used),
    time_since_burn = replace_na_within_group(time_since_burn)
  ) %>%
  ungroup()  # Remove grouping
head(veg.family)

veg.family$time_since_burn <- factor(veg.family$time_since_burn, levels = c("unmanaged", "0-1_years", "1-2_years", "2-3_years"))
veg.family$FAMILY_TYPE <- factor(veg.family$FAMILY_TYPE, levels = c("Shrub", "Forbs", "Asteraceae", "Rosales", "Native_Poales", "Nonnative_Poales"))
head(veg.family)

veg.family2 <- veg.family %>%
  group_by(POINT) %>%
  summarize(
    PERCENT_VEG = first(PERCENT_VEG),
    used = first(used),
    time_since_burn = first(time_since_burn),
    SD_Shrub = sum(ifelse(FAMILY_TYPE == "Shrub", STEM_DENSITY, 0)),
    SD_Forbs = sum(ifelse(FAMILY_TYPE == "Forbs", STEM_DENSITY, 0)),
    SD_Asteraceae = sum(ifelse(FAMILY_TYPE == "Asteraceae", STEM_DENSITY, 0)),
    SD_Rosales = sum(ifelse(FAMILY_TYPE == "Rosales", STEM_DENSITY, 0)),
    SD_Native_Poales = sum(ifelse(FAMILY_TYPE == "Native_Poales", STEM_DENSITY, 0)),
    SD_Nonnative_Poales = sum(ifelse(FAMILY_TYPE == "Nonnative_Poales", STEM_DENSITY, 0))
  )
head(veg.family2)

# Standardizing Continuous Variable
veg.family2$PERCENT_VEG.c <- veg.family2$PERCENT_VEG - mean(veg.family2$PERCENT_VEG)
veg.family2$SD_Shrub.c <- veg.family2$SD_Shrub - mean(veg.family2$SD_Shrub)
veg.family2$SD_Forbs.c <- veg.family2$SD_Forbs - mean(veg.family2$SD_Forbs)
veg.family2$SD_Asteraceae.c <- veg.family2$SD_Asteraceae - mean(veg.family2$SD_Asteraceae)
veg.family2$SD_Rosales.c <- veg.family2$SD_Rosales - mean(veg.family2$SD_Rosales)
veg.family2$SD_Native_Poales.c <- veg.family2$SD_Native_Poales - mean(veg.family2$SD_Native_Poales)
veg.family2$SD_Nonnative_Poales.c <- veg.family2$SD_Nonnative_Poales - mean(veg.family2$SD_Nonnative_Poales)
head(veg.family2)

saveRDS(veg.family2, "data/veg_sample_rsf_01/veg_family.rds")
