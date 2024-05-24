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
          select(-c(SPECIES, BURN_TRACT_ID))

# Just TYPE category (Forbs, Native_Poales, Nonnative_Poales, Shrub): 4
# Just FAMILY_TYPE category(Asteraceae, Forbs, Native_Poales, Nonnative_Poales, Rosales, Shrub): 6

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
    Diff.Years = first(Diff.Years),
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
    Diff.Years = replace_na_within_group(Diff.Years),
    time_since_burn = replace_na_within_group(time_since_burn)
  ) %>%
  ungroup()  # Remove grouping
head(veg.grass)
saveRDS(veg.grass, "data/veg_sample_rsf_01/veg_grass.rds")
###########################################

# Just TYPE category (Forbs, Native_Poales, Nonnative_Poales, Shrub): 4

veg.1 <- veg1 %>%
  select(-c(FAMILY_TYPE, GRASS, NATIVE_STATUS)) %>%
  group_by(POINT, TYPE) %>%
  summarize(
    STEM_DENSITY = sum(STEM_DENSITY),
    PERCENT_VEG = first(PERCENT_VEG),
    used = first(used),
    Diff.Years = first(Diff.Years),
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
    Diff.Years = replace_na_within_group(Diff.Years),
    time_since_burn = replace_na_within_group(time_since_burn)
  ) %>%
  ungroup()  # Remove grouping
head(veg.type)
saveRDS(veg.type, "data/veg_sample_rsf_01/veg_type.rds")
###########################################

# Just FAMILY_TYPE category(Asteraceae, Forbs, Native_Poales, Nonnative_Poales, Rosales, Shrub): 6

veg.1 <- veg1 %>%
  select(-c(TYPE, GRASS, NATIVE_STATUS)) %>%
  group_by(POINT, FAMILY_TYPE) %>%
  summarize(
    STEM_DENSITY = sum(STEM_DENSITY),
    PERCENT_VEG = first(PERCENT_VEG),
    used = first(used),
    Diff.Years = first(Diff.Years),
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
    Diff.Years = replace_na_within_group(Diff.Years),
    time_since_burn = replace_na_within_group(time_since_burn)
  ) %>%
  ungroup()  # Remove grouping
head(veg.family)
saveRDS(veg.family, "data/veg_sample_rsf_01/veg_family.rds")
