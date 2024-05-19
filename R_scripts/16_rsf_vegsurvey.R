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


# Adding a column for plant family type (at a finer scale)
veg_fil2 <- veg_fil %>%
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
                                                    "Ludwigia alternifolia", "Lythrum alatum", "Euphorbia maculata", "Euphorbia maculata ", "Diodia teres", "Polygonum setaceum", "red-stemmed plant",
                                                    "rose plant family", "Heterotheca gentianoides", "Stellaria media"
                                                   ) ~ "Forbs"
                                                 ))

# Checking for NAs
veg_fil2[is.na(veg_fil2$FAMILY_TYPE),] #yay! 0

# Adding a column for plant type (at a broader scale)
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
                                                  "Oenothera humifusa", "Sabatia stellaris", "Croton punctatus", "Parietaria floridana", "Euphorbia maculata ", "Stellaria media",
                                                  "red-stemmed plant", "rose plant family", "Heterotheca gentianoides"
                                                  ) ~ "Forbs"
                         ))                    
  
# Checking for NAs
veg_fil3[is.na(veg_fil3$TYPE),] # 0 yay!

# Creating a new column for managment
veg_fil4 <- veg_fil3 %>%
            mutate(time_since_burn = case_when(
              Diff.Years >= 0 & Diff.Years <= 1 ~ "0-1_years",
              Diff.Years > 1 & Diff.Years <= 2 ~ "1-2_years",
              Diff.Years > 2 & Diff.Years <= 3 ~ "2-3_years",
              Diff.Years > 3 & Diff.Years <= 4 ~ "3-4_years",
              Diff.Years > 4 & Diff.Years <= 5 ~ "4-5_years",
              BURN_TRACT_ID %in% c(13, 15) ~ "unmanaged"
            ))

# Checking for NAs
veg_fil4[is.na(veg_fil4$time_since_burn),] 

# checking data structure
str(veg_fil4)
veg_fil4$FAMILY_TYPE <- as.factor(veg_fil4$FAMILY_TYPE)
veg_fil4$TYPE <- as.factor(veg_fil4$TYPE)
veg_fil4$time_since_burn <- as.factor(veg_fil4$time_since_burn)
str(veg_fil4)

veg_fil5 <- veg_fil4 %>%
  mutate(GRASS = case_when(
    SPECIES %in% c("Muhlenbergia sericea", "Setaria parviflora", "Cenchrus tribuloides", "Juncus marginatus", "Dichanthelium oligosanthes", 
                   "marsh-like grass", "Andropogon longiberbis") ~ "Poaceae",
    SPECIES %in% c("Cyperus retrorsus", "Rhynchospora latifolia", "Cyperus polystachyos", "Cyperus strigosus", "Fimbristylis castanea",
                    "Cyperus brevifolius",  "Cyperus pseudovegetus", "Fimbristylis caroliniana") ~ "Cyperaceae",
    SPECIES %in% c("Paspalum notatum", "crabgrass", "Cynodon dactylon", "Eleusine indica") ~ "Nonnative_Poales", 
    SPECIES %in% c("Ampelopsis arborea", "Houstonia procumbens", "Hydrocotyle bonariensis", "Rubus trivialis",
                   "Boehmeria cylindrica", "Euthamia caroliniana", "Eupatorium capillifolium", "Cirsium nuttallii", "Solidago sempervirens",
                   "Mikania scandens", "Polypremum procumbens", "Parthenocissus quinquefolia", "Justicia ovata", "Heterotheca subaxillaris",
                   "Trichostema dichotomum", "Oxalis stricta", "Phyla nodiflora", "Solanum americanum", "Hypericum gentianoides", "Physalis walteri",
                   "Diodia teres", "Arenaria serpyllifolia", "Ludwigia alternifolia", "Lythrum alatum", "Polygonum setaceum",
                   "Oenothera humifusa", "Sabatia stellaris", "Croton punctatus", "Parietaria floridana", "Euphorbia maculata ", "Stellaria media",
                   "red-stemmed plant", "rose plant family", "Heterotheca gentianoides", "Hypericum hypericoides", "Myrica cerifera", "Baccharis halimifolia"
    ) ~ "Not_Grass"
  )) 
# Checking for NAs
veg_fil5[is.na(veg_fil5$time_since_burn),] 
veg_fil5$GRASS <- as.factor(veg_fil5$GRASS)
str(veg_fil5)
#veg_fil4 = dataset to use for the analyses

saveRDS(veg_fil5, "data/vegetation/veg_for_rsf.rds")
###################################################################################
# ANALYSIS OF VEG SURVEYS AS RSF
# FOR BROAD VEG TYPES

veg_fil4 <- readRDS("data/vegetation/veg_for_rsf.rds")

# Native grasses
veg_natgrass <- veg_fil4 %>%
                  filter(TYPE == "Native_Poales")
# pr(use) as a function of stem density of native grass
fm.natgrass <- glm(used ~ STEM_DENSITY, family = binomial(link = "logit"), data = veg_natgrass)
summary(fm.natgrass)
#Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -1.68799    0.21814  -7.738 1.01e-14 ***
# STEM_DENSITY  0.05874    0.02961   1.984   0.0473 *  

# predict how probability of use differs across differences in native grass density
predData.natgrass <- data.frame(STEM_DENSITY = seq(min(veg_natgrass$STEM_DENSITY), max(veg_natgrass$STEM_DENSITY), length = 50))
head(predData.natgrass)
pred.link <- predict(fm.natgrass, newdata = predData.natgrass, se.fit = TRUE)
predData.natgrass$p <- plogis(pred.link$fit) # back transform to probability scale
predData.natgrass$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.natgrass$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# graph
ggplot() +
  geom_point(data = veg_natgrass, aes(x = STEM_DENSITY, y = used)) +
  geom_path(data = predData.natgrass, aes(x = STEM_DENSITY, y = p)) +
  geom_ribbon(data = predData.natgrass, aes(x = STEM_DENSITY, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Stem Density of Native Grasses") +
  theme_classic()

# Nonnative grasses
veg_nonnatgrass <- veg_fil4 %>%
  filter(TYPE == "Nonnative_Poales")
# pr(use) as a function of stem density of native grass
fm.nonnatgrass <- glm(used ~ STEM_DENSITY, family = binomial(link = "logit"), data = veg_nonnatgrass)
summary(fm.nonnatgrass)
#Coefficients:
#               Estimate Std. Error z value Pr(>|z|)
#(Intercept)   0.044306   0.430824   0.103    0.918
#STEM_DENSITY -0.014229   0.009073  -1.568    0.117

# predict how probability of use differs across differences in nonnative grass density
predData.nonnatgrass <- data.frame(STEM_DENSITY = seq(min(veg_nonnatgrass$STEM_DENSITY), max(veg_nonnatgrass$STEM_DENSITY), length = 50))
head(predData.nonnatgrass)
pred.link <- predict(fm.nonnatgrass, newdata = predData.nonnatgrass, se.fit = TRUE)
predData.nonnatgrass$p <- plogis(pred.link$fit) # back transform to probability scale
predData.nonnatgrass$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.nonnatgrass$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# graph
ggplot() +
  geom_point(data = veg_nonnatgrass, aes(x = STEM_DENSITY, y = used)) +
  geom_path(data = predData.nonnatgrass, aes(x = STEM_DENSITY, y = p)) +
  geom_ribbon(data = predData.nonnatgrass, aes(x = STEM_DENSITY, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Stem Density of Nonnative Grasses") +
  theme_classic()

# Shrub
veg_shrub <- veg_fil4 %>%
  filter(TYPE == "Shrub")
# pr(use) as a function of stem density of native grass
fm.shrub <- glm(used ~ STEM_DENSITY, family = binomial(link = "logit"), data = veg_shrub)
summary(fm.shrub)
#Coefficients:
#              Estimate Std. Error z value Pr(>|z|)
#(Intercept)    0.6692     1.1442   0.585    0.559
#STEM_DENSITY  -1.1624     0.9019  -1.289    0.197

# predict how probability of use differs across differences in shrub density
predData.shrub <- data.frame(STEM_DENSITY = seq(min(veg_shrub$STEM_DENSITY), max(veg_shrub$STEM_DENSITY), length = 50))
head(predData.shrub)
pred.link <- predict(fm.shrub, newdata = predData.shrub, se.fit = TRUE)
predData.shrub$p <- plogis(pred.link$fit) # back transform to probability scale
predData.shrub$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.shrub$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# graph
ggplot() +
  geom_point(data = veg_shrub, aes(x = STEM_DENSITY, y = used)) +
  geom_path(data = predData.shrub, aes(x = STEM_DENSITY, y = p)) +
  geom_ribbon(data = predData.shrub, aes(x = STEM_DENSITY, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Stem Density of Shrubs") +
  theme_classic()


# Forbs
veg_forb <- veg_fil4 %>%
  filter(TYPE == "Forbs")
# pr(use) as a function of stem density of native grass
fm.forb <- glm(used ~ STEM_DENSITY, family = binomial(link = "logit"), data = veg_forb)
summary(fm.forb)
#Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.70842    0.15667  -10.90   <2e-16 ***
#STEM_DENSITY  0.03111    0.01496    2.08   0.0375 * 

# predict how probability of use differs across differences in shrub density
predData.forb <- data.frame(STEM_DENSITY = seq(min(veg_forb$STEM_DENSITY), max(veg_forb$STEM_DENSITY), length = 50))
head(predData.forb)
pred.link <- predict(fm.forb, newdata = predData.forb, se.fit = TRUE)
predData.forb$p <- plogis(pred.link$fit) # back transform to probability scale
predData.forb$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.forb$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# graph
ggplot() +
  geom_point(data = veg_forb, aes(x = STEM_DENSITY, y = used)) +
  geom_path(data = predData.forb, aes(x = STEM_DENSITY, y = p)) +
  geom_ribbon(data = predData.forb, aes(x = STEM_DENSITY, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Stem Density of Forbs") +
  theme_classic()
###################################################################################

# ANALYSIS OF VEG SURVEYS AS RSF
# FOR SLIGHTLY MORE SPECIFIC VEG TYPES

# Native grasses
veg_natgrass1 <- veg_fil4 %>%
  filter(FAMILY_TYPE == "Native_Poales")
# pr(use) as a function of stem density of native grass
fm.natgrass1 <- glm(used ~ STEM_DENSITY, family = binomial(link = "logit"), data = veg_natgrass1)
summary(fm.natgrass1)
#Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.68799    0.21814  -7.738 1.01e-14 ***
#STEM_DENSITY  0.05874    0.02961   1.984   0.0473 *

# predict how probability of use differs across differences in native grass density
predData.natgrass1 <- data.frame(STEM_DENSITY = seq(min(veg_natgrass1$STEM_DENSITY), max(veg_natgrass1$STEM_DENSITY), length = 50))
head(predData.natgrass1)
pred.link <- predict(fm.natgrass1, newdata = predData.natgrass1, se.fit = TRUE)
predData.natgrass1$p <- plogis(pred.link$fit) # back transform to probability scale
predData.natgrass1$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.natgrass1$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# graph
ggplot() +
  geom_point(data = veg_natgrass1, aes(x = STEM_DENSITY, y = used)) +
  geom_path(data = predData.natgrass1, aes(x = STEM_DENSITY, y = p)) +
  geom_ribbon(data = predData.natgrass1, aes(x = STEM_DENSITY, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Stem Density of Native Grasses") +
  theme_classic()


# Nonnative grasses
veg_nonnatgrass1 <- veg_fil4 %>%
  filter(FAMILY_TYPE == "Nonnative_Poales")
# pr(use) as a function of stem density of nonnative grass
fm.nonnatgrass1 <- glm(used ~ STEM_DENSITY, family = binomial(link = "logit"), data = veg_nonnatgrass1)
summary(fm.nonnatgrass1)
#Coefficients:
#               Estimate Std. Error z value Pr(>|z|)
#(Intercept)   0.044306   0.430824   0.103    0.918
#STEM_DENSITY -0.014229   0.009073  -1.568    0.117

# predict how probability of use differs across differences in nonnative grass density
predData.nonnatgrass1 <- data.frame(STEM_DENSITY = seq(min(veg_nonnatgrass1$STEM_DENSITY), max(veg_nonnatgrass1$STEM_DENSITY), length = 50))
head(predData.nonnatgrass1)
pred.link <- predict(fm.nonnatgrass1, newdata = predData.nonnatgrass1, se.fit = TRUE)
predData.nonnatgrass1$p <- plogis(pred.link$fit) # back transform to probability scale
predData.nonnatgrass1$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.nonnatgrass1$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# graph
ggplot() +
  geom_point(data = veg_nonnatgrass1, aes(x = STEM_DENSITY, y = used)) +
  geom_path(data = predData.nonnatgrass1, aes(x = STEM_DENSITY, y = p)) +
  geom_ribbon(data = predData.nonnatgrass1, aes(x = STEM_DENSITY, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Stem Density of Nonnative Grasses") +
  theme_classic()


# Shrub
veg_shrub1 <- veg_fil4 %>%
  filter(FAMILY_TYPE == "Shrub")
# pr(use) as a function of stem density of shrubs
fm.shrub1 <- glm(used ~ STEM_DENSITY, family = binomial(link = "logit"), data = veg_shrub1)
summary(fm.shrub1)
#Coefficients:
#             Estimate Std. Error z value Pr(>|z|)
#(Intercept)    0.6692     1.1442   0.585    0.559
#STEM_DENSITY  -1.1624     0.9019  -1.289    0.197

# predict how probability of use differs across differences in shrub density
predData.shrub1 <- data.frame(STEM_DENSITY = seq(min(veg_shrub1$STEM_DENSITY), max(veg_shrub1$STEM_DENSITY), length = 50))
head(predData.shrub1)
pred.link <- predict(fm.shrub1, newdata = predData.shrub1, se.fit = TRUE)
predData.shrub1$p <- plogis(pred.link$fit) # back transform to probability scale
predData.shrub1$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.shrub1$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# graph
ggplot() +
  geom_point(data = veg_shrub1, aes(x = STEM_DENSITY, y = used)) +
  geom_path(data = predData.shrub1, aes(x = STEM_DENSITY, y = p)) +
  geom_ribbon(data = predData.shrub1, aes(x = STEM_DENSITY, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Stem Density of Shrubs") +
  theme_classic()


# Forbs
veg_forb1 <- veg_fil4 %>%
  filter(FAMILY_TYPE == "Forbs")
# pr(use) as a function of stem density of forbs
fm.forb1 <- glm(used ~ STEM_DENSITY, family = binomial(link = "logit"), data = veg_forb1)
summary(fm.forb1)
#Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.90980    0.22094  -8.644   <2e-16 ***
#STEM_DENSITY  0.05155    0.02572   2.004    0.045 *  

# predict how probability of use differs across differences in forb density
predData.forb1 <- data.frame(STEM_DENSITY = seq(min(veg_forb1$STEM_DENSITY), max(veg_forb1$STEM_DENSITY), length = 50))
head(predData.forb1)
pred.link <- predict(fm.forb1, newdata = predData.forb1, se.fit = TRUE)
predData.forb1$p <- plogis(pred.link$fit) # back transform to probability scale
predData.forb1$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.forb1$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# graph
ggplot() +
  geom_point(data = veg_forb1, aes(x = STEM_DENSITY, y = used)) +
  geom_path(data = predData.forb1, aes(x = STEM_DENSITY, y = p)) +
  geom_ribbon(data = predData.forb1, aes(x = STEM_DENSITY, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Stem Density of Forbs") +
  theme_classic()


# Asteraceae
veg_aster <- veg_fil4 %>%
  filter(FAMILY_TYPE == "Asteraceae")
# pr(use) as a function of stem density of asters
fm.aster <- glm(used ~ STEM_DENSITY, family = binomial(link = "logit"), data = veg_aster)
summary(fm.aster)
#Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.56026    0.25030  -6.234 4.56e-10 ***
#STEM_DENSITY  0.01856    0.02017   0.920    0.358 

# predict how probability of use differs across differences in aster density
predData.aster <- data.frame(STEM_DENSITY = seq(min(veg_aster$STEM_DENSITY), max(veg_aster$STEM_DENSITY), length = 50))
head(predData.aster)
pred.link <- predict(fm.aster, newdata = predData.aster, se.fit = TRUE)
predData.aster$p <- plogis(pred.link$fit) # back transform to probability scale
predData.aster$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.aster$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# graph
ggplot() +
  geom_point(data = veg_aster, aes(x = STEM_DENSITY, y = used)) +
  geom_path(data = predData.aster, aes(x = STEM_DENSITY, y = p)) +
  geom_ribbon(data = predData.aster, aes(x = STEM_DENSITY, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Stem Density of Asteraceae") +
  theme_classic()


# Rosales
veg_rosales <- veg_fil4 %>%
  filter(FAMILY_TYPE == "Rosales")
# pr(use) as a function of stem density of rosales
fm.rosales <- glm(used ~ STEM_DENSITY, family = binomial(link = "logit"), data = veg_rosales)
summary(fm.rosales)
#Coefficients:
#              Estimate Std. Error z value Pr(>|z|)  
#(Intercept)   -1.4082     0.7495  -1.879   0.0602 .
#STEM_DENSITY   0.1876     0.3398   0.552   0.5808 

# predict how probability of use differs across differences in rosales density
predData.ros <- data.frame(STEM_DENSITY = seq(min(veg_rosales$STEM_DENSITY), max(veg_rosales$STEM_DENSITY), length = 50))
head(predData.ros)
pred.link <- predict(fm.rosales, newdata = predData.ros, se.fit = TRUE)
predData.ros$p <- plogis(pred.link$fit) # back transform to probability scale
predData.ros$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.ros$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# graph
ggplot() +
  geom_point(data = veg_rosales, aes(x = STEM_DENSITY, y = used)) +
  geom_path(data = predData.ros, aes(x = STEM_DENSITY, y = p)) +
  geom_ribbon(data = predData.ros, aes(x = STEM_DENSITY, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Stem Density of Rosales") +
  theme_classic()
###################################################################################

# LOOKING JUST AT NATIVE GRASS FAMILIES
# Poaceae vs Cyperaceae vs Nonnative grass vs not grass

# Poaceae grasses
veg_poaceae <- veg_fil4 %>%
  filter(GRASS == "Poaceae")
# pr(use) as a function of stem density of native grass
fm.poaceae <- glm(used ~ STEM_DENSITY, family = binomial(link = "logit"), data = veg_poaceae)
summary(fm.poaceae)
#Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -2.05592    0.29968  -6.860 6.87e-12 ***
#STEM_DENSITY  0.22073    0.08651   2.552   0.0107 * 

# predict how probability of use differs across differences in Poaceae density
predData.poaceae <- data.frame(STEM_DENSITY = seq(min(veg_poaceae$STEM_DENSITY), max(veg_poaceae$STEM_DENSITY), length = 50))
head(predData.poaceae)
pred.link <- predict(fm.poaceae, newdata = predData.poaceae, se.fit = TRUE)
predData.poaceae$p <- plogis(pred.link$fit) # back transform to probability scale
predData.poaceae$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.poaceae$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# graph
ggplot() +
  geom_point(data = veg_poaceae, aes(x = STEM_DENSITY, y = used)) +
  geom_path(data = predData.poaceae, aes(x = STEM_DENSITY, y = p)) +
  geom_ribbon(data = predData.poaceae, aes(x = STEM_DENSITY, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous(expression(paste("Stem Density of ", italic("Poaceae"), " Grasses"))) +
  theme_classic()


# Cyperaceae grasses
veg_cyper <- veg_fil4 %>%
  filter(GRASS == "Cyperaceae")
# pr(use) as a function of stem density of native grass
fm.cyper <- glm(used ~ STEM_DENSITY, family = binomial(link = "logit"), data = veg_cyper)
summary(fm.cyper)
#Coefficients:
#              Estimate Std. Error z value Pr(>|z|)   
#(Intercept)  -1.319432   0.461559  -2.859  0.00425 **
#STEM_DENSITY  0.007545   0.042818   0.176  0.86013 

# predict how probability of use differs across differences in Cyperaceae density
predData.cyper <- data.frame(STEM_DENSITY = seq(min(veg_cyper$STEM_DENSITY), max(veg_cyper$STEM_DENSITY), length = 50))
head(predData.cyper)
pred.link <- predict(fm.cyper, newdata = predData.cyper, se.fit = TRUE)
predData.cyper$p <- plogis(pred.link$fit) # back transform to probability scale
predData.cyper$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.cyper$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# graph
ggplot() +
  geom_point(data = veg_cyper, aes(x = STEM_DENSITY, y = used)) +
  geom_path(data = predData.cyper, aes(x = STEM_DENSITY, y = p)) +
  geom_ribbon(data = predData.cyper, aes(x = STEM_DENSITY, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous(expression(paste("Stem Density of ", italic("Cyperaceae"), " Grasses"))) +
  theme_classic()


# Not grasses
veg_notgrass <- veg_fil4 %>%
  filter(GRASS == "Not_Grass")
# pr(use) as a function of stem density of native grass
fm.notgrass <- glm(used ~ STEM_DENSITY, family = binomial(link = "logit"), data = veg_notgrass)
summary(fm.notgrass)
#Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.63174    0.14806 -11.021   <2e-16 ***
#STEM_DENSITY  0.02696    0.01485   1.815   0.0695 .

# predict how probability of use differs across differences in Cyperaceae density
predData.notgrass <- data.frame(STEM_DENSITY = seq(min(veg_notgrass$STEM_DENSITY), max(veg_notgrass$STEM_DENSITY), length = 50))
head(predData.notgrass)
pred.link <- predict(fm.notgrass, newdata = predData.notgrass, se.fit = TRUE)
predData.notgrass$p <- plogis(pred.link$fit) # back transform to probability scale
predData.notgrass$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.notgrass$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# graph
ggplot() +
  geom_point(data = veg_notgrass, aes(x = STEM_DENSITY, y = used)) +
  geom_path(data = predData.notgrass, aes(x = STEM_DENSITY, y = p)) +
  geom_ribbon(data = predData.notgrass, aes(x = STEM_DENSITY, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous(expression(paste("Stem Density of Non-Grasses"))) +
  theme_classic()


# Nonnative grasses
veg_nonnatgrass <- veg_fil4 %>%
  filter(GRASS == "Nonnative_Poales")
# pr(use) as a function of stem density of native grass
fm.nonnatgrass <- glm(used ~ STEM_DENSITY, family = binomial(link = "logit"), data = veg_nonnatgrass)
summary(fm.nonnatgrass)
#Coefficients:
#               Estimate Std. Error z value Pr(>|z|)
#(Intercept)   0.044306   0.430824   0.103    0.918
#STEM_DENSITY -0.014229   0.009073  -1.568    0.117

# predict how probability of use differs across differences in Cyperaceae density
predData.nonnatgrass <- data.frame(STEM_DENSITY = seq(min(veg_nonnatgrass$STEM_DENSITY), max(veg_nonnatgrass$STEM_DENSITY), length = 50))
head(predData.nonnatgrass)
pred.link <- predict(fm.nonnatgrass, newdata = predData.nonnatgrass, se.fit = TRUE)
predData.nonnatgrass$p <- plogis(pred.link$fit) # back transform to probability scale
predData.nonnatgrass$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.nonnatgrass$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# graph
ggplot() +
  geom_point(data = veg_nonnatgrass, aes(x = STEM_DENSITY, y = used)) +
  geom_path(data = predData.nonnatgrass, aes(x = STEM_DENSITY, y = p)) +
  geom_ribbon(data = predData.nonnatgrass, aes(x = STEM_DENSITY, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous(expression(paste("Stem Density of Nonnative Grasses"))) +
  theme_classic()

