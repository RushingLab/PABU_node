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
library(rempsyc)
library(flextable)
library(broom)
library(report)
library(effectsize)


# Reset R's brain - removes all previous objects
rm(list=ls())

###################################################################################

# Read in veg survey data from the 18_vegsurvey_01s.R script
veg_type <- readRDS("data/veg_sample_rsf_01/veg_type.rds")
veg_grass <- readRDS("data/veg_sample_rsf_01/veg_grass.rds")
veg_family <- readRDS("data/veg_sample_rsf_01/veg_family.rds")

# Reading in veg survey data
veg <- read.csv(file = "data-raw/veg_surveys.csv")
veg$POINT <- as.factor(veg$POINT)
veg10 <- veg %>%
  select(POINT, LOCATION) %>%
  distinct(POINT, LOCATION)%>%
  arrange(POINT) %>%
  mutate(ROAD = if_else(grepl("Rd$", LOCATION), "road", "grass")) %>%
  distinct(POINT, ROAD)
veg11 <- veg10[-c(16,22,28,35,45,100),]
veg11$ROAD <- as.factor(veg11$ROAD)
veg_type <- readRDS("data/veg_sample_rsf_01/veg_type.rds")
road_veg <- left_join(veg_type, veg11, by = "POINT")
road_veg2 <- road_veg %>%
  filter(ROAD == 'grass')

###################################################################################

# Multiple Regression Model 

fm.t <- glm(used ~ ROAD + time_since_burn + PERCENT_VEG + SD_Shrub + SD_Forbs + SD_Native_Poales + SD_Nonnative_Poales, family = binomial(link = "logit"), 
            data = road_veg)
summary(fm.t)

# GENERAL TYPE
# non-standardized
fm.t <- glm(used ~ time_since_burn + PERCENT_VEG + SD_Shrub + SD_Forbs + SD_Native_Poales + SD_Nonnative_Poales, family = binomial(link = "logit"), 
            data = veg_type)
summary(fm.t)
#Coefficients:
#                           Estimate Std. Error z value Pr(>|z|)   
#(Intercept)              -3.484909   1.204625  -2.893  0.00382 **
#time_since_burn0-1_years  0.819237   1.191792   0.687  0.49183   
#time_since_burn1-2_years  1.995068   1.090896   1.829  0.06742 . 
#time_since_burn2-3_years -1.042676   1.513950  -0.689  0.49100   
#PERCENT_VEG              -0.004151   0.010517  -0.395  0.69307   
#SD_Shrub                  0.459969   0.263914   1.743  0.08136 . 
#SD_Forbs                  0.018191   0.017642   1.031  0.30247   
#SD_Native_Poales          0.073618   0.037342   1.971  0.04867 * 
#SD_Nonnative_Poales       0.002134   0.005480   0.389  0.69693   

## USE THIS ONE
# continuous variables standardized
fm.t.c <- glm(used ~ time_since_burn + PERCENT_VEG.c + SD_Shrub.c + SD_Forbs.c + SD_Native_Poales.c + SD_Nonnative_Poales.c, family = binomial(link = "logit"), 
            data = veg_type)
summary(fm.t.c)
#Coefficients:
#                           Estimate Std. Error z value Pr(>|z|)   
#(Intercept)              -2.991147   1.039299  -2.878   0.0040 **
#time_since_burn0-1_years  0.819237   1.191792   0.687   0.4918   
#time_since_burn1-2_years  1.995068   1.090896   1.829   0.0674 . 
#time_since_burn2-3_years -1.042676   1.513950  -0.689   0.4910   
#PERCENT_VEG.c            -0.004151   0.010517  -0.395   0.6931   
#SD_Shrub.c                0.459969   0.263914   1.743   0.0814 . 
#SD_Forbs.c                0.018191   0.017642   1.031   0.3025   
#SD_Native_Poales.c        0.073618   0.037342   1.971   0.0487 * 
#SD_Nonnative_Poales.c     0.002134   0.005480   0.389   0.6969

# unburned
intercept <- plogis(-2.991147) #0.04782743
lowerval <- plogis(-2.991147 - 1.96 * 1.039299) # 0.006508134
upperval <- plogis(-2.991147 + 1.96 * 1.039299) #0.2780568
odds <- exp(-2.991147) #0.05022979

# odds occurence in 1-2 vs unburned
exp(1.995068) #7.352703
plogis(-2.991147 + 1.995068 ) #0.269713
lowerval <- plogis(-2.991147 + 1.995068 - 1.96 * 1.090896) #0.04171697
upperval <- plogis(-2.991147 + 1.995068 + 1.96 * 1.090896) #0.7580608

#sd_native_poales
plogis(-2.991147 + 0.073618 ) # 0.05129381
p1 <- plogis(-2.99 + 0.07 * 0)
p2 <- plogis(-2.99 + 0.07 * 1)
(OR1 <- (p2/(1-p2))/(p1/(1-p1))) #[1] 1.072508
exp(0.07)
exp(0.07 -1.96 *0.037342)
exp(0.07 +1.96 *0.037342)

stats.table <- broom::tidy(fm.t.c, conf.int = FALSE)
names(stats.table) <- c("Term", "Estimate", "SE", "t", "p")
stats.table$Term <- c("(Intercept)", "0-1 Years Since Burn", "1-2 Years Since Burn", "2-3 Years Since Burn",
                           "Vegetative Cover (%)", "Shrub Stem Density", "Forb Stem Density", 
                           "Native Poales Stem Density", "Nonnative Poales Stem Density")
tab <- knitr::kable(stats.table)
my_table <- rempsyc::nice_table(stats.table, broom = "glm",
                                title = c("Vegetative resource selection based on bunting foraging locations"),
                                note = c("* p < .05, ** p < .01"))
print(my_table, preview = "docx")

# GRASS TYPE
fm.g <- glm(used ~ time_since_burn + PERCENT_VEG + SD_Not_Grass + SD_Cyperaceae + SD_Poaceae + SD_Nonnative_Poales, family = binomial(link = "logit"), 
              data = veg_grass)
summary(fm.g)
#Coefficients:
#                           Estimate Std. Error z value Pr(>|z|)   
#(Intercept)              -2.824758   0.890153  -3.173  0.00151 **
#time_since_burn0-1_years  0.176420   0.831998   0.212  0.83207   
#time_since_burn1-2_years  0.598934   0.758585   0.790  0.42980   
#time_since_burn2-3_years -1.456061   1.187385  -1.226  0.22010   
#PERCENT_VEG              -0.003341   0.011543  -0.289  0.77224   
#SD_Not_Grass              0.041680   0.017803   2.341  0.01922 * 
#SD_Cyperaceae            -0.019163   0.052198  -0.367  0.71353   
#SD_Poaceae                0.175959   0.059287   2.968  0.00300 **
#SD_Nonnative_Poales       0.002341   0.005230   0.448  0.65439 

# continuous variables standardized
fm.g.c <- glm(used ~ time_since_burn + PERCENT_VEG.c + SD_Not_Grass.c + SD_Cyperaceae.c + SD_Poaceae.c + SD_Nonnative_Poales.c, family = binomial(link = "logit"), 
                data = veg_grass)
summary(fm.g.c)
#Coefficients:
#                           Estimate Std. Error z value Pr(>|z|)   
#(Intercept)              -2.072981   0.660408  -3.139   0.0017 **
#time_since_burn0-1_years  0.176420   0.831998   0.212   0.8321   
#time_since_burn1-2_years  0.598934   0.758585   0.790   0.4298   
#time_since_burn2-3_years -1.456061   1.187385  -1.226   0.2201   
#PERCENT_VEG.c            -0.003341   0.011543  -0.289   0.7722   
#SD_Not_Grass.c            0.041680   0.017803   2.341   0.0192 * 
#SD_Cyperaceae.c          -0.019163   0.052198  -0.367   0.7135   
#SD_Poaceae.c              0.175959   0.059287   2.968   0.0030 **
#SD_Nonnative_Poales.c     0.002341   0.005230   0.448   0.6544 

# FAMILY FOOD TYPE

fm.f <- glm(used ~ time_since_burn + PERCENT_VEG + SD_Shrub + SD_Forbs + SD_Asteraceae + SD_Rosales + SD_Native_Poales + SD_Nonnative_Poales, family = binomial(link = "logit"), 
            data = veg_family)
summary(fm.f)
#Coefficients:
#                           Estimate Std. Error z value Pr(>|z|)  
#(Intercept)              -2.9479156  1.1958560  -2.465   0.0137 *
#time_since_burn0-1_years  1.0823258  1.1710303   0.924   0.3554  
#time_since_burn1-2_years  1.5600763  1.1064717   1.410   0.1586  
#time_since_burn2-3_years -1.0365061  1.4736556  -0.703   0.4818  
#PERCENT_VEG              -0.0059688  0.0110436  -0.540   0.5889  
#SD_Shrub                  0.1556323  0.3096174   0.503   0.6152  
#SD_Forbs                 -0.0040762  0.0322239  -0.126   0.8993  
#SD_Asteraceae            -0.0108365  0.0231635  -0.468   0.6399  
#SD_Rosales                0.1616238  0.2598389   0.622   0.5339  
#SD_Native_Poales          0.0923488  0.0374700   2.465   0.0137 *
#SD_Nonnative_Poales       0.0009065  0.0058158   0.156   0.8761  

# continuous variables standardized
fm.f.c <- glm(used ~ time_since_burn + PERCENT_VEG.c + SD_Shrub.c + SD_Forbs.c + SD_Asteraceae.c + SD_Rosales.c + SD_Native_Poales.c + SD_Nonnative_Poales.c, family = binomial(link = "logit"), 
            data = veg_family)
summary(fm.f.c)
#Coefficients:
#                           Estimate Std. Error z value Pr(>|z|)   
#(Intercept)              -2.9110678  1.0361747  -2.809  0.00496 **
#time_since_burn0-1_years  1.0823258  1.1710303   0.924  0.35536   
#time_since_burn1-2_years  1.5600763  1.1064717   1.410  0.15855   
#time_since_burn2-3_years -1.0365061  1.4736556  -0.703  0.48183   
#PERCENT_VEG.c            -0.0059688  0.0110436  -0.540  0.58887   
#SD_Shrub.c                0.1556323  0.3096174   0.503  0.61520   
#SD_Forbs.c               -0.0040762  0.0322239  -0.126  0.89934   
#SD_Asteraceae.c          -0.0108365  0.0231635  -0.468  0.63991   
#SD_Rosales.c              0.1616238  0.2598389   0.622  0.53393   
#SD_Native_Poales.c        0.0923488  0.0374700   2.465  0.01372 * 
#SD_Nonnative_Poales.c     0.0009065  0.0058158   0.156  0.87614