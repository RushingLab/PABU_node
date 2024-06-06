# Loading necessary packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
install.packages("viridis")
install.packages("RColorBrewer")
library(viridis)  
library(RColorBrewer)

# Reset R's brain - removes all previous objects
rm(list=ls())

###################################################################################
# RSF Model for veg survesy

# Read in veg survey data from the 18_vegsurvey_01s.R script
veg_type <- readRDS("data/veg_sample_rsf_01/veg_type.rds")

# setting colors
colors2 <- c("#E3E418FF", "#5DC863FF", "#2C728EFF", "#481F70FF")
grass_colors <- c("#FE9F6DFF","#F1605DFF", "#9F2F7FFF", "#180F3EFF")


# GENERAL TYPE: Shrub, Forbs, Native Poales, Nonnative Poales
# continuous variables standardized
fm.t <- glm(used ~ time_since_burn + PERCENT_VEG + SD_Shrub + SD_Forbs + SD_Native_Poales + SD_Nonnative_Poales, family = binomial(link = "logit"), 
              data = veg_type)
summary(fm.t)

# GENERAL TYPE: Shrub, Forbs, Native Poales, Nonnative Poales
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

###################################################################################

# Creating graph for how pr(use) for PABU foraging differs as native grass stem density differs in differing burn trtmt grasslands
# Simplifying to just columns of interest
veg <- veg_type %>%
  select(-c(POINT, PERCENT_VEG, SD_Shrub, SD_Forbs, SD_Native_Poales, SD_Nonnative_Poales))
head(veg)

veg3 <- veg_type %>%
  select(-c(POINT, PERCENT_VEG.c, SD_Shrub.c, SD_Forbs.c, SD_Native_Poales.c, SD_Nonnative_Poales.c))
head(veg3)

veg.ub <- veg3 %>%
            filter(time_since_burn == "unmanaged")
veg.01 <- veg3 %>%
  filter(time_since_burn == "0-1_years")
veg.12 <- veg3 %>%
  filter(time_since_burn == "1-2_years")
veg.23 <- veg3 %>%
  filter(time_since_burn == "2-3_years")
###########

# Predicting and creating visualizations for unmanaged burn tracts
# Predicted data
predData.ngrass.unman <- data.frame(SD_Native_Poales = seq(min(veg3$SD_Native_Poales), max(veg3$SD_Native_Poales), length = 100),
                                 PERCENT_VEG = mean(veg3$PERCENT_VEG),
                                 time_since_burn = "unmanaged",
                                 SD_Shrub = mean(veg3$SD_Shrub),
                                 SD_Forbs = mean(veg3$SD_Forbs),
                                 SD_Nonnative_Poales = mean(veg3$SD_Nonnative_Poales))
predData.ngrass.01 <- data.frame(SD_Native_Poales = seq(min(veg3$SD_Native_Poales), max(veg3$SD_Native_Poales), length = 100),
                                 PERCENT_VEG = mean(veg3$PERCENT_VEG),
                                 time_since_burn = "0-1_years",
                                 SD_Shrub = mean(veg3$SD_Shrub),
                                 SD_Forbs = mean(veg3$SD_Forbs),
                                 SD_Nonnative_Poales = mean(veg3$SD_Nonnative_Poales))
predData.ngrass.12 <- data.frame(SD_Native_Poales = seq(min(veg3$SD_Native_Poales), max(veg3$SD_Native_Poales), length = 100),
                                 PERCENT_VEG = mean(veg3$PERCENT_VEG),
                                 time_since_burn = "1-2_years",
                                 SD_Shrub = mean(veg3$SD_Shrub),
                                 SD_Forbs = mean(veg3$SD_Forbs),
                                 SD_Nonnative_Poales = mean(veg3$SD_Nonnative_Poales))
predData.ngrass.23 <- data.frame(SD_Native_Poales = seq(min(veg3$SD_Native_Poales), max(veg3$SD_Native_Poales), length = 100),
                                 PERCENT_VEG = mean(veg3$PERCENT_VEG),
                                 time_since_burn = "2-3_years",
                                 SD_Shrub = mean(veg3$SD_Shrub),
                                 SD_Forbs = mean(veg3$SD_Forbs),
                                 SD_Nonnative_Poales = mean(veg3$SD_Nonnative_Poales))

# unmanaged
pred.link.unman <- predict(fm.t, newdata = predData.ngrass.unman, se.fit = TRUE)
predData.ngrass.unman$p <- plogis(pred.link.unman$fit) # back transform to probability scale
predData.ngrass.unman$lower <- plogis(pred.link.unman$fit - 1.96 * pred.link.unman$se.fit)
predData.ngrass.unman$upper <- plogis(pred.link.unman$fit + 1.96 * pred.link.unman$se.fit)

# 0-1
pred.link.01 <- predict(fm.t, newdata = predData.ngrass.01, se.fit = TRUE)
predData.ngrass.01$p <- plogis(pred.link.01$fit) # back transform to probability scale
predData.ngrass.01$lower <- plogis(pred.link.01$fit - 1.96 * pred.link.01$se.fit)
predData.ngrass.01$upper <- plogis(pred.link.01$fit + 1.96 * pred.link.01$se.fit)

# 1-2
pred.link.12 <- predict(fm.t, newdata = predData.ngrass.12, se.fit = TRUE)
predData.ngrass.12$p <- plogis(pred.link.12$fit) # back transform to probability scale
predData.ngrass.12$lower <- plogis(pred.link.12$fit - 1.96 * pred.link.12$se.fit)
predData.ngrass.12$upper <- plogis(pred.link.12$fit + 1.96 * pred.link.12$se.fit)

# 2-3
pred.link.23 <- predict(fm.t, newdata = predData.ngrass.23, se.fit = TRUE)
predData.ngrass.23$p <- plogis(pred.link.23$fit) # back transform to probability scale
predData.ngrass.23$lower <- plogis(pred.link.23$fit - 1.96 * pred.link.23$se.fit)
predData.ngrass.23$upper <- plogis(pred.link.23$fit + 1.96 * pred.link.23$se.fit)

colors2 <- c("#E3E418FF", "#5DC863FF", "#2C728EFF", "#481F70FF")
colors4 <- c("#1f78b4", "#ff7f00", "#33a02c", "#e31a1c")
# Plotting the predictions and observations
plot <- ggplot() +
  geom_point(data = veg3, aes(x = SD_Native_Poales, y = used, color = time_since_burn)) +
  geom_path(data = predData.ngrass.unman, aes(x = SD_Native_Poales, y = p, color = time_since_burn)) +
  geom_path(data = predData.ngrass.01, aes(x = SD_Native_Poales, y = p, color =  time_since_burn)) +
  geom_path(data = predData.ngrass.12, aes(x = SD_Native_Poales, y = p, color = time_since_burn)) +
  geom_path(data = predData.ngrass.23, aes(x = SD_Native_Poales, y = p, color = time_since_burn)) +
  geom_ribbon(data = predData.ngrass.unman, aes(x = SD_Native_Poales, ymin = lower, ymax = upper),
              fill = NA, color = "#1f78b4", linetype = "longdash") +
  geom_ribbon(data = predData.ngrass.01, aes(x = SD_Native_Poales, ymin = lower, ymax = upper),
              fill = NA, color = "#ff7f00",linetype = "longdash") +
  geom_ribbon(data = predData.ngrass.12, aes(x = SD_Native_Poales, ymin = lower, ymax = upper),
              fill = NA, color = "#33a02c",linetype = "longdash") +
  geom_ribbon(data = predData.ngrass.23, aes(x = SD_Native_Poales, ymin = lower, ymax = upper),
              fill = NA, color = "#e31a1c", linetype = "longdash") +
  scale_color_manual(values = colors4) +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Native Grass Stem Density in Grassland") +
  theme_classic()

plotun <- ggplot() +
  geom_point(data = veg.ub, aes(x = SD_Native_Poales, y = used)) +
  geom_path(data = predData.ngrass.unman, aes(x = SD_Native_Poales, y = p)) +
  geom_ribbon(data = predData.ngrass.unman, aes(x = SD_Native_Poales, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Native Grass Stem Density in Unburned Grassland") +
  theme_classic()
plot01 <- ggplot() +
  geom_point(data = veg.01, aes(x = SD_Native_Poales, y = used)) +
  geom_path(data = predData.ngrass.01, aes(x = SD_Native_Poales, y = p)) +
  geom_ribbon(data = predData.ngrass.01, aes(x = SD_Native_Poales, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Native Grass Stem Density 0-1 Years Post-Burn") +
  theme_classic()
plot12 <- ggplot() +
  geom_point(data = veg.12, aes(x = SD_Native_Poales, y = used)) +
  geom_path(data = predData.ngrass.12, aes(x = SD_Native_Poales, y = p)) +
  geom_ribbon(data = predData.ngrass.12, aes(x = SD_Native_Poales, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Native Grass Stem Density 1-2 Years Post-Burn") +
  theme_classic()
plot23 <- ggplot() +
  geom_point(data = veg.23, aes(x = SD_Native_Poales, y = used)) +
  geom_path(data = predData.ngrass.23, aes(x = SD_Native_Poales, y = p)) +
  geom_ribbon(data = predData.ngrass.23, aes(x = SD_Native_Poales, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Native Grass Stem Density 2-3 Years Post-Burn") +
  theme_classic()

combined_pr_use <- ggpubr::ggarrange(plot01,
                                     plot12,
                                     plot23,
                                     plotun,
                                     nrow =2, ncol =2)
# Saving plot
ggsave("data/figures/use_covariates/natgrass_years_correct.pdf", plot = combined_pr_use, width = 10, height = 10)
ggsave( "data/figures/use_covariates/natgrass_years_correct.jpeg", plot = combined_pr_use, width = 10, height = 10, dpi = 600)
###########

# Predicting and creating visualizations for unmanaged burn tracts
# Predicted data
predData.ngrass.un <- data.frame(SD_Native_Poales = seq(min(veg3$SD_Native_Poales), max(veg3$SD_Native_Poales), length = 100),
                                PERCENT_VEG = mean(veg3$PERCENT_VEG),
                                time_since_burn = "unmanaged",
                                SD_Shrub = mean(veg3$SD_Shrub),
                                SD_Forbs = mean(veg3$SD_Forbs),
                                SD_Nonnative_Poales = mean(veg3$SD_Nonnative_Poales))
head(predData.ngrass.un)
# Switching to the link scale to get confidence intervals on the probability scale
pred.link <- predict(fm.t, newdata = predData.ngrass.un, se.fit = TRUE)
predData.ngrass.un$p <- plogis(pred.link$fit) # back transform to probability scale
predData.ngrass.un$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.ngrass.un$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# Plotting the predictions and observations
plotun <- ggplot() +
  geom_point(data = veg3, aes(x = SD_Native_Poales, y = used)) +
  geom_path(data = predData.ngrass.un, aes(x = SD_Native_Poales, y = p)) +
  geom_ribbon(data = predData.ngrass.un, aes(x = SD_Native_Poales, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Native Grass Stem Density in Unburned Grassland") +
  theme_classic()


# Predicting and creating visualizations for 0-1year burn tracts
# Predicted data
predData.ngrass.un <- data.frame(SD_Native_Poales = seq(min(veg3$SD_Native_Poales), max(veg3$SD_Native_Poales), length = 100),
                                 PERCENT_VEG = mean(veg3$PERCENT_VEG),
                                 time_since_burn = "0-1_years",
                                 SD_Shrub = mean(veg3$SD_Shrub),
                                 SD_Forbs = mean(veg3$SD_Forbs),
                                 SD_Nonnative_Poales = mean(veg3$SD_Nonnative_Poales))
head(predData.ngrass.un)
# Switching to the link scale to get confidence intervals on the probability scale
pred.link <- predict(fm.t, newdata = predData.ngrass.un, se.fit = TRUE)
predData.ngrass.un$p <- plogis(pred.link$fit) # back transform to probability scale
predData.ngrass.un$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.ngrass.un$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# Plotting the predictions and observations
plot01 <- ggplot() +
  geom_point(data = veg3, aes(x = SD_Native_Poales, y = used)) +
  geom_path(data = predData.ngrass.un, aes(x = SD_Native_Poales, y = p)) +
  geom_ribbon(data = predData.ngrass.un, aes(x = SD_Native_Poales, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Native Grass Stem Density 0-1 Years Post-Burn") +
  theme_classic()


# Predicting and creating visualizations for 1-2year burn tracts
# Predicted data
predData.ngrass.un <- data.frame(SD_Native_Poales = seq(min(veg3$SD_Native_Poales), max(veg3$SD_Native_Poales), length = 100),
                                 PERCENT_VEG = mean(veg3$PERCENT_VEG),
                                 time_since_burn = "1-2_years",
                                 SD_Shrub = mean(veg3$SD_Shrub),
                                 SD_Forbs = mean(veg3$SD_Forbs),
                                 SD_Nonnative_Poales = mean(veg3$SD_Nonnative_Poales))
head(predData.ngrass.un)
# Switching to the link scale to get confidence intervals on the probability scale
pred.link <- predict(fm.t, newdata = predData.ngrass.un, se.fit = TRUE)
predData.ngrass.un$p <- plogis(pred.link$fit) # back transform to probability scale
predData.ngrass.un$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.ngrass.un$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# Plotting the predictions and observations
plot12 <- ggplot() +
  geom_point(data = veg3, aes(x = SD_Native_Poales, y = used)) +
  geom_path(data = predData.ngrass.un, aes(x = SD_Native_Poales, y = p)) +
  geom_ribbon(data = predData.ngrass.un, aes(x = SD_Native_Poales, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Native Grass Stem Density 1-2 Years Post-Burn") +
  theme_classic()


# Predicting and creating visualizations for 2-3year burn tracts
# Predicted data
predData.ngrass.un <- data.frame(SD_Native_Poales = seq(min(veg3$SD_Native_Poales), max(veg3$SD_Native_Poales), length = 100),
                                 PERCENT_VEG = mean(veg3$PERCENT_VEG),
                                 time_since_burn = "2-3_years",
                                 SD_Shrub = mean(veg3$SD_Shrub),
                                 SD_Forbs = mean(veg3$SD_Forbs),
                                 SD_Nonnative_Poales = mean(veg3$SD_Nonnative_Poales))
head(predData.ngrass.un)
# Switching to the link scale to get confidence intervals on the probability scale
pred.link <- predict(fm.t, newdata = predData.ngrass.un, se.fit = TRUE)
predData.ngrass.un$p <- plogis(pred.link$fit) # back transform to probability scale
predData.ngrass.un$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.ngrass.un$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# Plotting the predictions and observations
plot23 <- ggplot() +
  geom_point(data = veg3, aes(x = SD_Native_Poales, y = used)) +
  geom_path(data = predData.ngrass.un, aes(x = SD_Native_Poales, y = p)) +
  geom_ribbon(data = predData.ngrass.un, aes(x = SD_Native_Poales, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Native Grass Stem Density 2-3 Years Post-Burn") +
  theme_classic()

combined_pr_use <- ggpubr::ggarrange(plot01,
                                     plot12,
                                     plot23,
                                     plotun,
                                     nrow =2, ncol =2)
# Saving plot
ggsave("data/figures/use_covariates/natgrass_years.pdf", plot = combined_pr_use, width = 10, height = 10)
ggsave( "data/figures/use_covariates/natgrass_years.jpeg", plot = combined_pr_use, width = 10, height = 10, dpi = 600)
###################################################################################

# Creating graph for how pr(use) for PABU foraging differs for diff stem densities in 1-2yr managed grasslands
# Simplifying to just columns of interest

# Predicting and creating visualizations for native grass burn tracts
# Predicted data
predData.ngrass.un <- data.frame(SD_Native_Poales = seq(min(veg3$SD_Native_Poales), max(veg3$SD_Native_Poales), length = 100),
                                 PERCENT_VEG = mean(veg3$PERCENT_VEG),
                                 time_since_burn = "1-2_years",
                                 SD_Shrub = mean(veg3$SD_Shrub),
                                 SD_Forbs = mean(veg3$SD_Forbs),
                                 SD_Nonnative_Poales = mean(veg3$SD_Nonnative_Poales))
head(predData.ngrass.un)
# Switching to the link scale to get confidence intervals on the probability scale
pred.link <- predict(fm.t, newdata = predData.ngrass.un, se.fit = TRUE)
predData.ngrass.un$p <- plogis(pred.link$fit) # back transform to probability scale
predData.ngrass.un$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.ngrass.un$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# Plotting the predictions and observations
plot.n <- ggplot() +
  geom_point(data = veg3, aes(x = SD_Native_Poales, y = used)) +
  geom_path(data = predData.ngrass.un, aes(x = SD_Native_Poales, y = p)) +
  geom_ribbon(data = predData.ngrass.un, aes(x = SD_Native_Poales, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Native Grass Stem Density 1-2 Years Post-Burn") +
  theme_classic()


# Predicting and creating visualizations for nonnative grasses in 1-2yr burn tracts
# Predicted data
predData.ngrass.un <- data.frame(SD_Native_Poales = mean(veg3$SD_Native_Poales),
                                 PERCENT_VEG = mean(veg3$PERCENT_VEG),
                                 time_since_burn = "1-2_years",
                                 SD_Shrub = mean(veg3$SD_Shrub),
                                 SD_Forbs = mean(veg3$SD_Forbs),
                                 SD_Nonnative_Poales = seq(min(veg3$SD_Nonnative_Poales), max(veg3$SD_Nonnative_Poales), length = 100))
head(predData.ngrass.un)
# Switching to the link scale to get confidence intervals on the probability scale
pred.link <- predict(fm.t, newdata = predData.ngrass.un, se.fit = TRUE)
predData.ngrass.un$p <- plogis(pred.link$fit) # back transform to probability scale
predData.ngrass.un$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.ngrass.un$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# Plotting the predictions and observations
plot.nn <- ggplot() +
  geom_point(data = veg3, aes(x = SD_Nonnative_Poales, y = used)) +
  geom_path(data = predData.ngrass.un, aes(x = SD_Nonnative_Poales, y = p)) +
  geom_ribbon(data = predData.ngrass.un, aes(x = SD_Nonnative_Poales, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Nonnative Grass Stem Density 1-2 Years Post-Burn") +
  theme_classic()


# Predicting and creating visualizations for 1-2year burn tracts
# Predicted data
predData.ngrass.un <- data.frame(SD_Native_Poales = mean(veg3$SD_Native_Poales),
                                 PERCENT_VEG = mean(veg3$PERCENT_VEG),
                                 time_since_burn = "1-2_years",
                                 SD_Shrub = seq(min(veg3$SD_Shrub), max(veg3$SD_Shrub), length = 100),
                                 SD_Forbs = mean(veg3$SD_Forbs),
                                 SD_Nonnative_Poales = mean(veg3$SD_Nonnative_Poales))
head(predData.ngrass.un)
# Switching to the link scale to get confidence intervals on the probability scale
pred.link <- predict(fm.t, newdata = predData.ngrass.un, se.fit = TRUE)
predData.ngrass.un$p <- plogis(pred.link$fit) # back transform to probability scale
predData.ngrass.un$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.ngrass.un$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# Plotting the predictions and observations
plot.s <- ggplot() +
  geom_point(data = veg3, aes(x = SD_Shrub, y = used)) +
  geom_path(data = predData.ngrass.un, aes(x = SD_Shrub, y = p)) +
  geom_ribbon(data = predData.ngrass.un, aes(x = SD_Shrub, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Shrub Stem Density 1-2 Years Post-Burn") +
  theme_classic()


# Predicting and creating visualizations for forbs
# Predicted data
predData.ngrass.un <- data.frame(SD_Native_Poales = mean(veg3$SD_Native_Poales),
                                 PERCENT_VEG = mean(veg3$PERCENT_VEG),
                                 time_since_burn = "1-2_years",
                                 SD_Shrub = mean(veg3$SD_Shrub),
                                 SD_Forbs = seq(min(veg3$SD_Forbs), max(veg3$SD_Forbs), length = 100),
                                 SD_Nonnative_Poales = mean(veg3$SD_Nonnative_Poales))
head(predData.ngrass.un)
# Switching to the link scale to get confidence intervals on the probability scale
pred.link <- predict(fm.t, newdata = predData.ngrass.un, se.fit = TRUE)
predData.ngrass.un$p <- plogis(pred.link$fit) # back transform to probability scale
predData.ngrass.un$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.ngrass.un$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# Plotting the predictions and observations
plot.f <- ggplot() +
  geom_point(data = veg3, aes(x = SD_Forbs, y = used)) +
  geom_path(data = predData.ngrass.un, aes(x = SD_Forbs, y = p)) +
  geom_ribbon(data = predData.ngrass.un, aes(x = SD_Forbs, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Forb Stem Density 1-2 Years Post-Burn") +
  theme_classic()

combined_veg <- ggpubr::ggarrange(plot.n,
                                  plot.nn,
                                  plot.s,
                                  plot.f,
                                  nrow =2, ncol =2)

ggsave("data/figures/use_covariates/veg_1-2yrs.pdf", plot = combined_veg, width = 10, height = 10)
ggsave( "data/figures/use_covariates/veg_1-2yrs.jpeg", plot = combined_veg, width = 10, height = 10, dpi = 600)

##########


# Predicting and creating visualizations for unmanaged burn tracts
# Predicted data
predData.ngrass.un <- data.frame(SD_Native_Poales = seq(min(veg3$SD_Native_Poales), max(veg3$SD_Native_Poales), length = 100),
                                 PERCENT_VEG = mean(veg3$PERCENT_VEG),
                                 time_since_burn = "unmanaged",
                                 SD_Shrub = mean(veg3$SD_Shrub),
                                 SD_Forbs = mean(veg3$SD_Forbs),
                                 SD_Nonnative_Poales = mean(veg3$SD_Nonnative_Poales))
head(predData.ngrass.un)
# Switching to the link scale to get confidence intervals on the probability scale
pred.link <- predict(fm.t, newdata = predData.ngrass.un, se.fit = TRUE)
predData.ngrass.un$p <- plogis(pred.link$fit) # back transform to probability scale
predData.ngrass.un$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.ngrass.un$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# Plotting the predictions and observations
p.n <- ggplot() +
  geom_point(data = veg3, aes(x = SD_Native_Poales, y = used)) +
  geom_path(data = predData.ngrass.un, aes(x = SD_Native_Poales, y = p)) +
  geom_ribbon(data = predData.ngrass.un, aes(x = SD_Native_Poales, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Native Grass Stem Density in Unburned Grasslands") +
  theme_classic()

grass_colors <- c("#8C2981FF", "#DE4968FF", "#FE9F6DFF", "#FCFDBFFF")

# Predicting and creating visualizations for 1-2 year post-burn tracts for shrubs
# Predicted data
predData.ngrass.un <- data.frame(SD_Native_Poales = mean(veg3$SD_Native_Poales),
                                 PERCENT_VEG = mean(veg3$PERCENT_VEG),
                                 time_since_burn = "unmanaged",
                                 SD_Shrub = seq(min(veg3$SD_Shrub), max(veg3$SD_Shrub), length = 100),
                                 SD_Forbs = mean(veg3$SD_Forbs),
                                 SD_Nonnative_Poales = mean(veg3$SD_Nonnative_Poales))
head(predData.ngrass.un)
# Switching to the link scale to get confidence intervals on the probability scale
pred.link <- predict(fm.t, newdata = predData.ngrass.un, se.fit = TRUE)
predData.ngrass.un$p <- plogis(pred.link$fit) # back transform to probability scale
predData.ngrass.un$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.ngrass.un$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# Plotting the predictions and observations
p.s <- ggplot() +
  geom_point(data = veg3, aes(x = SD_Shrub, y = used)) +
  geom_path(data = predData.ngrass.un, aes(x = SD_Shrub, y = p)) +
  geom_ribbon(data = predData.ngrass.un, aes(x = SD_Shrub, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Shrub Stem Density in Unburned Grasslands") +
  theme_classic()



# Predicting and creating visualizations for 1-2 year post-burn tracts for forbs
# Predicted data
predData.ngrass.un <- data.frame(SD_Native_Poales = mean(veg3$SD_Native_Poales),
                                 PERCENT_VEG = mean(veg3$PERCENT_VEG),
                                 time_since_burn = "unmanaged",
                                 SD_Shrub = mean(veg3$SD_Shrub),
                                 SD_Forbs = seq(min(veg3$SD_Forbs), max(veg3$SD_Forbs), length = 100),
                                 SD_Nonnative_Poales = mean(veg3$SD_Nonnative_Poales))
head(predData.ngrass.un)
# Switching to the link scale to get confidence intervals on the probability scale
pred.link <- predict(fm.t, newdata = predData.ngrass.un, se.fit = TRUE)
predData.ngrass.un$p <- plogis(pred.link$fit) # back transform to probability scale
predData.ngrass.un$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.ngrass.un$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# Plotting the predictions and observations
p.f <- ggplot() +
  geom_point(data = veg3, aes(x = SD_Forbs, y = used)) +
  geom_path(data = predData.ngrass.un, aes(x = SD_Forbs, y = p)) +
  geom_ribbon(data = predData.ngrass.un, aes(x = SD_Forbs, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Forb Stem Density in Unburned Grasslands") +
  theme_classic()



# Predicting and creating visualizations for 1-2 year post-burn tracts for nonnative grasses
# Predicted data
predData.ngrass.un <- data.frame(SD_Native_Poales = mean(veg3$SD_Native_Poales),
                                 PERCENT_VEG = mean(veg3$PERCENT_VEG),
                                 time_since_burn = "unmanaged",
                                 SD_Shrub = mean(veg3$SD_Shrub),
                                 SD_Forbs = mean(veg3$SD_Forbs),
                                 SD_Nonnative_Poales = seq(min(veg3$SD_Nonnative_Poales), max(veg3$SD_Nonnative_Poales), length = 100))
head(predData.ngrass.un)
# Switching to the link scale to get confidence intervals on the probability scale
pred.link <- predict(fm.t, newdata = predData.ngrass.un, se.fit = TRUE)
predData.ngrass.un$p <- plogis(pred.link$fit) # back transform to probability scale
predData.ngrass.un$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.ngrass.un$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
# Plotting the predictions and observations
p.non <- ggplot() +
  geom_point(data = veg3, aes(x = SD_Nonnative_Poales, y = used)) +
  geom_path(data = predData.ngrass.un, aes(x = SD_Nonnative_Poales, y = p)) +
  geom_ribbon(data = predData.ngrass.un, aes(x = SD_Nonnative_Poales, ymin = lower, ymax = upper),
              fill = NA, color = "black", linetype = "longdash") +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_continuous("Nonnative Grass Stem Density in Unburned Grasslands") +
  theme_classic()

combined_veg12 <- ggpubr::ggarrange(p.n,
                                  p.non,
                                  p.s,
                                  p.f,
                                  nrow =2, ncol =2)
# Saving plot
ggsave("data/figures/use_covariates/veg_in_unburned.pdf", plot = combined_veg12, width = 10, height = 10)
ggsave( "data/figures/use_covariates/veg_in_unburned.jpeg", plot = combined_veg12, width = 10, height = 10, dpi = 600)

################################################
# working to make linear model graphs of different covariates and time since burn

veg4 <- veg_type %>%
  select(-c(PERCENT_VEG.c, SD_Shrub.c, SD_Forbs.c, SD_Native_Poales.c, SD_Nonnative_Poales.c))
head(veg4)

veg5 <- veg_type %>%
  select(-c(PERCENT_VEG, SD_Shrub, SD_Forbs, SD_Native_Poales, SD_Nonnative_Poales))
head(veg5)

#### NATIVE GRASSES
# model to use for table
lm.nat.c <- lm(SD_Native_Poales.c ~ time_since_burn, data = veg5)
summary(lm.nat.c)
#                         Estimate Std. Error t value Pr(>|t|)  
#(Intercept)                -2.732      1.509  -1.811   0.0722 .
#time_since_burn0-1_years    4.920      2.093   2.351   0.0200 *
#time_since_burn1-2_years    2.085      1.790   1.165   0.2457  
#time_since_burn2-3_years    3.708      1.848   2.007   0.0466 *

# model to use for graphs
lm.nat <- lm(SD_Native_Poales ~ time_since_burn, data = veg4)
summary(lm.nat)

predData.burn <- data.frame(time_since_burn = c('unmanaged', '0-1_years', '1-2_years', '2-3_years'))
pred.burn <- predict(lm.nat, newdata = predData.burn, se.fit = TRUE)
predData.burn$p <- (pred.burn$fit) 
predData.burn$lower <- (pred.burn$fit - pred.burn$se.fit)
predData.burn$upper <- (pred.burn$fit + pred.burn$se.fit)
predData.burn$time_since_burn <- factor(predData.burn$time_since_burn,
                                        levels = c('0-1_years', '1-2_years', '2-3_years', 'unmanaged'),
                                        labels = c("0-1 Years", "1-2 Years", "2-3 Years", "Unburned"))

nat.pl <-ggplot() +
  geom_col(data = predData.burn, aes(x = time_since_burn, y = p, fill = time_since_burn), show.legend = FALSE) +
  geom_errorbar(data = predData.burn, aes(x = time_since_burn, ymin = lower, ymax = upper),
                width = 0.1) +
  scale_fill_manual(values = colors2) +
  scale_y_continuous(limit = c(0,50),"Native Grass Stem Density") +
  scale_x_discrete("Time Since Burn")+
  theme_classic()


#### NONNATIVE GRASSES
# model to use for table
lm.nat.c <- lm(SD_Nonnative_Poales.c ~ time_since_burn, data = veg5)
summary(lm.nat.c)

# model to use for graphs
lm.nat <- lm(SD_Nonnative_Poales ~ time_since_burn, data = veg4)
summary(lm.nat)
#                         Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                39.208      9.838   3.985 0.000104 ***
#time_since_burn0-1_years   -4.901     13.643  -0.359 0.719937    
#time_since_burn1-2_years  -39.208     11.669  -3.360 0.000984 ***
#time_since_burn2-3_years  -39.208     12.049  -3.254 0.001401 ** 

predData.burn <- data.frame(time_since_burn = c('unmanaged', '0-1_years', '1-2_years', '2-3_years'))
pred.burn <- predict(lm.nat, newdata = predData.burn, se.fit = TRUE)
predData.burn$p <- (pred.burn$fit) 
predData.burn$lower <- (pred.burn$fit - pred.burn$se.fit)
predData.burn$upper <- (pred.burn$fit + pred.burn$se.fit)
predData.burn$time_since_burn <- factor(predData.burn$time_since_burn,
                                        levels = c('0-1_years', '1-2_years', '2-3_years', 'unmanaged'),
                                        labels = c("0-1 Years", "1-2 Years", "2-3 Years", "Unburned"))

nonnat.pl <-ggplot() +
  geom_col(data = predData.burn, aes(x = time_since_burn, y = p, fill = time_since_burn), show.legend = FALSE ) +
  geom_errorbar(data = predData.burn, aes(x = time_since_burn, ymin = lower, ymax = upper),
                width = 0.1) +
  scale_fill_manual(values = colors2)+
  scale_y_continuous(limit = c(0,50),"Nonnative Grass Stem Density") +
  scale_x_discrete("Time Since Burn")+
  theme_classic()

#### SHRUB
# model to use for table
lm.nat.c <- lm(SD_Shrub.c ~ time_since_burn, data = veg5)
summary(lm.nat.c)


# model to use for graphs
lm.nat <- lm(SD_Shrub ~ time_since_burn, data = veg4)
summary(lm.nat)
#                         Estimate Std. Error t value Pr(>|t|)
#(Intercept)                0.2083     0.1668   1.249    0.214
#time_since_burn0-1_years   0.0609     0.2313   0.263    0.793
#time_since_burn1-2_years   0.0798     0.1978   0.403    0.687
#time_since_burn2-3_years   0.1458     0.2043   0.714    0.476

predData.burn <- data.frame(time_since_burn = c('unmanaged', '0-1_years', '1-2_years', '2-3_years'))
pred.burn <- predict(lm.nat, newdata = predData.burn, se.fit = TRUE)
predData.burn$p <- (pred.burn$fit) 
predData.burn$lower <- (pred.burn$fit - pred.burn$se.fit)
predData.burn$upper <- (pred.burn$fit + pred.burn$se.fit)
predData.burn$time_since_burn <- factor(predData.burn$time_since_burn,
                                        levels = c('0-1_years', '1-2_years', '2-3_years', 'unmanaged'),
                                        labels = c("0-1 Years", "1-2 Years", "2-3 Years", "Unburned"))

shrub.pl <-ggplot() +
  geom_col(data = predData.burn, aes(x = time_since_burn, y = p, fill = time_since_burn), show.legend = FALSE ) +
  geom_errorbar(data = predData.burn, aes(x = time_since_burn, ymin = lower, ymax = upper),
                width = 0.1) +
  scale_fill_manual(values = colors2) +
  scale_y_continuous(limit = c(0,50),"Shrub Stem Density") +
  scale_x_discrete("Time Since Burn")+
  theme_classic()


#### FORB
# model to use for table
lm.nat.c <- lm(SD_Forbs.c ~ time_since_burn, data = veg5)
summary(lm.nat.c)

# model to use for graphs
lm.nat <- lm(SD_Forbs ~ time_since_burn, data = veg4)
summary(lm.nat)
#                         Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                11.500      2.918   3.941 0.000123 ***
#time_since_burn0-1_years   10.808      4.046   2.671 0.008378 ** 
#time_since_burn1-2_years    3.466      3.461   1.002 0.318124    
#time_since_burn2-3_years   -2.021      3.573  -0.566 0.572551 

predData.burn <- data.frame(time_since_burn = c('unmanaged', '0-1_years', '1-2_years', '2-3_years'))
pred.burn <- predict(lm.nat, newdata = predData.burn, se.fit = TRUE)
predData.burn$p <- (pred.burn$fit) 
predData.burn$lower <- (pred.burn$fit - pred.burn$se.fit)
predData.burn$upper <- (pred.burn$fit + pred.burn$se.fit)
predData.burn$time_since_burn <- factor(predData.burn$time_since_burn,
                                        levels = c('0-1_years', '1-2_years', '2-3_years', 'unmanaged'),
                                        labels = c("0-1 Years", "1-2 Years", "2-3 Years", "Unburned"))

forb.pl <-ggplot() +
  geom_col(data = predData.burn, aes(x = time_since_burn, y = p, fill = time_since_burn), show.legend = FALSE ) +
  geom_errorbar(data = predData.burn, aes(x = time_since_burn, ymin = lower, ymax = upper),
                width = 0.1) +
  scale_fill_manual(values = colors2) +
  scale_y_continuous(limit = c(0,50),"Forb Stem Density") +
  scale_x_discrete("Time Since Burn")+
  theme_classic()


#### PERCENT COVER
# model to use for table
lm.nat.c <- lm(PERCENT_VEG.c ~ time_since_burn, data = veg5)
summary(lm.nat.c)


# model to use for graphs
lm.nat <- lm(PERCENT_VEG ~ time_since_burn, data = veg4)
summary(lm.nat)
#                         Estimate Std. Error t value Pr(>|t|)    
#(Intercept)               58.1250     5.3834  10.797   <2e-16 ***
#time_since_burn0-1_years   4.1827     7.4654   0.560    0.576    
#time_since_burn1-2_years   0.5191     6.3851   0.081    0.935    
#time_since_burn2-3_years  -8.0208     6.5933  -1.217    0.226 

predData.burn <- data.frame(time_since_burn = c('unmanaged', '0-1_years', '1-2_years', '2-3_years'))
pred.burn <- predict(lm.nat, newdata = predData.burn, se.fit = TRUE)
predData.burn$p <- (pred.burn$fit) 
predData.burn$lower <- (pred.burn$fit - pred.burn$se.fit)
predData.burn$upper <- (pred.burn$fit + pred.burn$se.fit)
predData.burn$time_since_burn <- factor(predData.burn$time_since_burn,
                                        levels = c('0-1_years', '1-2_years', '2-3_years', 'unmanaged'),
                                        labels = c("0-1 Years", "1-2 Years", "2-3 Years", "Unburned"))

pc.pl <-ggplot() +
  geom_col(data = predData.burn, aes(x = time_since_burn, y = p, fill = time_since_burn), show.legend = FALSE ) +
  geom_errorbar(data = predData.burn, aes(x = time_since_burn, ymin = lower, ymax = upper),
                width = 0.1) +
  scale_fill_manual(values = colors2) +
  scale_y_continuous("Percent Vegetative Cover") +
  scale_x_discrete("Time Since Burn")+
  theme_classic()


tsb.sd.pcover <- ggpubr::ggarrange(nat.pl,
                            nonnat.pl,
                            forb.pl,
                            shrub.pl,
                            nrow =2, ncol =2)
# Saving plot
ggsave("data/figures/use_covariates/covars_samescale_color_shrub.pdf", plot = tsb.sd.pcover, width = 10, height = 10)
ggsave( "data/figures/use_covariates/covars_samescale_color_shrub.jpeg", plot = tsb.sd.pcover, width = 10, height = 10, dpi = 600)


#######################

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


#######################
## Looking at probability of use for foraging across different time since burn tracts

# GENERAL TYPE: Shrub, Forbs, Native Poales, Nonnative Poales
# continuous variables standardized
fm.t <- glm(used ~ time_since_burn + PERCENT_VEG + SD_Shrub + SD_Forbs + SD_Native_Poales + SD_Nonnative_Poales, family = binomial(link = "logit"), 
            data = veg_type)
summary(fm.t)

veg3 <- veg_type %>%
  select(-c(POINT, PERCENT_VEG.c, SD_Shrub.c, SD_Forbs.c, SD_Native_Poales.c, SD_Nonnative_Poales.c))
head(veg3)

predData.ngrass.un <- data.frame(SD_Native_Poales = mean(veg3$SD_Native_Poales),
                                 PERCENT_VEG = mean(veg3$PERCENT_VEG),
                                 time_since_burn = c("unmanaged","0-1_years", "1-2_years", "2-3_years"),
                                 SD_Shrub = mean(veg3$SD_Shrub),
                                 SD_Forbs = mean(veg3$SD_Forbs),
                                 SD_Nonnative_Poales = mean(veg3$SD_Nonnative_Poales))
head(predData.ngrass.un)
# Switching to the link scale to get confidence intervals on the probability scale
pred.link <- predict(fm.t, newdata = predData.ngrass.un, se.fit = TRUE)
predData.ngrass.un$p <- plogis(pred.link$fit) # back transform to probability scale
predData.ngrass.un$lower <- plogis(pred.link$fit - 1.96 * pred.link$se.fit)
predData.ngrass.un$upper <- plogis(pred.link$fit + 1.96 * pred.link$se.fit)
predData.ngrass.un$time_since_burn <- factor(predData.ngrass.un$time_since_burn,
                                        levels = c('0-1_years', '1-2_years', '2-3_years', 'unmanaged'),
                                        labels = c("0-1 Years", "1-2 Years", "2-3 Years", "Unburned"))
# Plotting the predictions and observations
colors <- brewer.pal(4, "YlGnBu")
colors.v <- c("#7AD151FF", "#22A884FF", "#2A788EFF", "#414487FF")
colors2 <- c("#E3E418FF", "#5DC863FF", "#2C728EFF", "#481F70FF")
show_col(viridis(4))

# Predicting and creating visualizations for unmanaged burn tracts
# Predicted data
predData.ngrass.unman <- data.frame(SD_Native_Poales = mean(veg3$SD_Native_Poales),
                                    PERCENT_VEG = mean(veg3$PERCENT_VEG),
                                    time_since_burn = "unmanaged",
                                    SD_Shrub = mean(veg3$SD_Shrub),
                                    SD_Forbs = mean(veg3$SD_Forbs),
                                    SD_Nonnative_Poales = mean(veg3$SD_Nonnative_Poales))
predData.ngrass.01 <- data.frame(SD_Native_Poales = mean(veg3$SD_Native_Poales),
                                 PERCENT_VEG = mean(veg3$PERCENT_VEG),
                                 time_since_burn = "0-1_years",
                                 SD_Shrub = mean(veg3$SD_Shrub),
                                 SD_Forbs = mean(veg3$SD_Forbs),
                                 SD_Nonnative_Poales = mean(veg3$SD_Nonnative_Poales))
predData.ngrass.12 <- data.frame(SD_Native_Poales = mean(veg3$SD_Native_Poales),
                                 PERCENT_VEG = mean(veg3$PERCENT_VEG),
                                 time_since_burn = "1-2_years",
                                 SD_Shrub = mean(veg3$SD_Shrub),
                                 SD_Forbs = mean(veg3$SD_Forbs),
                                 SD_Nonnative_Poales = mean(veg3$SD_Nonnative_Poales))
predData.ngrass.23 <- data.frame(SD_Native_Poales = mean(veg3$SD_Native_Poales),
                                 PERCENT_VEG = mean(veg3$PERCENT_VEG),
                                 time_since_burn = "2-3_years",
                                 SD_Shrub = mean(veg3$SD_Shrub),
                                 SD_Forbs = mean(veg3$SD_Forbs),
                                 SD_Nonnative_Poales = mean(veg3$SD_Nonnative_Poales))

# unmanaged
pred.link.unman <- predict(fm.t, newdata = predData.ngrass.unman, se.fit = TRUE)
predData.ngrass.unman$p <- plogis(pred.link.unman$fit) # back transform to probability scale
predData.ngrass.unman$lower <- plogis(pred.link.unman$fit - 1.96 * pred.link.unman$se.fit)
predData.ngrass.unman$upper <- plogis(pred.link.unman$fit + 1.96 * pred.link.unman$se.fit)

# 0-1
pred.link.01 <- predict(fm.t, newdata = predData.ngrass.01, se.fit = TRUE)
predData.ngrass.01$p <- plogis(pred.link.01$fit) # back transform to probability scale
predData.ngrass.01$lower <- plogis(pred.link.01$fit - 1.96 * pred.link.01$se.fit)
predData.ngrass.01$upper <- plogis(pred.link.01$fit + 1.96 * pred.link.01$se.fit)

# 1-2
pred.link.12 <- predict(fm.t, newdata = predData.ngrass.12, se.fit = TRUE)
predData.ngrass.12$p <- plogis(pred.link.12$fit) # back transform to probability scale
predData.ngrass.12$lower <- plogis(pred.link.12$fit - 1.96 * pred.link.12$se.fit)
predData.ngrass.12$upper <- plogis(pred.link.12$fit + 1.96 * pred.link.12$se.fit)

# 2-3
pred.link.23 <- predict(fm.t, newdata = predData.ngrass.23, se.fit = TRUE)
predData.ngrass.23$p <- plogis(pred.link.23$fit) # back transform to probability scale
predData.ngrass.23$lower <- plogis(pred.link.23$fit - 1.96 * pred.link.23$se.fit)
predData.ngrass.23$upper <- plogis(pred.link.23$fit + 1.96 * pred.link.23$se.fit)

p.n <- ggplot() +
  geom_col(data = predData.ngrass.unman, aes(x = time_since_burn, y = p, fill = time_since_burn), show.legend = FALSE) +
  geom_col(data = predData.ngrass.01, aes(x = time_since_burn, y = p, fill = time_since_burn), show.legend = FALSE) +
  geom_col(data = predData.ngrass.12, aes(x = time_since_burn, y = p, fill = time_since_burn), show.legend = FALSE) +
  geom_col(data = predData.ngrass.23, aes(x = time_since_burn, y = p, fill = time_since_burn), show.legend = FALSE) +
  geom_errorbar(data = predData.ngrass.unman, aes(x = time_since_burn, ymin = lower, ymax = upper),
                width = 0.1) +
  geom_errorbar(data = predData.ngrass.01, aes(x = time_since_burn, ymin = lower, ymax = upper),
                width = 0.1) +
  geom_errorbar(data = predData.ngrass.12, aes(x = time_since_burn, ymin = lower, ymax = upper),
                width = 0.1) +
  geom_errorbar(data = predData.ngrass.23, aes(x = time_since_burn, ymin = lower, ymax = upper),
                width = 0.1) +
  scale_fill_manual(values = colors2) +
  scale_y_continuous("Probability of Foraging Use") +
  scale_x_discrete("Time Since Burn") +
  theme_classic()



ggsave("data/figures/use_covariates/pr_use_tsb_color.pdf", plot = p.n, width = 5, height = 5)
ggsave( "data/figures/use_covariates/pr_use_tsb_color.jpeg", plot = p.n, width = 5, height = 5, dpi = 600)

###########################################

glm1 <- glm(SD_Native_Poales ~ time_since_burn, data = veg1, family = "poisson")
summary(glm1)
performance::check_overdispersion(glm1)

glm2 <- glmer(SD_Native_Poales ~ time_since_burn + (1|POINT), data = veg1, family = "poisson")
summary(glm2)
performance::check_overdispersion(glm2)

nd2 <- data.frame(time_since_burn=rep(c("unmanaged", "0-1_years", "1-2_years", "2-3_years"), each = 50))
E2 <- predict(lm.nat, newdata=nd2, se.fit = TRUE, interval = "confidence")
predData2 <- data.frame(E2$fit, nd2)

ggplot() +
  geom_point(data = veg1, aes(x = time_since_burn, y = SD_Native_Poales)) +
  geom_line(data = predData2, aes(x = time_since_burn, y = fit)) +
  guides(colour= guide_legend(reverse= T)) +
  scale_y_continuous("Stem Density of Native Grasses") +
  scale_x_discrete("Time since Burn")