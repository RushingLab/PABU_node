###################################################################################
## Diane Klement
## May 8 2024
##
## Code to clean and combine used RSF locations ("data/trilateration/Error.sd.cov.Estimated.Locations90p-90_2023-05-10_2023-11-20.rds")
## with available RSF locations ("data/clark_hr/results/xy/")
##
## Output:
##    - data/rsf/used_avail.rds
##        - Combined and cleaned used and available locations of PABU for further use
##
###################################################################################

# Loading necessary packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(sp)
library(adehabitatLT)
library(tidyverse)
library(hablar)
library(rlang)
library(ggmap)
library(MASS)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(raster)
library(data.table)
library(Rcpp)
library(terra)


################################################################
# Read in used location data
locations <- readRDS("data/trilateration/Error.sd.cov.Estimated.Locations90p-90_2023-05-10_2023-11-20.rds")

# Removing outliers based on being located on island
locations_filtered1 <- locations %>%
  filter(y.UCI <= 3470000) %>%
  filter(y.LCI >= 3450000) %>%
  filter(x.UCI <= 475000) %>%
  filter(x.LCI >= 467000)

locations_tags <- locations_filtered1 %>%
  group_by(TagId) %>%
  summarize(count=n()) %>% #counting up the number of records per TagId
  filter(count > 45, TagId != "6166071E") %>% #keeping tags where beeps >45 and removing the test tag
  pull(TagId) #subsetting tagids with counts greater than 45
droplevels(locations_tags)

# Filtering the original location data with counts > 45
subset_locations <- locations_filtered1 %>%
  filter(TagId %in% locations_tags)
locations_subset <- droplevels(subset_locations) #dropping low tag levels
str(locations_subset)

# Filtering out locations where the standard deviation is above the 90% quantile
locs_fil <- locations_subset %>%
  filter(x.sd <= quantile(locations$x.sd, probs = 0.9)) %>%
  filter(y.sd <= quantile(locations$y.sd, probs = 0.9))

used <- locs_fil #used
used$TagId <- as.factor(used$TagId)
used$Used <- rep(1, nrow(used))
#########################################################################

# Combining and reading in available simulated locations
# Create an empty dataframe to store the combined node data after the for loop
combined_data <- data.frame()

# Create an object of combined beep data from all nodes 
# for the pattern = added 0-9 since some beep files are beep_1
directory <- "data/clark_hr/results/xy/"
my_files <- list.files(directory, pattern = "*_xy.rds", full.names = TRUE, include.dirs = TRUE, recursive = TRUE)

total_birds <- length(my_files) #14

# Loop through each node subfolder
for (i in 1:length(my_files)) {
  
  current_bird <- readRDS(my_files[i]) #grab first node folder 
  colnames(current_bird) <- c("UTMx", "UTMy")
  TagId <- substr(my_files[i], 26,33) # Creating a column in the dataset that includes the name of the node. We included character 1-8 since it was reading the entire tag id
  bird_comb <- cbind(current_bird, TagId)
  head(bird_comb)
  birds <- as.data.frame(bird_comb)# Checking that data imported properly.
  combined_data <- rbind(combined_data, birds) # Combine the data with the existing combined_data blank dataframe.
}

# Checking the data structure
str(combined_data) 
length(unique(combined_data$TagId)) # 14 birds
nrow(combined_data)
avail <- combined_data #available
avail$TagId <- as.factor(avail$TagId)
avail$Used <- rep(0, nrow(avail))
##########################################################################
# Subsampling used points to only a max of 500 pts per bird

# Subset used to only necessary columns
used1 <- used %>%
            rename(UTMx = UTMx_est) %>%
            rename(UTMy = UTMy_est) %>%
            dplyr::select("UTMx", "UTMy", "TagId", "Used") %>%
            filter(TagId != "2D55194C")
droplevels(used1)

# Set seed for reducibility
set.seed(444)

# Function to randomly select rows
random_sample <- function(df, n) {
  if (nrow(df) > n) {
    df <- df %>% slice_sample(n = n)
  }
  return(df)
}

# Applying the function to each group
sampled_used_500 <- used1 %>%
                      group_by(TagId) %>%
                      group_modify(~random_sample(.x, 500)) %>%
                      ungroup()

# Converting back to a dataframe
used_500 <- as.data.frame(sampled_used_500)

######################################################################

# Combining used and available data into one df for further use

# Combined df
used_avail_comb <- rbind(used_500, avail) 
used_avail_comb$UTMx<- as.numeric(used_avail_comb$UTMx) 
used_avail_comb$UTMy<- as.numeric(used_avail_comb$UTMy) 
str(used_avail_comb)

saveRDS(used_avail_comb, "data/rsf/used_avail.rds") # changed name for ease

######################################################################
# calculating mean/sed for paper

used_avail <- readRDS("data/rsf/used_avail.rds")

use <- used_avail %>%
          filter(Used == 1) %>%
          group_by(TagId) %>%
          summarise(nrows = n())
mean_use <- mean(use$nrows) #318.2143
sd_use <- sd(use$nrows) #199.1822

av <- used_avail %>%
  filter(Used == 0) %>%
  group_by(TagId) %>%
  summarise(nrows = n())
mean_av <- mean(av$nrows) #318.2143
sd_av <- sd(av$nrows) #199.1822