###################################################################################
##  
##  Diane Klement
##  March 1 2024
##
##  2-fold purpose: 
##
##  1. Code to clean up location data generated with standard error, covariance, and variance estimates from modifications to the 
##  "05_functions_rss.based.locatizations.R" and "08_trilateration.from.beep.data.R" scripts
##        - Filtered upper and lower CIs for values that reside off of LSSI
##
##  2. Code to use the multivariate normal distribution to generate location estimates based on CTT Node estimated locations
##  and their corresponding error
##        - Generate 10k estimates for each known location
##  
##
##  Input File:
##        - Error.sd.cov.Estimated.Locations90p-90_2023-05-10_2023-11-20.rds
##            - Rds file that contains the estimated locations along with the sd and error estimates associated with locations.
##            - Generated from the 'trilateration' function in "05_functions_rss.based.locations.R" script
##            - Columns:
##                - TagId: CTT tag ID of the bird detected at the location point, factor
##                - Time.group: yyyy-mm-dd hh:mm:ss, POSIXct, time when the location point was detected
##                - Hour: hour when the location point was detected
##                - No.Nodes: number of nodes that provided RSSI values for a location point
##                - UTMx_est: estimated location (mean) for UTMx for a location point
##                - UTMy_est: estimated location (mean) for UTMy for a location point
##                - x.LCI: lower confidence interval value for UTMx for a location point
##                - x.UCI: upper confidence interval value for UTMx for a location point
##                - y.LCI: lower confidence interval value for UTMy for a location point
##                - y.UCI: upper confidence interval value for UTMy for a location point
##                - x.sd: standard deviation for UTMx for a location point
##                - y.sd: standard deviation for UTMy for a location point
##                - x.var: variance for UTMx for a location point
##                - y.var: variance for UTMy for a location point
##                - xy.cov: covariance between UTMx's and UTMy's expected values for a location point
##
##  Output File:
##        - Simulated.Locations500.HomeRange.90p-90.locs.filtered_2023-05-10_2023-11-20.rds
##        - Rds file that contains 500 simulated locations based off of the multivariate normal distribution for each location point from 
##            "Error.sd.cov.Estimated.Locations90p-90_2023-05-10_2023-11-20.rds"
##        - Columns:
##                - UTMx_sim: randomly generated UTMx location generated from the multivariate normal distribution for a location point from the "Error...rds"
##                - UTMy_sim: randomly generated UTMy location generated from the multivariate normal distribution for a location point from the "Error...rds"
##                - TagId: Factor, CTT tag ID of the bird detected at the original location point
##                - Time.group: yyyy-mm-dd hh:mm:ss, POSIXct, time when the original location point was detected
##                - Hour: hour when the original location point was detected
##                - No.Nodes: number of nodes that provided RSSI values for the original location point
##                - UTMx_est: estimated location (mean) for UTMx for the original location point
##                - UTMy_est: estimated location (mean) for UTMy for the original location point
##                - x.LCI: lower confidence interval value for UTMx for the original location point
##                - x.UCI: upper confidence interval value for UTMx for the original location point
##                - y.LCI: lower confidence interval value for UTMy for the original location point
##                - y.UCI: upper confidence interval value for UTMy for the original location point
##
###################################################################################

# Loading packages
library(tidyverse)
library(MASS)

# Setting outpath
outpath <- "data/homerange/simulations/"

# Read in file
locations <- readRDS("data/trilateration/Error.sd.cov.Estimated.Locations90p-90_2023-05-10_2023-11-20.rds")

# Setting the start and end date
START <- "2023-05-10"
END <- "2023-11-20"

###################################################################################

## PART 1

## Filtering file for CI locations located far off of LSSI
# Looking at format
str(locations)
tail(locations)
range(locations$x.LCI) #354078.3 473563.5 (good)
# 95 percent confidence interval:
#472609.2 472630.0

range(locations$x.UCI) #472726.8 592249.8
#95 percent confidence interval:
#473633.2 473653.4

range(locations$y.LCI) #3404634 3458045
#95 percent confidence interval:
#3456822 3456843

range(locations$y.UCI) #3456746 3510096
#95 percent confidence interval:
#3457839 3457859

# locations_filtered
# Removing outliers off the island
locations_filtered <- locations %>%
  filter(y.UCI <= 3500000) %>%
  filter(y.LCI >= 3430000) %>%
  filter(x.UCI <= 500000) %>%
  filter(x.LCI >= 400000)

# locations_filtered1
# Removing outliers based on being located on island
locations_filtered1 <- locations %>%
  filter(y.UCI <= 3470000) %>%
  filter(y.LCI >= 3450000) %>%
  filter(x.UCI <= 475000) %>%
  filter(x.LCI >= 467000)

#locations_filtered = locations with the way off outliers that were designated off the island
#locations_filtered1 = locations only including locs that could possibly be on LSSI
###################################################################################

# PART 2

# Creating multivariate normal distribution for each location point
# To be used to create home ranges

# Create a dataframe for output estimates
estimated.location_results <- data.frame(UTMx_sim=numeric(), UTMy_sim=numeric(), TagId=factor(), Time.group = POSIXct(), Hour = numeric(), 
                                         No.nodes = numeric(), UTMx_est = numeric(), UTMy_est = numeric(), x.LCI = numeric(), x.UCI = numeric(), y.LCI = numeric(), y.UCI = numeric())

# Create for loop to loop through each location point and generate 10,000 random locations based on the error generated from the nls() model
for (i in 1:nrow(locations_filtered1)){
    
    set.seed(98989) # replicability
    sample_size <- 500 # sample each point 500 times 
    
    sample_meanvector <- c(locations_filtered1$UTMx_est[i], locations_filtered1$UTMy_est[i]) #estimated UTMx and UTMy locations per each location                                 

    #figuring out the covariance -- degree to which 2 variables are linearly associated
    sample_covariance_matrix <- matrix(c(locations_filtered1$x.var[i], locations_filtered1$xy.cov[i], locations_filtered1$xy.cov[i], locations_filtered1$y.var[i]), 
                                        ncol = 2) 
    
    # create bivariate normal distribution for each location point
    sample_distribution <- mvrnorm(n = sample_size, 
                                  mu = sample_meanvector,  
                                  Sigma = sample_covariance_matrix) 
    
    # changing to a dataframe and renaming columns
    sample_distribution1 <- as.data.frame(sample_distribution)
    sample_distribution1 <- sample_distribution1 %>%
                              rename(UTMx_sim = V1, UTMy_sim = V2)
    
    # adding metadata (TagId, Time.group, Hour) back into the dataset
    # simulated location of the point data
    sample_distribution2 <- sample_distribution1 %>%
                              mutate(TagId = locations_filtered1$TagId[i], 
                                     Time.group = locations_filtered1$Time.group[i],
                                     Hour = locations_filtered1$Hour[i],
                                     No.Nodes = locations_filtered1$No.Nodes[i],
                                     UTMx_est = locations_filtered1$UTMx_est[i],
                                     UTMy_est = locations_filtered1$UTMy_est[i],
                                     x.LCI = locations_filtered1$x.LCI[i],
                                     x.UCI = locations_filtered1$x.UCI[i],
                                     y.LCI = locations_filtered1$y.LCI[i],
                                     y.UCI = locations_filtered1$y.UCI[i]
                                     ) 

    # print top of distribution 
    head(sample_distribution2)
    
    # Populate dataframe with results
    estimated.location_results <- rbind(estimated.location_results, sample_distribution2)
}


# save estimated locations
saveRDS(estimated.location_results, paste0(outpath, "Simulated.Locations500.HomeRange.90p-90.locs.filtered1_", START, "_", END, ".rds"))