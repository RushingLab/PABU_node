
# load library MASS 
library(MASS) 

# Reading in locations
locations_filtered <- readRDS("data/trilateration/locations_filtered.rds")

# set seed and create data vectors 
set.seed(98989) # replicability
sample_size <- 10000 # sample each point 10k times                                        
sample_meanvector <- c(UTMx_est, UTMy_est)   #estimated UTMx and UTMy locations per each location                                 

#figuring out the covariance -- degree to which 2 variables are linearly associated
# can also use cov() to create a covariance matrix from given data
sample_covariance_matrix <- matrix(c(10, 5, 2, 9), 
                                   ncol = 2) 


# create bivariate normal distribution for each location point
sample_distribution <- mvrnorm(n = sample_size, 
                               mu = sample_meanvector,  
                               Sigma = sample_covariance_matrix) 

# print top of distribution 
head(sample_distribution)
















## Reading trilateration location estimates
locations <- readRDS("data/trilateration/Estimated.Locations-feb20-90p-90_2023-05-10_2023-11-20.rds") #53565 obs. of  10 variables
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

# Removing outliers off the island
locations_filtered <- locations %>%
  filter(y.UCI <= 3500000) %>%
  filter(y.LCI >= 3430000) %>%
  filter(x.UCI <= 500000) %>%
  filter(x.LCI >= 400000)

saveRDS(locations_filtered, "data/trilateration/locations_filtered.rds")