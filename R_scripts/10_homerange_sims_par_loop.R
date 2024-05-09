# Loading packages
library(MASS)
library(ctmm)
library(tidyverse)

# Read in file
locations <- readRDS("data/trilateration/Error.sd.cov.Estimated.Locations90p-90_2023-05-10_2023-11-20.rds")

##########################################################################
## CLEANING THE DATA FOR FURTHER USAGE
###################################

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

##########################################################################
bird <- locations%>%
  filter(TagId == "52787819")
range(bird$Time.group)

tagged_bird <- read.csv("data-raw/birds_tagged.csv")
tagged_bird$Start_fil <- as.POSIXct(tagged_bird$Start_fil, format="%Y%m%d %H%M%S")
tagged_bird1<- tagged_bird%>%
                  as.POSIXct(, format="%Y%m%d %H%M%S"))
##########################################################################
## PREPARING DATA FOR CTMM PACKAGE
###################################

# To use the ctmm package to make aKDEs will have to format data according to MoveBank conventions
locs1 <- locs_fil %>%
  rename(ID = TagId) %>% # changing the TagId name to ID
  rename(timestamp = Time.group) %>% # changing the Time.group column name to timestamp
  rename(utm.easting = UTMx_est) %>% # changing simulated UTMx column name to utm.easting
  rename(utm.northing = UTMy_est) %>% # changing simulated UTMy column name to utm.northing
  mutate(UTM.zone = "17 +north") # creating a column for the UTM zone

# Create column 't' (time in seconds from 1970), this is the time interval in seconds/minutes/hours between detection pings
locs1[['t']] <- time_length(interval(ymd("1970-01-01"), locs1$timestamp), "hour")

# Removing the seconds from the POSIXct time format
locs1[['timestamp']] <- as.POSIXct(locs1[['timestamp']],
                                   format = "%m/%d/%Y %H:%M")

# Checking the data structure
str(locs1)


##########################################################################
## ADDING doParallel TO RUN ON CORES
###################################
# Load required packages
library(foreach)
library(doParallel)  # You can choose other parallel backend if you prefer

cl <- makeCluster(10)
registerDoParallel(cl)

# Function to convert simulated locations to telemetry object within nested for loop
convert_to_telemetry <- function(xy_sim, loc_temp) {
  
  # Create a data frame with simulated coordinates and original time information
  # Need to be a dataframe with timestamp, utm.easting, utm.northing, t, ID, and UTM.zone columns for data to be read as a telemetry object
  telemetry_data <- data.frame(
    timestamp = loc_temp$timestamp,
    utm.easting = xy_sim[, 1],
    utm.northing = xy_sim[, 2],
    t = loc_temp$t,
    ID = loc_temp$ID, 
    UTM.zone = "17 +north"
  )
  
  # Convert the dataframe to a telemetry object
  telemetry_obj <- as.telemetry(telemetry_data, 
                                timezone = 'UTC')
  
  return(telemetry_obj)
}


# Setting up data to feed through for loop
tagID <- unique(locs1$ID) # vector of individual birds
nInd <- length(tagID) # number of individual birds
sample_size <- 500 # small for testing; number of simulations

maxDet <- 500 # if individuals have more than this many detections, estimate HR from a sample of this many detections for each simulation (to reduce really computational steps)

# hr50 <- hr90 <- matrix(NA, nrow = nInd, ncol = sample_size)


# Looping through every individual (i) using %dopar%
result_list <- foreach(i = 1:nInd, .packages= c("tidyverse", "ctmm", "MASS"), .combine = "list") %dopar% {
  loc_temp <- dplyr::filter(locs1, ID == tagID[i])
  nDet <- nrow(loc_temp)
  
  hr50 <- hr90 <- numeric(sample_size)  # Initialize hr50 and hr90 vectors
  
  # Looping through every simulation (j)
  for (j in 1:sample_size) {
    if(nDet > maxDet){
      loc_temp <- loc_temp[sample(1:nDet, size = maxDet, replace = F),]
      nDet <- maxDet
    }
    xy_sim <- matrix(NA, nrow = nDet, ncol = 2)
    
    # Looping through every detection (k)
    for (k in 1:nDet) {
      sample_meanvector <- c(loc_temp$utm.easting[k], loc_temp$utm.northing[k])
      
      sample_covariance_matrix <- matrix(c(loc_temp$x.var[k], 
                                           loc_temp$xy.cov[k], 
                                           loc_temp$xy.cov[k], 
                                           loc_temp$y.var[k]), 
                                         ncol = 2)
      
      xy_sim[k,] <- MASS::mvrnorm(n = 1, 
                                  mu = sample_meanvector, 
                                  Sigma = sample_covariance_matrix)
    }
    
    # Convert simulated locations to telemetry object
    telemetry_obj <- convert_to_telemetry(xy_sim, loc_temp)
    
    # Preparing simulated locations to calculate wAKDE
    GUESS <- ctmm.guess(telemetry_obj, interactive = FALSE)
    M.OUF <- ctmm.select(telemetry_obj, GUESS)
    
    # Calculating wAKDE
    wAKDE <- akde(telemetry_obj, M.OUF, weights=TRUE)
    
    # Calculate home range metrics from the wAKDE
    sum_out_50 <- summary(wAKDE, level.UD = 0.50)
    hr50[j] <- sum_out_50$CI[1,2]
    
    sum_out_90 <- summary(wAKDE, level.UD = 0.90)
    hr90[j] <- sum_out_90$CI[1,2]
  }
  
  # Return hr50 and hr90 vectors
  hr <- list(hr50 = hr50, hr90 = hr90)
  saveRDS(object = hr, file = paste0("results/hr/", tagID[i], "_hr.rds"))
  saveRDS(object = xy_sim, file = paste0("results/xy/", tagID[i], "_xy.rds"))
}

# Stop parallel backend
stopCluster(cl)

# Combine results
library(dplyr)
library(ggplot2)
library(tidyr)

# hr50_list <- lapply(result_list, function(x) x$hr50)
# hr90_list <- lapply(result_list, function(x) x$hr90)
hr_df <- data.frame(matrix(nrow = 0, ncol = 3))
names(hr_df) <- c("ID", "hr", "est")
hr_df$ID <- as.factor(hr_df$ID)
hr_df$hr <- as.factor(hr_df$hr)
hr_df$est <- as.numeric(hr_df$est)

f <- list.files("data/clark_hr/results/hr")

for(i in 1:length(f)){
  hr <- readRDS(paste0("data/clark_hr/results/hr/", f[i]))
  temp_df <- data.frame(ID = substr(f[i], start = 1, stop = 8),
                        hr = rep(c("50", "90"), each = length(hr$hr50)),
                        est = c(hr$hr50, hr$hr90))
  hr_df <- bind_rows(hr_df, temp_df)
}

saveRDS(hr_df, "results/hr_all.rds")

###############################################################
# Where to start from since Clark did the above

hr_df <- readRDS("data/clark_hr/results/hr_all.rds")
hr_summ <- hr_df %>% group_by(ID, hr) %>% summarise(med = quantile(est, probs = 0.5), 
                                                    lci = quantile(est, probs = 0.025),
                                                    uci = quantile(est, probs = 0.975))

ggplot() +
  geom_point(data = hr_df, aes(x = est, y = ID, color = hr), alpha = 0.1, size = 1) +
  geom_boxplot(data = hr_df, aes(x = est, y = ID, color = hr)) +
  theme_classic() +
  scale_x_continuous("Home range size", limits = c(0, 30))
