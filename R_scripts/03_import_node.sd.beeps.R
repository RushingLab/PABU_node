###################################################################################
## Diane Klement
## January 22 2024
##
## Code to collect/download raw detections (or beeps) from Node microSD cards into a file
##                            and clean those files
##
###################################################################################
###################################################################################
##   Cleaning up the node data to match the format of Paxton's BeepData_Example
##    
##        > str(BeepData_Example)
##        'data.frame':	477 obs. of  12 variables:
##        $ SensorId  : Factor w/ 3 levels "8EEEF7F20F8E",..: 2 2 2 2 2 2 2 2 2 2 ...
##        $ Time      : POSIXct, format: "2021-03-05 03:16:01" "2021-03-05 03:16:04" "2021-03-05 03:16:07" "2021-03-05 03:16:10" ...
##        $ RadioId   : int  1 1 1 1 1 1 1 1 1 1 ...
##        $ TagId     : Factor w/ 2 levels "662D3455","662D4C2A": 1 1 1 1 1 1 1 1 1 1 ...
##        $ TagRSSI   : int  -91 -91 -91 -90 -91 -90 -90 -90 -90 -89 ...
##        $ NodeId    : Factor w/ 11 levels "325AB4","325C3E",..: 2 2 2 2 2 2 2 2 2 2 ...
##        $ Validated : int  0 0 0 0 0 0 0 0 0 0 ...
##        $ Time.local: POSIXct, format: "2021-03-05 13:16:01" "2021-03-05 13:16:04" "2021-03-05 13:16:07" "2021-03-05 13:16:10" ...
##        $ v         : num  2 2 2 2 2 2 2 2 2 2 ...
##        $ Date      : Date, format: "2021-03-05" "2021-03-05" "2021-03-05" "2021-03-05" ...
##        $ Hour      : int  13 13 13 13 13 13 13 13 13 13 ...
##        $ Min       : int  16 16 16 16 16 16 16 16 16 16 ...
##
##
##    Files Needed
##        1. Nodes.csv file with a list of all nodes in your network that is saved in the working directory defined below
##            - Column names needed: NodeId
##            - Other columns can also be in the file for your reference
##            - Ensure all letters in NodeId are capitalized 
##            - If Node names are being converted to scientific notation in Excel open the file with a text editor (e.g. BBedit) to change names to the correct format and save the file
##
##        2. Tags.csv file with a list of all active tags for the specified time period that is saved in the working directory defined below
##            -- Column names needed: TagId
##            -- other columns can also be in the file for your reference
##            -- Ensure all letters in TagId are capitalized
##
##        3. Folder including folders of each node's microSD card downloaded data
##            -- 2023_node_files: folder name, includes all node downloads
##                -- 325AB4: folder name, named for the name of each node (# of folders = # of nodes)
##                    -- beep_0.csv: beep_0 or beep_1, includes all beep/detection data for the period deployed
##                    -- gps.csv: includes gps locations of node taken while deployed
##                    -- test.txt: unsure of utility regarding these files
##
#####################################################################################
# loading required packages
library(dplyr)
library(lubridate)
library(purrr)
library(hablar)

# Reset R's brain - removes all previous objects
rm(list=ls())

#####################################################################################

####### Combining beep data from each node microSD into one single dataset ###########


# Create an empty dataframe to store the combined node data after the for loop
combined_data <- data.frame()

# Create an object of combined beep data from all nodes 
# for the pattern = added 0-9 since some beep files are beep_1
directory <- "data-raw/2023_node_files/"
my_beeps <- list.files(directory, pattern = "*beep_[0-9].csv$", full.names = TRUE, include.dirs = TRUE, recursive = TRUE)

total_nodes <- length(my_beeps) #31

# Loop through each node subfolder
for (i in 1:length(my_beeps)) {
  
  current_beep <- read.csv(my_beeps[i]) #grab first node folder 
  
  #readLines(my_beeps[30]) #code for figuring out where problem characters were located in the beep dataset -- had to do for a few nodes that contained weird characters
  
  current_beep$NodeId <- substr(my_beeps[i], 26,31) # Creating a column in the dataset that includes the name of the node. We included character 26-31 since it was reading the entire file path.
  
  head(current_beep) # Checking that data imported properly.
  
  combined_data <- rbind(combined_data, current_beep) # Combine the data with the existing combined_data blank dataframe.
  }

str(combined_data) # Checking the data structure
length(unique(combined_data$NodeId)) # 30 nodes (due to 1 node not having beep data)

# Save the node combined data (new csv with all the beep data from each file and a new column listing node id) to a new dataset or perform further analysis
head(combined_data)
nrow(combined_data)# 10169876 rows! Exceeded the 100 MB file size limit for GitHub.:-( whomp whomp)

## combined_data = the list of all node data without any formatting or removing of records

###########################################################################################

####### Cleaning and formatting the node microSD beep data to match Paxton's format #######


# Converted NodeId into a factor
combined_data$NodeId <- as.factor(combined_data$NodeId)

# Converting rssi into a factor
combined_data$rssi <- as.integer(combined_data$rssi) # wouldn't convert due to odd character rssis

# Finding incorrect rssi values and removing those rows from the dataset
unique(combined_data$rssi) #finding the non-integer values in the rssi column

# Removing non-integer value rows in the new df combined_data1
values_to_remove <- c("\xf5\003\x99\xfb\xee֕t\xd0o\xaa\030\xb5\xb4\xc6O\x87\xa6\xeeU\x93\xe8\xc2{n",
                      "a\xf0\xf7\xde\006\x9a0\022\x88\xb4\x9f\xd0=\001{\xea̹3\x94\x83\xf6\xb4\023:9z\035؇\x96[\xa3\x80催\002M\xfbQ\x91;\x8b\xfb\xf7\v<\005\xf6\xee%ı\034V\x88\xb2}K\xae\x89=",
                      "" )
combined_data1<- combined_data %>% filter(!(rssi %in% values_to_remove))

unique(combined_data1$rssi) #checking that the non-integers have been removed
combined_data1$rssi <- as.integer(combined_data1$rssi)
str(combined_data1) #10169864 obs.
unique(combined_data1$id) 

combined_data$id <- as.factor(combined_data$id) # making tag id a factor
unique(combined_data1$id) 

# changing the column names to fit Paxton's convention
colnames(combined_data1) <- c("Time", "TagId", "TagRSSI", "NodeId")
colnames(combined_data1) # checking that column names have changed

# Changing the Time column to a POSIXct format from this format "%Y-%m-%dT%H:%M:%SZ" in UTC time
combined_data1$Time <- as.POSIXct(combined_data1$Time, format = "%Y-%m-%dT%H:%M:%SZ", tz= "UTC")
str(combined_data1)

# Make Time the local timezone column in df
combined_data1$Time.local <- as.POSIXct(combined_data1$Time, tz="America/New_York")
str(combined_data1)

# Checking that NodeId letters are capitalized
unique(combined_data1$NodeId) 

# Creating a Date column
combined_data1$Date <- as.Date(combined_data1$Time.local, tz="America/New_York" )

# Creating a hour and minute column
combined_data1$Hour <- hour(combined_data1$Time.local)
combined_data1$Min <- minute(combined_data1$Time.local)
str(combined_data1)

# Creating a Sensor Station Id column
combined_data1$SensorId <- 'V30B0154CD2D'
combined_data1$SensorId <-factor(combined_data1$SensorId) # making sensorid a factor
str(combined_data1) # checking the structure

# Adding a column for RadioId and converting TagId into a factor
combined_data1$RadioId <- as.integer(3) # adding the radio id as a column
combined_data1$TagId <- as.factor(combined_data1$TagId) # converting tags into factors

# Reorganizing the strucutre of columns to match the api beep data's structure
beep_sd <- combined_data1[, c("SensorId", "Time","RadioId", "TagId", "TagRSSI", "NodeId", "Time.local", "Date", "Hour", "Min")]
str(beep_sd) 
#10169864 obs.

###################################################################################################################################

### Code for bringing in other Node-related data before cleaning the all of the node data (combined_data)


# Bring in files with TagId and NodeId information to use to filter out non included tags and nodes

## this file includes columns for TagID, StartDate, Species, Name, and Band
tags <- read.csv("data-raw/2023_node_files/Tags_trilateration.csv", header = T) 
str(tags)  # check that data imported properly
colnames(tags)[1] <- "TagId" # changing the incorrectly typed (Id vs ID) column name to match the beep_sd data structure

## this file includes columns for NodeId, NodeUTMx, and NodeUTMy
nodes <- read.csv("data-raw/2023_node_files/Nodes_Example.csv", header = T)
str(nodes) # check that data imported properly


# Keeping only rows with TagId values from the Tag lookup table
beep_sd_filter <- droplevels(beep_sd[beep_sd$TagId %in% tags$TagId, ]) # removing tags not included in our Tags$TagId look-up column
str(beep_sd_filter) # checking that records were removed and that the unused tag levels were removed
#5620799 obs

# Keep only rows with NodeId values in lookup table 
beep_sd_filter <- droplevels(beep_sd_filter[beep_sd_filter$NodeId %in% nodes$NodeId, ]) 
str(beep_sd_filter) #5620799 obs

# Start and end time of nodes deployed
START <- "2023-05-10"
END <- "2023-11-20"

#Define start and end date to select from files
start_range <- as.Date(START, format = "%Y-%m-%d")
end_range <- as.Date(END,  format = "%Y-%m-%d")

# Filter local time so only requested dates are outputted
# to include times past 00:00:00 needed to have end date = 1 day past end date specified
beep_sd_time <- beep_sd_filter%>%
  dplyr::filter(Time.local >= start_range & Time.local <= seq(end_range + 1, end_range + 1, by = "1 day"))
str(beep_sd_time) #5456587
nrow(beep_sd_filter) - nrow(beep_sd_time) #164212 records removed due to dates outside range


# Remove rows with NA values
# example for a list - beep_data <- lapply(beep_data,function(x) x[complete.cases(x),])
beep_sd_clean <- beep_sd_time[complete.cases(beep_sd_time),]
nrow(beep_sd_time) - nrow(beep_sd_clean) # 0 records removed
#beep_sd_clean = clean sd card dataset
#beep_sd_clean has 5456587 rows

# Saving beep_sd_clean as an RDS
saveRDS(beep_sd_clean, file = "data/beep_sd.rds")
