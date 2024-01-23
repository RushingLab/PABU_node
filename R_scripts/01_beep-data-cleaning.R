###################################################################################
##
##    Based off of code by Kristina L Paxton
##    
##    January 20 2021
##
##    Code to import raw detections (or beeps) from multiple Sensor Stations
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
##        3. Functions_CTT.Network.R file that contains function to run the script below - saved in the working directory defined below
##
##        4. Raw Beep data files
##            - For this, I am attempting to just use the Node beep data downloaded via the CTT Node SD cards, but most instructions include downloading data using an API (which I did not do here).
##            - When you download data from CTT using API - all csv files of raw beep data will be in a folder for your project and within that folder there will be folders for each Base Station 
##             - and within each Base Station folder there will be a folder named 'raw' that has raw beep data
##                   Ex. "/Users/kpaxton/DataFiles_CTT/Guam Sali/8EEEF7F20F8E/raw/CTT-8EEEF7F20F8E-raw-data.2020-08-26_111951.csv"
##                         --  'Guam Sali' is the folder name of the Project
##                         -- '8EEEF7F20F8E' is the folder name of the Sensor Station - all base stations are 12 characters
##                         -- 'raw' is the folder name with raw beep data 
##                         -- 'CTT-8EEEF7F20F8E-raw-data.2020-08-26_111951.csv' is an example file name within the raw folder
##                         --  Everything prior to Project Name is the path on the computer where the files are found
##            - Verify that the date of the raw data file starts at the 44th character and ends at the 53rd character when counting from the Sensor Station name in the path
##              ***** if this is not true for your data then you will need to change the numbers in Functions_CTT.Network.R line 27 to match your data **************
##     
##
##    Output
##      Beep data will be exported to specified outpath as a .rds file
##          -- Data will be filtered to only include date range specified
##          -- New column 'SensorId' will be added indicating the name of the Sensor Station where the data was gathered
##          -- New column 'Time.local' will be added indicating the time for the local time zone defined by user
##          -- New column 'v' will be added indicating the CTT node version
##     
################################################################################################################################################################################################################



## Installing packages needed:
#install.packages('dplyr')
#install.packages('lubridate')
#install.packages('purrr')
#install.packages('hablar')

# loading required packages
library(dplyr)
library(lubridate)
library(purrr)
library(hablar)

# Reset R's brain - removes all previous objects
rm(list=ls())


############# Combining beep data from each node into one single dataset (with help from Heather Gaya) ###############################

# Set the path to the main folder containing the node subfolders with the beep_0 CSV files
main_folder<- "/data-raw/2023_node_files"

# Get a list of all node subfolders in the main folder
subfolders <- list.dirs(main_folder, full.names = FALSE, recursive = FALSE)

# Create an empty dataframe to store the combined node data after the for loop
combined_data <- data.frame()

# Create an object of combined beep data from all nodes 
# for the pattern = added 0-9 since some beep files are beep_1
my_beeps <- list.files(pattern = "*beep_[0-9].csv$", include.dirs = TRUE, recursive = TRUE)

total_nodes <- length(my_beeps)

# Loop through each node subfolder
for (i in 1:length(my_beeps)) {
  current_beep <- read.csv(my_beeps[i]) #grab first node folder 
  
  #readLines(my_beeps[30]) #code for figuring out where problem characters were located in the beep dataset -- had to do for a few nodes that contained weird characters
  
  current_beep$NodeId <- substr(my_beeps[i], 26,31) # Creating a column in the dataset that includes the name of the node. We included character 26-31 since it was reading the entire file path.
  
  head(current_beep) # Checking that data imported properly.
  
  combined_data <- rbind(combined_data, current_beep) # Combine the data with the existing combined_data blank dataframe.
  }

str(combined_data) # Checking the data structure
unique(combined_data$NodeId) # Checking that all 31ish nodes are included
length(unique(combined_data$NodeId)) # 30 nodes (due to 1 node not having beep data)

# Save the node combined data (new csv with all the beep data from each file and a new column listing node id) to a new dataset or perform further analysis
#write.csv(combined_data,file= 'data-raw/2023_node_combined_data.csv', row.names = FALSE)
head(combined_data)
tail(combined_data)
nrow(combined_data)# 10169876 rows! Exceeded the 100 MB file size limit for GitHub.:-( whomp whomp)

## combined_data = the list of all node data without any formatting or removing of records

####################################################################################################################################

### Cleaning up the node data

# Converted NodeId into a factor
combined_data$NodeId <- as.factor(combined_data$NodeId)

# Converting rssi into a factor
combined_data$rssi <- as.integer(combined_data$rssi) # wouldn't convert due to odd character rssis


# Finding incorrect rssi values and removing those rows from the dataset
unique(combined_data$rssi) #finding the non-integer values in the rssi column

# removing non-integer value rows in the new df combined_data1
values_to_remove <- c("\xf5\003\x99\xfb\xee֕t\xd0o\xaa\030\xb5\xb4\xc6O\x87\xa6\xeeU\x93\xe8\xc2{n",
                      "a\xf0\xf7\xde\006\x9a0\022\x88\xb4\x9f\xd0=\001{\xea̹3\x94\x83\xf6\xb4\023:9z\035؇\x96[\xa3\x80催\002M\xfbQ\x91;\x8b\xfb\xf7\v<\005\xf6\xee%ı\034V\x88\xb2}K\xae\x89=",
                      "" )
combined_data1<- combined_data %>% filter(!(rssi %in% values_to_remove))

unique(combined_data1$rssi) #checking that the non-integers have been removed


###################################################################################################################################

### Code for bringing in other Node-related data before cleaning the all of the node data (combined_data)

# Directory for Output data - Provide/path/where/you/want/output/data/to/be/stored/
outpath <- "PABU_node/data/"


# Bring in functions 
source("R_scripts/00_functions-CTT.Network.R")

# Bring in files with TagId and NodeId information 

## this file includes columns for TagID, StartDate, Species, Name, and Band
tags <- read.csv("data-raw/2023_node_files/Tags_trilateration.csv", header = T) 
str(tags)  # check that data imported properly
head(tags)

## this file includes columns for NodeId, NodeUTMx, and NodeUTMy
nodes <- read.csv("data-raw/2023_node_files/Nodes_Example.csv", header = T)
str(nodes) # check that data imported properly
head(nodes)


########### Run function to get beep data and a count of the detections removed at different steps ############################

## Variables to define for function
## INFILE = Path where folders for multiple sensor stations are found 
## NODE.VERSION = Version of CTT node (needed for some CTT work flows)
## RADIOID = vector of RadioId to include - all values not included in the vector will be removed (e.g. radioid <- c(1,2,3) will include RadioId equal to 1,2, or 3)
## TIMEZONE = Time zone where data was collected, use grep("<insert location name here>", OlsonNames(), value=TRUE) to find valid time zone name
## START and END = Start and end dates of interest based on UTC time (e.g., Guam (+10 UTC))

## Output in R environment
# beep.output - list containing:
# beep_data (data frame with all beep data meeting specified time period and TagIds)
# count.date (number of rows removed that did not fall within specified time period)
# beep.bad.dates (data frame with beep data with bad dates, e.g. 1970)
# count.import (number of rows imported)
# count.NA (number of rows removed that had NA values - e.g. BaseStations have NA for NodeId)
# count.nodes (number of rows removed where the NodeId did not match the NodeId in the provided lookup table)
# count.duplicates (number of rows removed because of duplicate data, e.g. Base Stations with the same data),
# count.ghosts (number of rows removed where the TagId did not match a TagId in the provided lookup table)
# count.RadioId (number of rows removed that did not match the specified RadioId)

## Output saved
# .rds file of the beep_data save in the specified outpath

## We want our beep data to have the following format:
## 2. BeepData.rds: file with the raw RSS values collected by the node network during the time period of the test. The file should be saved in the working directory defined below
##            -- output file from Import_beep.data.Github.R script (see BeepData_Example.rds)
##            -- If using a file created from another source the following columns are needed in the specified format: 
##                -- TagId: Factor identifying the unique code of a tag
##                -- Time.local: POSIXct value in the format: "2021-12-29 09:02:51" that identifies the unique datetime stamp (IN THE LOCAL TIME ZONE OF THE STUDY) when the tag was detected by the specified node
##                    ***** If you don't have a column with the local time, but only UTC time - then change lines 42-44 in Functions_RSS.Based.Localizations.R from 'Time.local' to 'Time' ************
##                    ***** BUT be sure that Dates and Times in TestInfo.csv are also in UTC time and not local time *********
##                -- NodeId: Factor identifying the node in the network that detected the specified tag
##                -- TagRSSI: Integer indicating the RSS value (in dB) of the signal of the specified tag received by the specified node
##
## Diane's data currently has: time, id, rssi, node



# Variables to define for function - replace values below with user specified values -
INFILE <- "/data-raw/2023_node_combined_data.csv"
NODE.VERSION <- 2
RADIOID <- c(1,2,3)
TIMEZONE <- "America/Halifax" 
START <- "2023-05-11"
END <- "2023-11-19"


