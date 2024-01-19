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
##            - For this, I am attempting to just use the Node beep data downloaded via the CTT Node SD cards.
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



# installing packages needed
#install.packages('dplyr')
#install.packages('lubridate')
#install.packages('purrr')

# loading required packages
library(dplyr)
library(lubridate)
library(purrr)

# Reset R's brain - removes all previous objects
rm(list=ls())


############# Combining beep data from each node into one single dataset (with help from Heather Gaya) ###############################

# Set the path to the main folder containing the node subfolders with the beep_0 CSV files
main_folder<- "C:/Users/dklem/Documents/Git_Rstudio/PABU_node/data-raw/2023_node_files"

# Get a list of all node subfolders in the main folder
subfolders <- list.dirs(main_folder, full.names = FALSE, recursive = FALSE)

# Create an empty dataframe to store the combined node data after the for loop
combined_data <- data.frame()

# Create an object of 
# for the pattern = added 0-9 since some beep files are beep_1
my_beeps <- list.files(pattern = "*beep_[0-9].csv$", include.dirs = TRUE, recursive = TRUE)

# Loop through each node subfolder
for (i in 1:length(my_beeps)) {
  my_current_beep <- read.csv(my_beeps[i]) #grab first node folder 
 
  #readLines(my_beeps[30]) #code for figuring out where problem characters were located in the beep dataset 
  
  my_current_beep$node <- substr(my_beeps[i], 1,6) # Creating a column in the dataset that includes the name of the node. We included character 1-6 since it was reading the entire file path.
  
  head(my_current_beep)
  
  combined_node_data <- rbind(combined_data, my_current_beep) # Combine the data with the existing combined_data blank dataframe.
  }

str(combined_node_data) # Checking the data structure

# Save the node combined data (new csv with all the beep data from each file and a new column listing node id) to a new dataset or perform further analysis
write.csv(combined_node_data, "C:/Users/dklem/Documents/Git_Rstudio/PABU_node/data-raw/2023_node_combined_data.csv", row.names = FALSE)


# Print the combined dataset
print(combined_data)

###################################################################################################################################

### Set by User
# Working Directory - Provide/path/on/your/computer/where/master/csv/file/of/tags/and/nodes/is/found/and/where/Functions_CTT.Network.R/is/located
working.directory <- "PABU_node/data-raw/2023_node_files"

# Directory for Output data - Provide/path/where/you/want/output/data/to/be/stored/
outpath <- "PABU_node/data/"


# Bring in functions 
setwd(working.directory)
source("00_functions_CTT.Network.R")

# Bring in files with TagId and NodeId information 

## this file includes columns for TagID, StartDate, Species, Name, and Band
tags <- read.csv("C:/Users/dklem/Documents/Git_Rstudio/PABU_node/data-raw/2023_node_files/Tags_trilateration.csv", header = T) 
str(tags)  # check that data imported properly
head(tags)

## this file includes columns for NodeId, NodeUTMx, and NodeUTMy
nodes <- read.csv("C:/Users/dklem/Documents/Git_Rstudio/PABU_node/data-raw/2023_node_files/Nodes_Example.csv", header = T)
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



# Variables to define for function - replace values below with user specified values -
INFILE <- "/Users/kpaxton/DataFiles_CTT/Guam Sali"
NODE.VERSION <- 2
RADIOID <- c(1,2)
TIMEZONE <- "Pacific/Guam" 
START <- "2023-05-11"
END <- "2021-06-02"

grep("<UTC-4>", OlsonNames(), value=TRUE)

# Function to import raw beep data
beep.output <- import.beeps(INFILE, NODE.VERSION, RADIOID, TIMEZONE, START, END)


# Isolate raw detection data to be used in next steps
beep_data <- beep.output[[1]]

# Examine beep data that had bad dates (e.g., 1970, 2024, etc.) to see help diagnose potential nodes that are malfunctioning
beep_bad <- beep.output[[2]]