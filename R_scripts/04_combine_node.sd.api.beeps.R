###################################################################################
## Diane Klement
## January 30 2024
##
## Code for combining CTT Node beep data retrieved from physical Node microSD cards and the CTT cellular API
##
###################################################################################
##
## R Script for combining CTT Node beep data retrieved from physical Node microSD cards and the CTT cellular API
##
##    Files Needed:
##      To use this script, you will need two rds files created using previous scripts:
##        - beep_api : this rds will contain all beep data that was cleaned and downloaded using using "02_import_node.api.beeps.R" and "02_functions_node.api.beeps.R" 
##        - beep_sd : this rds will contain all beep data that was cleaned and downloaded using using "03_import_node.sd.beeps.R"
##
##    Workflow:
##        First step: Data preparation -- clean up node sd and api beeps so that they match the same format
##        Second step: Data combination -- combine node sd and api data into one rds file
##
##    Output:
##        - BeepMerge.rds: merged beep data from api and node sd cards as a rds (R Data Serialization) file
##        - BeepMerge.csv: merged beep data from api and node sd cards as a csv  (comma-separated values) file
##            - Columns
##                - SensorId: Factor identifying the SensorStation name for the beep detection
##                - Time: POSIXct value in the format: "2021-12-29 09:02:51" that identifies the unique datetime stamp in UTC
##                - RadioId: Integer value of the RadioId from the tower (ie. 1, 2, or 3)
##                - TagId: Factor identifying the unique code of a tag
##                - TagRSSI: Integer indicating the RSS value (in dB) of the signal of the specified tag received by the specified node
##                - NodeId: Factor identifying the node in the network that detected the specified tag
##                - Time.local: POSIXct value in the format: "2021-12-29 09:02:51" that identifies the unique datetime stamp (IN THE LOCAL TIME ZONE OF THE STUDY) when the tag was detected by the specified node
##                - Date: Date when the specified beep was received
##                - Hour: Hour when the specified beep was received
##                - Min: Minute the specified beep was received
##
###################################################################################

# uploading packages needed
library(lubridate)
library(dplyr)
library(arsenal)


# Reset R's brain - removes all previous objects
rm(list=ls())

###################################################################################
#Bringing in and cleaning up the node Api beep data output from "Import_beep.data.Github.R"


# Bring in RDS of node API data as generated from "Import_beep.data.Github.R"
beep_api <- readRDS("data/beeps/beep_api.rds")

# Creating a Date column
beep_api$Date <- as.Date(beep_api$Time.local, tz="America/New_York" ) # for New York timezone

# Creating a hour and minute column
beep_api$Hour <- hour(beep_api$Time.local)
beep_api$Min <- minute(beep_api$Time.local)
str(beep_api) #3312579 obs.

# Removing Validated and V columns from beep_api df
beep_api_clean <- subset(beep_api, select = -c(Validated, v))
str(beep_api_clean) 
nrow(beep_api_clean) # 3312579
#beep_api_clean = all clean api beep data that we will use for next steps

#################################################################################

# Bringing in the node microSD card beep data output from "04_import_node.sd.beeps.R"

beep_sd_clean <- readRDS(file = "data/beeps/beep_sd.rds") #5531354

#################################################################################

# Combining SD card and API databases then cleaning them

## We want to combine the beep_api_clean and beep_sd_clean into a single database then remove duplicate records.
print(nrow(beep_api_clean) + nrow(beep_sd_clean)) #8843933 rows should be in combined database

BeepMerge <- rbind(beep_api_clean, beep_sd_clean)
str(BeepMerge)
nrow(BeepMerge) #8843933

# remove duplicate data (e.g. detected at multiple Sensor Stations)
BeepMerge1 <- BeepMerge %>%
  dplyr::distinct(Time, TagId, NodeId, TagRSSI, .keep_all = T)
str(BeepMerge1) # BeepMerge1 = removing duplicates
nrow(BeepMerge1) #5762216
nrow(BeepMerge)-nrow(BeepMerge1) #3081717 rows lost due to duplication

# comparing this record of removing duplicate data to just removing duplicate rows
BeepMerge2 <- BeepMerge %>% distinct() #BeepMerge2 = combined data after removing duplicate rows
nrow(BeepMerge2) #5762216
# BeepMerge2 = removing duplicates in a different way to see if there was a difference

#seeing if each method produced the same number of rows
summary(comparedf(BeepMerge1, BeepMerge2)) #summary showed that dataframes are the same!

# Saving BeepMerge1 
saveRDS(BeepMerge1, file= "data/beeps/BeepMerge.rds ")

######################################################################################

## Bring in Test Information for next script
#test.info <- read.csv("data-raw/2023_node_files/Test.Info_Example.csv", header = T)
test.info <- read.csv("data-raw/2023_node_files/Test.Info_ExampleAB.csv", header = T)
str(test.info) # check that data imported properly
test.info$TagId <- "6166071E" # had to change TagId value column to all one value because for some reason R was importing it as 6166071 instead of 6166071E
test.info <- subset(test.info, select = -Notes)
str(test.info) # check that data imported properly


# Preparing Test.Info_Example.csv for use in the next few examples

# Setting seed for reproducibility
set.seed(123)

# Get the number of unique values for TestId in the dataframe
#num.tests <- n_distinct(test.info$TestId)
#num <- unique(test.info$TestId)

# Subsetting dataset to just use nodes in BassCrk Node grid
BassNode <- test.info[test.info$BassNode == "Y", ]
MosNode <- test.info[test.info$BassNode == "N", ] # in Mosquito Creek grid

# Getting the unique locations in BassCrk Node grid
locs.Bass <- unique(BassNode$TestId)
locs.Mos <- unique(MosNode$TestId) # in Mosquito Creek grid

# Getting the number of unique locations in each node grid
num_Bass <- n_distinct(BassNode$TestId)
num_Mos <- n_distinct(MosNode$TestId)

# Proportion of values to assign as A(calibration) or B(test) data
proportion_cali_A <- 0.8
prop_cali_A <- c(0.8, 0.85, 0.9)



# Number of unique locations at Bass Creek that will be A (triangulation test data)
num_bass_A <- round(num_Bass * proportion_cali_A)
num_mos_A <- round(num_Mos* proportion_cali_A)

#Bass Crk Node Grid Data
# Selecting the proportion of TestId values within Bass Crk Node to be either A or B
random_values_A_bass <- sample(locs.Bass, size= num_bass_A, replace = FALSE)
random_values_B_bass <- locs.Bass[!locs.Bass %in% random_values_A_bass] #test

# Creating a new dataframe with the column Test_80 which assigns an A or B for Bass Crk locs based on 80% being calibration data
BassNode$Test_80 <- ifelse(BassNode$TestId %in% random_values_A_bass, 'A', 'B') 


#Mosquito Crk  Node Grid data
# Selecting the proportion of TestId values within Bass Crk Node to be either A or B
random_values_A_mos <- sample(locs.Mos, size= num_mos_A, replace = FALSE)
random_values_B_mos <- locs.Mos[!locs.Mos %in% random_values_A_mos] #test

# Creating a new dataframe with the column Test_80 which assigns an A or B for Bass Crk locs based on 80% being calibration data
MosNode$Test_80 <- ifelse(MosNode$TestId %in% random_values_A_mos, 'A', 'B') 

# Combine MosNode and BassNode dataframes
 test.info <- rbind(BassNode, MosNode)
 write.csv(test.info, file= 'data-raw/2023_node_files/Test.Info_Example80.csv')
 
# Randomly select 87 unique values for TestId
#random_values_A <- sample(1:num.tests, 87) #triangulation
#random_values_B <- num[!num %in% random_values_A] #test

# Create a new dataframe with a column that assigns an A or B depending on which number is in the TestId column
#test.info$TestAB <- ifelse(test.info$TestId %in% random_values_A, 'A', 'B')

#write.csv(test.info, file= 'data-raw/2023_node_files/Test.Info_ExampleAB.csv')

 
 #Playing with writing a for loop to loop through proportions
 
 prop_cali_A <- c(0.8, 0.85, 0.9) # proportions using
 outpath <- "data-raw/2023_node_files/"
 
 for(i in 1:length(prop_cali_A)) {
 
   # Setting seed for reproducibility
   set.seed(123)  
   
 # Number of unique locations at Bass Creek that will be A (triangulation test data)
 num_bass_A <- round(num_Bass * prop_cali_A[i])
 num_mos_A <- round(num_Mos* prop_cali_A[i])
 
 #Bass Crk Node Grid Data
 # Selecting the proportion of TestId values within Bass Crk Node to be either A or B
 random_values_A_bass <- sample(locs.Bass, size= num_bass_A, replace = FALSE)
 random_values_B_bass <- locs.Bass[!locs.Bass %in% random_values_A_bass] #test
 
 # Creating a new dataframe with the column Test_80 which assigns an A or B for Bass Crk locs based on 80% being calibration data
 BassNode$PropTest <- ifelse(BassNode$TestId %in% random_values_A_bass, 'A', 'B') 
 
 
 #Mosquito Crk  Node Grid data
 # Selecting the proportion of TestId values within Bass Crk Node to be either A or B
 random_values_A_mos <- sample(locs.Mos, size= num_mos_A, replace = FALSE)
 random_values_B_mos <- locs.Mos[!locs.Mos %in% random_values_A_mos] #test
 
 # Creating a new dataframe with the column Test_80 which assigns an A or B for Bass Crk locs based on 80% being calibration data
 MosNode$PropTest <- ifelse(MosNode$TestId %in% random_values_A_mos, 'A', 'B') 
 
 # Combine MosNode and BassNode dataframes
 test.info <- rbind(BassNode, MosNode)
 
 # Write csv for combined data with the proportion amount saved at the end
 write.csv(test.info, paste0(outpath, "Test.Info_Example", prop_cali_A[i], ".csv"))
           }
 

 