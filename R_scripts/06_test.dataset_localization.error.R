###################################################################################
## Diane Klement
## January 31 2024
##
##    -- Code to assess the localization error associated with RSS-based localization estimates for a node network based on a test dataset
##        -- Test dataset should be locations that differ from the dataset used to calibrate the relationship between RSS and distance for the node network
##        -- localization error = difference in distance (m) between true and estimated location
##
##
###################################################################################
###########################################################################################################################################################
##
##  Kristina L Paxton
##
##  April 1 2021
##
##    -- Code to assess the localization error associated with RSS-based localization estimates for a node network based on a test dataset
##        -- Test dataset should be locations that differ from the dataset used to calibrate the relationship between RSS and distance for the node network
##        -- localization error = difference in distance (m) between true and estimated location
##
##    -- Analysis in Ecology and Evolution Paper is based on data from a test with a radio transmitter at 54 known locations randomly distributed throughout the Guam Network 
##        -- At each test location a transmitter was held stationary for a 5-minute time period 
##            -- Removed first and last minute of each test to ensure that times matched between tests and the node network
##            -- For each test, calculated an average RSS value for the 3-min middle time period individually for each node that detected the transmitter
##
##
##    1st step: Data preparation - isolates raw RSS data from node network that is associated with test data 
##       -- Creates the data file that was published with this paper at: https://doi.org/10.5066/P94LQWIE 
##            -- Data associated with this test is coded as 'B' in the column 'DataSet'
##    2nd step: Estimate the distance of the test signal from each node that detected the signal based on the exponential decay relationship between RSS and Distance 
##    3rd step: Calculate the error associated with RSS-based localization estimates when filters are applied prior to trilateration analysis
##
##
##    Files Needed
##        Files Needed
##        1. TestInfo.csv: file with test information that is saved in the working directory defined below
##            - For each unique test location there should a row of information associated with each of the 3 inner minutes of that test
##                - meaning that each test will have 3 rows of information with each row identifying one of the 3 inner minutes of the test (see TestInfo_Example.csv)
##            - Columns
##                -- TagId: unique identifier of the transmitter used during the specified test
##                -- TestId: unique identifier given to each of the unique locations where a test was conducted
##                -- Date: date when the specified test was conducted
##                -- Start.Time: time when the specified test was started
##                -- End.Time: time when the specified test was ended
##                -- Min: 1-minute time period of the specified test
##                -- Hour: hour of the the specified minute of the test
##                -- TestUTMx: Easting location of the specified test 
##                -- TestUTMy: Northing location of the specified test 
##
##       2. BeepData.rds: file with the raw RSS values collected by the node network during the time period of the test. The file should be saved in the working directory defined below
##            -- output file from Import_beep.data.Github.R script (see BeepData_Example.rds)
##            -- If using a file created from another source the following columns are needed in the specified format: 
##                -- TagId: Factor identifying the unique code of a tag
##                -- Time.local: POSIXct value in the format: "2021-12-29 09:02:51" that identifies the unique datetime stamp (IN THE LOCAL TIME ZONE OF THE STUDY) when the tag was detected by the specified node
##                    ***** If you don't have a column with the local time, but only UTC time - then change lines 42-44 in Functions_RSS.Based.Localizations.R from 'Time.local' to 'Time' ************
##                    ***** BUT be sure that Dates and Times in TestInfo.csv are also in UTC time and not local time *********
##                -- NodeId: Factor identifying the node in the network that detected the specified tag
##                -- TagRSSI: Integer indicating the RSS value (in dB) of the signal of the specified tag received by the specified node
##
##       3. Nodes.csv: file with a list of all nodes in your network and their UTM locations. The file should be saved in the working directory defined below
##            -- Columns
##                -- NodeId: unique identifier of given to each node 
#                 -- NodeUTMx: Easting location of the specified node
##                -- NodeUTMy: Northing location of the specified node 
##            -- Other columns can also be in the file for your reference 
##            -- If Node names are being converted to scientific notation in Excel open the file with a text editor (e.g. BBedit) to change names to the correct format and save the file  
##
##       4. Model coefficients from exponential decay model that describes the relationship between RSS and Distance for the node network where test data was collected
##            -- values are from final model calculated in: Github_RSS-by_Distance_Calibration.R script
##            -- exponential model formula: avgRSS ~ a * exp(-S * distance) + K
##                     -- a = intercept
##                     -- S = decay factor
##                     -- K = horizontal asymptote 
##
##       5. Functions_RSS.Based.Localizations.R - R script with functions needed to run the code below - file should be saved in the working directory defined below    
##
##    Important Output
##        1. Dataframes with summary statistics of localization error when different filters (No Filter, RSS filters, Distance filters) 
##           are applied prior to trilateration analysis
##  
##########################################################################################################################################################


# Packages needed
library(tidyverse)
library(raster)
library(nlstools)

# Reset R's brain - removes all previous objects
rm(list=ls())


## Set by User

# Directory for Output data - Provide/path/where/you/want/output/data/to/be/stored/
outpath <- "data/localizations/"



## Bring in functions 
source("R_scripts/05_functions_rss.based.localizations.R")



## Bring in 3 Needed files - Test Information, RSS values, and Node Information - change file names in " " as needed

test.info <- read.csv("data-raw/2023_node_files/Test.Info_Example0.9.csv", header = T)
str(test.info) # check that data imported properly
test.info$TagId <- "6166071E" # had to change TagId value column to all one value because for some reason R was importing it as 6166071 instead of 6166071E
str(test.info) # check that data imported properly
# Subsetting test.info based on whether it is the calibration (A) or test dataset (B)
#test.info <- test.info[test.info$TestAB == 'B', ] #for the TestAB run
test.info <- test.info[test.info$PropTest == 'B', ]


beep.dat <- readRDS("data/beeps/BeepMerge.rds") 
str(beep.dat) # check that data imported properly

nodes <- read.csv("data-raw/2023_node_files/Nodes_Example.csv", header = T)
str(nodes) # check that data imported properly




#################################################################
#   Step 1
# Run function to isolate raw RSS data from a node network that
# is associated with test data  
#################################################################

## Variables to define for function
## TEST.TYPE = category indicating the type of dataset (Calibration or LocError)
## "Calibration" - test dataset used for calibrating the relationship between RSS and Distance (purpose of this R script)
## "LocError" - test data used to determine localization error associated with RSS-based localization estimates for a node network
## DATE.FORMAT = format of the date column in Test.Info.csv file using R standard date expressions (e.g., "%m-%d-%y", "%Y-%m-%d")
## TIMEZONE = Time zone where data was collected, use grep("<insert location name here>", OlsonNames(), value=TRUE) to find valid time zone name


## Output in R environment
## combined.data - dataframe that contains the average RSS values for a given node associated with each unique test
## Columns:
## NodeId - unique identifier of the specified node
## TestId - unique identifier of the specified test
## avgRSS - average RSS value across a 3-min time period for the given node and test
## sdRSS - standard deviation of RSS values across the 3-min time period for the given node and test
## distance - true distance (in meters) between the specified node and test location
## NodeUTMx - Easting location of the specified node
## NodeUTMy - Northing location of the specified node
## TestUTMx - Easting location of the specified test
## TestUTMy - Northing location of the specified test


##******* Define Variables - replace values below with user specified values *******## 
TEST.TYPE <- "LocError"
DATE.FORMAT <- "%m/%d/%Y" # changed Paxton code to have year represented as %Y due to putting in year as 2023 rather than 23 (which would require %y)
TIME.ZONE <- "America/New_York" 


# Function to combine RSS data from a node network with test information into a dataframe
combined.data <- data.setup(TEST.TYPE, DATE.FORMAT, TIME.ZONE)




##########################################################################################################
# Step 2 - Estimate Distance
#     Function to estimate the distance of each test location based on the RSS value detected by a node
#     Use the model coefficients from exponential decay model created with calibration data
###########################################################################################################


## Variables to define for function  
## Coefficients in the exponential model: use output from Github_RSS_by_Distance_Calibration (e.g., coef(nls.mod))
## a = intercept
## S = decay factor
## K = horizontal asymptote

## Output in R environment
## combined.data - with the added column:
## e.dist = estimated distance of the test signal from the specified node based on RSS values

## Output saved
## LocError_Dataset.csv - .csv file of the dataframe 'combine.data' saved in the folder specified by the outpath


##******** Define Variables - replace values below with user specified values ********##  
a <- 4.980584e+01
S <- 5.304308e-03
K <- -1.042341e+02


# Function to estimate the distance of each test signal from the node based on the RSS values detected
combined.data <- estimate.distance(combined.data)




####################################
# Step 3 - Trilateration Estimates
####################################

########################################################################################
# Trilateration of Test Data - No filter
#    Function that will estimate the location of each test location in the test
#    dataset with no filters applied prior to trilateration analysis and then 
#    summarize localization error
#######################################################################################

## Output in R environment
## no.filters - dataframe that contains the average localization error when no filters are applied prior to trilateration analysis
## Columns
## n.pts - number of random points a location estimate could be calculated based on trilateration 
## avg.no.nodes - average number of nodes used in each trilateration analysis
## avg.diff - average localization error of trilateration estimates when the specified filter was applied
## localization error = difference in distance (m) between true and estimated location
## sd.diff - standard deviation of localization error of trilateration estimates when the specified filter was applied
## lower.ci - lower 95% confidence interval of localization error of trilateration estimates when the specified filter was applied
## upper.ci - upper 95% confidence interval of localization error of trilateration estimates when the specified filter was applied
## med.diff - median localization error of trilateration estimates when the specified filter was applied
## min.diff - minimum localization error of trilateration estimates when the specified filter was applied
## max.dist - maximum localization error of trilateration estimates when the specified filter was applied
## filter - filter applied prior to trilateration analysis


## Output saved
## Trilateration.TestData_NoFilter_Summary.Stats.csv - .csv file of the dataframe 'no.filters' saved in the folder specified by the outpath
## Trilateration.TestData_TestData_NoFilter_Summary.Results.csv - one .csv file that has the summary statistics of localization error when the specified
##                                                                filter was applied prior to trilateration. Each row represents the summary statistics 
##                                                                for a given test location. Files are for your reference and not used in downstream functions. 



# Calculate error of location estimates of each test location when no filters are applied prior to trilateration 
no.filters <- trilateration.TestData.NoFilter(combined.data)

#TestAB (.71 A, .29 B proportions)
#   n.est.tests avg.no.nodes avg.diff  sd.diff lower.ci upper.ci med.diff min.diff max.dist    filter
#1          35         16.6 126.1591 72.79541 102.0424 150.2759 112.0258 16.78713 312.3077 No Filter


# 80% test
#   n.est.tests avg.no.nodes avg.diff sd.diff lower.ci upper.ci med.diff min.diff max.dist    filter
#1          24        17.25 128.6139 74.2808 98.89589 158.3318    119.9 15.33274  327.631 No Filter

# 85% test
#   n.est.tests avg.no.nodes avg.diff  sd.diff lower.ci upper.ci med.diff min.diff max.dist    filter
#1          18     16.33333 126.5223 79.25606 89.90852  163.136 119.0209 15.40831 311.9528 No Filter

# 90% Test
#n   .est.tests avg.no.nodes avg.diff  sd.diff lower.ci upper.ci med.diff min.diff max.dist    filter
#1          13     16.07692 129.2614 72.73669 89.72201 168.8008 117.9137 15.43882 312.0199 No Filter

#####################################################################################
# Trilateration of Test Data - RSS filters
#    Function that will estimate the location of each test location in the test
#    dataset when RSS filters are applied prior to trilateration and then 
#    summarize localization error
######################################################################################

## Variable to define for function 
## RSS.FILTER = RSS values that will be used to filter data prior to trilateration analysis
## All RSS values less than the specified value will be removed from the test data


## Output in R environment
## RSS.filters - dataframe that contains the overall localization error when RSS filters are applied prior to trilateration analysis
## columns are the same as described in the no.filter table

## Output saved
## Trilateration.TestData_Filters.RSS_Summary.Stats.csv - .csv file of the dataframe 'RSS.filters' saved in the folder specified by the outpath
## _Summary.Results.csv - 4 .csv files each with the RSS filter name in the title that has the summary statistics of localization error when the specified
##                        filter was applied prior to trilateration. Each row represents the summary statistics for a given test location. 
##                        Files are for your reference and not used in down stream functions. 




##****** Define variables - indicate the RSS values to filter data prior to trilateration ************#
RSS.FILTER <- c(-80, -85, -90, -95, -100)


# Calculate error of location estimates of each test location when RSS filters are applied prior to trilateration 
RSS.filters <- trilateration.TestData.RSS.Filter(combined.data)

#TestAB
#  n.est.tests avg.no.nodes  avg.diff  sd.diff lower.ci  upper.ci med.diff  min.diff  max.dist   filter
#1          25     4.400000  44.59437 26.91353 34.04446  55.14428 40.52192  7.392617  98.48705  RSS -80
#2          32     5.656250  57.40483 37.47396 44.42100  70.38866 48.80380  3.139547 171.65307  RSS -85
#3          35     7.742857  59.43676 36.92419 47.20398  71.66953 51.04100 11.162317 169.37888  RSS -90
#4          35    10.285714  71.81589 39.63679 58.68444  84.94734 69.99608 17.737631 199.27781  RSS -95
#5          35    13.400000 104.13393 53.02057 86.56852 121.69935 95.52331 18.214143 236.39299 RSS -100

#80% Test
#  n.est.tests avg.no.nodes  avg.diff  sd.diff lower.ci  upper.ci med.diff  min.diff  max.dist   filter
#1          17     4.176471  48.16910 26.76576 35.44570  60.89251 42.17643 12.049329  99.36314  RSS -80
#2          23     5.478261  49.82695 30.73962 37.26426  62.38964 41.48246 15.020738 132.65096  RSS -85
#3          24     7.125000  55.01325 30.41085 42.84660  67.17990 49.09238 15.372330 140.70334  RSS -90
#4          24    10.041667  69.03006 43.32264 51.69771  86.36241 63.92932 16.102160 199.05715  RSS -95
#5          24    13.708333 107.05476 70.98501 78.65536 135.45415 89.20285  9.089119 279.92702 RSS -100

#85% Test
#  n.est.tests avg.no.nodes avg.diff  sd.diff lower.ci  upper.ci med.diff  min.diff max.dist   filter
#1          15     4.066667 43.91443 25.22455 31.14928  56.67957 41.59222 12.102932  99.8151  RSS -80
#2          17     5.705882 49.98245 31.12316 35.18771  64.77719 43.43354 14.975832 135.6896  RSS -85
#3          18     7.166667 53.09623 29.00035 39.69900  66.49346 47.92295 16.055996 117.9022  RSS -90
#4          18    10.000000 77.21545 62.62535 48.28454 106.14636 55.88477 17.169940 242.6521  RSS -95
#5          18    12.777778 99.14239 74.34651 64.79668 133.48809 73.02116  9.187807 277.1567 RSS -100

#90% Test
#   n.est.tests avg.no.nodes avg.diff  sd.diff lower.ci  upper.ci med.diff  min.diff  max.dist   filter
#1          10     3.900000 44.66907 27.57074 27.58086  61.75728 39.56420 12.204832  99.40561  RSS -80
#2          12     5.416667 45.24136 24.41435 31.42790  59.05482 39.32277 15.255940  86.54131  RSS -85
#3          13     6.846154 51.33188 27.61343 36.32133  66.34244 42.42674 15.255940  87.87364  RSS -90
#4          13     9.769231 68.15967 56.00864 37.71359  98.60576 49.22074 15.826907 199.08900  RSS -95
#5          13    12.307692 92.29867 68.11930 55.26927 129.32806 63.15869  9.068458 237.70269 RSS -100

###############################################################################################
# Trilateration - Distance from strongest node filter 
#   Function that will estimate the location of test location in the test dataset when
#   distance filters are applied prior to trilateration and then summarize localization error 
################################################################################################

## Variable to define for function
## DIST.FILTER = distances that are a multiple (x1.25, x2, x3, x4) of the average grid spacing
# For each random test location the node with the strongest signal is identified and then only nodes
# within the specified distance from the strongest node are kept for trilateration analysis


## Output in R environment
## Dist.filters - dataframe that contains the overall localization error when Distance filters are applied prior to trilateration analysis
## columns are the same as described in the no.filter table

## Output saved
## Trilateration.TestData_Filters.Distance_Summary.Stats.csv - .csv file of the dataframe 'Dist.filters' saved in the folder specified by the outpath
## _Summary.Results.csv - 4 .csv files each with the Distance filter name in the title that has the summary statistics of localization error when the specified
##                        filter was applied prior to trilateration. Each row represents the summary statistics for a given test location. 
##                        Files are for your reference and not used in down stream functions. 



#****** Define variables - indicate the Distance values to filter data prior to trilateration ************#
DIST.FILTER <- c(187.5,300,450,600) #150 m node spacing


# Calculate error of location estimates of each test location when Distance filters are applied prior to trilateration 
Dist.filters <- trilateration.TestData.Distance.Filter(combined.data)

#TestAB
#  n.est.tests avg.no.nodes avg.diff  sd.diff lower.ci  upper.ci med.diff  min.diff max.dist         filter
#1          35     5.742857 83.22402 62.70347 62.45071 103.99732 78.21754  5.772987 257.0282 Distance 187.5
#2          35     9.485714 83.08474 48.73026 66.94068  99.22881 76.66315  5.346328 229.9563   Distance 300
#3          35    12.171429 81.31035 53.78500 63.49168  99.12902 63.89836 11.004267 255.4333   Distance 450
#4          35    13.742857 82.17348 54.59701 64.08580 100.26116 71.42343 12.823683 255.4333   Distance 600

#80% Test
#n.est.tests avg.no.nodes avg.diff  sd.diff lower.ci upper.ci med.diff min.diff max.dist         filter
#1          24     5.708333 84.86014 60.61259 60.61050 109.1098 86.23560  7.96801 227.5294 Distance 187.5
#2          24     9.291667 89.30112 57.84702 66.15792 112.4443 72.49934 12.79090 231.1680   Distance 300
#3          24    12.041667 82.03558 53.56156 60.60688 103.4643 78.95449 13.12819 255.5040   Distance 450
#4          24    14.000000 86.59534 51.51025 65.98733 107.2034 84.09907 15.33274 255.5040   Distance 600

#85% Test
#n.est.tests avg.no.nodes avg.diff  sd.diff lower.ci upper.ci med.diff min.diff max.dist         filter
#1          18     5.833333 91.64185 72.57724 58.11350 125.1702 59.11135 12.35920 227.8154 Distance 187.5
#2          18     9.333333 88.76873 71.72803 55.63268 121.9048 58.77177 12.17950 231.5752   Distance 300
#3          18    11.944444 83.77470 63.25919 54.55098 112.9984 72.38583 12.60478 255.8165   Distance 450
#4          18    13.611111 91.18136 62.48238 62.31650 120.0462 85.02995 15.40831 255.8165   Distance 600

#90% Test
#   n.est.tests avg.no.nodes  avg.diff  sd.diff lower.ci upper.ci  med.diff min.diff max.dist         filter
#1          13     5.769231 101.42964 79.77327 58.06519 144.7941  65.14877 17.57499 228.2401 Distance 187.5
#2          13     9.230769 101.53675 79.19712 58.48549 144.5880  72.72432 12.88807 232.0707   Distance 300
#3          13    11.769231  92.77738 68.73108 55.41542 130.1393 104.01095 12.44916 256.3667   Distance 450
#4          13    13.615385 103.64574 66.68604 67.39546 139.8960 105.46957 15.43882 256.3667   Distance 600