###################################################################################
## Diane Klement
## January 26 2024
##
## Code to collect/download raw detections (or beeps) from multiple Sensor Stations into a folder using the CTT API method
##
###################################################################################
## This code exists to pull raw beep/detection data from CTT SensorStations using an API. 
## I did this because I noticed that the beep data downloaded from the Node microSD cards was corrupted from some nodes or had dates that were way off.
## Thus, I downloaded this data as a measure of security to ensure that I was recovering as many possible beeps from the nodes as I could.
## I will later combine this data with my node microSD card beep data and then remove duplicates.
##
## BEFORE GETTING STARTED:
##        - Ensure that you have visited https://cellular-tracking-technologies.github.io/ and read over Chapter 1 API
##        - PostgreSQL
##            - Download with advice using the above URL.Be sure to remember your password!
##            - Open pgAdmin4. Click on 'Servers' on the lefthand side then 'PostgreSQL 16'. Enter your password.
##            - If your computer is like mine, it only created a 'postgres' username rather than a username named for your computer system.
##            - To save further headache, go ahead and create a postgres username for your computer systems name (such as 'dklem')
##                  - I find the following instructions here under the pgAdmin section (https://www.commandprompt.com/education/how-to-create-a-user-in-postgresql/) helpful
##                  - Be sure to remember your password!
##            - Then create a database (I called mine "node_api") owned by that new username (i.e., dklem)
##        - API token
##            - Request an API token through this form: https://celltracktech.com/pages/csd-radio-api-key-request ... It will take a couple of days to receive.
##
## WARNINGS/ Suggestions for moving forward
##        - I would not recommend running this code through RStudio. 
##        - I did this the first time, and the first pull lasted >36 hours (and rendered my computer unusable for that time--lol it froze but was still downloading data).
##        - If you can please conduct this by running the code through your computer command line because it is very intensive and will likely crash your computer.
##        - To do this, open the R program file (located within C:\Program Files\R\R-4.3.2\bin\R.exe)
##        - Then run the following code with your own information wihtin the R.exe file (except please substitute your own information):
##            - C:\Users\dklem>"C:\Program Files\R\R-4.3.2\bin\Rscript.exe" "C:\Users\dklem\Documents\Grad School Research Papers\api_run.r"
##            - The code in "" are what you will paste on the command line
##            - The first code in "" is the path to the Rscript.exe on your computer
##            - The second code in "" is the path to the folder where you want the api data downloaded on your computer
##        - Again, the computer will run this script in the command line much faster (and better for your computer / mental health) than trying to run it through RStudio.
##        - I also added my node files using the import_node_data() function with the proper format, but I am wholly unsure of if it did anything. It didn't seem to actually incorporate the files into my local database.
##
##
## Output
##        - The output for this code will be a folder containing ALL of the beep/health data from ALL SensorStations under the project.
##        - Format:
##            - Little St. Simons Motus (ProjectName)
##                - 4F627F78EC34 (SensorStation)
##                    - gps
##                        - CTT-4F627F78EC34-gps.2023-03-21_182540.csv.gz (gps data-- recorded every hour)
##                    - node_health
##                        - CTT-4F627F78EC34-node-health.2023-03-21_182540.csv.gz (health data-- recorded every hour)
##                    - raw
##                        - CTT-4F627F78EC34-raw-data.2023-03-21_182540.csv.gz (beep data-- recorded every hour)
##
####################################################################################

# Packages Needed
library(devtools)
install_github('cellular-tracking-technologies/celltracktech')
library(celltracktech)
library(DBI)

# Setting the system start time
start <- Sys.time()


####SETTINGS#####
my_token <- "734b29e9fe8fc7ac9c6b3083f548c1df66c38508c1dec8a4424b7a6bb48f7d11" #this is your API through CTT
db_name <- "node_api" #this is the name of your database name on Postgres
myproject <- "Little St. Simons Motus" #this is your project name on your CTT account
conn <- dbConnect(RPostgres::Postgres(), dbname=db_name, password= "clover08") #be sure to include the password to your Postgres account using the same user as your system user
################

# Setting where your downloaded files will go
outpath <- "C:/Users/dklem/Documents/Grad School Research Papers/node_api_files" 
# I saved this outside of the R project because it will download ALL of the data from ALL of the sensor stations under Clark's CTT Project.
# I then brought the folder from the sensor station that I needed (V30B0154CD2D) into the "raw-data" folder of my project

# This is the most memory/server intensive line of code -- run within R studio with caution! (Or use the command line (as detailed in the cautionary tales above!))
get_my_data(my_token, "C:/Users/dklem/Documents/Grad School Research Papers/node_api_files", conn, myproject=myproject)

update_db(conn, outpath, myproject) # updating your database
dbDisconnect(conn)

db_cleanup(conn)

#findfiles(outpath, "directory path where you want your caught files to go")

time_elapse <- Sys.time() - start
print(time_elapse)

####Incorporating Node Data into the Database
import_node_data(conn, outpath, myproject = "Little St. Simons Motus")

