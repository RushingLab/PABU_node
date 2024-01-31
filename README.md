# PABU_node README
Files for the analysis of Painted Bunting (PABU) movement data from Cellular Tracking Technology (CTT) Nodes on Little Saint Simons, Island (LSSI), Georgia from the 2023 field season.

## Directory structure:

---

### data-raw

* 2023_node_files

  + Folders 37A5AA through 377990
      - Contains files "beep_0", "gps", and "test" from each node installed during the 2023 LSSI field season. The "beep_0" file contains all beep information downloaded off of the node USB in November 2023. Includes 31 folders for all 31 nodes.

  + Nodes_Example.csv
      - csv file that contains a list of nodes in the node network and their UTM zone 17N locations (based off of Paxton 2022 scripts)
  
  + Nodes_trilateration.csv
      - csv file that contains a list of nodes in the node network and their UTM zone 17N locations - to be used for trilateration (based off of Paxton 2022 scripts)
  
  + Tags_trilateration.csv
      - csv file that contains a list of all PABU transmitter tags - to be used for trilateration (based off of Paxton 2022 scripts)
  
  + Test.Info_Example.csv
      - csv file that contains a formatted test data set from node triangulation tests conducted near the end of the field season (based off of Paxton 2022 scripts)

* node_api_files

  + Little St. Simons Motus
  
      - 02_functions_node.api.beeps.R
          * R_script that has to be embedded within this folder in order to run the "02_functions_node.api.beeps.R" script in the R_scripts folder
      
      - beep_data_2023-05-10_2023-11-20.rds
          * rds file that contains output from the "02_functions_node.api.beeps.R" script from Paxton 2022
      
      - nodes
          * Nodes_Example.csv: csv file that contains a list of nodes in the node network and their UTM zone 17N locations (based off of Paxton 2022 scripts)
          
          * Nodes_trilateration.csv: csv file that contains a list of nodes in the node network and their UTM zone 17N locations - to be used for trilateration (based                   off of Paxton 2022 scripts)
          
          * Tags_trilateration.csv: csv file that contains a list of all PABU transmitter tags - to be used for trilateration (based off of Paxton 2022 scripts)
          
          * Test.Info_Example.csv: csv file that contains a formatted test data set from node triangulation tests conducted near the end of the field season (based                   off of Paxton 2022 scripts)
      
      - V30B0154CD2D (SensorStation)
          * gps: contains all gps point locations for the nodes and SensorStation collected every hour
          
          * node_health: contains the health metrics for nodes collected every hour
          
          * raw: contains the beep/detection data collected off of the SensorStation every hour
  

### data

  + beep_api.rds
      - rds file that contains cleaned node beep/detection data from the API and formatted according to Paxton 2022
      
  + beep_sd.rds
      - rds file that contains cleaned node beep/detection data from the physical node microSD cards and formatted according to Paxton 2022
      
  + BeepMerge.csv
      - beep_api.rds and beep_sd.rds files combined together and presented as a csv
      
  + BeepMerge.rds
      - - beep_api.rds and beep_sd.rds files combined together and presented as a rds

### R_scripts

  + 01_collect_api.beeps.R
    - R script that includes methods to download beep/detection data from SensorStations through an API.

  + 02_import_node.api.beeps.R
    - R script to import beep/detection data into a usable format (based on Paxton 2022 (GitHub: kpaxton75) "Import_beep.data.Github.R" script)
    
  + 02_functions_node.api.beeps.R
    - R script that includes functions to import beep/detection data into a usable format (based on Paxton 2022 (GitHub: kpaxton75) "Functions_CTT.Network.R" script)  
  + 03_import_node.sd.beeps.R
    - R script to import beep/detection data into a usable format from node microSD cards 
  
  + 04_combine_node.sd.beeps.R
    - R script to combine beep/detection data into a usable format from node microSD cards and the API  