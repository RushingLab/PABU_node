# PABU_node README
Files for the analysis of Painted Bunting (PABU) movement data from Cellular Tracking Technology (CTT) Nodes on Little Saint Simons, Island (LSSI), Georgia from the 2023 field season. r

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
      
* 2023_node_combined_dataset

      - Includes all of the beep data from all nodes combined into one datasheet with a seperate column for node name.
  
### data
Empty but hope to fill soon!

### R_scripts

  + 00_functions-CTT.Network.R
    - R script that includes the functions that Kristina Paxton (GitHub: kpaxton75) used to manipulate CTT Node data.

  + 01_beep-data-cleaning.R
    - R script that includes functions to import CTT node data from node micro-SDs into one dataset and clean up the data.
