require(dplyr)
require(readxl)
require(lubridate)
require(odbc)
require(RSQLite)
require(tidyr)
require(ggplot2)
require(cowplot)

## LAPTOP ##


sad<-read.csv('C:/Users/katey.huggler/Dropbox/PostDoc/Data/Capture/MasterDB/Intermediates/SAD.csv')
collar<-readxl::read_xlsx('C:/Users/katey.huggler/State of Idaho/Research Group-Lower Panther - Main Salmon Sheep Research - Collars/LPMSCollarDeployment_250304.xlsx')
cap<-readxl::read_xlsx('D:/Dropbox/PostDoc/Data/Capture/MasterDB/Intermediates/LPMSSheepCapture_250311.xlsx')
dis<-readxl::read_xlsx('C:/Users/katey.huggler/Dropbox/PostDoc/Data/Capture/MasterDB/Intermediates/MoviQuery.xlsx')
preg<-readxl::read_xlsx('C:/Users/katey.huggler/Dropbox/PostDoc/Data/Capture/MasterDB/Intermediates/MoviQuery.xlsx', sheet = "Preg")

gps<-st_read('C:/Users/katey.huggler/Dropbox/PostDoc/Data/GPSData/LPMS_GPS.shp')


source('C:/Users/katey.huggler/Dropbox/PostDoc/Code/DBManagement/AnimalInfo_Table.R')
source('C:/Users/katey.huggler/Dropbox/PostDoc/Code/DBManagement/DiseaseTable.R')
source('C:/Users/katey.huggler/Dropbox/PostDoc/Code/DBManagement/CollarDeployment_Table.R')
source('C:/Users/katey.huggler/Dropbox/PostDoc/Code/DBManagement/Mortality_Table.R')
source('C:/Users/katey.huggler/Dropbox/PostDoc/Code/DBManagement/MoviHistory_Table.R')
source('C:/Users/katey.huggler/Dropbox/PostDoc/Code/DBManagement/FailSlip_Table.R')

# run once 
#source('C:/Users/katey.huggler/Dropbox/PostDoc/Code/DBManagement/CollarInventory_Table.R')


dbpath = 'C:/Users/katey.huggler/Dropbox/PostDoc/Data/Capture/MasterDB/'


## DESKTOP ## 
sad<-read.csv('D:/Dropbox/PostDoc/Data/Capture/MasterDB/Intermediates/SAD.csv')
collar<-readxl::read_xlsx('D:/State of Idaho/Research Group-Lower Panther - Main Salmon Sheep Research - LPMS/Collars/LPMSCollarDeployment_250304.xlsx')
cap<-readxl::read_xlsx('D:/Dropbox/PostDoc/Data/Capture/MasterDB/Intermediates/LPMSSheepCapture_250311.xlsx')
dis<-readxl::read_xlsx('D:/Dropbox/PostDoc/Data/Capture/MasterDB/Intermediates/MoviQuery.xlsx')
preg<-readxl::read_xlsx('D:/Dropbox/PostDoc/Data/Capture/MasterDB/Intermediates/MoviQuery.xlsx', sheet = "Preg")

gps<-read.csv('D:/Dropbox/PostDoc/Data/GPSData/LPMS_GPS.csv')


source('D:/Dropbox/PostDoc/Code/DBManagement/AnimalInfo_Table.R')
source('D:/Dropbox/PostDoc/Code/DBManagement/DiseaseTable.R')
source('D:/Dropbox/PostDoc/Code/DBManagement/CollarDeployment_Table.R')
source('D:/Dropbox/PostDoc/Code/DBManagement/Mortality_Table.R')
source('D:/Dropbox/PostDoc/Code/DBManagement/MoviHistory_Table.R')
source('D:/Dropbox/PostDoc/Code/DBManagement/FailSlip_Table.R')


# run once 
#source('D:/Dropbox/PostDoc/Code/DBManagement/CollarInventory_Table.R')


dbpath = 'D:/Dropbox/PostDoc/Data/Capture/MasterDB/'



animalinfo(sad, collar, cap, dbpath, export = T)
diseasetable(dis, preg, dbpath, export = T)
collartable(sad, dbpath, export = T)
mortable(sad, dbpath, export = T)
movihistory(dis, preg, sad, collar, cap, gps, dbpath, export = T)
failtable(sad, gps, dbpath, export = T)
#collarinventory(sad, gps, dbpath)


## ----------------------- ##
## FIGURES AND TABLES ## 
## ----------------------- ## 

source('D:/Dropbox/PostDoc/Code/DBManagement/PrevPlots.R')

acc_path = 'D:/Dropbox/PostDoc/Data/Capture/MasterDB/'
savewd = 'C:/Users/Katey/OneDrive - State of Idaho/Documents/GitHub/LPMS-BHS/Plots/'

prev<-prevplots(acc_path, savewd = savewd, collar = collar, return_summary = T)



## adult survival 
source('D:/Dropbox/PostDoc/Code/DBManagement/AdultSurvival.R')
yearstart = 2020
yearend = 2024

adultsurvival(acc_path, yearstart, yearend, savewd)

