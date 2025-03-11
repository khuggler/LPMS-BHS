
animalinfo<-function(sad, collar, cap, dbpath){

# calculate age and dob for each animal 
agetab<-sad %>%
  distinct(Animal_ID, Capture_Date, .keep_all = T) %>%
  filter(Capture_GMU %in% c('21', '21A', '28')) %>%
  mutate(Capture_Date = ymd(Capture_Date)) %>%
  filter(Capture_Date > "2020-01-01") %>%
  dplyr::select(Animal_ID, Years_Old, Age_Class, Sex, Capture_Date, CensorDate, CensorType) %>%
  group_by(Animal_ID) %>%
  arrange(Capture_Date) %>%
  filter(Capture_Date == min(Capture_Date, na.rm = T)) %>%
  ungroup() %>%
  mutate(Years_Old = ifelse(Age_Class == "Yearling", 1.9, ifelse(Age_Class == "Lamb", 0.9, Years_Old))) %>%
  mutate(CapYear = as.numeric(strftime(Capture_Date, format = "%Y"))) %>%
  mutate(BirthYear = CapYear - floor(as.numeric(Years_Old))) %>%
  mutate(DOB = mdy(paste0('06-01-', BirthYear))) %>%
  mutate(AgeType = ifelse(Sex == "Female" & Years_Old >=4, 'Minimum', 
                          ifelse(Sex == "Male", 'Known', 
                                 ifelse(Sex == "Female" & Years_Old < 4, 'Known', NA))))%>%
  # mutate(AgeYears = ifelse(Sex == "Female" & as.numeric(Years_Old) >= 4, paste0(round(as.numeric(abs(difftime(DOB, Sys.Date(), units = "weeks"))/52), digits = 1), "+"),
  #                          ifelse(Sex == "Male", Years_Old, 
  #                                 round(as.numeric(abs(difftime(DOB, Sys.Date(), units = "weeks"))/52), digits = 1)))) %>%
  # mutate(AgeDays = ifelse(Sex == "Female" & as.numeric(Years_Old) >= 4, paste0(as.numeric(abs(difftime(DOB, Sys.Date(), units = "days"))), "+"),
  #                         ifelse(Sex == "Male", Years_Old, 
  #                                as.numeric(abs(difftime(DOB, Sys.Date(), units = "days")))))) %>%
  # select(-Sex, -CensorDate, -CensorType) %>%
  rename(AgeatCap = Years_Old)
  
# there are some that don't have a DOB- were they aged on their second capture?
missingages<-agetab %>%
  filter(is.na(DOB)) %>%
  dplyr::select(Animal_ID)

agetab2<-sad %>%
  distinct(Animal_ID, Capture_Date, .keep_all = T) %>%
  filter(Capture_GMU %in% c('21', '21A', '28')) %>%
  mutate(Capture_Date = ymd(Capture_Date)) %>%
  filter(Capture_Date > "2020-01-01") %>%
  dplyr::select(Animal_ID, Years_Old, Age_Class, Sex, Capture_Date, CensorDate, CensorType) %>%
  filter(Animal_ID %in% missingages$Animal_ID) %>%
  group_by(Animal_ID) %>%
  filter(n() > 1) %>%
  arrange(Capture_Date) %>%
  filter(Capture_Date == max(Capture_Date, na.rm = T)) %>%
  ungroup() %>%
  mutate(Years_Old = ifelse(Age_Class == "Yearling", 1.9, ifelse(Age_Class == "Lamb", 0.9, Years_Old))) %>%
  mutate(CapYear = as.numeric(strftime(Capture_Date, format = "%Y"))) %>%
  mutate(BirthYear = CapYear - floor(as.numeric(Years_Old))) %>%
  mutate(DOB = mdy(paste0('06-01-', BirthYear))) %>%
  mutate(AgeType = ifelse(Sex == "Female" & Years_Old >=4, 'Minimum', 
                          ifelse(Sex == "Male", 'Known', 
                          ifelse(Sex == "Female" & Years_Old < 4, 'Known', NA)))) %>%
  rename(AgeatCap = Years_Old) 

agetab<-plyr::rbind.fill(agetab, agetab2)
agetab<-agetab %>%
  filter(!is.na(DOB))


# calculate status of each animal 
morttab<-sad %>%
  distinct(Animal_ID, Capture_Date, .keep_all = T) %>%
  group_by(Animal_ID) %>%
  mutate(Capture_Date = ymd(Capture_Date)) %>%
  filter(Capture_Date == max(Capture_Date, na.rm = T)) %>%
  mutate(CensorDate = ymd(CensorDate)) %>%
  mutate(FateDate = ymd(FateDate)) %>%
  mutate(AnimalStatus = ifelse(is.na(FateDate) & is.na(CensorDate), 'Alive', 
                               ifelse(!is.na(CensorDate) & !is.na(FateDate), 'Dead', 
                                      ifelse(is.na(FateDate) & !is.na(CensorDate), 
                                             'Unknown/Censored', 
                                             ifelse(!is.na(FateDate) & is.na(CensorDate), 'Dead', NA)))))


# animal information
animalinfo<-sad %>%
  distinct(Animal_ID, Capture_Date, .keep_all = T) %>%
  filter(Capture_GMU %in% c('21', '21A', '28')) %>%
  mutate(Capture_Date = ymd(Capture_Date)) %>%
  filter(Capture_Date > "2020-01-01") %>%
  dplyr::select(Animal_ID, Capture_GMU, Sex, Capture_Date, 
         Cap_Lat, Cap_Long, Collar_Serial_No, CollarType, Radio_Frequency, CensorDate, CensorType,
         BCS, Oral_Swab, Ear_Swab, Fecal_Swab, Blood_Drawn) %>%
  group_by(Animal_ID) %>%
  arrange(Capture_Date) %>%
  mutate(CaptureNumber = row_number()) %>%
  ungroup() %>%
  left_join(agetab[, c('Animal_ID', 'Age_Class', 'DOB', 'AgeType')], by = "Animal_ID") %>%
  mutate(AgeatCapYears = round(abs(as.numeric(difftime(DOB, Capture_Date, units = "weeks"))/52), digits = 1)) %>%
  mutate(AgeYears = ifelse(Sex == "Female" & AgeType == "Minimum", 
                           paste0(round(as.numeric(abs(difftime(DOB, Sys.Date(), units = "weeks"))/52), digits = 1), "+"),
                           round(as.numeric(abs(difftime(DOB, Sys.Date(), units = "weeks"))/52), digits = 1))) %>% 
          mutate(AgeDays = ifelse(Sex == "Female" & AgeType == "Minimum", 
                                   paste0(as.numeric(abs(difftime(DOB, Sys.Date(), units = "days"))), "+"),
                                   as.numeric(abs(difftime(DOB, Sys.Date(), units = "days"))))) %>%
  left_join(morttab[, c('Animal_ID', 'AnimalStatus', 'Capture_Date')], by = c('Capture_Date', 'Animal_ID')) %>%
  mutate(AnimalStatus = ifelse(is.na(AnimalStatus), 'Alive', AnimalStatus)) %>%
  dplyr::select(Animal_ID, DOB, AgeatCapYears, AgeYears, AgeDays, Sex, Capture_Date, Cap_Lat, Cap_Long, CaptureNumber, AnimalStatus, 
         Collar_Serial_No, Radio_Frequency, CollarType, CensorDate, CensorType, BCS,Oral_Swab, Ear_Swab, Fecal_Swab, Blood_Drawn) %>%
  mutate(Pkey = paste0(Animal_ID, "_", CaptureNumber))
              

collar<-collar %>%
  rename(Radio_Frequency = Frequency, 
         Animal_ID = `Animal ID`) %>%
  mutate(Radio_Frequency = as.numeric(Radio_Frequency))

## append collar mark information
animalinfo<- animalinfo %>%
  left_join(collar[, c('Animal_ID', 'Type', 'Radio_Frequency', 'Manufacturer', 'Hardware side', 
                       'Bottom collar color', 'Top collar color', 'EweGroup')], by = c('Animal_ID', 'Radio_Frequency'))

## append ear tag information
cap<-cap %>%
  group_by(`Animal ID`) %>%
  arrange(`Capture Date`) %>%
  mutate(CaptureNumber = row_number()) %>%
  ungroup() %>%
  mutate(Pkey = paste0(`Animal ID`, "_", CaptureNumber)) %>%
  dplyr::select(EarTagColor, EarTagNumber, EarTagButton, Weight, MaxFat, BloodTubes, 
         NasalWash, `Nasal Swab`, PharyngealSwab, DNA, Pkey)


animalinfo<-animalinfo %>%
  left_join(cap, by = 'Pkey') %>%
  dplyr::select(Animal_ID, EweGroup, DOB, AgeatCapYears, AgeYears, AgeDays, Sex, Capture_Date, 
         Cap_Lat, Cap_Long, CaptureNumber, AnimalStatus, 
         Collar_Serial_No, Radio_Frequency, CollarType, Manufacturer, 
         `Hardware side`, `Bottom collar color`, `Top collar color`, 
         EarTagColor, EarTagNumber, EarTagButton, 
         Weight, MaxFat, BloodTubes, NasalWash, `Nasal Swab`, PharyngealSwab, DNA)%>%
  rename(AID = Animal_ID, 
         CapLat = Cap_Lat, 
         CapLong = Cap_Long, 
         Serial = Collar_Serial_No, 
         Frequency = Radio_Frequency, 
         Type = CollarType, 
         Brand = Manufacturer, 
         Hardware = `Hardware side`, 
         BottomCollar = `Bottom collar color`, 
         TopCollar = `Top collar color`)

## clean up

type<-c(
  'Expandable (VHF) ' = 'VHF', 
  'Non-Expandable (VHF) ' = 'VHF'
)

brand<-c(
  'Vectronic' = "VEC", 
  'Telonics' = 'TEL'
)

hardware<-c(
  'Left' = 'L', 
  'Right' = 'R'
)

color<- c(
  'Yellow' = 'YL', 
  'Brown' = 'BR', 
  'Red' = 'RD', 
  'Green' = 'GR', 
  'Blue' = 'BL', 
  'SkyBlue' = 'SKYBL', 
  'Pink' = 'PNK', 
  'White' = 'WH', 
  'Orange' = 'OR', 
  'Purple' = 'PURP', 
  'Sky Blue' = 'SKYBL', 
  'Light Blue' = 'LTBL',
  'Dk Blue' = 'DRKBL', 
  'dk blue' = 'DRKBL', 
  'red' = 'RD', 
  'Light Green/Yellow' = 'LTGR/YL', 
  'green' = 'GR', 
  'Purpe' = 'PURP',
  'Light Green' = 'LTGR', 
  'Collar tag' = 'CT', 
  'collar tag' = 'CT', 
  'Collar Tag' = 'CT', 
  'yellow' = 'YL'
  
)

dna<-c(
  'Yes' = 'TRUE', 
  'yes' = 'TRUE', 
  'No' = 'FALSE', 
  'Biopsy' = 'TRUE', 
  'none' = 'FALSE'
  
)

animalinfo<-animalinfo %>%
  mutate(Type = stringr::str_replace_all(Type, type), 
         Brand = stringr::str_replace_all(Brand, brand), 
         Hardware = stringr::str_replace_all(Hardware, hardware), 
         TopCollar = stringr::str_replace_all(TopCollar, color),
         BottomCollar = stringr::str_replace_all(BottomCollar, color), 
         EarTagColor = stringr::str_replace_all(EarTagColor, color), 
         EarTagButton = stringr::str_replace_all(EarTagButton, color), 
         DNA = stringr::str_replace_all(DNA, dna)) %>%
  mutate(Pkey = paste0(AID, "_", CaptureNumber))



con <- dbConnect(odbc::odbc(),
                 Driver = "Microsoft Access Driver (*.mdb, *.accdb)",
                 DBQ = paste0(dbpath, 'LPMS_MasterDatabase.accdb'))

#con <- dbConnect(odbc::odbc(), "LPMS")

for(k in 1:nrow(animalinfo)){
  
  if(k == 1){
    dbWriteTable(con, "AnimalInfo", animalinfo[k,], append = FALSE, overwrite = TRUE, row.names = FALSE)
  }else{
  dbWriteTable(con, "AnimalInfo", animalinfo[k,], append = TRUE, row.names = FALSE)
  }
}

RODBC::odbcCloseAll()


}


