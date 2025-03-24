movihistory<-function(dis, preg, sad, collar, cap, gps, ss, yearstart = 2019, yearend = 2028, dbpath = NULL, export = F){
  
  bio.years<-data.frame(Year = yearstart:yearend)
  bio.years$StartDate<-as.Date(paste0(bio.years$Year, "-", "05-01"), format = "%Y-%m-%d")
  bio.years$EndDate<-as.Date(paste0(bio.years$Year + 1, "-", "04-30"), format = "%Y-%m-%d")
  
  
  
  info<-animalinfo(sad, collar, cap, ss, dbpath, export = F)
  info<-info %>%
    group_by(AID) %>%
    arrange(AID, Capture_Date) %>%
    summarize(AgeYears = last(AgeYears), 
              Age_Class_Current = last(Age_Class_Current), 
              Age_Class_Cap = first(Age_Class_Cap),
              EweGroup = last(EweGroup))
  
  morts<-mortable(sad, dis, export = F)
  
  fails<-failtable(sad, gps)
  
  wide_df<-diseasetable(dis, preg, export = F) %>%
    ungroup()
  
  wide_df<- wide_df %>%
   rowwise() %>%
    mutate(BioYear = bio.years$Year[which(CaptureDate >= bio.years$StartDate & CaptureDate < bio.years$EndDate)]) %>%
    ungroup()
  
  # bind whether each individual was a chronic carrier in the particular year of interest
  
  # cap<-readxl::read_xlsx('C:/Users/katey.huggler/State of Idaho/Research Group-Lower Panther - Main Salmon Sheep Research - Capture/LPMS_Movi_Query.xlsx')
  # cap$CapYear<-strftime(cap$`Capture Date`, format = "%Y")
  # cap$AIDYear<-paste0(cap$`Animal ID`, "_", cap$CapYear)
  # 
  
  # remove duplicate rows in the capture database 
  cap<-wide_df %>%
    distinct(Pkey, .keep_all = T) %>%
    filter(!is.na(pcr)) 
  
  animals_with_multiple_positives <- cap %>%
    group_by(AID) %>%
    filter(sum(pcr == "DETECTED") >= 2, n() >= 2) %>%  # Only animals with at least 2 positive tests and 2 captures
    ungroup()
  
  
  animals_sorted <- animals_with_multiple_positives %>%
    arrange(Pkey) %>%
    group_by(AID) %>%
    mutate(StateChange = ifelse(length(unique(tail(pcr,2))) == 1, 'NO', 'YES')) %>%
    mutate(BioYearChange = ifelse(length(unique(BioYear)) == 1, "NO", 'YES')) %>%
    ungroup() %>%
    mutate(Carrier = ifelse(StateChange == "YES", 'NO', 
                            ifelse(StateChange == 'NO' & BioYearChange == "NO", 'MAYBE', 
                                   'YES')))
    
  
  
  carriers<-animals_sorted %>%
    group_by(AID) %>%
    arrange(CaptureDate) %>%
    mutate(FirstPos = ifelse(Carrier == "YES", as.character(min(CaptureDate, na.rm = T)), NA), 
           LastPos = ifelse(Carrier == "YES", as.character(max(CaptureDate, na.rm = T)), NA)) %>%
    mutate(FirstPos = ifelse(Carrier == "MAYBE", as.character(min(CaptureDate, na.rm = T)), NA), 
           LastPos = ifelse(Carrier == "MAYBE", as.character(max(CaptureDate, na.rm = T)), NA)) %>%
    distinct(AID, .keep_all = T) %>%
    filter(Carrier == "YES") %>%
    mutate(HealthStatus = "Carrier")
  
  
  animals_captured_twice <- cap %>%
    group_by(AID) %>%
    filter(n() >= 2) %>% 
    ungroup() %>%
    filter(!AID %in% carriers$AID) 
  
  # not removing carriers
  twicecap <- cap %>%
    group_by(AID) %>%
    filter(n() >= 2) %>% 
    mutate(CapName = strftime(CaptureDate, format = "%b-%y")) %>%
    ungroup() 
  
  # not removing carriers
  oncecap <- cap %>%
    group_by(AID) %>%
    filter(n() <2) %>% 
    mutate(CapName = strftime(CaptureDate, format = "%b-%y")) %>%
    ungroup()

  
  allcap<-plyr::rbind.fill(twicecap, oncecap)
  
  
  # intermittents and healthy
  non_carriers <- animals_captured_twice %>%
    arrange(AID, CaptureDate) %>%
    group_by(AID) %>%
    mutate(StateChange = ifelse(length(unique(pcr)) == 1, 'NO', 'YES')) %>%
    mutate(BioYearChange = ifelse(length(unique(BioYear)) == 1, 'NO', 'YES')) %>%
    ungroup() %>%
    mutate(HealthStatus = ifelse(StateChange == "YES", 'Intermittent',
                                 ifelse(StateChange == 'NO' & pcr == 'NOT DETECTED',
                                        'Uninfected', 'Possible')))
  
  
  status<-plyr::rbind.fill(carriers, non_carriers)
  status<-status %>%
    dplyr::select(HealthStatus, AID) %>%
    distinct()
  
  # pos<-pos %>%
  #   left_join(status, by = "Animal ID")
  # 
  cap<-cap %>%
    group_by(AID) %>%
    summarize(nTest = n())
  
  pos<-cap %>%
    left_join(status, by = "AID")
  
  
  movi<-c(
    'NOT DETECTED' = 'NEG', 
    'DETECTED' = 'POS', 
    'INDETERMINATE' = 'IND'
  )
  
  
  
  ## add a record of Movi PCRs 
  records<-allcap %>%
    arrange(AID, CaptureDate) %>%
    group_by(AID) %>%
    filter(!is.na(pcr)) %>%
    mutate(pcr = stringr::str_replace_all(pcr, movi)) %>%
    mutate(elisa = stringr::str_replace_all(elisa, movi)) %>%
    summarise(DateRecord = paste(CapName, collapse = "|"), 
              `PCR Record` = paste(pcr, collapse = "|"), 
              `ELISA Record` = paste(elisa, collapse = "|")) %>%
    ungroup() %>%
    left_join(info, by = "AID") %>%
    mutate(MortStatus = case_when(AID %in% morts$AID ~ 'Dead',
                              AID %in% fails$AID ~ 'Unknown', 
                              T ~ 'Alive'))
  
  
  
  movi<-c(
    'Carrier' = 'CC', 
    'Intermittent' = 'INT', 
    'Uninfected' = 'HEALTHY', 
    'Possible' = 'POSSIBLE CC'
  )
  
  
  pos<- pos %>%
    mutate(Status = stringr::str_replace_all(HealthStatus, movi)) %>%
    dplyr::select(-HealthStatus) %>%
    left_join(records, by = "AID") 
  
  
  firstpos<-wide_df %>%
    distinct(Pkey, .keep_all = T) %>%
    filter(!is.na(pcr))  %>%
    filter(pcr == "DETECTED") %>%
    group_by(AID) %>%
    arrange(AID, CaptureDate) %>% 
    slice_min(CaptureDate, n = 1, with_ties = F) %>%
  ungroup() %>%
    left_join(pos[, c('AID', 'AgeYears')], by = "AID") %>%
    mutate(AgeYears = as.numeric(stringr::str_remove(AgeYears, "\\+"))) %>%
    mutate(DOB = Sys.Date() - lubridate::years(round(AgeYears))) %>%
    mutate(AgeatFirstPos = abs(as.numeric(difftime(CaptureDate, DOB, units = "weeks")/52))) %>%
    mutate(AgeatFirstPos = case_when(AgeatFirstPos < 1 ~ 'Lamb', 
                                     AgeatFirstPos > 1 & AgeatFirstPos < 2 ~ 'Yearling', 
                                      T ~ 'Adult'))
    
  pos<-pos %>%
    left_join(firstpos[, c('AID', 'AgeatFirstPos')], by = 'AID')

  if(!is.null(dbpath)){
  con <- dbConnect(odbc::odbc(),
                   Driver = "Microsoft Access Driver (*.mdb, *.accdb)",
                   DBQ = paste0(dbpath, 'LPMS_MasterDatabase.accdb'))
  }
  
  #con <- dbConnect(odbc::odbc(), "LPMS")
  if(export == TRUE){
  for(k in 1:nrow(pos)){
    
    if(k == 1){
      dbWriteTable(con, "MoviHistory", pos[k,], append = FALSE, overwrite = TRUE, row.names = FALSE)
    }else{
      dbWriteTable(con, "MoviHistory", pos[k,], append = TRUE, row.names = FALSE)
    }
    
  }
  }else{
    return(pos)
  }
  
  
  RODBC::odbcCloseAll()
  
}
