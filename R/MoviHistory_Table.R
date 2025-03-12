movihistory<-function(dis, preg, dbpath){
 
  wide_df <- dis %>%
    mutate(test = case_when(TestLab == "HUYVAERT" ~ 'BIOMEME', T ~ test)) %>%
    mutate(TestLab = 'WADDL') %>% 
    distinct(ScanID, test, .keep_all = T) %>%
    dplyr::select(ScanID, Animal_ID, Capture_Date, test, TestResult, TestLab)
  # 
  # x<-wide_df %>%
  #   group_by(Animal_ID, test, Capture_Date) %>%
  #   summarize(n = n()) %>%
  #   ungroup() %>%
  #   filter(n >= 2)
  
  wide_df<-wide_df %>%
    spread(key = test, value = TestResult)
  
  
  preg_df<-preg %>%
    distinct(ScanID, PregTestType, .keep_all = T) %>%
    dplyr::select(ScanID, Animal_ID, Capture_Date, PregTestType, PregResult)
  
  preg_wid<-preg_df %>%
    spread(key = PregTestType, value = PregResult)
  
  
  wide_df<-wide_df %>%
    left_join(preg_wid, by = c("Animal_ID", "Capture_Date")) %>%
    dplyr::select(-ScanID.y)
  
  names(wide_df)<-c('ScanID', 'AID', 'CaptureDate', 'TestLab', 'biomeme', 'elisa', 
                    'mlst', 'pcr', 'pspb', 'ultrasound')
  
  wide_df<-wide_df %>%
    mutate(CaptureDate = ymd(CaptureDate)) %>%
    group_by(AID) %>%
    arrange(AID, CaptureDate) %>%
    mutate(CaptureNumber = row_number()) %>%
    mutate(pspb = ifelse(pspb == "NOT-PREGNANT (OPEN)", 'NOT PREGNANT', pspb)) %>%
    mutate(ultrasound = ifelse(ultrasound == "NOT-PREGNANT (OPEN)", 'NOT PREGNANT', ultrasound)) %>%
    mutate(pregnancy = ifelse(ultrasound == "NOT PREGNANT" & !is.na(pspb), 
                              pspb, ultrasound)) %>%
    mutate(pregnancy = ifelse(is.na(ultrasound), pspb, pregnancy)) %>%
    mutate(CaptureDate = ymd(CaptureDate)) %>%
    mutate(SampleYear = strftime(CaptureDate, format = "%Y")) %>%
    dplyr::select(ScanID, AID, CaptureDate, CaptureNumber, SampleYear, elisa, pcr, mlst, pspb, ultrasound, 
                  pregnancy) %>%
    mutate(Pkey = paste0(AID, "_", CaptureNumber))
  
  
  
  
  
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
    mutate(StateChange = ifelse(length(unique(pcr)) == 1, 'NO', 'YES')) %>%
    ungroup() %>%
    mutate(Carrier = ifelse(StateChange == "YES", 'NO', 'YES'))
  
  
  carriers<-animals_sorted %>%
    group_by(AID) %>%
    arrange(CaptureDate) %>%
    mutate(FirstPos = ifelse(Carrier == "YES", as.character(min(CaptureDate, na.rm = T)), NA), 
           LastPos = ifelse(Carrier == "YES", as.character(max(CaptureDate, na.rm = T)), NA)) %>%
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
    ungroup() %>%
    mutate(HealthStatus = ifelse(StateChange == "YES", 'Intermittent', 'Uninfected'))
  
  
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
              `ELISA Record` = paste(elisa, collapse = "|")) 
  
  
  movi<-c(
    'Carrier' = 'CC', 
    'Intermittent' = 'INT', 
    'Uninfected' = 'HEALTHY'
  )
  
  
  pos<- pos %>%
    mutate(Status = stringr::str_replace_all(HealthStatus, movi)) %>%
    dplyr::select(-HealthStatus) %>%
    left_join(records, by = "AID") 
  

  
  con <- dbConnect(odbc::odbc(),
                   Driver = "Microsoft Access Driver (*.mdb, *.accdb)",
                   DBQ = paste0(dbpath, 'LPMS_MasterDatabase.accdb'))
  
  #con <- dbConnect(odbc::odbc(), "LPMS")
  
  for(k in 1:nrow(pos)){
    
    if(k == 1){
      dbWriteTable(con, "MoviHistory", pos[k,], append = FALSE, overwrite = TRUE, row.names = FALSE)
    }else{
      dbWriteTable(con, "MoviHistory", pos[k,], append = TRUE, row.names = FALSE)
    }
    
  }
  
  
  RODBC::odbcCloseAll()
  
}