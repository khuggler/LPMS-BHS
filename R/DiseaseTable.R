diseasetable<-function(dis, preg, dbpath){
  
  wide_df <- dis %>%
    mutate(test = case_when(TestLab == "HUYVAERT" ~ 'BIOMEME', T ~ test)) %>%
    mutate(TestLab = 'WADDL') %>% 
    distinct(ScanID, test, .keep_all = T) %>%
    dplyr::select(ScanID, Animal_ID, Capture_Date, test, TestResult, TestLab)
   
  
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
  
  
  
  con <- dbConnect(odbc::odbc(),
                   Driver = "Microsoft Access Driver (*.mdb, *.accdb)",
                   DBQ = paste0(dbpath, 'LPMS_MasterDatabase.accdb'))
  
  #con <- dbConnect(odbc::odbc(), "LPMS")
  
  for(k in 1:nrow(wide_df)){
    
    if(k == 1){
      dbWriteTable(con, "Disease", wide_df[k,], append = FALSE, overwrite = TRUE, row.names = FALSE)
    }else{
    dbWriteTable(con, "Disease", wide_df[k,], append = TRUE, row.names = FALSE)
    }
    
  }
  
  RODBC::odbcCloseAll()
  
                
}
