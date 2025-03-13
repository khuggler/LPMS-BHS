diseasetable<-function(dis, preg, dbpath = NULL, export = F){
  
  wide_df <- dis %>%
    mutate(test = case_when(TestLab == "HUYVAERT" ~ 'BIOMEME', T ~ test)) %>%
    mutate(TestLab = 'WADDL') %>% 
    dplyr::select(ScanID, Animal_ID, Capture_Date, test, TestResult, TestLab, TestLabdate) %>%
    distinct(ScanID, Animal_ID, Capture_Date, test, TestResult, TestLabdate) %>%
    select(-TestLabdate)
   
  wide_df<- wide_df %>%
    group_by(ScanID, Animal_ID, Capture_Date, test) %>%
    filter(
      all(TestResult == "INDETERMINATE") | 
        TestResult != 'INDETERMINATE' |
        n() == 1
    ) %>%
    slice(1) %>%
    ungroup()
  
  wide_df<-wide_df %>%
    spread(key = test, value = TestResult)
  
  inhib<-dis %>%
    filter(test == "M.OVI (ELISA)") %>%
    filter(!is.na(GrowthAmount))
  
  wide_df<-wide_df %>%
    left_join(inhib[, c('ScanID', 'Animal_ID', 'GrowthAmount')], by = c('ScanID', 'Animal_ID'))
  
  
  preg_df<-preg %>%
    distinct(ScanID, PregTestType, .keep_all = T) %>%
    dplyr::select(ScanID, Animal_ID, Capture_Date, PregTestType, PregResult) %>%
    mutate(Capture_Date = as.Date(as.numeric(Capture_Date), origin = "1899-12-30")) %>%
    filter(PregTestType != "NA")
  
  preg_wid<-preg_df %>%
    spread(key = PregTestType, value = PregResult)
  
  
  wide_df<-wide_df %>%
    left_join(preg_wid, by = c("Animal_ID", "Capture_Date")) %>%
    dplyr::select(-ScanID.y)
  
  names(wide_df)<-c('ScanID', 'AID', 'CaptureDate', 'biomeme', 'elisa', 
                    'mlst', 'pcr', 'elisa_c', 'pspb', 'ultrasound')
  
  wide_df<-wide_df %>%
    filter(!is.na(CaptureDate)) %>%
    mutate(CaptureDate = ymd(CaptureDate)) %>%
    group_by(AID) %>%
    arrange(AID, CaptureDate) %>%
    mutate(CaptureNumber = row_number()) %>%
    mutate(pspb = ifelse(pspb == "NOT-PREGNANT (OPEN)", 'NOT PREGNANT', pspb)) %>%
    mutate(ultrasound = ifelse(ultrasound %in% c("NOT-PREGNANT (OPEN)", 'NOT-PREGNANT (RE-CHECK)', 'INDETERMINATE'), 'NOT PREGNANT', ultrasound)) %>%
    mutate(pregnancy = ifelse(ultrasound == "NOT PREGNANT" & !is.na(pspb), 
                              pspb, ultrasound)) %>%
    mutate(pregnancy = ifelse(is.na(ultrasound), pspb, pregnancy)) %>%
    filter(!is.na(CaptureDate)) %>%
    mutate(CaptureDate = ymd(CaptureDate)) %>%
    mutate(SampleYear = strftime(CaptureDate, format = "%Y")) %>%
    dplyr::select(ScanID, AID, CaptureDate, CaptureNumber, SampleYear, biomeme, elisa, elisa_c, pcr, mlst, pspb, ultrasound, 
           pregnancy) %>%
    mutate(Pkey = paste0(AID, "_", CaptureNumber))
  
  
  if(!is.null(dbpath)){
  con <- dbConnect(odbc::odbc(),
                   Driver = "Microsoft Access Driver (*.mdb, *.accdb)",
                   DBQ = paste0(dbpath, 'LPMS_MasterDatabase.accdb'))
  
  }
  
  #con <- dbConnect(odbc::odbc(), "LPMS")
  if(export == TRUE){
  for(k in 1:nrow(wide_df)){
    
    if(k == 1){
      dbWriteTable(con, "Disease", wide_df[k,], append = FALSE, overwrite = TRUE, row.names = FALSE)
    }else{
    dbWriteTable(con, "Disease", wide_df[k,], append = TRUE, row.names = FALSE)
    }
    
  }
  }else{
    return(wide_df)
  }
  
  RODBC::odbcCloseAll()
  
                
}
