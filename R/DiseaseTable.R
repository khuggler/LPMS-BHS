diseasetable<-function(dis, preg, vir, par, export_morts = F, dbpath = NULL, export = F){
  
  wide_df <- dis %>%
    mutate(test = case_when(TestLab == "HUYVAERT" ~ 'BIOMEME', T ~ test)) %>%
    mutate(TestLab = 'WADDL') %>% 
    filter(UNIT %in% c('21', '21A', '28')) %>%
    mutate(Animal_ID = AnimalID) %>%
    filter(AnimalID != "NA") %>%
    dplyr::select(ScanID, TestCaseNo, Animal_ID, Capture_Date, SOURCE, SampleType, test, TestResult, TestLab, TestLabdate) %>%
    # remove duplicate rows 
    distinct(ScanID, Animal_ID, Capture_Date, test, TestResult, TestLabdate, .keep_all = T) %>%
    mutate(Capture_Date = as.Date(as.numeric(Capture_Date), origin = "1899-12-30")) %>%
    mutate(test = case_when(test == "SWAB NASAL" ~ "M.OVI (PCR)", 
                            test == "CULTURE" ~ TestResult, 
                            T ~ test)) %>%
    mutate(SampleType = case_when(SampleType %in% c('SWAB NASAL DRY', 'SWAB', 'SWAB NASAL', 'SWAB NASAL DRY 2') ~ 'NASAL', 
                                  SampleType == 'SWAB OP TSB' ~ 'OP_TSB', 
                                  SampleType == 'SWAB NASAL TSB' ~ 'NASAL_TSB',
                                  SampleType == 'SWAB BRONCHIAL' ~ 'BRONCHIAL', 
                                  T ~ SampleType)) %>%
    mutate(test = case_when(test == 'M.OVI (PCR)' ~ paste0(test, "_", SampleType), 
                            T ~ test)) %>%
    filter(test %in% c('M.OVI (PCR)_LUNG', 'M.OVI (PCR)_BRONCHIAL', 'M.OVI (PCR)_NASAL', 'M.OVI (ELISA)', 'M.OVI (MLST)', 'BIOMEME', 'HISTOLOGY (M.OVI NASAL SINUS TUMOR)', 'BIBERSTEINIA TREHALOSI', 'MANHEIMIA HAEMOLYTICA', 'MIXED BACTERIA')) %>%
    dplyr::select(-TestLabdate, -SampleType)
  
  vir_df <- vir %>%
    #mutate(test = case_when(TestLab == "HUYVAERT" ~ 'BIOMEME', T ~ test)) %>%
    mutate(TestLab = 'WADDL') %>% 
    filter(UNIT %in% c('21', '21A', '28')) %>%
    mutate(Animal_ID = AnimalID) %>%
    filter(AnimalID != "NA") %>%
    dplyr::select(ScanID, TestCaseNo, Animal_ID, Capture_Date, SOURCE, Test, result, TestLab, TestLabDate) %>%
    # remove duplicate rows 
    distinct(ScanID, Animal_ID, Capture_Date, Test, result, TestLabDate, .keep_all = T) %>%
    mutate(Capture_Date = as.Date(as.numeric(Capture_Date), origin = "1899-12-30")) %>%
    mutate(Test = case_when(Test == "MCF (ELISA" ~ "MCF (ELISA)", 
                            T ~ Test)) %>%
    filter(Test %in% c('BRSV (VN)', 'IBR (VN)', 'PIV3 (VN)', 'MCF (ELISA)', 'BTV (ELISA)', 'EHD (ELISA)', 'BTV (PCR)', 'EHD (PCR)', 'BTV (cELISA)', 'EHD (cELISA)')) %>%
    dplyr::select(-TestLabDate)
  
  names(vir_df)<-names(wide_df)
  
  
  # parasites
  par_df <- par %>%
    #mutate(test = case_when(TestLab == "HUYVAERT" ~ 'BIOMEME', T ~ test)) %>%
    mutate(ParaTestLab = 'WADDL') %>% 
    filter(UNIT %in% c('21', '21A', '28')) %>%
    mutate(Animal_ID = AnimalID) %>%
    filter(AnimalID != "NA") %>%
    dplyr::select(ScanID, ParaTestLabNo, Animal_ID, Capture_Date, SOURCE, ParaTestType, ParaIdentified, ParaTestLab, ParaDate) %>%
    # remove duplicate rows 
    distinct(ScanID, Animal_ID, Capture_Date, ParaTestType, ParaIdentified, ParaDate, .keep_all = T) %>%
    mutate(Capture_Date = as.Date(as.numeric(Capture_Date), origin = "1899-12-30")) %>%
    mutate(ParaDate = as.Date(as.numeric(ParaDate), origin = "1899-12-30")) %>%
    mutate(ParaTestType = case_when(ParaTestType == 'HISTOLOGY' ~ ParaIdentified, 
                                    ParaTestType == 'PARASITE ID' ~ ParaIdentified, 
                                    T ~ ParaTestType)) %>%
    filter(ParaTestType %in% c('TOXOPLASMA (IFA)', 'NEMATODIRUS SPP', 'SARCOCYSTIS SP', 'CESTODE SP', 'PSOROPTES OVIS', 'BAERMANN', 'O/P FLOAT', 'OTOBIUS SPP', 'ROUNDWORM', 'DERMACENTOR SPP')) %>%
    dplyr::select(-ParaDate)
  
  names(par_df)<-names(wide_df)
  
  
  
  wide_df<-rbind(wide_df, vir_df, par_df)
  
  if(export_morts == TRUE){
    wide_df<-wide_df %>%
      filter(SOURCE %in% c('WILD-MORT', 'HUNTER-SURVEILLANCE')) 
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
        
    names(wide_df)<-c('ScanID', 'WADDLCaseNo', 'AID', 'CaptureDate', 'Source', 'TestLab', 
                      'baermann', 'btv', 'cestode', 'ehd', 'sinustumor', 'elisa', 
                                                                                            'mlst', 'pcr_bronchial', 'pcr_lung', 'pcr_nasal', 'nematode', 'o/p float', 'roundworm', 'sarcocystis', 'elisa_c')                                                                 
                                                                          
    
  }else{
    wide_df<-wide_df %>%
      filter(SOURCE == 'WILD')
  
   
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
    dplyr::select(-ScanID.y) %>%
    filter(!is.na(Animal_ID)) %>%
    filter(Animal_ID != "NA")
  
  names(wide_df)<-c('ScanID', 'WADDLCaseNo', 'AID', 'CaptureDate', 'Source', 'TestLab', 'baermann', 'b_trehalosi', 'biomeme', 'brsv', 'btv_celisa', 'btv_elisa', 'dermacentor', 'ehd_celisa', 'ehd_elisa', 'ibr', 'elisa', 'mlst', 'pcr_nasal', 'mcf_elisa', 'mixed_bac', 'otobius', 'piv3', 'psoroptes', 'toxoplasma', 'elisa_c', 'pspb', 'ultrasound')
  
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
    dplyr::select('ScanID', 'WADDLCaseNo', 'AID', 'CaptureDate', 'Source', 'TestLab', 'baermann', 'biomeme', 'brsv', 'btv_celisa', 'btv_elisa', 'dermacentor', 'ehd_celisa', 'ehd_elisa', 'ibr', 'elisa', 'mlst', 'pcr_nasal', 'mcf_elisa', 'otobius', 'piv3', 'psoroptes', 'toxoplasma', 'elisa_c', 'pspb', 'ultrasound', 'pregnancy', 'CaptureNumber') %>%
    mutate(Pkey = paste0(AID, "_", CaptureNumber))
  
  }
  
  wide_df<-wide_df %>%
    group_by(AID, CaptureDate) %>%
    summarise(
      across(everything(), ~ first(na.omit(.))),  
      .groups = "drop"
    )
  
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
