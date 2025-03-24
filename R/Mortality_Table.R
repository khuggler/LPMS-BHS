mortable<-function(sad, dis, dbpath = NULL, export = F){
  
  mortab<-sad %>%
    distinct(Animal_ID, Capture_Date, .keep_all = T) %>%
    filter(Capture_GMU %in% c('21', '21A', '28')) %>%
    mutate(Capture_Date = ymd(Capture_Date)) %>%
    filter(Capture_Date > ymd("2020-01-01")) %>%
    mutate(FateDate = ymd(FateDate)) %>%
    mutate(CensorDate = ymd(CensorDate))
  
  animals<-unique(mortab$Animal_ID)
  
  morts<-data.frame(AID = rep(NA, length(animals)), Sex = rep(NA, length(animals)), 
       MortDate = rep(NA, length(animals)), MortCause = rep(NA, length(animals)), 
       CensorDate = rep(NA, length(animals)), CensorType = rep(NA, length(animals)), 
       Mortality = rep(NA, length(animals)))
  
  
  for(j in 1:length(animals)){
    sub<-subset(mortab, Animal_ID == animals[j])
    sub<-sub %>%
      arrange(Capture_Date)
    
    # for some unknown reason, reacptures are censored the day they are recpatured, need to change that 
    sub$CensorDate<-ifelse(sub$CensorType == "Recapture", NA, as.character(sub$CensorDate))
    sub$CensorDate<-ymd(sub$CensorDate)
    
    morts$AID[j]<-animals[j]
    morts$Sex[j]<-sub$Sex[1]
    
    # did the animal die?
    mortcheck<-is.na(sub$FateDate)
    censorcheck<-is.na(sub$CensorDate)
    
    if(FALSE %in% mortcheck){ # the animal does have a mort date
      mortdate<-max(sub$FateDate, na.rm = T)
      mortcause<-sub$FateDesc[nrow(sub)]
      censordate<-NA
      censortype<-NA
      
    }else if(FALSE %in% censorcheck & !FALSE %in% mortcheck){ # there is a censor date, no mortdate
      mortdate<-NA
      mortcause<-NA
      censordate<-as.character(max(sub$CensorDate, na.rm = T))
      censortype<-sub$CensorType[nrow(sub)]
    }else{
      mortdate<-NA
      mortcause<-NA
      censordate<-NA
      censortype<-NA
    }
    
    morts$MortDate[j]<-as.character(mortdate)
    morts$MortCause[j]<-mortcause
    morts$CensorDate[j]<-as.character(censordate)
    morts$CensorType[j]<-censortype
    
    morts$Mortality[j]<-ifelse(!is.na(mortdate), 1, 0)
    
    
    ## change some mortality causes 
    
    mortcauses<-c(
      'Control/Removal' = 'Carrier Removal', 
      'Lion Predation' = 'Mountain Lion', 
      'Harvested' = 'Hunter Harvest', 
      'Coyote Predation' = 'Coyote', 
      'Accident (Auto)' = 'Car', 
      'Bobcat Predation' = 'Bobcat', 
      'Capture Myopathy' = 'Capture Mortality', 
      'Capture Mortality' = 'Capture Mortality'
    )
    
    mortcat<-c(
      'Control/Removal' = 'Carrier Removal', 
      'Lion Predation' = 'Predation', 
      'Coyote Predation' = 'Predation', 
      'Bobcat Predation' = 'Predation', 
      'Unknown Predation' = 'Predation',
      'Diseased' = 'Disease', 
      'Harvested' = 'Human-Related', 
      'Accident (Auto)' = 'Human-Related', 
      'Accident' = 'Accident', 
      'Unknown' = 'Unknown', 
      'Capture Mortality' = 'Capture Mortality', 
      'Unknown Nonpredation' = 'Unknown', 
      'Hunter Harvest' = 'Human-Related', 
      'Mountain Lion' = 'Predation', 
      'Coyote' = 'Predation', 
      'Bobcat' = 'Predation'
      
    )
    
    
    morts<-morts %>%
      mutate(Mortality = ifelse(MortCause %in% c("Control/Removal", 'Capture Mortality'), 0, Mortality)) %>%
      mutate(MortCategory = MortCause) %>%
      mutate(MortCause = stringr::str_replace_all(MortCause, mortcauses)) %>%
      mutate(MortCategory = stringr::str_replace_all(MortCategory, mortcat)) %>%
      mutate(MortCause = ifelse(MortCause == "Accident (Auto)", 'Car', MortCause)) %>%
      mutate(MortCategory = ifelse(MortCategory == "Car", 'Human-Related', MortCategory))
    }
  
  lastcap<-sad %>%
    group_by(Animal_ID) %>%
    arrange(Animal_ID, Capture_Date) %>%
    filter(Capture_Date == max(Capture_Date, na.rm = T)) %>%
    dplyr::select(Animal_ID, Capture_Date) %>%
    rename(LastCaptureDate = Capture_Date)
  
  morts<-morts %>%
    mutate(MortDate = ymd(MortDate)) %>%
    mutate(LastKnownAlive = MortDate - days(1)) %>%
    left_join(lastcap, by = c("AID" = "Animal_ID")) %>%
    filter(!is.na(MortDate))
  
  disease<-diseasetable(dis, preg = NULL, export_morts = T, export = F)
  
  morts<-morts %>%
    left_join(disease[, c('AID', 'elisa', 'mlst', 'pcr', 'elisa_c')], by = 'AID')

  if(!is.null(dbpath)){
  con <- dbConnect(odbc::odbc(),
                   Driver = "Microsoft Access Driver (*.mdb, *.accdb)",
                   DBQ = paste0(dbpath, 'LPMS_MasterDatabase.accdb'))
  }
  
  #con <- dbConnect(odbc::odbc(), "LPMS")
  if(export == TRUE){
  for(k in 1:nrow(morts)){
    
    if(k == 1){
      dbWriteTable(con, "Mortalities", morts[k,], append = FALSE, overwrite = TRUE, row.names = FALSE)
    }else{
    dbWriteTable(con, "Mortalities", morts[k,], append = TRUE, row.names = FALSE)
    }
  }
  }else{
    return(morts)
  }
  
  RODBC::odbcCloseAll()
  
  
}