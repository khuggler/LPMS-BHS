failtable<-function(sad, gps, dbpath){
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
        'Bobcat Predation' = 'Bobcat'
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
        'Unknown Nonpredation' = 'Unknown'
        
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
      left_join(lastcap, by = c("AID" = "Animal_ID"))
    
    mmm<-morts %>%
      filter(is.na(CensorDate) & is.na(MortDate))
    
    # get last fix date 
    lpms<-subset(gps, Herd == "Lower Panther Main Salmon")
    lpms$acquisitiontime<-as.POSIXct(lpms$acquisitiontime, format = "%Y-%m-%d %H:%M:%S")
    
    lpms<-lpms %>%
      filter(AnimalID %in% mmm$AID) %>%
      group_by(AnimalID) %>%
      filter(acquisitiontime == max(acquisitiontime, na.rm = T)) %>%
      ungroup() %>%
      dplyr::select(AnimalID, acquisitiontime) %>%
      rename(LastFix = acquisitiontime)
   
    mmm<-mmm %>%
      left_join(lpms, by = c('AID' = 'AnimalID')) %>%
      mutate(Diff = abs(as.numeric(difftime(Sys.Date(), LastFix, units = "days")))) %>%
      filter(Diff > 30) %>%
      mutate(CensorDate = strftime(LastFix, format = "%Y-%m-%d")) %>%
      mutate(CensorType = 'Failed Collar') %>%
      dplyr::select(-LastFix, -Diff)
    
    failed<-morts %>%
      filter(!is.na(CensorDate))
    
    failslip<-rbind(mmm, failed)
    
    con <- dbConnect(odbc::odbc(),
                     Driver = "Microsoft Access Driver (*.mdb, *.accdb)",
                     DBQ = paste0(dbpath, 'LPMS_MasterDatabase.accdb'))
    
    #con <- dbConnect(odbc::odbc(), "LPMS")
    
    for(k in 1:nrow(failslip)){
      
      if(k == 1){
        dbWriteTable(con, "Failed", failslip[k,], append = FALSE, overwrite = TRUE, row.names = FALSE)
      }else{
        dbWriteTable(con, "Failed", failslip[k,], append = TRUE, row.names = FALSE)
      }
    }
    
    RODBC::odbcCloseAll()
    
    
  }