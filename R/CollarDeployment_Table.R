
collartable<-function(sad, dbpath = NULL, export = F){
  
  collars<-sad %>%
    distinct(Animal_ID, Capture_Date, .keep_all = T) %>%
    filter(Capture_GMU %in% c('21', '21A', '28')) %>%
    mutate(Capture_Date = ymd(Capture_Date)) %>%
    filter(Capture_Date > ymd("2020-01-01")) %>%
    mutate(FateDate = ymd(FateDate)) %>%
    mutate(CensorDate = ymd(CensorDate))
  
  animals<-unique(collars$Animal_ID)
  
  collar_hist<-data.frame()
  for(j in 1:length(animals)){
    sub<-subset(collars, Animal_ID == animals[j])
    sub<-sub %>%
      arrange(Capture_Date)
    
    # set up the dataframe 
    new_df <- data.frame(AID = NA)
    num_collars<-length(unique(sub$Collar_Serial_No))
    # Add dynamic columns for collar serials, start, and end
    for (i in 1:num_collars) {
      new_df[[paste("Collar", i, "Serial")]] <- NA
      new_df[[paste("Collar", i, "Start")]] <- NA
      new_df[[paste("Collar", i, "End")]] <- NA
    }
    
    # calc start and end dates for each serial number
    unique_cols<-unique(sub$Collar_Serial_No)
    for(p in 1:length(unique_cols)){
      
      subsub<-subset(sub, Collar_Serial_No == unique_cols[p])
      start<-subsub$Capture_Date
      
      if(length(start) > 1){
        start<-min(start)
      }
      
      if(nrow(subsub) == 1){
        end<-max(c(subsub$FateDate, subsub$CensorDate), na.rm = T)
      
      }else{
      #end_dates<-subset(subsub, CensorType %in% c('Shed', 'Not Retrieved', 'Failed Collar', 'Recapture', "")) ## sometimes recapture written in censor type, but not actually censored
      
      final_row<-subsub[nrow(subsub),]
      end<-max(c(final_row$CensorDate, final_row$FateDate), na.rm = T)
      # 
      # censortypes<-end_dates$CensorType ## is there a blank in censor type (she hasn't been recaptured again)
      # 
      # if("" %in% censortypes){
      #   end<-Sys.Date() + days(1)
      # }else{
      # end_date<-max(c(end_dates$CensorDate, end_dates$FateDate), na.rm = T)
      # end<-end_date
      # }
      
      }
      
      if(end == -Inf){
        end<-Sys.Date() + days(1)
      }
      
      
      new_df[[paste("Collar", p, "Serial")]] <- unique_cols[p]
      new_df[[paste("Collar", p, "Start")]] <- start
      new_df[[paste("Collar", p, "End")]] <- end
      
      new_df$AID<-sub$Animal_ID[1]
    }
    
    collar_hist<-plyr::rbind.fill(new_df, collar_hist)
    
    
  }
  
  if(!is.null(dbpath)){
  con <- dbConnect(odbc::odbc(),
                   Driver = "Microsoft Access Driver (*.mdb, *.accdb)",
                   DBQ = paste0(dbpath, 'LPMS_MasterDatabase.accdb'))
  }
  
  #con <- dbConnect(odbc::odbc(), "LPMS")
  
  if(export == TRUE){
  for(k in 1:nrow(collar_hist)){
    
    if(k == 1){
      dbWriteTable(con, "CollarDeployment", collar_hist[k,], append = FALSE, overwrite = TRUE, row.names = FALSE)
    }else{
        dbWriteTable(con, "CollarDeployment", collar_hist[k,], append = TRUE, row.names = FALSE)
    }
  }
  
}else{
  return(collar_hist)
}
  
  RODBC::odbcCloseAll()
  
}