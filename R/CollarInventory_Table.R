collarinventory<-function(sad, gps){
  
  sad<-sad %>%
    filter(Capture_GMU %in% c('21', '21A', '28')) %>%
    mutate(Capture_Date = ymd(Capture_Date)) %>%
    filter(Capture_Date >= ymd("2020-01-01")) %>%
    mutate(CensorDate = ymd(CensorDate)) %>%
    mutate(FateDate = ymd(FateDate))
 
  sadcollars<-unique(sad$Collar_Serial_No)
  
  # pull LPMS
  lpms<-subset(gps, Herd == "Lower Panther Main Salmon")
  lpms$acquisitiontime<-as.POSIXct(lpms$acquisitiontime, format = "%Y-%m-%d %H:%M:%S")
  
  lpms<-lpms %>%
    group_by(AnimalID) %>%
    distinct(acquisitiontime, .keep_all = T) %>%
    ungroup()
  
  collars<-data.frame(Serial = rep(NA, length(sadcollars)), 
                      CollarFreq = rep(NA, length(sadcollars)), 
                      CollarStatus = rep(NA, length(sadcollars)), 
                      LastFix = rep(NA, length(sadcollars)),
                      FirstDeployment = rep(NA, length(sadcollars)), 
                      FinalDeployment = rep(NA, length(sadcollars)),
                      IridiumStatus = rep(NA, length(sadcollars)) ,
                      VHFSchedule = rep(NA, length(sadcollars)), 
                      GPSFixRate = rep(NA, length(sadcollars)), 
                      TransmissionSchedule = rep(NA, length(sadcollars)))
  
  for(j in 1:length(sadcollars)){
    
    # is collar deployed?
    ss<-subset(sad, Collar_Serial_No == sadcollars[j])
    
    collars$Serial[j]<-sadcollars[j]
    collars$CollarFreq[j]<-ss$Radio_Frequency[1]
    collars$FirstDeployment[j]<-as.character(min(ss$Capture_Date, na.rm = T))
    

    lp<-subset(lpms, Serialnumber == sadcollars[j])
    collars$LastFix[j]<-as.character(max(lp$acquisitiontime))
  }
  
 
  con <- dbConnect(odbc::odbc(),
                   Driver = "Microsoft Access Driver (*.mdb, *.accdb)",
                   DBQ = "C:/Users/katey.huggler/Dropbox/PostDoc/Data/Capture/MasterDB/LPMS_MasterDatabase.accdb")
  
  #con <- dbConnect(odbc::odbc(), "LPMS")
  
  for(k in 1:nrow(collars)){
    
    if(k == 1){
      dbWriteTable(con, "Collar Inventory", collars[k,], append = FALSE, overwrite = TRUE, row.names = FALSE)
    }else{
    dbWriteTable(con, "Collar Inventory", collars[k,], append = TRUE, row.names = FALSE)
    }
  }
  
}
