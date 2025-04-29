gpsdata<-function(sad, collar, cap, ss, ATS_usrs, ATS_pass, telonic_usrs, telonic_pass, veckeys, dbpath = NULL, export = F){
  
  # get the collars to pull
  saddat<-animalinfo(sad, collar, cap,ss, export = F)
  uni_collars<-unique(saddat$Serial)
  
  
  ## get Telonics data 
  
  if(!dir.exists(tempdir)){
    dir.create(tempdir)
  }
  
  full.tel<-data.frame()
  tel<-NA
  
  for(l in 1:length(telonic_usrs)){
      username = telonic_usrs[l]
      password =telonic_pass[l]   # must be \\ slashes
      TDC_path = "C:\\Program Files (x86)\\Telonics\\Data Converter\\TDC.exe"
      keep.reports = FALSE
      
      # create a folder for telonics data to go
      fldr_out<-paste0(tempdir, "/", 'TelonicsGPSData')
      if(!dir.exists(fldr_out)){
        dir.create(fldr_out)
      }
      
      if(all(c("processx","sf") %in% installed.packages()[, 1]) == FALSE) 
        stop("You must install the following packages: processx and sf")
      
      # some tests
      if(length(dir(fldr_out))>1)
        stop("Your fldr_out must be empty to proceed!")
      
      # create a reports folder
      fldr_reports <- paste0(fldr_out, "\\reports")
      dir.create(fldr_reports)
      
      
      # create the xml file
      txt <- paste("<BatchSettingsV2>",
                   "\t<Globalstar>", 
                   paste0("\t\t<Username>",username,"</Username>"),
                   paste0("\t\t<Password>",password,"</Password>"),
                   "\t</Globalstar>", 
                   "\t<DownloadData>true</DownloadData>",
                   "\t<ConvertAllData />",
                   paste0("\t<BatchLog>",fldr_out,"\\BatchLog.txt</BatchLog>"),
                   paste0("\t<OutputFolder>",fldr_reports,"</OutputFolder>"),
                   "<ReportFileMode>overwrite</ReportFileMode>",
                   "</BatchSettingsV2>",
                   sep="\n")
      
      Batch_path <- paste0(fldr_out, "\\TelonicsBatchFile.xml")
      # save the xml file
      cat(txt, file=Batch_path)
      
      print("Downloading data from Telonics")
      Batch_path = paste0("/batch:", Batch_path)  # create new batch path for processx
      processx::run(TDC_path, Batch_path)  # TDC should be closed on your computer
      
      print("Importing CSV files")
      #Import the csv files from batch ####
      # Create a list of the data you will import
      fls <- list.files(fldr_reports, ".csv$")
      
      ## Run a loop that goes over the list, cleans and merges the data
      # Create an empty data frame where all the individuals will be merged in
      fixes <- do.call(rbind, lapply(1:length(fls), function(i){
        # The skip parameter is because there is some meta information above where the recordings begin
        df.i = read.csv(paste0(fldr_reports,"/",fls[i]), skip = 22, header = TRUE)
        
        # Get the ID and add it as a column (I am using the name the file is saved under and extracting the
        # component that will match with the way it is saved in my meta data column)
        df.i$CollarSerialNumber <- substr(fls[i], 1, 7)
        
        # Isolate the cases with a successful fix attempt
        df.i <- df.i[which(df.i$GPS.Fix.Attempt=="Succeeded"),]
        
        print(paste0(nrow(df.i), " rows of data imported for individual: ", substr(fls[i], 1, 7)))
        
        # Work on the DateTime stamp. It is a character string so I will first convert it to POSIXct
        # I always try to avoid deleting raw data (you never know when you will need it) so I will create a new DateTime Column
        df.i$GPS.Fix.Time = as.POSIXct(df.i$GPS.Fix.Time, format="%Y.%m.%d %H:%M:%S", tz = "UTC")
        
        return(df.i)
        
      }))
      
      # order by serial number and then by date
      fixes <- fixes[order(fixes$CollarSerialNumber, fixes$GPS.Fix.Time),]
      
      
      tel<-data.frame(fixes)
      
      tel <- tel %>%
        rename(tdate = "GPS.Fix.Time", x = "GPS.Longitude", y = "GPS.Latitude", SN = "CollarSerialNumber") %>%
        dplyr::select(SN, tdate, x, y) %>%
        mutate(tdate = lubridate::with_tz(tdate, tzone = "America/Denver"))
      
      full.tel<-rbind(tel, full.tel)
      
      
      
    }
    
    # remove all the temporary files if keep.reports = FALSE
    
    fls = dir(fldr_out, full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
    unlink(fls, force=TRUE, recursive = TRUE)
    
    
    
    
    ## -------------------- ## 
    ## ATS DATA             ## 
    ## -------------------- ##
    
    # ATS data 
    ats<-NA
    ats.full<-data.frame()

    for (i in 1:length(ATS_usrs)){
        collar::ats_login(ATS_usrs[i], ATS_pass[i])
        
        out.acct <- collar::fetch_ats_positions(device_id = uni_collars)
        
        
                # ats timezones are programmed to individual collars, all of our as of Jan 2021 are programmed to Pacific time
        programmed_pacific <- c("043205", "043202", "043204", "043207",
                                "042803", "042804","046977","047204", "047205", "047207", "047208", "047212", "047213", "047214")
        programmed_mountain <- c("030230", "042800", "048782", "048783", "048784", "048785", "048786", "048787", "048788", "048789", "048790", "048791", "048792", "048793", "048794", "048796", "048797") # we can add collar ids as new collars are deployed
        
        #out <- dplyr::bind_rows(out.acct.1, out.acct.2, out.acct.3) 
        
        out.acct$JulianDay = formatC(out.acct$JulianDay, width = 3, format = "d", flag = "0")
        out.acct$Hour = formatC(out.acct$Hour, width = 2, format = "d", flag = "0")
        out.acct$Minute = formatC(out.acct$Minute, width = 2, format = "d", flag = "0")
        
        
        ats <- out.acct %>%
          rename(tdate = "DateLocal", SN = 'CollarSerialNumber', x = 'Longitude', y = 'Latitude') %>%
          dplyr::select(SN, tdate, x, y)
        ats<-data.frame(ats)
        
        collar::ats_logout()
        
        ats.full<-rbind(ats, ats.full)
        
        
      }
      
    
    
    ## -------------------- ## 
    ## VECTRONIC DATA             ## 
    ## -------------------- ##
    
  
    vec<-NA
  
      
      key_path <- collar::get_paths(veckeys)
      vecdat<-collar::fetch_vectronics(key_path, type = "gps")
      
      vec <- vecdat %>%
        rename(tdate = "acquisitiontime", SN = 'idcollar', x = 'longitude', y = 'latitude') %>%
        dplyr::select(SN, tdate, x, y)
      
      vec$tdate <-as.POSIXct(vec$tdate,
                             format = paste0("%Y-%m-%d", "T", "%H:%M:%S"),
                             tz = "UTC",
                             origin = vec$tdate)
      vec<-data.frame(vec)
      
      vec$tdate<-lubridate::with_tz(vec$tdate, tzone = 'America/Denver')
    
    
    # bind all the data together 
    gps<-rbind(ats.full, vec, full.tel)
    
    gps<-subset(gps, SN %in% uni_collars)
    
  
    ############################################################################
    
    collar<-collartable(sad, export = F)
  
    
   gpslist<-list()
    for(j in 1:nrow(collar)){
      sub<-collar[j,]
      
      new_df <- sub[, !is.na(sub), drop = FALSE]
      
      collar_numbers <- unique(gsub("Collar ([0-9]+) .*", "\\1", grep("Collar", names(new_df), value = TRUE)))
      
      final_gps<-data.frame()
      for (i in collar_numbers) {
        serial_col <- paste0("Collar ", i, " Serial")
        start_col <- paste0("Collar ", i, " Start")
        end_col <- paste0("Collar ", i, " End")
        
        # Check if these columns exist and aren't NA
        if (all(c(serial_col, start_col, end_col) %in% names(new_df))) {
          print(paste("Processing Collar", i, "for AID", sub["AID"]))
          print(sub[c(serial_col, start_col, end_col)])
        }
        
        subgp<-subset(gps, SN == sub[1,serial_col] & tdate > sub[1,start_col] + days(2) & tdate <= sub[1,end_col] - days(1))
        if(nrow(subgp) == 0){
          print(paste('Animal ID', sub$AID, 'does not have any data'))
          next
        }else{
        subgp$AID<-sub$AID
        }
        
        
        final_gps<-rbind(subgp, final_gps)
        
      }
      
      if(nrow(final_gps) == 0){next}
      
      ## remove days around capture ##
      sadsub<-subset(saddat, AID == final_gps$AID[1])
      dates<-sadsub$Capture_Date
      
      differences<- outer(final_gps$tdate, dates, FUN = function(x, y) abs(difftime(x, y, units = "days")))
      within_2_days <- apply(differences, 1, function(x) any(x <= 2))
      
      # Filter out locations within 2 days of capture
      filtered_locations <- final_gps[!within_2_days, ]
      
      
      gpslist[[j]]<-filtered_locations
      
      print(paste('Completed Animal ID', sub$AID))
      print(head(final_gps))
    }
   
   
   gpsfull<-data.table::rbindlist(gpslist)
  
   
   gpsfull<-gpsfull %>%
     left_join(saddat[, c('AID', 'EweGroup', 'DOB', 'AgeatCapYears', 'Sex')], by = "AID") %>%
     distinct(AID, tdate, .keep_all = T)
   
   if(!is.null(dbpath)){
     con <- dbConnect(odbc::odbc(),
                      Driver = "Microsoft Access Driver (*.mdb, *.accdb)",
                      DBQ = paste0(dbpath, 'LPMS_MasterDatabase.accdb'))
   }
   
   #con <- dbConnect(odbc::odbc(), "LPMS")
   
   if(export == TRUE){
     for(k in 1:nrow(gpsfull)){
       
       if(k == 1){
         dbWriteTable(con, "GPSData", gpsfull[k,], append = FALSE, overwrite = TRUE, row.names = FALSE)
       }else{
         dbWriteTable(con, "GPSData", gpsfull[k,], append = TRUE, row.names = FALSE)
       }
     }
     
   }else{
     return(gpsfull)
   }
   
   RODBC::odbcCloseAll()
    
}