adultsurvival<-function(acc_path, yearstart, yearend, savewd){

setwd(acc_path)

DBPath <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                            dbq=./LPMS_MasterDatabase.accdb") #dbq=./HighlandsBHS.accdb")

morts<-sqlQuery(DBPath, "SELECT * FROM Mortalities")
fails<-sqlQuery(DBPath, "SELECT * FROM Failed")

morts<-plyr::rbind.fill(morts, fails)


aid <- sqlQuery(DBPath, "SELECT * FROM AnimalInfo") %>%
  left_join(morts, by = 'AID') %>%
  group_by(AID) %>%
  mutate(CensorDate = as.POSIXct(CensorDate)) %>%
  mutate(EntryDate = min(Capture_Date, na.rm = T), 
         DLS = case_when(!is.na(MortDate) | !is.na(CensorDate) ~ min(MortDate, CensorDate, na.rm = T), 
                         T ~ NA)) %>%
  mutate(DLS = case_when(is.na(DLS) ~ as.POSIXct(Sys.Date()), 
         T ~ DLS)) %>%
  distinct(AID, .keep_all = T) %>%
  mutate(Mortality = case_when(is.na(Mortality) ~ 0, 
                               T ~ Mortality))



aid<-aid %>%
  filter(Sex.x == "Female" & Age_Class != 'Lamb')




#' ==================================
#' STEP 3: FORMAT SURVIVAL DATA 
#' ==================================

#' update end year if necessary
bio.years<-data.frame(Year = yearstart:yearend)
bio.years$StartDate<-as.Date(paste0(bio.years$Year, "-", "05-01"), format = "%Y-%m-%d")
bio.years$EndDate<-as.Date(paste0(bio.years$Year + 1, "-", "04-30"), format = "%Y-%m-%d")




#' list of animal IDs
uni<-unique(aid$AID)

adult.survival<-data.frame()
for(i in 1:length(uni)){
  sub<-data.frame(aid[aid$AID == uni[i],])
  
  # date<-ifelse(is.na(unique(sub[nrow(sub), 'DLS'])) %in% FALSE, 
  #              as.character(max(sub[,'DLS'], na.rm = T)), NA)
  # 
  # if(!is.na(date)){
  #   date<-as.Date(date, format = "%Y-%m-%d")
  #   mort<-1
  # }
  # 
  # if(is.na(date)){
  #   date<-as.Date(sub$DLS, format= "%Y-%m-%d")
  #   mort<-0
  # }
  # 
  
  start<-as.Date(sub$EntryDate, format = "%Y-%m-%d")
  end<-as.Date(sub$DLS, format = "%Y-%m-%d")
  
  #' remove years where the animal was not alive or in the study
  
  bio.years$start.seq<-ifelse(start >= bio.years$StartDate & start <= bio.years$EndDate, 1, 0)
  bio.years$end.seq<-ifelse(end >= bio.years$StartDate & end <= bio.years$EndDate, 1, 0)
  start.row<-which(bio.years$start.seq == 1)
  end.row<-which(bio.years$end.seq == 1)
  
  
  if(identical(end.row, integer(0)) == TRUE){
    end.row<-nrow(bio.years)
  }
  
  if(identical(start.row, integer(0)) == TRUE){
    start.row<-1
  }
  
  
  
  bio.year.new<-bio.years[start.row:end.row,]
  end.row<-nrow(bio.year.new)
  
  bio.year.new$start<-ifelse(bio.year.new$start.seq == 1, as.character(start), NA)
  bio.year.new$end<-ifelse(bio.year.new$end.seq == 1, as.character(end), NA)
  
  c<-nrow(sub)
  
  bio.year.new$aid<-sub[, 'AID'][1]
  bio.year.new[end.row, 'cause']<-sub$MortCause
  
  bio.year.new$mort<-0
  bio.year.new[end.row, 'mort']<-sub$Mortality
  
  bio.year.new$Sex<-sub$Sex.x
  bio.year.new$Herd<-sub$EweGroup
  bio.year.new$DOB<-sub$DOB
  
  bio.year.new$StartTime<-ifelse(is.na(bio.year.new$start),as.character(bio.year.new$StartDate), as.character(bio.year.new$start))
  bio.year.new$EndTime<-ifelse(is.na(bio.year.new$end), as.character(bio.year.new$EndDate), as.character(bio.year.new$end))
  
  bio.year.new$StartTime<-as.Date(bio.year.new$StartTime,format = "%Y-%m-%d")
  bio.year.new$EndTime<-as.Date(bio.year.new$EndTime, format = "%Y-%m-%d")
  
  bio.year.new$TimeAlive<-abs(as.numeric(difftime(bio.year.new$StartTime, bio.year.new$EndTime, units = "days")/30.5))
  
  # cc<-stringr::str_detect(sub$Comments, c('Chronic Carrier removal', 'Chronic carrier removal'))
  # bio.year.new$CCRemoval<-ifelse(TRUE %in% cc, 'yes', 'no')
  
  #' censors<-'CollarFailure'
  #' 
  #' #' add event information
  #' x<-nrow(bio.year.new)
  #' bio.year.new$status<-ifelse(is.na(bio.year.new$end),0, NA)
  #' 
  #' bio.year.new$status<-ifelse(!is.na(bio.year.new$end) & bio.year.new$mort == 0, 0, bio.year.new$status)
  #' 
  #' bio.year.new$status<-ifelse(!is.na(bio.year.new$end) & bio.year.new$mort == 1, 1, bio.year.new$status)
  #' 
  #' bio.year.new$status<-ifelse(bio.year.new$status == 1 & bio.year.new$cause %in% censors, 0, bio.year.new$status)
  #' 
  print(bio.year.new)
  
  adult.survival<-rbind(bio.year.new, adult.survival)
}



years<-unique(adult.survival$Year)

for(j in 1:length(years)){
  
  # Bio-Year 2021-2022
  adult<-subset(adult.survival, Year == years[j])
  adult.survivalyear<- adult%>%
    mutate(StartDate = as.character(StartDate), EndDate = as.character(EndDate))%>%
    mutate(StartDatenew = ifelse(start.seq == "1", start, ifelse(start.seq == "0", StartDate, 0)), 
           EndDatenew = ifelse(end.seq == "1", end, ifelse(end.seq == "0", EndDate, 0)))%>%
    mutate(StartDatenew = as.Date(StartDatenew, format = "%Y-%m-%d" )) %>%
    mutate(EndDatenew = as.Date(EndDatenew, format = "%Y-%m-%d" ))%>%
    mutate(startmonth = format(StartDatenew, "%m"), endmonth = format(EndDatenew, "%m"))%>% #extract month from start and end dates
    mutate(Start = recode(startmonth, "05" = 0, "06" = 2, "07" = 3, "08" = 4, "09" = 5, "10" = 6, "11" = 7, 
                          "12" = 8, "01" = 9, "02" = 10, "03" = 11, "04" = 12), End =  recode(endmonth, 
                                                                                              "05" = 1, "06" = 2, "07" = 3, "08" = 4, "09" = 5, "10" = 6, "11" = 7, 
                                                                                              "12" = 8, "01" = 9, "02" = 10, "03" = 11, "04" = 12)) %>% #recode month to centralize around bio year
    select(c("Year", "aid", "cause", "StartDatenew", "EndDatenew", "Start", "End", "mort", "Sex", "Herd"))
  
  # sometimes animals die within the same month (start date = end date)
  adult.survivalyear$Start<-ifelse(adult.survivalyear$Start == adult.survivalyear$End, adult.survivalyear$End - 0.10, adult.survivalyear$Start)
  
  # survival analysis
  
  library(survival)
  library(survminer)
  survival<- survSplit(Surv(Start, End, mort)~.,
                       data = adult.survivalyear,
                       cut = 1:12,
                       end = "End", 
                       start = "Start")
  
  # females only 
  km_fit<-survfit(Surv(Start, End, mort) ~ Sex + Herd, data = survival)
  summary(km_fit)
  res = summary(km_fit, time = 1:12)
  
  savedf <- as.data.frame(res[c("strata", "time", "n.risk", "n.event", "surv", "std.err", "lower", "upper")])
  
  #' format survival table
  library(stringr)
  savedf[c('Herd', 'Sex')] <- str_split_fixed(savedf$strata, ',', 2)
  
  herd_survival <- savedf%>%
    mutate(Herd = str_remove_all(Herd, "Herd="), Sex = str_remove_all(Sex, "Sex="))%>%
    group_by(Herd, Sex)%>%
    mutate(n.risk = max(n.risk), n.event = sum(n.event))%>%
    filter(time == 12)%>%
    mutate(Year = years[j])%>%
    select(-c("strata", "time"))%>%
    select(Year, Herd, Sex, n.risk, n.event, surv, std.err, lower, upper)
  
  
  
  setwd(savewd)
  write.csv(herd_survival, paste0("BioYear_", years[j], ".csv"))
  
  #' survival curve graphs
  my_labels<-c('May', 'Nov', 'Apr')
  ggsurv<-ggsurvplot(km_fit, conf.int = TRUE, censor = F, color='Sex',
                     legend.title="Sex",legend="bottom",
                     ggtheme=theme_bw())
  ggsurv=ggsurv$plot + facet_wrap(~Herd, ncol=4)
  ggsurv = ggsurv + labs(x = " ", y = "Survival")+
    ylim(0.2, 1)+
    scale_x_continuous(breaks= c(0,6,12), labels = my_labels)+
    ggtitle("Biological Year Survival")+
    theme(plot.title = element_text(hjust= 0.5), 
          legend.text = element_text(size = 10), 
          legend.title = element_blank(), 
          strip.text = element_text(size = 10), 
          title = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          axis.text = element_text(size = 10), 
          panel.spacing = unit(2, "lines"), 
          plot.margin = margin(c(0.25,0.25,0.25,0.25), unit = "cm"))
  
  ggsurv
  
  ggsave(paste0(savewd, "BioYear_", years[j], "_km.jpg"), ggsurv, device = "jpg", width = 5, height = 5, units = "in", dpi = 500)
  
  


  
}

}



