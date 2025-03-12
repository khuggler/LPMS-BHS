prevplots<-function(acc_path, collar, savewd, return_summary = T){
  
setwd(acc_path)

DBPath <- RODBC::odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                            dbq=./LPMS_MasterDatabase.accdb") #dbq=./HighlandsBHS.accdb")

movi <- RODBC::sqlQuery(DBPath, "SELECT * FROM Disease")

cap <- RODBC::sqlQuery(DBPath, "SELECT * FROM AnimalInfo")



long_data <- movi %>%
  left_join(cap[, c('EweGroup', 'Sex', 'Age_Class', 'Pkey')], by = 'Pkey') %>%
  mutate(EweGroup = case_when(AID == "230059" & CaptureDate > ymd("2024-01-01") ~ 'Tower Kriley', 
                              T ~ EweGroup)) %>%
  filter(Sex == "Female" & Age_Class != "Lamb") %>%
  filter(!is.na(EweGroup))



# Plot
ts<-ggplot(long_data, aes(x = CaptureDate, y = AID, fill = pcr)) +
  facet_grid(~EweGroup)+
  geom_tile() +
  scale_fill_manual(values = c("DETECTED" = "red", "NOT DETECTED" = "blue", "INDETERMINATE" = "gray")) +
  theme_minimal() +
  labs(title = "Movi PCR", x = "Date", y = "Individual", fill = "Test Result") +
  theme_bw()+
  theme(axis.text.y = element_blank(),  # Remove individual labels if too many
        axis.ticks.y = element_blank()) 
ts

ggsave(paste0(savewd, 'MoviTimeSeries.jpg'), ts, dpi = 500, height = 8, width = 8, units = "in")


moviprev<-long_data %>%
  mutate(BioYear = case_when(CaptureDate >= ymd("2020-05-01") & CaptureDate < ymd("2021-04-30") ~ '2020', 
                             CaptureDate >= ymd("2021-05-01") & CaptureDate < ymd("2022-04-30") ~ '2021', 
                             CaptureDate >= ymd("2022-05-01") & CaptureDate < ymd("2023-04-30") ~ '2022', 
                             CaptureDate >= ymd("2023-05-01") & CaptureDate < ymd("2024-04-30") ~ '2023', 
                             CaptureDate >= ymd("2024-05-01") & CaptureDate < ymd("2025-04-30") ~ '2024', 
                             T ~ NA)) %>%
  group_by(BioYear, EweGroup) %>%
  
  summarize(TotalSampled = n(), 
            NumberPos = sum(pcr == 'DETECTED')) %>%
  ungroup() %>%
  mutate(Prevalence = NumberPos/TotalSampled, 
         SE = sqrt(Prevalence * (1-Prevalence)/TotalSampled), 
         z = qnorm(0.975), 
         Upper = Prevalence + z*SE, 
         Lower = Prevalence - z*SE) %>%
  mutate(Prevalence = Prevalence*100, 
         Upper = Upper*100, 
         Lower = Lower*100)


prevtime<-ggplot(moviprev, aes(x = BioYear, y = Prevalence, color = EweGroup))+
  geom_point(position = position_dodge(0.75), size = 1.0) + 
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2,size = 1.0, position = position_dodge(0.75)) + 
  ylab('Movi prevalence (%)') + 
  xlab('Biological year') + 
  theme_bw()
prevtime


ggsave(paste0(savewd, 'MoviPrevalence.jpg'), prevtime, dpi = 500, height = 5, width = 6, units = "in")

if(return_summary == TRUE){
  
  write.csv(moviprev, paste0(savewd, 'MoviPrevalence.csv'), row.names = F)
}

return(moviprev)


}
