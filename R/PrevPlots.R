prevplots<-function(acc_path, cap, savewd){
  
setwd(acc_path)

DBPath <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                            dbq=./LPMS_MasterDatabase.accdb") #dbq=./HighlandsBHS.accdb")

movi <- sqlQuery(DBPath, "SELECT * FROM MoviHistory")


long_data <- movi %>%
  mutate(across(c(DateRecord, `PCR Record`, `ELISA Record`), as.character)) %>%  # Ensure character type
  separate_rows(DateRecord, `PCR Record`, `ELISA Record`, sep = "\\|")  %>%
  mutate(Date = myd(paste0(DateRecord, "-01"))) %>%
  filter(Date > ymd("2021-09-1")) %>%
  left_join(cap[, c('Animal ID', 'EweGroup')], by = c("AID" = 'Animal ID')) %>%
  mutate(EweGroup = case_when(AID == "230059" & Date > ymd("2024-01-01") ~ 'Tower Kriley', 
                              T ~ EweGroup)) %>%
  filter(!is.na(EweGroup))



# Plot
ts<-ggplot(long_data, aes(x = Date, y = AID, fill = `PCR Record`)) +
  facet_grid(~EweGroup)+
  geom_tile() +
  scale_fill_manual(values = c("POS" = "red", "NEG" = "blue", "IND" = "gray")) +
  theme_minimal() +
  labs(title = "Movi PCR", x = "Date", y = "Individual", fill = "Test Result") +
  theme_bw()+
  theme(axis.text.y = element_blank(),  # Remove individual labels if too many
        axis.ticks.y = element_blank()) 
ts

ggsave(paste0(savewd, 'MoviTimeSeries.jpg'), ts, dpi = 500, height = 8, width = 8, units = "in")


moviprev<-long_data %>%
  mutate(BioYear = case_when(Date >= ymd("2020-05-01") & Date < ymd("2021-04-30") ~ '2020', 
                             Date >= ymd("2021-05-01") & Date < ymd("2022-04-30") ~ '2021', 
                             Date >= ymd("2022-05-01") & Date < ymd("2023-04-30") ~ '2022', 
                             Date >= ymd("2023-05-01") & Date < ymd("2024-04-30") ~ '2023', 
                             Date >= ymd("2024-05-01") & Date < ymd("2025-04-30") ~ '2024', 
                             T ~ NA)) %>%
  group_by(BioYear, EweGroup) %>%
  summarize(TotalSampled = n(), 
            NumberPos = sum(`PCR Record` == 'POS')) %>%
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
  ylab('Movi prevalence') + 
  xlab('Biological year') + 
  theme_bw()
prevtime


ggsave(paste0(savewd, 'MoviPrevalence.jpg'), prevtime, dpi = 500, height = 5, width = 6, units = "in")


return(moviprev)

}
