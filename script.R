library(tidyverse)
library(readxl)

# Read Data ---------------------------------------------------------------
badghis_data <- read_excel_func("output/province/ACO_SMART_Survey_2022_Badghis_2022-05-16.xlsx")
faryab_data <- read_excel_func("output/province/ACO_SMART_Survey_2022_Faryab_2022-05-16.xlsx")
ghor_data <- read_excel_func("output/province/ACO_SMART_Survey_2022_Ghor_2022-05-16.xlsx")
hirat_data <- read_excel_func("output/province/ACO_SMART_Survey_2022_Hirat_2022-05-16.xlsx")


# Analysis --------------------------------------------------
# names(badghis_data)
Badghis_PLC <- badghis_data$preg_lact_wom %>% 
  filter(!is.na(wom_muac)) %>% 
  mutate(
    psu = paste0(CLUSTER, HH),
    psu = as.numeric(psu),
    wom_muac_less_230 = case_when(
      wom_muac < 230 ~ "less_230",
      TRUE ~ "Other"
    ),
    wom_muac_between_185_230 = case_when(
      wom_muac < 230 & wom_muac >= 185 ~ "between_185_230",
      TRUE ~ "Other"
    ),
    wom_muac_less_185 = case_when(
      wom_muac < 185 ~ "less_185",
      TRUE ~ "Other"
    )
  )


Badghis_Child <- badghis_data$child %>% 
  mutate(
    psu = paste0(CLUSTER, HH),
    psu = as.numeric(psu)
    )


#Export
write.csv(Badghis_PLC, "output/analysis/ACO_SMART_Survey_2022_Badghis_PLW.csv", row.names = F)
write.csv(Badghis_Child, "output/analysis/ACO_SMART_Survey_2022_Badghis_CHILD.csv", row.names = F)


# Faryab
Faryab_PLC <- faryab_data$preg_lact_wom %>% 
  filter(!is.na(wom_muac)) %>% 
  mutate(
    psu = paste0(CLUSTER, HH),
    psu = as.numeric(psu),
    wom_muac_less_230 = case_when(
      wom_muac < 230 ~ "less_230",
      TRUE ~ "Other"
    ),
    wom_muac_between_185_230 = case_when(
      wom_muac < 230 & wom_muac >= 185 ~ "between_185_230",
      TRUE ~ "Other"
    ),
    wom_muac_less_185 = case_when(
      wom_muac < 185 ~ "less_185",
      TRUE ~ "Other"
    )
    
  )


Faryab_Child <- faryab_data$child %>% 
  mutate(
    psu = paste0(CLUSTER, HH),
    psu = as.numeric(psu)
  )

#Export
write.csv(Faryab_PLC, "output/analysis/ACO_SMART_Survey_2022_Faryab_PLW.csv", row.names = F)
write.csv(Faryab_Child, "output/analysis/ACO_SMART_Survey_2022_Faryab_CHILD.csv", row.names = F)





# GHOR
Ghor_PLC <- ghor_data$preg_lact_wom %>% 
  filter(!is.na(wom_muac)) %>% 
  mutate(
    psu = paste0(CLUSTER, HH),
    psu = as.numeric(psu),
    wom_muac_less_230 = case_when(
      wom_muac < 230 ~ "less_230",
      TRUE ~ "Other"
    ),
    wom_muac_between_185_230 = case_when(
      wom_muac < 230 & wom_muac >= 185 ~ "between_185_230",
      TRUE ~ "Other"
    ),
    wom_muac_less_185 = case_when(
      wom_muac < 185 ~ "less_185",
      TRUE ~ "Other"
    )
    
  )


Ghor_Child <- ghor_data$child %>% 
  mutate(
    psu = paste0(CLUSTER, HH),
    psu = as.numeric(psu)
  )

#Export
write.csv(Ghor_PLC, "output/analysis/ACO_SMART_Survey_2022_Ghor_PLW.csv", row.names = F)
write.csv(Ghor_Child, "output/analysis/ACO_SMART_Survey_2022_Ghor_CHILD.csv", row.names = F)


# Herat
Hirat_PLC <- hirat_data$preg_lact_wom %>% 
  filter(!is.na(wom_muac)) %>% 
  mutate(
    psu = paste0(CLUSTER, HH),
    psu = as.numeric(psu),
    wom_muac_less_230 = case_when(
      wom_muac < 230 ~ "less_230",
      TRUE ~ "Other"
    ),
    wom_muac_between_185_230 = case_when(
      wom_muac < 230 & wom_muac >= 185 ~ "between_185_230",
      TRUE ~ "Other"
    ),
    wom_muac_less_185 = case_when(
      wom_muac < 185 ~ "less_185",
      TRUE ~ "Other"
    )
    
  )


Hirat_Child <- hirat_data$child %>% 
  mutate(
    psu = paste0(CLUSTER, HH),
    psu = as.numeric(psu)
  )


#Export
write.csv(Hirat_PLC, "output/analysis/ACO_SMART_Survey_2022_Hirat_PLW.csv", row.names = F)
write.csv(Hirat_Child, "output/analysis/ACO_SMART_Survey_2022_Hirat_CHILD.csv", row.names = F)






