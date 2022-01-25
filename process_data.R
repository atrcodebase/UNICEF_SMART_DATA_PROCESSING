library(dplyr)
library(readxl)
library(tidyr)
library(readr)
library(lubridate)
# library(nipnTK)

# Read Data ---------------------------------------------------------------
data_path <- "input/raw_data/xml/ACO_SMART_Survey_2022_-_all_versions_-_English_-_2022-01-23-07-13-13.xlsx"

main <- read_excel(data_path, sheet = "ACO_SMART_Survey_2022", guess_max = 5000) %>% type_convert()
hh_roster <- read_excel(data_path, sheet = "hh_roster", guess_max = 5000) %>% type_convert()
child <- read_excel(data_path, sheet = "child", guess_max = 5000) %>% type_convert()
preg_lact_wom <- read_excel(data_path, sheet = "preg_lact_wom", guess_max = 5000) %>% type_convert()


# Change Data type
main$province <- as.character(main$province)
main$consent <- as.character(main$consent)

# Recode Data -------------------------------------------------------------

main <- main %>% 
  mutate(
    Date = as.Date(start),
    Duration = difftime(end, start, units = c("mins")),
    province = case_when(
      province == 14 ~ "Kabul",
      province == 15 ~ "Kandahar",
      province == 4 ~ "Balkh",
      province == 22 ~ "Nangarhar",
      province == 12 ~ "Hirat",
      TRUE ~ province
    ),
    district = case_when(
      province == "Kabul" ~ "Kabul",
      province == "Kandahar" ~ "Kandahar",
      province == "Balkh" ~ "Mazar-e-Sharif",
      province == "Nangarhar" ~ "Jalalabad",
      province == "Hirat" ~ "Hirat",
      TRUE ~ district
    ),
    consent = case_when(
      consent == 1 ~ "Yes",
      consent == 2 ~ "No",
      consent == 3 ~ "Other (No one present)"
    )
    
  ) %>% 
  select(
    -c(
      `__version__`,
      `_version_`,
      `_version__001`,
      `_version__002`,
      `_version__003`,
      `_version__004`,
      `_version__005`,
      `_version__005`,
    )
  )


# Filter Data By Date  ----------------------------------------------------

# Filter Only Pilot Data
# uuid_pilot <- read_excel("input/UUID_Pilot.xlsx")
# main <- main %>% filter(`_uuid` %in% uuid_pilot$`_uuid`)

# Check Number of Interviews by date
table(main$Date)

# Filter One Day
main <- main %>% filter(Date == '2022-01-24')

# Filter by Date Range
# main <- main %>% filter(Date > '2022-01-23' & Date < "2022-01-25")


# Create A subset of main sheet to be merged with child sheets ------------

main_sub <- main %>% 
  select(
    Date,
    province,
    district,
    CLUSTER,
    TEAM,
    HH,
    `_uuid`
  )


# Filter Child and Merge Child sheets -------------------------------------

hh_roster <- hh_roster %>%  filter(`_submission__uuid` %in% unique(main$`_uuid`))
hh_roster_joined <- hh_roster %>% left_join(main_sub, by = c("_submission__uuid" = "_uuid")) %>% 
  relocate(Date:HH, .before = name_lower)


child <- child %>% filter(`_submission__uuid` %in% main$`_uuid`) 
child_joined <- child %>% left_join(main_sub, by = c("_submission__uuid" = "_uuid")) %>% 
  relocate(province:TEAM, .before = child_selected_position)

# Filter Children Above 59 months, this should be tracked and reported to field
child_joined <- child_joined %>% 
  filter(MONTHS < 60)

preg_lact_wom <- preg_lact_wom %>% filter(`_submission__uuid` %in% main$`_uuid`)
preg_lact_wom_joined <- preg_lact_wom %>% left_join(main_sub, by = c("_submission__uuid" = "_uuid")) %>% 
  relocate(province:TEAM, .before = wom_selected_position)


# Create a subset for ENA software ----------------------------------------
child_anthropometry_data <- child_joined %>% 
  select(
    Date,
    CLUSTER,
    TEAM,
    ID = `_index`,
    HH,
    CHSEX,
    BIRTHDAT,
    MONTHS,
    WEIGHT,
    HEIGHT,
    EDEMA,
    MUAC,
    BIRTHDAT_2,
    MONTHS_2,
    WEIGHT_2,
    HEIGHT_2,
    MUAC_2,
    BIRTHDAT_3,
    MONTHS_3,
    WEIGHT_3,
    HEIGHT_3,
    MUAC_3
    
  )
  
# Create a list of dataframes to be exported ------------------------------

export_list <- list(
  ACO_SMART_Survey_2022 = main,
  hh_roster = hh_roster_joined,
  child = child_joined,
  preg_lact_wom = preg_lact_wom_joined,
  child_anthropometry_data = child_anthropometry_data
)

# Export Data -------------------------------------------------------------

openxlsx::write.xlsx(export_list, paste0("output/ACO_SMART_Survey_2022_", lubridate::today() ,".xlsx"))
