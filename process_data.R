if(!require("dplyr")) install.packages("dplyr")
if(!require("readxl")) install.packages("readxl")
if(!require("tidyr")) install.packages("tidyr")
if(!require("readr")) install.packages("readr")
if(!require("googlesheets4")) install.packages("googlesheets4")
if(!require("lubridate")) install.packages("lubridate")
# if(!require("nipnTK")) install.packages("nipnTK")
`%notin%` <- Negate(`%in%`)

# Read Data ---------------------------------------------------------------
data_path <- "input/raw_data/xml/ACO_SMART_Survey_2022_-_all_versions_-_English_-_2022-01-23-07-13-13.xlsx"
sample_sheet_path <- "input/sample_sheet.xlsx"

main <- read_excel(data_path, sheet = "ACO_SMART_Survey_2022", guess_max = 5000)
hh_roster <- read_excel(data_path, sheet = "hh_roster", guess_max = 5000)
child <- read_excel(data_path, sheet = "child", guess_max = 5000)
preg_lact_wom <- read_excel(data_path, sheet = "preg_lact_wom", guess_max = 5000)

#add child_status column
child <- child %>% 
  mutate(child_status = "", .after = CHSEX)
preg_lact_wom <- preg_lact_wom %>% 
  mutate(wom_status = "", .after = wom_valid)

# Correction Log ----------------------------------------------------------
gs4_deauth()
translation_log <- readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR8Am11iZsn4xQJYFMlb9b3y_DtON13CcSNTVBj06ac1jDsvntgfCArhwtT2Ra73po3UqEtbwHmePWZ/pub?gid=662183319&single=true&output=csv")
translation_log_yes <- translation_log %>% filter(`Translated?` %in% "Yes")
translation_log_no <- translation_log %>% filter(`Translated?` %in% "No")
correction_log <- readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR8Am11iZsn4xQJYFMlb9b3y_DtON13CcSNTVBj06ac1jDsvntgfCArhwtT2Ra73po3UqEtbwHmePWZ/pub?gid=1795705035&single=true&output=csv")

apply_log <- function(UNICEF_correction_log, log_type){
  for (rowi in 1:nrow(UNICEF_correction_log)){
    uuid_i <- UNICEF_correction_log$uuid[rowi]
    var_i <- UNICEF_correction_log$question[rowi]
    old_i <- UNICEF_correction_log$old_value[rowi]
    new_i <- UNICEF_correction_log$new_value[rowi]
    if(log_type %in% "correction"){
      ch_hh_pos <- UNICEF_correction_log$child_hh_position[rowi]
      wom_hh_pos <- UNICEF_correction_log$wom_hh_position[rowi]
    }
    
    print(paste("uuid", uuid_i, "Old value: ", old_i, "changed to", new_i, "for", var_i))
    #Find which sheet the variable belongs to
    if(var_i %in% colnames(main)){
      main[main$`_uuid` %in% uuid_i, var_i] <<- new_i
    } else if(var_i %in% colnames(hh_roster)){
      hh_roster[hh_roster$`_submission__uuid` %in% uuid_i, var_i] <<- new_i
    } else if(var_i %in% colnames(child)){
      #handling the two unique identifiers for child_status
      if(!is.na(ch_hh_pos)){
        child[child$`_submission__uuid` %in% uuid_i & child$child_hh_position %in% ch_hh_pos, var_i] <<- new_i
      } else {
        child[child$`_submission__uuid` %in% uuid_i, var_i] <<- new_i
      }
    } else if(var_i %in% colnames(preg_lact_wom)){
      #handling the two unique identifiers for wom_status
      if(!is.na(wom_hh_pos)){
        preg_lact_wom[preg_lact_wom$`_submission__uuid` %in% uuid_i & preg_lact_wom$wom_hh_position %in% wom_hh_pos, var_i] <<- new_i
      } else {
        preg_lact_wom[preg_lact_wom$`_submission__uuid` %in% uuid_i, var_i] <<- new_i
      }
    }
  }
}

apply_log(translation_log_yes, log_type = "translation")
apply_log(correction_log, log_type = "correction")

# Converting column data types
main <- main %>% type_convert()
hh_roster <- hh_roster %>% type_convert()
child <- child %>% type_convert()
preg_lact_wom <- preg_lact_wom %>% type_convert()
  
rejection_log <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1VeGTxPUpX44el3Jr6tY15OalMiigUhCqypJKGudYpbw/edit?usp=sharing", sheet = "Rejection_log")

main <- main %>% 
  filter(`_uuid` %notin% unique(rejection_log$UUID))

# Generating Translation Log -------------------------------------------------------
generate_translation_log <- function(data, sheet_i){
  question <- c()
  old_value <- c() 
  uuid <- c()
  sheet_name <- c()
  #checking each row of each column
  for(col_name in colnames(data)){
    for(i in 1:nrow(data)){
      cell_val <- data[[col_name]][i]

      if(is.character(cell_val)){
        if(Encoding(cell_val) %in% "UTF-8"){
          question <- c(question, col_name)
          old_value <- c(old_value, cell_val)
          sheet_name <- c(sheet_name, sheet_i)
          #because the uuid column name is different in sheets
          if(sheet_i %in% "ACO_SMART_Survey_2022"){
            uuid <- c(uuid, data$`_uuid`[i])
          } else {
            uuid <- c(uuid, data$`_submission__uuid`[i])
          }
        }
      }
    }
  }
  log <- data.frame(question, old_value, new_value=NA, uuid, sheet_name, Remarks=NA) %>% unique()
}

main_log <- generate_translation_log(main, "ACO_SMART_Survey_2022")
roster_log <- generate_translation_log(hh_roster %>% select(-name_lower), "hh_roster")
log <- rbind(main_log, roster_log) %>% 
  anti_join(., translation_log_no, by=(c("uuid", "question", "old_value")))


sample_sheet <- read_excel(sample_sheet_path)
sample_sheet <- sample_sheet %>% filter(CLUSTER != "RC") %>% mutate(CLUSTER = as.numeric(CLUSTER))

# Change Data type
main$province <- as.character(main$province)
main$consent <- as.character(main$consent)

# Recode Data -------------------------------------------------------------

main <- main %>% 
  mutate(
    Date = base::as.Date(start),
    Duration = difftime(end, start, units = c("mins")),
    province = case_when(
      province	== 1	~ "Badakhshan",
      province	== 2	~ "Badghis",
      province	== 3	~ "Baghlan",
      province	== 4	~ "Balkh",
      province	== 5	~ "Bamyan",
      province	== 6	~ "Daikundi",
      province	== 7	~ "Farah",
      province	== 8	~ "Faryab",
      province	== 9	~ "Ghazni",
      province	== 10	~ "Ghor",
      province	== 11	~ "Helmand",
      province	== 12	~ "Hirat",
      province	== 13	~ "Jawzjan",
      province	== 14	~ "Kabul",
      province	== 15	~ "Kandahar",
      province	== 16	~ "Kapisa",
      province	== 17	~ "Khost",
      province	== 18	~ "Kunar",
      province	== 19	~ "Kunduz",
      province	== 20	~ "Laghman",
      province	== 21	~ "Logar",
      province	== 22	~ "Nangarhar",
      province	== 23	~ "Nimroz",
      province	== 24	~ "Nooristan",
      province	== 25	~ "Paktia",
      province	== 26	~ "Paktika",
      province	== 27	~ "Panjshir",
      province	== 28	~ "Parwan",
      province	== 29	~ "Samangan",
      province	== 30	~ "Sar-e-Pul",
      province	== 31	~ "Takhar",
      province	== 32	~ "Urozgan",
      province	== 33	~ "Wardak",
      province	== 34	~ "Zabul",
      TRUE ~ province
    ),
    consent = case_when(
      consent == 1 ~ "Yes",
      consent == 2 ~ "No",
      consent == 3 ~ "Other (No one present)"
    )
    
  ) %>% 
  left_join(
    sample_sheet %>% select(province, CLUSTER, District),
    by = c("province", "CLUSTER")
  ) %>% 
  relocate(District, .after = district) %>% 
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
    province,
    child_malnourished,
    child_referred,
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

#Province & Gender disaggregation
#!!! update the min_months & max_months accordingly !!!
min_months <- 6
max_months <- 59
gender_disagg <- child_anthropometry_data %>% 
  filter(MONTHS >= min_months & MONTHS <= max_months) %>% 
  group_by(province, CHSEX) %>% 
  summarize(child_malnourished = sum(child_malnourished, na.rm = T),
            child_referred = sum(child_referred, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Age_group = paste0(min_months,"-",max_months, " months"),
         CHSEX = case_when(
           CHSEX == "f" ~ "Female",
           CHSEX == "m" ~ "Male",
           TRUE ~ CHSEX
         )) %>% 
  pivot_wider(names_from = CHSEX, values_from = c(child_malnourished, child_referred))
  
# Create a list of dataframes to be exported ------------------------------
export_list <- list(
  ACO_SMART_Survey_2022 = main,
  hh_roster = hh_roster_joined,
  child = child_joined,
  preg_lact_wom = preg_lact_wom_joined,
  child_anthropometry_data = child_anthropometry_data,
  gender_disagg = gender_disagg
)

# count of cluster, by date and province
n_cluster_by_date_province <- main %>% 
  group_by(Date, province) %>% 
  count(CLUSTER, name = "N")

#cleaned data only list
cleaned_data <- list(
  ACO_SMART_Survey_2022 = main,
  hh_roster = hh_roster,
  child = child,
  preg_lact_wom = preg_lact_wom
)

# Export Data -------------------------------------------------------------
openxlsx::write.xlsx(export_list, paste0("output/ACO_SMART_Survey_2022_", lubridate::today() ,".xlsx"))
openxlsx::write.xlsx(n_cluster_by_date_province, paste0("output/count_of_clusters_by_date_province_", lubridate::today() ,".xlsx"))
#export translation log
writexl::write_xlsx(log, "output/translation_log.xlsx")
#export cleaned data
writexl::write_xlsx(cleaned_data, paste0("output/ACO_SMART_Survey_2022_cleaned_data_", today(),".xlsx"))
