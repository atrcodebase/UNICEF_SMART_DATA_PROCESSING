if(!require("dplyr")) install.packages("dplyr")
if(!require("readxl")) install.packages("readxl")
if(!require("tidyr")) install.packages("tidyr")
if(!require("readr")) install.packages("readr")
if(!require("googlesheets4")) install.packages("googlesheets4")
if(!require("lubridate")) install.packages("lubridate")
# if(!require("nipnTK")) install.packages("nipnTK")
`%notin%` <- Negate(`%in%`)

# Read Data ---------------------------------------------------------------
data_path <- "input/raw_data/xml/ACO_SMART_Survey_2022_New_Round_-_all_versions_-_False_-_2022-03-19-16-48-03.xlsx"
sample_sheet_path <- "input/sample_sheet.xlsx"

main <- read_excel(data_path, sheet = "ACO_SMART_Survey_2022_New_Round", guess_max = 5000)
hh_roster <- read_excel(data_path, sheet = "hh_roster", guess_max = 5000)
child <- read_excel(data_path, sheet = "child", guess_max = 5000)
preg_lact_wom <- read_excel(data_path, sheet = "preg_lact_wom", guess_max = 5000)

#add child_status column
child <- child %>% 
  mutate(child_status = "", .after = CHSEX,
         uuid = case_when(
           !is.na(child_hh_position) ~ paste0(`_submission__uuid`,child_hh_position),
           TRUE ~ `_submission__uuid`
         ))
preg_lact_wom <- preg_lact_wom %>% 
  mutate(wom_status = "", .after = wom_valid,
         uuid = case_when(
           !is.na(wom_hh_position) ~ paste0(`_submission__uuid`,wom_hh_position),
           TRUE ~ `_submission__uuid`
         ),
         wom_valid = case_when(
           wom_valid == "f" ~ "female",
           wom_valid == "m" ~ "male",
           TRUE ~ NA_character_
         )) %>% 
  filter(wom_valid %in% "female" & (curr_pregnant %in% 1 | curr_breastfeed %in% 1))

#Raw Data for log verification
main_raw <- main
hh_roster_raw <- hh_roster
child_raw <- child
preg_lact_wom_raw <- preg_lact_wom

# Correction Log ----------------------------------------------------------
gs4_deauth()
translation_log <- readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSBUVtEC6ehPCFUJ9qIlJP42XmVRrX2NfQ3UejjtC6TYKByM8tn3W5hwf-CfGKKSxysigbk4PhJGWEw/pub?gid=662183319&single=true&output=csv")
translation_log_yes <- translation_log %>% filter(`Translated?` %in% "Yes")
translation_log_no <- translation_log %>% filter(`Translated?` %in% "No")
correction_log <- readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSBUVtEC6ehPCFUJ9qIlJP42XmVRrX2NfQ3UejjtC6TYKByM8tn3W5hwf-CfGKKSxysigbk4PhJGWEw/pub?gid=1795705035&single=true&output=csv")

cols <- c("uuid", "question", "old_value", "new_value")
UNICEF_correction_log <- correction_log %>% 
  mutate(uuid = uuid,
         uuid = case_when(
           !is.na(child_hh_position)  ~ paste0(uuid,child_hh_position),
           !is.na(wom_hh_position) ~ paste0(uuid,wom_hh_position),
           TRUE ~ uuid
         )) %>% 
  select(all_of(cols)) %>% 
  rbind(translation_log_yes %>% select(all_of(cols)))

apply_log <- function(UNICEF_correction_log){
  for (rowi in 1:nrow(UNICEF_correction_log)){
    uuid_i <- UNICEF_correction_log$uuid[rowi]
    var_i <- UNICEF_correction_log$question[rowi]
    old_i <- UNICEF_correction_log$old_value[rowi]
    new_i <- UNICEF_correction_log$new_value[rowi]
    
    print(paste("uuid", uuid_i, "Old value: ", old_i, "changed to", new_i, "for", var_i))
    #Find which sheet the variable belongs to
    if(var_i %in% colnames(main)){
      main[main$`_uuid` %in% uuid_i, var_i] <<- new_i
    } else if(var_i %in% colnames(hh_roster)){
      hh_roster[hh_roster$`_submission__uuid` %in% uuid_i, var_i] <<- new_i
    } else if(var_i %in% colnames(child)){
      child[child$uuid %in% uuid_i, var_i] <<- new_i
    } else if(var_i %in% colnames(preg_lact_wom)){
      preg_lact_wom[preg_lact_wom$uuid %in% uuid_i, var_i] <<- new_i
    }
  }
}

apply_log(UNICEF_correction_log)

#Test if log is applied correctly------------------------
verify_log_changes <- function(raw_data, cleaned_data, identifier){
  uuid <- vector()
  question <- vector()
  old_value <- vector()
  new_value <- vector()
  
  for(col_name in colnames(cleaned_data) ){
    
    for(i in 1:length(cleaned_data[[col_name]])) {
      id <- cleaned_data[[identifier]][i]
      oldVal <- raw_data[[col_name]][raw_data[[identifier]] %in% id]
      newVal <- cleaned_data[[col_name]][i]
      
      if(newVal %notin% oldVal){
        uuid <- c(uuid, id)
        question <- c(question, col_name)
        old_value <- c(old_value, oldVal)
        new_value <- c(new_value, newVal)
      }  
    }
  }
  log <- data.frame(uuid, question, old_value, new_value)
  return(log)
}

main_log <- verify_log_changes(main_raw, main, "_uuid")
roster_log <- verify_log_changes(hh_roster_raw, hh_roster, "_submission__uuid")
child_log <- verify_log_changes(child_raw, child, "uuid")
wom_log <- verify_log_changes(preg_lact_wom_raw, preg_lact_wom, "uuid")

manual_log <- rbind(main_log, roster_log, child_log, wom_log)

#if discrep is null, then the log is applied correctly
discrep <- anti_join(UNICEF_correction_log, manual_log, c("uuid", "question", "new_value"))
discrep


# Converting column data types
main <- main %>% type_convert()
hh_roster <- hh_roster %>% type_convert()
child <- child %>% select(-uuid) %>% type_convert()
preg_lact_wom <- preg_lact_wom %>% select(-uuid) %>% type_convert()

rejection_log <- readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSBUVtEC6ehPCFUJ9qIlJP42XmVRrX2NfQ3UejjtC6TYKByM8tn3W5hwf-CfGKKSxysigbk4PhJGWEw/pub?gid=1383764528&single=true&output=csv")

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
          if(sheet_i %in% "ACO_SMART_Survey_2022_New_Round"){
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

main_log <- generate_translation_log(main, "ACO_SMART_Survey_2022_New_Round")
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
main <- main %>% filter(Date > '2022-01-23' & Date < "2022-08-01")

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
child_anthropometry_data <- child_joined

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

child_anthropometry_data <- child_anthropometry_data %>% 
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
    province
  )

# Create a list of dataframes to be exported ------------------------------
export_list <- list(
  ACO_SMART_Survey_2022_New_Round = main,
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
  ACO_SMART_Survey_2022_New_Round = main,
  hh_roster = hh_roster,
  child = child,
  preg_lact_wom = preg_lact_wom
)

#Export Data -------------------------------------------------------------
openxlsx::write.xlsx(export_list, paste0("output/ACO_SMART_Survey_2022_New_Round_", lubridate::today() ,".xlsx"))
openxlsx::write.xlsx(n_cluster_by_date_province, paste0("output/count_of_clusters_by_date_province_", lubridate::today() ,".xlsx"))
#export translation log
writexl::write_xlsx(log, paste0("output/ACO_SMART_Survey_2022_New_Round_Translation_log_", today(),".xlsx"))
#export cleaned data
writexl::write_xlsx(cleaned_data, paste0("output/ACO_SMART_Survey_2022_New_Round_cleaned_data_", today(),".xlsx"))
