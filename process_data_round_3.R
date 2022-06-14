##### Data Processing Script #####
# Install/load required packages ---------------------------------------------------------
if(!require(dplyr)) install.packages("dplyr")
if(!require(readxl)) install.packages("readxl")
if(!require(readr)) install.packages("readr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(stringr)) install.packages("stringr")
if(!require(glue)) install.packages("glue")
if(!require(googlesheets4)) install.packages("googlesheets4")
if(!require(lubridate)) install.packages("lubridate")
source("R/custom_functions.R")

# Read Data ------------------------------------------------------------------------------
# file.edit("R/read_data.R")
data_path <- "input/raw_data/xml/" # data path
sample_sheet_path <- "input/" #Sample sheet
convert_to_na <- c("NA", "N/A", "-", " ") # values to convert to NA
source("R/read_data.R") # read data

# Applying filters & adding new columns --------------------------------------------------
# file.edit("R/filter_and_add.R")
source("R/filter_and_add.R")

# Correction Log -------------------------------------------------------------------------
url <- "https://docs.google.com/spreadsheets/d/1MEz9dDOTrgVXJDd5Mu0nHi8b6fKdHcqGqz6CusO4Ows/edit?usp=sharing"
gs4_deauth()
translation_log <- read_sheet(url, sheet = "Translation_log", col_types = "c")
correction_log <- read_sheet(url, sheet = "Correction_log", col_types = "c")
rejection_log <- read_sheet(url, sheet = "Rejection_log", col_types = "c")

# apply correction/translation log -------------------------------------------------------
# file.edit("R/apply_cleaning_log.R")
source("R/apply_cleaning_log.R")
#if correction_log_discrep is null, then the log is applied correctly
if(exists("correction_log_discrep")){
  correction_log_discrep
}

# Remove rejected keys -------------------------------------------------------------------
main <- main %>%
  filter(`_uuid` %notin% unique(rejection_log$UUID))


# Generate data with missing translations ------------------------------------------------
missing_translation <- rbind(
  generate_translation_log(main, "ACO_SMART_Survey_2022_New_Round"),
  generate_translation_log(hh_roster %>% select(-name_lower), "hh_roster")
  ) %>% anti_join(., translation_log %>% filter(`Translated?` %in% "No"),
            by=(c("uuid", "question", "old_value")))

# Recode Data ----------------------------------------------------------------------------
# file.edit("R/recode.R")
source("R/recode.R")

# Filter Data By Date  -------------------------------------------------------------------
# Check Number of Interviews by date
table(main$Date)

# # Filter One Day
# main <- main %>% filter(Date == '2022-10-24')
# Filter by Date Range
main <- main %>% filter(Date > '2022-06-11' & Date < "2022-07-13")

# Filter repeating groups based on the mainsheet -----------------------------------------
##Filter Children Above 59 months, this should be tracked and reported to field
max_months <- 59
# file.edit("R/filter_repeating_groups.R")
source("R/filter_repeating_groups.R")


#Province & Gender disaggregation  -------------------------------------------------------
#*** update the min_months & max_months accordingly
min_months <- 6
max_months <- 59
gender_disagg <- child_joined %>% 
  filter(MONTHS >= min_months & MONTHS <= max_months) %>% 
  group_by(province, CHSEX) %>% 
  summarize(child_malnourished = sum(child_malnourished, na.rm = T),
            child_referred = sum(child_referred, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Age_group = paste0(min_months,"-",max_months, " months")) %>% 
  pivot_wider(names_from = CHSEX, values_from = c(child_malnourished, child_referred))
rm(min_months, max_months)

# Filter By Provinces  -------------------------------------------------------------------
# file.edit("R/extract_province.R")
source("R/extract_province.R")

## Demographics  -------------------------------------------------------------------------
# file.edit("R/demographic_analysis.R")
source("R/demographic_analysis.R")
#Average number of family members per enumerator should be btw 6 and 10
main %>% 
  group_by(TEAM) %>% 
  summarize(avg_household_members = round(mean(numfamily))) %>% 
  filter(avg_household_members < 6 | avg_household_members > 10) %>%
  ungroup()

# Mortality  -------------------------------------------------------------------
main_sub <- main %>% 
  select(
    Date,
    start,
    end,
    province,
    district,
    CLUSTER,
    TEAM,
    HH,
    numfamily,
    total_left,
    total_died,
    `_index`,
    `_uuid`)

#rooster sheet
hh_roster_sub <- hh_roster_joined %>%
  mutate(join = case_when(
    join == "n" ~ NA_character_,
    join == "y" & born == "y" ~ NA_character_,
    TRUE ~ join),
    left="", died="", cause="", location="") %>% 
  select(sex, age=age_years, join, born, `_parent_index`, left, died, cause, location)
#Left sheet
left_sub <- left %>%
  select(left, sex=left_gender, age=left_age_years, `_parent_index`) %>% 
  mutate(join="", born="", died="", cause="", location="")
#Died sheet
died_sub <- died %>%
  select(died, sex=died_gender, age=died_age_years, cause=cause_of_death, location=location_of_death, `_parent_index`) %>% 
  mutate(left="", join="", born="")

#Joining
main_sub_roster <- left_join(main_sub, hh_roster_sub, by = c("_index" = "_parent_index"))
main_sub_left <-   left_join(main_sub %>% filter(total_left > 0), left_sub, by = c("_index" = "_parent_index"))
main_sub_died <-   left_join(main_sub %>% filter(total_died > 0), died_sub, by = c("_index" = "_parent_index"))
  

joined_data <- rbind(main_sub_roster, main_sub_left, main_sub_died) %>% 
  mutate(across(c(sex:location), as.character)) %>% 
  group_by(`_index`) %>% 
  mutate(n = row_number()) %>% 
  pivot_longer(-c(Date:`_uuid`, n), names_to = "question", values_to = "val") %>%
  ungroup() %>% 
  mutate(question = paste0("P",n , "_", question))

joined_data_wider <- joined_data %>% 
  pivot_wider(-n, names_from = "question", values_from = "val")

openxlsx::write.xlsx(joined_data_wider, paste0("output/Mortality", lubridate::today() ,".xlsx"))


# Export results ---------------------------------------------
# Cumulative
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

#cleaned data
cleaned_data <- list(
  ACO_SMART_Survey_2022_New_Round = main,
  hh_roster = hh_roster,
  child = child,
  preg_lact_wom = preg_lact_wom
)

# create the output paths
if (!file.exists(glue::glue("output/cleaned_data"))) {
  dir.create(glue::glue("output/cleaned_data"), showWarnings = TRUE, recursive = TRUE)
  cat("Created 'output/cleaned_data' folder")
} else {
  cat("The 'output/cleaned_data' folder already exists")
}

## Export cleaned data
openxlsx::write.xlsx(export_list, paste0("output/cleaned_data/ACO_SMART_Survey_2022_New_Round_", lubridate::today() ,".xlsx")) # processed cleaned data
openxlsx::write.xlsx(cleaned_data, paste0("output/cleaned_data/ACO_SMART_Survey_2022_New_Round_cleaned_data_", today(),".xlsx")) # cleaned data
## Export missing translation, number of clusters, demographics etc.
openxlsx::write.xlsx(demographic, paste0("output/ACO_SMART_Survey_2022_Demographics_", lubridate::today() ,".xlsx")) #Demographics
openxlsx::write.xlsx(n_cluster_by_date_province, paste0("output/count_of_clusters_by_date_province_", lubridate::today() ,".xlsx")) #Num of Clusters
openxlsx::write.xlsx(missing_translation, paste0("output/ACO_SMART_Survey_2022_New_Round_Translation_log_", today(),".xlsx")) #Missing translations

# create the output paths
if (!file.exists(glue::glue("output/cleaned_data/province"))) {
  dir.create(glue::glue("output/cleaned_data/province"), showWarnings = TRUE, recursive = TRUE)
  cat("Created 'output/cleaned_data/province' folder")
} else {
  cat("The 'output/cleaned_data/province' folder already exists")
}
## Export Province variables
for(prov_list in ls(pattern = "export_list_")){
  prov_name <- str_remove(prov_list, "export_list_")
  path <- paste0("output/cleaned_data/province/ACO_SMART_Survey_2022_",prov_name,"_", lubridate::today() ,".xlsx")
  data <- get(prov_list)
  
  #export
  openxlsx::write.xlsx(data, path)
}