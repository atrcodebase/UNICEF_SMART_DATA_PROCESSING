# pull file names --------------------------------------------------
files <- c(
  list.files(data_path, pattern = ".xls"),
  list.files(sample_sheet_path, pattern = ".xls"))

for (data_name in files) {
  # UNICEF SMART
  if(str_detect(data_name, "ACO_SMART_Survey_2022")){
    unicef_smart <- data_name
  }
  # UNICEF SMART sample sheet
  if(str_detect(data_name, "Sampling_Frame")){
    unicef_smart_sample <- data_name
  }
}

# Read data --------------------------------------------------
#Dataset
if (exists("unicef_smart")) {
  main <- read_excel(glue("{data_path}{unicef_smart}"), sheet = "ACO_SMART_Survey_2022_New_Round", guess_max = 100000, na = convert_to_na)
  hh_roster <- read_excel(glue("{data_path}{unicef_smart}"), sheet = "hh_roster", guess_max = 100000, na = convert_to_na)
  child <- read_excel(glue("{data_path}{unicef_smart}"), sheet = "child", guess_max = 100000, na = convert_to_na)
  preg_lact_wom <- read_excel(glue("{data_path}{unicef_smart}"), sheet = "preg_lact_wom", guess_max = 100000, na = convert_to_na)
  
  }
#Sample Sheet
if (exists("unicef_smart_sample")) {
  sample_sheet <- read_excel(glue("{sample_sheet_path}{unicef_smart_sample}"), guess_max = 100000, na = convert_to_na)
}

# remove extra variables --------------------------------------------------
rm(convert_to_na, data_name, data_path, sample_sheet_path, files, unicef_smart, unicef_smart_sample)
