# pull file names --------------------------------------------------
files <- list.files(data_path, pattern = ".xls")

for (data_name in files) {
  # UNICEF SMART
  if(str_detect(data_name, "ACO_SMART_Survey_2022")){
    unicef_smart <- data_name
  }
}

# Read data --------------------------------------------------
#Dataset
if (exists("unicef_smart")) {
  main <- read_excel(glue("{data_path}{unicef_smart}"), sheet = "ACO_SMART_Survey_2022_Round ...", guess_max = 100000, na = convert_to_na)
  hh_roster <- read_excel(glue("{data_path}{unicef_smart}"), sheet = "hh_roster", guess_max = 100000, na = convert_to_na)
  child <- read_excel(glue("{data_path}{unicef_smart}"), sheet = "child", guess_max = 100000, na = convert_to_na)
  preg_lact_wom <- read_excel(glue("{data_path}{unicef_smart}"), sheet = "preg_lact_wom", guess_max = 100000, na = convert_to_na)
  left <- read_excel(glue("{data_path}{unicef_smart}"), sheet = "left", guess_max = 100000, na = convert_to_na)
  died <- read_excel(glue("{data_path}{unicef_smart}"), sheet = "died", guess_max = 100000, na = convert_to_na)
  
  }

# remove extra variables --------------------------------------------------
rm(convert_to_na, data_name, data_path, files, unicef_smart)
