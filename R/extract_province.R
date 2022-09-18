# Filter By Provinces --------------------------------------------------------------------
datasets <- c("main", "hh_roster_joined", "child_joined", "preg_lact_wom_joined", 
              "left_joined", "died_joined", "child_anthropometry_data", 
              "mortality_data_wide", "mortality_data_wide2")

for(province_i in unique(main$province)){
  
  province_list <- list()
  for(dataset in datasets){
    
    #Filter province
    data <- get(dataset) %>% 
      filter(province %in% province_i)
    
    #Renaming sheets for export
    sheet_name <- case_when(
      dataset == "main" ~ "ACO_SMART_Survey_2022_New_Round",
      dataset == "hh_roster_joined" ~ "hh_roster",
      dataset == "child_joined" ~ "child",
      dataset == "preg_lact_wom_joined" ~ "preg_lact_wom",
      dataset == "left_joined" ~ "left",
      dataset == "died_joined" ~ "died",
      dataset == "child_anthropometry_data" ~ "child_anthropometry_data",
      dataset == "mortality_data_wide" ~ "Mortality",
      dataset == "mortality_data_wide2" ~ "Mortality_v2")
    #Add sheet
    province_list[[sheet_name]] = data
  }
  
  variable <- paste0("export_list_", province_i)
  #assign variable
  assign(variable, province_list, envir = .GlobalEnv)
}

# remove extra variables -----------------------------------------------------------------
rm(datasets, data, province_list, variable, sheet_name, province_i, dataset)
