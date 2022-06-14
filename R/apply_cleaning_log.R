#Raw Data for log verification -----------------------------------------------------------
main_raw <- main
hh_roster_raw <- hh_roster
child_raw <- child
preg_lact_wom_raw <- preg_lact_wom


#Merging the logs ------------------------------------------------------------------------
cols <- c("uuid", "question", "old_value", "new_value")
UNICEF_correction_log <- rbind(
  correction_log %>%
    mutate(uuid = case_when(
      !is.na(child_hh_position) ~ paste0(uuid,child_hh_position),
      !is.na(wom_hh_position) ~ paste0(uuid,wom_hh_position),
      !is.na(index) ~ paste0(uuid, index),
      TRUE ~ uuid
    )) %>% select(all_of(cols)),
  translation_log %>% 
    filter(`Translated?` %in% "Yes") %>% 
    select(all_of(cols)))

#Assigning variable types to Correction Log ----------------------------------------------
datasets <- c("main", "hh_roster", "child", "preg_lact_wom")
ques_types <- extract_question_type(datasets)

UNICEF_correction_log <- UNICEF_correction_log %>%
  left_join(ques_types, by="question")

#Apply log -------------------------------------------------------------------------------
if(nrow(UNICEF_correction_log) > 0){
  apply_log(UNICEF_correction_log)
  
  #Verify log ----------------------------------------------------------------------------
  main_log <- verify_log_changes(main_raw, main, "_uuid")
  roster_log <- verify_log_changes(hh_roster_raw, hh_roster, "uuid")
  child_log <- verify_log_changes(child_raw, child, "uuid")
  wom_log <- verify_log_changes(preg_lact_wom_raw, preg_lact_wom, "uuid")
  
  #Merging the changes
  manual_log <- rbind(main_log, roster_log, child_log, wom_log)
  
  correction_log_discrep <- anti_join(UNICEF_correction_log[1:4], 
                                      manual_log, c("uuid", "question", "new_value"))
  
}


# remove extra objects -------------------------------------------------------------------
rm(url, main_log, roster_log, child_log, wom_log, manual_log, ques_types,
   datasets, main_raw, hh_roster_raw, child_raw, preg_lact_wom_raw, cols)




