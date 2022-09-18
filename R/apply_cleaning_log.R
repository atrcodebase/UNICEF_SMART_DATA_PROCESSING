#Raw Data for log verification -----------------------------------------------------------
main_raw <- main
hh_roster_raw <- hh_roster
child_raw <- child
preg_lact_wom_raw <- preg_lact_wom
left_raw <- left
died_raw <- died

#Merging the logs ------------------------------------------------------------------------
cols <- c("uuid", "question", "old_value", "new_value")
UNICEF_correction_log <- rbind(
  correction_log %>%
    mutate(uuid = case_when(
      !is.na(child_hh_position) ~ paste0(uuid,child_hh_position),
      !is.na(wom_hh_position) ~ paste0(uuid,wom_hh_position),
      !is.na(index) & question %notin% names(main) ~ paste0(uuid, index),
      TRUE ~ uuid
    )) %>% select(all_of(cols)),
  translation_log %>% 
    filter(`Translated?` %in% "Yes") %>% 
    select(all_of(cols)))

#Assigning variable types to Correction Log ----------------------------------------------
datasets <- c("main", "hh_roster", "child", "preg_lact_wom", "left", "died")
ques_types <- extract_question_type(datasets)

UNICEF_correction_log <- UNICEF_correction_log %>%
  left_join(ques_types, by="question")

#Apply log -------------------------------------------------------------------------------
main <- apply_log(main, UNICEF_correction_log, key="_uuid")
hh_roster <- apply_log(hh_roster ,UNICEF_correction_log)
child <- apply_log(child ,UNICEF_correction_log)
preg_lact_wom <- apply_log(preg_lact_wom ,UNICEF_correction_log)
left <- apply_log(left ,UNICEF_correction_log)
died <- apply_log(died ,UNICEF_correction_log)

#Verify log ----------------------------------------------------------------------------
message("Verifying Correction log, please wait!")
main_log <- verify_log_changes(main_raw, main, key="_uuid")
roster_log <- verify_log_changes(hh_roster_raw, hh_roster)
child_log <- verify_log_changes(child_raw, child)
wom_log <- verify_log_changes(preg_lact_wom_raw, preg_lact_wom)
left_log <- verify_log_changes(left_raw, left)
died_log <- verify_log_changes(died_raw, died)

#Merging the changes
manual_log <- rbind(main_log, roster_log, child_log, wom_log, left_log, died_log)
correction_log_discrep <- anti_join(UNICEF_correction_log[1:4], 
                                    manual_log, c("uuid", "question", "new_value"))
correction_log_discrep <- correction_log_discrep %>% 
  filter(question %notin% c('start', 'end'))

# remove extra objects -------------------------------------------------------------------
rm(url, main_log, roster_log, child_log, wom_log, ques_types,
   datasets, main_raw, hh_roster_raw, child_raw, preg_lact_wom_raw, cols)




