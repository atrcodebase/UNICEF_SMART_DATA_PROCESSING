#Sample Sheet ----------------------------------------------------------------------------
sample_sheet <- sample_sheet %>% 
  filter(CLUSTER != "RC") %>% 
  mutate(CLUSTER = as.numeric(CLUSTER))

#Household roster sheet ------------------------------------------------------------------
hh_roster <- hh_roster %>% 
  mutate(uuid = paste0(`_submission__uuid`,`_index`))

#Child sheet -----------------------------------------------------------------------------
child <- child %>%
  mutate(child_status = "", .after = CHSEX,
         uuid = case_when(
           !is.na(child_hh_position) ~ paste0(`_submission__uuid`,child_hh_position),
           TRUE ~ `_submission__uuid`
         ))

#Pregnant Lacted Women Sheet -------------------------------------------------------------
preg_lact_wom <- preg_lact_wom %>% 
  mutate(wom_status = "", .after = wom_valid,
         uuid = case_when(
           !is.na(wom_hh_position) ~ paste0(`_submission__uuid`,wom_hh_position),
           TRUE ~ `_submission__uuid`
         )) %>% 
  filter(wom_valid %in% "f" & (curr_pregnant %in% 1 | curr_breastfeed %in% 1))
