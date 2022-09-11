### filter repeating group sheets based on the main sheet

# Create A subset of main sheet to be merged with child sheets ---------------------------
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

#Household roster ------------------------------------------------------------------------
hh_roster <- hh_roster %>%
  filter(`_submission__uuid` %in% unique(main$`_uuid`))
hh_roster_joined <- hh_roster %>% 
  left_join(main_sub, by = c("_submission__uuid" = "_uuid")) %>% 
  relocate(Date:HH, .before = name_lower)

#Child sheet -----------------------------------------------------------------------------
child <- child %>% 
  filter(`_submission__uuid` %in% main$`_uuid`) 
child_joined <- child %>% 
  left_join(main_sub, by = c("_submission__uuid" = "_uuid")) %>% 
  relocate(province:TEAM, .before = child_selected_position)
# Filter Children Above 59 months, this should be tracked and reported to field
child_joined <- child_joined %>% 
  filter(MONTHS <= max_months)

#Pregnant Lacted Women sheet -------------------------------------------------------------
preg_lact_wom <- preg_lact_wom %>% 
  filter(`_submission__uuid` %in% main$`_uuid`)
preg_lact_wom_joined <- preg_lact_wom %>% 
  left_join(main_sub, by = c("_submission__uuid" = "_uuid")) %>% 
  relocate(province:TEAM, .before = wom_selected_position)

#Left sheet -------------------------------------------------------------
left <- left %>%
  filter(`_submission__uuid` %in% main$`_uuid`)
left_joined <- left %>%
  left_join(main_sub, by = c("_submission__uuid" = "_uuid")) %>%
  relocate(Date:HH, .before = left_name)

#Died sheet -------------------------------------------------------------
died <- died %>%
  filter(`_submission__uuid` %in% main$`_uuid`)
died_joined <- died %>%
  left_join(main_sub, by = c("_submission__uuid" = "_uuid")) %>%
  relocate(Date:HH, .before = died_name)

# Create a subset for ENA software -------------------------------------------------------
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
    MEASURE,
    CLOTHES,
    province,
    child_status_first,
    child_hh_position,
  ) %>% 
  mutate(CHSEX = case_when(
    CHSEX == "Female" ~ "f",
    CHSEX == "Male" ~ "m",
    TRUE ~ CHSEX
  ))

# remove extra objects -------------------------------------------------------------------
rm(main_sub, max_months)
