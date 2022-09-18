#Pregnant Lacted Women sheet -------------------------------------------------------------
preg_lact_wom <- preg_lact_wom %>% 
  mutate(wom_valid = case_when(
    wom_valid == "f" ~ "female",
    wom_valid == "m" ~ "male",
    TRUE ~ wom_valid))

#Converting Column Types -----------------------------------------------------------------
main <- main %>% type_convert()
hh_roster <- hh_roster %>%  select(-uuid) %>% type_convert()
child <- child %>% select(-uuid) %>% type_convert()
preg_lact_wom <- preg_lact_wom %>% select(-uuid) %>% type_convert()
left <- left %>% select(-uuid) %>% type_convert()
died <- died %>% select(-uuid) %>% type_convert()

#Main sheet ------------------------------------------------------------------------------
main <- main %>% 
  mutate(
    Date = base::as.Date(start),
    Duration = difftime(end, start, units = c("mins")),
  ) %>% 
  left_join(
    sample_sheet %>% select(province=Province, CLUSTER, District),
    by = c("province", "CLUSTER")
  ) %>% 
  relocate(District, .after = district) %>% 
  select(
    -c(
      `__version__`,
      `_version_`,
    )
  )

#Child sheet -----------------------------------------------------------------------------
child <- child %>% 
  mutate(CHSEX = case_when(
           CHSEX == "f" ~ "Female",
           CHSEX == "m" ~ "Male",
           TRUE ~ CHSEX
         ))
