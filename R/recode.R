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
    province = as.character(province),
    consent = as.character(consent),
    province = case_when(
      province	== 1	~ "Badakhshan",
      province	== 2	~ "Badghis",
      province	== 3	~ "Baghlan",
      province	== 4	~ "Balkh",
      province	== 5	~ "Bamyan",
      province	== 6	~ "Daykundi",
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
      province	== 32	~ "Uruzgan",
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
         ),
         child_status_first = case_when(
           child_status_first == 1 ~ "The child is present",
           child_status_first == 2 ~ "The child is present but his/her family does not allow the child to be measured",
           child_status_first == 3 ~ "The child is asleep but his/her family does not wake her/him up",
           child_status_first == 4 ~ "The child is absent"
         ))
