#Analysis --------------------------------------------------------------------------------
total_household <- hh_roster_joined %>% 
  filter(!is.na(age_years)) %>% 
  group_by(province) %>% 
  summarize(total = length(age_years))

demographic_data <- rbind(
  hh_roster_joined %>% 
    filter(!is.na(age_years)) %>% 
    group_by(province) %>% 
    summarize(freq = length(age_years),
              parameters = "All household members"),
  
  main %>% 
    group_by(province) %>% 
    summarize(freq = round(mean(numfamily), 2),
              parameters = "Average household size"),
  
  child_anthropometry_data %>% 
    group_by(province) %>% 
    summarize(freq = n(),
              parameters = "% Children <5 years"),
  
  child_anthropometry_data %>% 
    filter(MONTHS < 25) %>% 
    group_by(province) %>% 
    summarize(freq = n(),
              parameters = "% Children <2 years"),
  
  hh_roster_joined %>% 
    filter(!is.na(age_years)) %>% 
    mutate(sex = case_when(
      sex == "f" ~ "% Female",
      sex == "m" ~ "% Male",
      TRUE ~ "NA")) %>% 
    group_by(province, parameters=sex) %>% 
    summarize(freq = length(age_years)),
  
  hh_roster_joined %>% 
    filter(sex %in% "f") %>% 
    filter(age_years >= 15 & age_years < 50) %>% 
    group_by(province) %>% 
    summarize(freq = n(),
              parameters = "% All women 15-49 Years"),
  
  preg_lact_wom_joined %>% 
    group_by(province) %>% 
    summarize(freq = n(),
              parameters = "% Pregnant & Lactating Women (PLW)")
  
) %>% left_join(total_household, by="province") %>% 
  mutate(perc = case_when(
    parameters %notin% c("All household members", "Average household size") ~ round((freq/total)*100, 2),
    TRUE ~ freq
  )) %>% 
  pivot_wider(id_cols = parameters, names_from = province, values_from = perc)

total_province <- main %>% 
  count(province, name="total")

non_respondent <- main %>% 
  group_by(province, consent) %>% 
  summarize(freq = n(),
            parameters = "Average household size") %>% 
  left_join(total_province, by="province") %>% 
  filter(consent %notin% "Yes") %>% 
  mutate(perc = round(freq/total*100,1)) %>% 
  pivot_wider(id_cols = consent, names_from = province, values_from = perc)

demographic <- list(
  Demographic = demographic_data,
  Non_respondent = non_respondent
)

# remove extra variables -----------------------------------------------------------------
rm(total_household, demographic_data, total_province, non_respondent)
