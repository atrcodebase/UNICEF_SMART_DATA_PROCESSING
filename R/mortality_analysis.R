#Creating subsets of each sheet ----------------------------------------------------------
main_sub <- main %>% 
  select(
    start,
    end,
    province,
    district,
    numfamily,
    total_left,
    total_died,
    `_index`,
    `_uuid`,
    Date,
    CLUSTER,
    TEAM,
    HH)
#rooster sheet
hh_roster_sub <- hh_roster_joined %>%
  mutate(join = case_when(
    join == "n" ~ NA_character_,
    join == "y" & born == "y" & age_years < 1 ~ NA_character_,
    TRUE ~ join),
    born = case_when(
      born == "n" ~ NA_character_,
      TRUE ~ born)) %>% 
  select(sex, age=age_years, join, born, `_parent_index`) %>% 
  mutate(left="", died="", cause="", location="") %>% 
  relocate(left, .before = born)
#Left sheet
left_sub <- left %>%
  select(left, sex=left_gender, age=left_age_years, `_parent_index`) %>% 
  mutate(join="", born="", died="", cause="", location="")
#Died sheet
died_sub <- died %>%
  select(died, sex=died_gender, age=died_age_years, cause=cause_of_death, 
         location=location_of_death, `_parent_index`) %>% 
  mutate(left="", join="", born="")

#Merging the sheets ----------------------------------------------------------------------
main_sub_roster <- left_join(main_sub, 
                             hh_roster_sub, by = c("_index" = "_parent_index"))
main_sub_left <- left_join(main_sub %>% filter(total_left > 0), 
                           left_sub, by = c("_index" = "_parent_index"))
main_sub_died <- left_join(main_sub %>% filter(total_died > 0), 
                           died_sub, by = c("_index" = "_parent_index"))
joined_data <- rbind(main_sub_roster, main_sub_left, main_sub_died) 

#Reshaping -------------------------------------------------------------------------------
joined_data <- joined_data %>% 
  group_by(`_index`) %>% 
  mutate(across(c(sex:location), as.character),
         n = row_number(), .before=sex) %>% 
  pivot_longer(-(start:n), names_to = "question", values_to = "val") %>%
  ungroup()
#Final output
mortality_data_wide <- joined_data %>% 
  pivot_wider(names_from = c(n, question), values_from = "val", names_prefix = "P")

# remove extra variables -----------------------------------------------------------------
rm(main_sub, hh_roster_sub, left_sub, died_sub, main_sub_roster, main_sub_left, main_sub_died, joined_data)
