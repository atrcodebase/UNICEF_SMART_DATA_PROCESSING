#Creating subsets of each sheet ----------------------------------------------------------
main_sub <- main %>% 
  select(
    start,
    end,
    province,
    district,
    numfamily,
    `_index`,
    `_uuid`,
    Date,
    CLUSTER,
    TEAM,
    HH)
#rooster sheet
hh_roster_sub <- hh_roster_joined %>%
  left_join(child %>% select(child_status_first, hh_position=child_hh_position, `_submission__uuid`), 
            by=c("hh_position", "_submission__uuid")) %>% #joining child_status_first for Mortality v2
  mutate(join = case_when(
    join == "n" ~ NA_character_,
    join == "y" & born == "y" & age_years < 1 ~ NA_character_,
    TRUE ~ join),
    born = case_when(
      born == "n" ~ NA_character_,
      TRUE ~ born)) %>% 
  select(sex, age=age_years, join, born, child_status_first, `_parent_index`) %>% 
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
main_sub_roster <- left_join(hh_roster_sub,
                             main_sub, by = c("_parent_index" = "_index"))
main_sub_left <- left_join(left_sub, 
                           main_sub, by = c("_parent_index" = "_index"))
main_sub_died <- left_join(died_sub, 
                           main_sub, by = c("_parent_index" = "_index"))
joined_data <- rbind(main_sub_roster %>% select(-child_status_first), main_sub_left, main_sub_died)

joined_data <- joined_data %>% 
  rename(`_index`=`_parent_index`) %>% 
  select(names(main_sub), everything())

#Reshaping -------------------------------------------------------------------------------
joined_data <- joined_data %>% 
  group_by(`_index`) %>% 
  mutate(across(c(sex:location), as.character),
         n = case_when(
           row_number() > 20 ~ as.numeric(row_number()-20),
           TRUE ~ as.numeric(row_number())
         ),
         extra = case_when(
           row_number() > 20 ~ "yes",
           TRUE ~ NA_character_
         ), .before=sex) %>% 
  mutate(sex = case_when(
    sex == "Male" ~ "m",
    sex == "Female" ~ "f",
    TRUE ~ sex
  )) %>% 
  pivot_longer(-(start:extra), names_to = "question", values_to = "val") %>%
  ungroup()

#Final output
mortality_data_wide <- joined_data %>% 
  pivot_wider(names_from = c(n, question), values_from = "val", names_prefix = "P")


# Subset 2 -------------------------------------------------------------------------------
main_sub_roster2 <- main_sub_roster %>% 
  filter((age < 5 & child_status_first %in% "The child is present.") | (age >= 5)) %>% 
  select(-child_status_first)

joined_data2 <- rbind(main_sub_roster2, main_sub_left, main_sub_died)

joined_data2 <- joined_data2 %>% 
  rename(`_index`=`_parent_index`) %>% 
  select(names(main_sub), everything())

#Reshaping -------------------------------------------------------------------------------
joined_data2 <- joined_data2 %>% 
  group_by(`_index`) %>% 
  mutate(across(c(sex:location), as.character),
         n = case_when(
           row_number() > 20 ~ as.numeric(row_number()-20),
           TRUE ~ as.numeric(row_number())
         ),
         extra = case_when(
           row_number() > 20 ~ "yes",
           TRUE ~ NA_character_
         ), .before=sex) %>% 
  mutate(sex = case_when(
    sex == "Male" ~ "m",
    sex == "Female" ~ "f",
    TRUE ~ sex
  )) %>% 
  pivot_longer(-(start:extra), names_to = "question", values_to = "val") %>%
  ungroup()

#Final output
mortality_data_wide2 <- joined_data2 %>% 
  pivot_wider(names_from = c(n, question), values_from = "val", names_prefix = "P")

# remove extra variables -----------------------------------------------------------------
rm(main_sub, hh_roster_sub, left_sub, died_sub, main_sub_roster, main_sub_left, 
   main_sub_died, joined_data, main_sub_roster2, joined_data2)
