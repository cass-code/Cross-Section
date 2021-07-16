clean <- function(data){

  clean <- data %>% mutate(age = 2021 - w5_a_dob_y) %>% mutate(age2 = age * age) %>% 
    rename (gender = w5_a_gen, race = w5_a_popgrp, income = w5_a_em1pay, married = w5_a_mar, school = w5_a_edschgrd, union = w5_a_em1tru, father_schl = w5_a_fthsch , mother_schl = w5_a_mthsch, father_tert = w5_a_fthtert, mother_tert = w5_a_mthtert, father_occ = w5_a_fthwrk_isco_c, mother_occ = w5_a_mthwrk_isco_c) %>%
    select(age, age2, gender, race, income, married, school, union, father_schl, father_tert, father_occ, mother_schl, mother_tert, mother_occ) %>% 
    filter(income>0, race>0, married>0, school>0, union>0, father_schl>0, father_tert>0, father_occ>0, mother_schl>0, mother_tert>0, mother_occ>0) %>%
    # mutate(income = replace(income, income > 0, log(income)))
    as.data.frame()
  
  clean <- data %>% mutate(age = 2021 - w5_a_dob_y) %>% mutate(age2 = age * age) %>% 
    rename (gender = w5_a_gen, race = w5_a_popgrp, income = w5_a_em1pay, married = w5_a_mar, school = w5_a_edschgrd, union = w5_a_em1tru, father_schl = w5_a_fthsch , mother_schl = w5_a_mthsch, father_tert = w5_a_fthtert, mother_tert = w5_a_mthtert, father_occ = w5_a_fthwrk_isco_c, mother_occ = w5_a_mthwrk_isco_c) %>%
    select(age, age2, gender, race, income, married, school, union, father_schl, father_tert, father_occ, mother_schl, mother_tert, mother_occ) %>% 
    filter(income>0, race>0, married>0, school>0, union>0, father_schl>0, father_tert>0, father_occ>0, mother_schl>0, mother_tert>0, mother_occ>0) %>%
    mutate(income = replace(income, income > 0, log(income))) %>% as.data.frame()
    

    clean$age = as.numeric(clean$age)
    clean$race = as.numeric(clean$race)
    clean$ag2 = as.numeric(clean$age2)
    clean$gender = as.numeric(clean$gender)
    clean$income = as.numeric(clean$income)
    clean$married = as.numeric(clean$married)
    clean$union = as.numeric(clean$union)
    clean$school = as.numeric(clean$school)
    clean$father_schl = as.numeric(clean$father_schl)
    clean$mother_schl = as.numeric(clean$mother_schl)
    clean$father_occ = as.numeric(clean$father_occ)
    clean$mother_occ = as.numeric(clean$mother_occ)
    clean$father_tert = as.numeric(clean$father_tert)
    
    clean <- clean %>% mutate(race = replace(race, race == 1, "African")) %>% mutate(race = replace(race, race == 2, "Coloured")) %>%
      mutate(race = replace(race, race == 3, "Asian/Indian")) %>% mutate(race = replace(race, race == 4, "White")) %>%
      mutate(gender = replace(gender, gender == 2, 0)) %>%
      mutate(married = replace(married, married >= 2, 0)) %>%
      rename (male = gender)
    
    #%>% 
    #filter(race > 0 & race <5, income >=0, gender >0, married >0, school >=0, union >0, father_schl>=0, father_tert>=0, father_occ>=0, mother_schl>=0, mother_tert>=0, mother_occ>=0)
    
    # The NIDS data set has many variables so just selecting the ones I want
  
  clean$race <- as.numeric(clean$race)
  cleaned <- clean %>% filter(race > 0 & race <5, income >=0, gender >0, married >0, school >=0) %>%
    mutate(race = replace(race, race == 1, "African")) %>% mutate(race = replace(race, race == 2, "Coloured")) %>%
    mutate(race = replace(race, race == 3, "Asian/Indian")) %>% mutate(race = replace(race, race == 4, "White")) %>%
    mutate(age2 = age * age) %>%
    mutate(income = replace(income, income > 0, log(income))) %>%
    mutate(gender = replace(gender, gender == 2, 0)) %>%
    #mutate(tertiary = replace(tertiary, tertiary == 2, 0)) %>%
    mutate(married = replace(married, married >= 2, 0)) %>%
    rename (male = gender) %>%
    drop_na()
  # 
  # 
  # cleaned$income <- as.numeric(cleaned$income) # realised these should be numeric
  # cleaned$gender <- as.numeric(cleaned$male)
  # cleaned$married <- as.numeric(cleaned$married)
  # cleaned$school <- as.numeric(cleaned$school)
  # #cleaned$tertiary <- as.numeric(cleaned$tertiary)
  # cleaned$race <- as.factor(cleaned$race)

  clean
}