basic_clean4<- function(w4){
  
  clean <- w4 %>% mutate(age = 2021 - w4_a_dob_y) %>% mutate(age2 = age * age) %>% 
    rename (gender = w4_a_gen, tertiary = w4_a_edter, race = w4_a_popgrp, income = w4_a_em1pay, married = w4_a_mar, school = w4_a_edschgrd, union = w4_a_em1tru, father_schl = w4_a_fthsch , mother_schl = w4_a_mthsch, father_tert = w4_a_fthtertyn, mother_tert = w4_a_mthtertyn, father_occ = w4_a_fthwrk_isco_c, mother_occ = w4_a_mthwrk_isco_c) %>%
    select(age, age2, gender, race, income, married, school, tertiary, union, father_schl, father_tert, father_occ, mother_schl, mother_tert, mother_occ) 
  
  clean$age = as.numeric(clean$age)
  clean$race = as.numeric(clean$race)
  clean$ag2 = as.numeric(clean$age2)
  clean$gender = as.numeric(clean$gender)
  clean$income = as.numeric(clean$income)
  clean$married = as.numeric(clean$married)
  clean$union = as.numeric(clean$union)
  clean$school = as.numeric(clean$school)
  clean$tertiary = as.numeric(clean$tertiary)
  clean$father_schl = as.numeric(clean$father_schl)
  clean$mother_schl = as.numeric(clean$mother_schl)
  clean$father_occ = as.numeric(clean$father_occ)
  clean$mother_occ = as.numeric(clean$mother_occ)
  clean$father_tert = as.numeric(clean$father_tert)
  
  
  clean
  
}
