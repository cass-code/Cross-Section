plot_eduInc <- function(data){
  
  clean <- data %>% mutate(age = 2021 - w5_a_dob_y) %>% mutate(age2 = age * age) %>% 
    rename (gender = w5_a_gen, tertiary=w5_a_edter, race = w5_a_popgrp, income = w5_a_em1pay, married = w5_a_mar, school = w5_a_edschgrd, union = w5_a_em1tru, father_schl = w5_a_fthsch , mother_schl = w5_a_mthsch, father_tert = w5_a_fthtert, mother_tert = w5_a_mthtert, father_occ = w5_a_fthwrk_isco_c, mother_occ = w5_a_mthwrk_isco_c) %>%
    select(income, school, tertiary) %>% 
    filter(income>=0 & income<100000, school>=0) %>%
    mutate(education = school +1) %>% 
    mutate(education = replace(education, education == 26, 0)) %>%   
    mutate(education = replace(education, tertiary ==1, 14)) %>% filter(education <=14) %>%  
    as.data.frame()
    # mutate(primary = school) %>% 
    # mutate(primary = replace(primary, primary != 7, 0)) %>% 
    # mutate(primary = replace(primary, primary == 7, 1)) %>% 
    # mutate(highschool = school) %>% 
    # mutate(highschool = replace(highschool, highschool != 12, 0)) %>% 
    # mutate(highschool = replace(highschool, highschool == 12, 1)) %>% 
    # mutate(noschool = school) %>% 
    # mutate(noschool = replace(noschool, noschool != 25, 0)) %>% 
    # mutate(noschool = replace(noschool, noschool == 25, 1))
  clean$income = as.numeric(clean$income) 
  clean$education = as.numeric(clean$education) 
  clean
    #max(clean$income)
    
  
}