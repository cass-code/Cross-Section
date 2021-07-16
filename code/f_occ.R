f_occ <- function(data){
  source("code/basic_clean.R")
  library(tidyverse)
  clean <- basic_clean(data)
  occ<- clean %>% select(income, age, age2, school, ter, gender, tertiary, race, father_schl, father_terlvl, father_occ) %>% 
    filter(income >0 & age>0 & school>=0 & tertiary>=0 & father_schl >=0 & race<= 4 & race >=1 & father_occ >=0 & father_occ<10) %>% 
    mutate(father_terlvl = replace_na(father_terlvl, 0)) %>% 
    mutate(ter = replace_na(ter, 0)) %>% 
    mutate(income = replace(income, income > 0, log(income))) %>% 
    mutate(school = replace(school, school >=0, school +1)) %>% 
    mutate(school = replace(school, school ==26, 0)) %>% 
    mutate(father_schl = replace(father_schl, father_schl >=0, father_schl +1)) %>% 
    mutate(father_schl = replace(father_schl, father_schl ==26, 0)) %>% 
    mutate(father_terlvl = replace(father_terlvl, father_terlvl >=16 & father_terlvl<=19 | father_terlvl ==30 | father_terlvl ==31 | father_terlvl ==27 | father_terlvl ==28, 1)) %>% 
    mutate(father_terlvl = replace(father_terlvl, father_terlvl ==20 | father_terlvl ==29 | father_terlvl ==32, 3)) %>% 
    mutate(father_terlvl = replace(father_terlvl, father_terlvl ==21 | father_terlvl ==22 | father_terlvl ==33 | father_terlvl ==34, 4)) %>% 
    mutate(father_terlvl = replace(father_terlvl, father_terlvl ==23 | father_terlvl ==35, 5)) %>% 
    mutate(ter = replace(ter, ter >=16 & ter<=19 | ter ==30 | ter ==31 | ter ==27 | ter ==28, 1)) %>% 
    mutate(ter = replace(ter, ter ==20 | ter ==29 | ter ==32, 3)) %>% 
    mutate(ter = replace(ter, ter ==21 | ter ==22 | ter ==33 | ter ==34, 4)) %>% 
    mutate(ter = replace(ter, ter ==23 | ter ==35, 5)) %>% 
    mutate(education = school + ter) %>% 
    mutate(gender = replace(gender, gender == 2, "Female")) %>% 
    mutate(gender = replace(gender, gender == 1, "Male")) %>% 
    mutate(father_ed = father_schl + father_terlvl) %>% 
    mutate(race = replace(race, race == 1, "African")) %>% mutate(race = replace(race, race == 2, "Coloured")) %>%
    mutate(race = replace(race, race == 3, "Asian/Indian")) %>% mutate(race = replace(race, race == 4, "White")) %>%
    filter(education >=0 & education <=18) %>%  
    mutate(father_occ = replace(father_occ, father_occ == 1, "Legislator/Manager")) %>% mutate(father_occ = replace(father_occ, father_occ == 2, "Professional")) %>%
    mutate(father_occ = replace(father_occ, father_occ == 3, "Technician")) %>% mutate(father_occ = replace(father_occ, father_occ == 4, "Clerk")) %>%
    mutate(father_occ = replace(father_occ, father_occ == 5, "Service Worker")) %>% mutate(father_occ = replace(father_occ, father_occ ==6 , "Agriculture")) %>%
    mutate(father_occ = replace(father_occ, father_occ ==7 , "Craft/Trade")) %>% mutate(father_occ = replace(father_occ, father_occ == 8, "Machine Operator")) %>%
    mutate(father_occ = replace(father_occ, father_occ == 9, "Elementary")) %>%
    as.data.frame()
  
  occ
  
  
}
