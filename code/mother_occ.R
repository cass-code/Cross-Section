mother_occ <- function(data){
  library
  source("code/basic_clean.R")
  clean <- basic_clean(data)
  iv1<- clean %>% select(income, age, age2, school, gender, tertiary, race, mother_occ) %>% filter(income >0 & age>0 & school>=0 & tertiary>=0 & mother_occ >=0 & race<= 4 & race >=1) %>% 
    mutate(income = replace(income, income > 0, log(income))) %>% 
    mutate(school = replace(school, school >=0, school +1)) %>% 
    mutate(school = replace(school, school ==26, 0)) %>% 
    mutate(tertiary = replace(tertiary, tertiary >1, 0)) %>% 
    mutate(education = school + tertiary) %>% 
    mutate(gender = replace(gender, gender == 2, "Female")) %>% 
    mutate(gender = replace(gender, gender == 1, "Male")) %>% 
    mutate(race = replace(race, race == 1, "African")) %>% mutate(race = replace(race, race == 2, "Coloured")) %>%
    mutate(race = replace(race, race == 3, "Asian/Indian")) %>% mutate(race = replace(race, race == 4, "White")) %>%
    filter(education >=0 & education <=14) %>%  
    filter(mother_occ <10) %>% 
    mutate(mother_occ = replace(mother_occ, mother_occ == 1, "Legislator/Manager")) %>% mutate(mother_occ = replace(mother_occ, mother_occ == 2, "Professional")) %>%
    mutate(mother_occ = replace(mother_occ, mother_occ == 3, "Technician")) %>% mutate(mother_occ = replace(mother_occ, mother_occ == 4, "Clerk")) %>%
    mutate(mother_occ = replace(mother_occ, mother_occ == 5, "Service Worker")) %>% mutate(mother_occ = replace(mother_occ, mother_occ ==6 , "Agriculture")) %>%
    mutate(mother_occ = replace(mother_occ, mother_occ ==7 , "Craft/Trade")) %>% mutate(mother_occ = replace(mother_occ, mother_occ == 8, "Machine Operator")) %>%
    mutate(mother_occ = replace(mother_occ, mother_occ == 9, "Elementary")) %>%
    as.data.frame()
  
  iv1
  
  
}
