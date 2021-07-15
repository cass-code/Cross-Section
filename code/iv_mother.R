iv_mother <- function(data){
  
  source("code/basic_clean.R")
  clean <- basic_clean(data)
  iv1<- clean %>% select(income, age, age2, school, gender, tertiary, race, mother_schl, mother_tert) %>% filter(income >0 & age>0 & school>=0 & tertiary>=0 & mother_schl >=0 & mother_tert >=0 & race<= 4 & race >=1) %>% 
    mutate(income = replace(income, income > 0, log(income))) %>% 
    dplyr::mutate(mother_tert = replace_na(mother_tert, 0)) %>% 
    mutate(school = replace(school, school >=0, school +1)) %>% 
    mutate(school = replace(school, school ==26, 0)) %>% 
    mutate(mother_schl = replace(mother_schl, mother_schl >=0, mother_schl +1)) %>% 
    mutate(mother_schl = replace(mother_schl, mother_schl ==26, 0)) %>% 
    mutate(mother_tert = replace(mother_tert, mother_tert ==2, 0)) %>% 
    mutate(tertiary = replace(tertiary, tertiary >1, 0)) %>% 
    mutate(education = school + tertiary) %>% 
    mutate(gender = replace(gender, gender == 2, "Female")) %>% 
    mutate(gender = replace(gender, gender == 1, "Male")) %>% 
    mutate(mother_ed = mother_schl + mother_tert) %>% 
    mutate(race = replace(race, race == 1, "African")) %>% mutate(race = replace(race, race == 2, "Coloured")) %>%
    mutate(race = replace(race, race == 3, "Asian/Indian")) %>% mutate(race = replace(race, race == 4, "White")) %>%
    filter(education >=0 & education <=14) %>%  as.data.frame()
  
  cor(iv1$tertiary, iv1$mother_tert, method = "pearson")
  iv1
  
  
}
