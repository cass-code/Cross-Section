iv2_clean <- function(data){
  
  source("code/basic_clean.R")
  clean <- basic_clean(data)
  
    reg2<- clean %>% select(income, age, age2, school, tertiary, race, gender, father_schl, father_tert) %>% filter(income >0 & age>0 & school>=0 & tertiary>=0 & race>=1 &race<=4 & father_schl >=0) %>% 
      mutate(income = replace(income, income > 0, log(income))) %>% 
      mutate(tertiary = replace(tertiary, tertiary == 2, "No")) %>% 
      dplyr::mutate(father_tert = replace_na(father_tert, "No")) %>% 
      mutate(tertiary = replace(tertiary, tertiary == 1, "Yes")) %>% 
      mutate(school = replace(school, school >=0, school +1)) %>% 
      mutate(father_schl = replace(father_schl, father_schl >=0, father_schl +1)) %>% 
      mutate(father_schl = replace(father_schl, father_schl ==26, 0)) %>% 
      mutate(father_tert = replace(father_tert, father_tert >=1, "Yes")) %>% 
      mutate(school = replace(school, school ==26, 0)) %>% 
      filter(school >=0 & school <=14) %>%  as.data.frame()
    
    reg2<- reg2 %>%  mutate(race = replace(race, race == 1, "African")) %>% mutate(race = replace(race, race == 2, "Coloured")) %>%
      mutate(race = replace(race, race == 3, "Asian/Indian")) %>% mutate(race = replace(race, race == 4, "White")) %>%
      mutate(gender = replace(gender, gender == 2, "Female")) %>% 
      mutate(gender = replace(gender, gender == 1, "Male")) 
    reg2
    
  
  
}