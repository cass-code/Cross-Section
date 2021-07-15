iv1_clean4 <- function(w4){
  
  source("code/basic_clean4.R")
  clean <- basic_clean4(w4)
  iv1<- clean %>% select(income, age, age2, school, gender, tertiary, race, father_schl, father_tert) %>% filter(income >0 & age>0 & school>=0 & tertiary>=0 & father_schl >=0 & father_tert >=0 & race<= 4 & race >=1) %>% 
    mutate(income = replace(income, income > 0, log(income))) %>% 
    mutate(school = replace(school, school >=0, school +1)) %>% 
    mutate(school = replace(school, school ==26, 0)) %>% 
    mutate(father_schl = replace(father_schl, father_schl >=0, father_schl +1)) %>% 
    mutate(father_schl = replace(father_schl, father_schl ==26, 0)) %>% 
    mutate(father_tert = replace(father_tert, father_tert ==2, 0)) %>% 
    mutate(tertiary = replace(tertiary, tertiary >1, 0)) %>% 
    mutate(education = school + tertiary) %>% 
    mutate(gender = replace(gender, gender == 2, "Female")) %>% 
    mutate(gender = replace(gender, gender == 1, "Male")) %>% 
    mutate(father_ed = father_schl + father_tert) %>% 
    mutate(race = replace(race, race == 1, "African")) %>% mutate(race = replace(race, race == 2, "Coloured")) %>%
    mutate(race = replace(race, race == 3, "Asian/Indian")) %>% mutate(race = replace(race, race == 4, "White")) %>%
    filter(education >=0 & education <=14 & father_ed >=0 & father_ed <=14 ) %>%  as.data.frame()
  
  cor(iv1$education, iv1$father_ed, method = "pearson")
  iv1
  
  
}
