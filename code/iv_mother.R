iv_mother <- function(data){
  
  source("code/basic_clean.R")
  clean <- basic_clean(data)
  iv1<- clean %>% select(income, age, age2, school, ter, gender, tertiary, race, mother_schl, mother_terlvl, mother_tert) %>% filter(income >0 & age>0 & school>=0 & tertiary>=0 & mother_schl >=0 & mother_tert >=0 & race<= 4 & race >=1) %>% 
    mutate(mother_terlvl = replace_na(mother_terlvl, 0)) %>% 
    mutate(ter = replace_na(ter, 0)) %>% 
    mutate(income = replace(income, income > 0, log(income))) %>% 
    mutate(school = replace(school, school >=0, school +1)) %>% 
    mutate(school = replace(school, school ==26, 0)) %>% 
    mutate(mother_schl = replace(mother_schl, mother_schl >=0, mother_schl +1)) %>% 
    mutate(mother_schl = replace(mother_schl, mother_schl ==26, 0)) %>% 
    mutate(mother_terlvl = replace(mother_terlvl, mother_terlvl >=16 & mother_terlvl<=19 | mother_terlvl ==30 | mother_terlvl ==31 | mother_terlvl ==27 | mother_terlvl ==28, 1)) %>% 
    mutate(mother_terlvl = replace(mother_terlvl, mother_terlvl ==20 | mother_terlvl ==29 | mother_terlvl ==32, 3)) %>% 
    mutate(mother_terlvl = replace(mother_terlvl, mother_terlvl ==21 | mother_terlvl ==22 | mother_terlvl ==33 | mother_terlvl ==34, 4)) %>% 
    mutate(mother_terlvl = replace(mother_terlvl, mother_terlvl ==23 | mother_terlvl ==35, 5)) %>% 
    mutate(ter = replace(ter, ter >=16 & ter<=19 | ter ==30 | ter ==31 | ter ==27 | ter ==28, 1)) %>% 
    mutate(ter = replace(ter, ter ==20 | ter ==29 | ter ==32, 3)) %>% 
    mutate(ter = replace(ter, ter ==21 | ter ==22 | ter ==33 | ter ==34, 4)) %>% 
    mutate(ter = replace(ter, ter ==23 | ter ==35, 5)) %>% 
    mutate(education = school + ter) %>% 
    mutate(gender = replace(gender, gender == 2, "Female")) %>% 
    mutate(gender = replace(gender, gender == 1, "Male")) %>% 
    mutate(mother_ed = mother_schl + mother_terlvl) %>% 
    mutate(race = replace(race, race == 1, "African")) %>% mutate(race = replace(race, race == 2, "Coloured")) %>%
    mutate(race = replace(race, race == 3, "Asian/Indian")) %>% mutate(race = replace(race, race == 4, "White")) %>%
    filter(education >=0 & education <=18) %>%  as.data.frame()
  
  # cor(iv1$education, iv1$mother_ed, method = "pearson")
  iv1
  
  
}
