iv1_clean <- function(data){
  
  source("code/basic_clean.R")
  clean <- basic_clean(data)
  iv1<- clean %>% select(income, age, age2, school, tertiary, father_schl, father_tert) %>% filter(income >0 & age>0 & school>=0 & tertiary>=0 & father_schl >=0) %>% 
    mutate(income = replace(income, income > 0, log(income))) %>% 
    dplyr::mutate(father_tert = replace_na(father_tert, "No")) %>% 
    mutate(school = replace(school, school >=0, school +1)) %>% 
    mutate(school = replace(school, school ==26, 0)) %>% 
    mutate(father_schl = replace(father_schl, father_schl >=0, father_schl +1)) %>% 
    mutate(father_schl = replace(father_schl, father_schl ==26, 0)) %>% 
    mutate(father_tert = replace(father_tert, father_tert >=1, "Yes")) %>% 
    mutate(tertiary = replace(tertiary, tertiary >1, "No")) %>% 
    mutate(tertiary = replace(tertiary, tertiary ==1, "Yes")) %>% 
    filter(school >=0 & school <=14) %>%  as.data.frame()
  
  
  
}