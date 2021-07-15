reg1_clean <- function(data){
  
  source("code/basic_clean.R")
  clean <- basic_clean(data)
  reg1<- clean %>% select(income, age, age2, school, tertiary) %>% filter(income >0 & age>0 & school>=0 & tertiary>=0) %>% 
    mutate(income = replace(income, income > 0, log(income))) %>% 
    mutate(tertiary = replace(tertiary, tertiary == 2, "No")) %>% 
    mutate(tertiary = replace(tertiary, tertiary == 1, "Yes")) %>% 
    mutate(school = replace(school, school >=0, school +1)) %>% 
    mutate(school = replace(school, school ==26, 0)) %>% 
    filter(school >=0 & school <=14) %>%  as.data.frame()
 
   reg1
  
  
}