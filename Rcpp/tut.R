library(tidyverse)
library(skimr)
data(mtcars)
# analyze relationship with all numeric variables to mpg using skimr


mtcars %>% 
  select_if(is.numeric) %>% 
  skim()

write_csv(mtcars, "mtcars.csv")
