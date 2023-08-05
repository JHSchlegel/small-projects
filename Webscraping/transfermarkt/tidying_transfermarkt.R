install.packages("rjson")
library(rjson)
library(tidyverse)

dat <- fromJSON(file = "./transfermarkt/pages.json")

df <- as_tibble(dat)

View(df)

lapply(lapply(dat[[1]], function(x) cbind(player_name = c("number", "position", "market_value"), as_tibble(x))), function(x) as_tibble(x)  %>% pivot_longer(-player_name))
