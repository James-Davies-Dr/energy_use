library(tidyverse)
library(janitor)
##import data from csv file

gas <- read.csv("gas.csv") %>% clean_names()
electric <- read.csv("electric.csv") %>% clean_names()

colnames(electric)
colnames(gas)


gas <- gas %>% 
  rename(
    gasm3 = consumption_m)
  
electric <- electric %>% 
  rename(
    kwh = consumption_k_wh)
  

gas2<-gas %>% 
  mutate(
    start_date = date(ymd_hms(start)),
    start_time = time(ymd_hms(start)),
    end_date = date(ymd_hms(end)),
    end_time = time(ymd_hms(end))
) %>% 
 select(-start,-end)

electric2 <- electric %>% 
  mutate(
    start_date = date(ymd_hms(start)),
    start_time = time(ymd_hms(start)),
    end_date = date(ymd_hms(end)),
    end_time = time(ymd_hms(end))
  ) %>% 
  select(-start,-end)

combo <- inner_join(gas2, electric2, by = "start_date")
