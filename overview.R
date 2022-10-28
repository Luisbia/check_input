library(luispack)
library(tidyverse)
library(data.table)

country_sel<- "DK"
gdp<- luispack::get_annual_GDP(na_item_sel="B1GQ",
                               unit_sel = c("CP_MEUR","CP_MNAC","CP_MPPS_EU27_2020"),
                               min_time="2000-01-01") %>% 
  filter(geo==country_sel) %>% 
    rename(sto=na_item,
           time_period=time,
           ref_area=geo,
           obs_value=values,
           unit_measure=unit) %>% 
mutate(time_period=as.integer(str_sub(time_period,1,4))) 


exc_rates<- gdp %>% 
  pivot_wider(names_from=unit_measure,
              values_from=obs_value) %>% 
  mutate(EUR=CP_MNAC/CP_MEUR,
         PPS=CP_MNAC/CP_MPPS_EU27_2020 ) %>% 
  select(ref_area,time_period,EUR,PPS)

new<-list.files(path="data/denodo",
                  pattern= glob2rx(paste0("*",country_sel,"*")),
                  full.names=TRUE) %>% 
  as_tibble() %>% 
  mutate(date=map(value,file.mtime)) %>% 
  unnest(date) %>% 
  arrange(desc(date)) %>% 
  head(1) %>% 
  select(value) %>% 
  pull() %>% 
  fread() %>%
  .[type =="T" & activity %in% c("_T","_Z"),] %>% 
  .[,NUTS:=as.factor(NUTS)] 

prev<- regacc_eurobase %>% 
  filter(country =="DK" & activity %in% c("TOTAL","Z"))

# load the NUTS for the country  
NUTS2021<-luispack::NUTS_2021 %>% 
  filter(country %in% country_sel) %>% 
  mutate(across(c(NUTS,label),as_factor)) %>% 
  mutate(country=as.character(country)) %>% 
  as.data.table()

new<- left_join(files,NUTS2021) %>% 
  relocate(label,.after = ref_area)
