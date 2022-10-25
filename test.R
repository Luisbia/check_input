library(tidyverse)

time_min<- "2021-10-12"
time_max<- "2023-03-01"
country_sel<- "SI"
table_sel <- c("T1001","T1002", "T1200", "T1300")

df<-list.files(path= "data/xml",
               pattern = glob2rx("*xml$"),
               full.names = TRUE,
               recursive=FALSE) %>%
  as_tibble() %>% 
  mutate(date=map(value,file.mtime)) %>% 
  unnest(date) %>% 
  filter(date > time_min & date < time_max) %>% 
  mutate(country= str_sub(value,-22,-21),
         table = str_sub(value,-30,-26)) %>% 
  filter(country %in% country_sel &
           table %in% table_sel) %>%
  group_by(table) %>% 
  arrange(date) %>% 
  slice_tail(n=1) %>% 
  ungroup() %>% 
  select(value) %>% 
  pull() %>% 
  map(read_sdmx)

  
  read_sdmx()
  map_dfr(readsdmx::read_sdmx) %>% 
  unnest(data)
                    
