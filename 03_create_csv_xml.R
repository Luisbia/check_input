library(tidyverse)
library(data.table)
library(splitstackshape)
library(luispack)

tem<- luispack::load_xml(folder =  "data/xml",
                          country_sel = country_sel,
                          consolidate = FALSE) %>%
  arrange(date) %>%
  group_by(across(
    c(
      table_identifier,
      ref_area,
      sto,
      accounting_entry,
      activity,
      unit_measure,
      time_period
    )
  )) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(country = str_sub(ref_area, 1, 2),
         NUTS = str_length(ref_area) - 2,
         NUTS=as.factor(NUTS)) %>%
  mutate(type = "T") %>% 
  as.data.table()

val<- data.table::fread("data/denodo/regacc_orig.csv")

# Reshape

val<- val %>% 
  select(name,date,value) %>% 
  na.omit() %>% 
  cSplit("name", sep = ".") %>% 
  rename("type"=name_01,
         "table_identifier"=name_02,
         "freq"=name_03,
         "ref_area"=name_04,
         "counterpart_area"=name_05,
         "ref_sector"=name_06,
         "counterpart_sector"=name_07,
         "accounting_entry"=name_08,
         "sto"=name_09,
         "activity"=name_10,
         "valuation"=name_11,
         "prices"=name_12,
         "transformation"=name_13,
         "unit_measure"=name_14
  ) %>% 
  rename(time_period=date,
         obs_value=value)%>% 
  filter(table_identifier!="TNAMA" & type =="V") %>% 
  mutate(time_period=as.integer(time_period),
         obs_value=as.numeric(obs_value),
         NUTS=as.factor(str_length(ref_area)-2),
         country= as.character(str_sub(ref_area,1,2))) %>% 
  filter(country %in% country_sel) 

# Reconstruct series

##V
gvagr<- val %>% 
  filter(type=="V" & unit_measure=="PC")

lasty<- val %>% 
  filter(type=="V" & table_identifier=="T1001" & time_period>= 2020 & 
           prices !="Y")# remove GVA in PYP

prevy<- val %>% 
  filter(type=="V" & table_identifier=="T1200" & time_period< 2020 & 
           prices !="Y" & NUTS!="3" & activity %in% c("_T","_Z")) %>% # remove GVA in PYP
  mutate(table_identifier="T1001")

no_t1001<- val %>% 
  filter(type=="V" & table_identifier!="T1001" & prices!="Y")

val <- bind_rows(gvagr, lasty, prevy,no_t1001)


regacc<- bind_rows(tem,val) %>% 
  select(type,table_identifier,country,ref_area,NUTS,accounting_entry,sto,activity,unit_measure,time_period,obs_value)


fwrite(regacc,paste0("data/denodo/regacc_",country_sel,"_",format(Sys.time(),"%Y-%m-%d"),".csv"))
# List of countries we want to create files
# list<- c("AL", "AT", "BE" ,"BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES",
#          "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "ME", "MK", "MT",
#          "NL", "NO", "PL","PT", "RO", "SE", "SI", "SK","TR", "RS")

# A function to create the csv files
# save_list<- function(x){
#   df_regacc %>%
#     filter(country == x) %>%
#     fwrite(.,paste0("data/denodo/",x,".csv"))
# }
# 
# # iterate the function over the list
# purrr::walk(list,~ save_list(.x))

# unlink("data/denodo/regacc",
#        recursive = TRUE)
#
# write_dataset(df_regacc,"data/denodo/regacc/",
#               partitioning = c("country"))

l <- ls()
rm(list = l[sapply(l, function(x) is.data.frame(get(x)))])
rm(l)
gc()

cat("Done")
