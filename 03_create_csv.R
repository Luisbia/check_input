

library(odbc) # This library is not normally installed and will need to be installed
library(tidyverse)
library(data.table)
library(splitstackshape)

# Connect to denodo
vdp_con <- dbConnect(odbc(), "DenodoODBC")

#### REGACC

# Creating the query. This should take 2 minutes
sql_regacc <- "  SELECT * FROM hv_fame_regacc4regacc_all_dbs "
df_regacc<- dbGetQuery(vdp_con, sql_regacc)

# we write the results to an external file
fwrite(df_regacc,"data/denodo/regacc_orig.csv")
#df_regacc<- fread("data/denodo/regacc_orig.csv")

# we disconnect from denodo
dbDisconnect(vdp_con)

# Reshape

df_regacc<- df_regacc %>% 
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
  filter(table_identifier!="TNAMA") %>% 
  mutate(time_period=as.integer(time_period),
         obs_value=as.numeric(obs_value),
         NUTS=as.factor(str_length(ref_area)-2),
         country= as.character(str_sub(ref_area,1,2))) %>% 
  filter(country %in% country_sel) %>% 
  pivot_wider(names_from=type,
              values_from=obs_value) %>% 
  mutate(T=coalesce(T,V)) %>% 
  pivot_longer(cols=c(T,V),
               names_to="type",
               values_to="obs_value")

# Reconstruct series
##T
gvagr<- df_regacc %>% 
  filter(type=="T" & unit_measure=="PC")

lasty<- df_regacc %>% 
  filter(type=="T" & table_identifier=="T1001" & time_period== 2021 & 
         prices !="Y")# remove GVA in PYP

prevy<- df_regacc %>% 
  filter(type=="T" & table_identifier=="T1200" & time_period< 2021 & 
           prices !="Y" & NUTS!="3" & activity %in% c("_T","_Z") & 
           sto %in% c("POP","EMP","B1G") ) %>% # remove GVA in PYP
 mutate(table_identifier="T1001")

no_t1001<- df_regacc %>% 
  filter(type=="T" & table_identifier!="T1001" & prices!="Y")

T_series <- bind_rows(gvagr, lasty, prevy,no_t1001)

##V
gvagr<- df_regacc %>% 
  filter(type=="V" & unit_measure=="PC")

lasty<- df_regacc %>% 
  filter(type=="V" & table_identifier=="T1001" & time_period>= 2020 & 
           prices !="Y")# remove GVA in PYP

prevy<- df_regacc %>% 
  filter(type=="V" & table_identifier=="T1200" & time_period< 2020 & 
           prices !="Y" & NUTS!="3" & activity %in% c("_T","_Z")) %>% # remove GVA in PYP
  mutate(table_identifier="T1001")

no_t1001<- df_regacc %>% 
  filter(type=="V" & table_identifier!="T1001" & prices!="Y")

V_series <- bind_rows(gvagr, lasty, prevy,no_t1001)


df_regacc<- bind_rows(T_series,V_series) %>% 
  select(type,table_identifier,country,ref_area,NUTS,accounting_entry,sto,activity,unit_measure,time_period,obs_value)


fwrite(df_regacc,paste0("data/denodo/regacc_",country_sel,"_",format(Sys.time(),"%Y-%m-%d"),".csv"))
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
