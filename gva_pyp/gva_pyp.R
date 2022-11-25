library(tidyverse)
library(rio)
library(data.table)
library(openxlsx)
library(data.table)


### National data
nat_nama <- fread("gva_pyp/NAMA.csv", header=TRUE) %>% 
  mutate(across(everything(),as.character)) %>% 
  pivot_longer(cols=c(4:26),
               names_to="time_period",
               values_to="obs_value") %>%  # CAREFUL
  filter(country %in% country_sel) %>% 
  na.omit() %>% 
  mutate(time_period=as.integer(time_period),
         obs_value=as.numeric(obs_value)) 
  


### Regional data

regacc<- read_parquet("data/denodo/all_primary.parquet") %>% 
  filter(country==country_sel)

gvagr<- regacc %>% 
  filter(type=="V" & unit_measure=="PC" & transformation =="G1" & sto=="B1G")  

lasty<- regacc %>% 
  filter(type=="V" & table_identifier=="T1001" & time_period== 2021 & 
           prices !="Y" & sto=="B1G" & unit_measure=="XDC")

prevy<- regacc %>% 
  filter(type=="V" & table_identifier=="T1200" &  sto =="B1G" & time_period< 2021 & 
           prices !="Y" & NUTS!="3" & activity=="_T" & unit_measure=="XDC" ) %>% 
  mutate(table_identifier="T1001")

regacc<- bind_rows(gvagr,lasty,prevy) %>% 
  select(country,ref_area,NUTS,sto,prices,unit_measure,time_period,obs_value)

### Calculate GVA PYP from CUP and Growth Rates

calculate_gva_pyp(country_sel=country_sel,
                  nat_dat=nat_nama,
                  reg_dat=regacc,
                  output_dir = "gva_pyp/output")
