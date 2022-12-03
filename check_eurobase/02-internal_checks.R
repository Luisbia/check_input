
library(tidyverse)
library(regacc)
library(openxlsx)

# read data
df_new <- read_parquet("check_eurobase/data/new.parquet") %>% 
  select(-obs_status,-value,-date) %>% 
  filter(country %in% sel_countries) 


## NUTS aggregation -----

NUTS<- df_new %>% 
  filter(unit_measure %in% c("MIO_EUR", "MIO_NAC", "MIO_PPS_EU27_2020", "PS", "THS", "HW") & 
         !table %in% c("gvagr2")) %>% 
  rename(table_identifier = table) %>% 
  check_NUTS(ths_abs = 1, ths_rel = 0.1)


# NACE

NACE<- df_new %>% 
  filter(table %in% c("gva3", "emp3", "coe2", "gfcf2", "emphw2")) %>% 
        check_NACE(ths_abs = 1)
  

# Table 13 ----

t1300 <- df_new %>%
  filter(table== "hh2" & unit_measure =="MIO_NAC") %>% 
  unite("sto",c(accounting_entry,sto)) %>% 
  select(-activity) %>% 
  pivot_wider(names_from=sto, 
              values_from=obs_value) %>% 
  mutate(
    B_B5N_c = round(B_B2A3N + C_D1 + C_D4 - D_D4, digits = 0),
    B_B5N_d = round(B_B5N_c - B_B5N, digits = 0)) %>% 
    mutate(B_B6N_c=case_when(
                  country %in% c("AT","BE","CY","DK","EE","FI","HU","LV","MT","PL","PT","RO","SK")
                  ~round(B_B5N + C_D61 +  C_D62 + C_D7 - D_D5  - D_D61 - D_D62 - D_D7, digits =0),
                  country %in% c("BG","CZ","DE","EL","ES","FR","IE","IT","LT","LU","NL","NO","RS","SE","SI")
                   ~round(B_B5N + C_D62 + C_D7 - D_D5  - D_D61  - D_D7, digits =0),
                  country %in% c("HR")
           ~round(B_B5N + C_D61 +  C_D62 + C_D7 - D_D5  - D_D61 - D_D7, digits =0))) %>% 
  mutate(B_B6N_d = round(B_B6N_c - B_B6N, digits = 0)) %>% 
  filter(if_any(ends_with("_d"), ~ abs(.x) > 3))

  

## Negative values ----
negative <- df_new %>%
  filter(unit_measure !="PCH_PRE") %>% 
  filter(obs_value < 0) 

## EMP > SAL

empsal<- df_new %>% 
filter(sto %in% c("EMP","SAL")) %>% 
  pivot_wider(names_from=sto,
              values_from=obs_value) %>% 
  mutate(SELF=EMP-SAL) %>% 
  filter(SELF<0)

#### GVA volume----

vol<- df_new %>% 
  filter(table %in% c("gva3","gvagr2") & 
          NUTS !="3" &
          unit_measure %in% c("MIO_NAC","PCH_PRE") &
          activity=="TOTAL") %>% 
  select(NUTS,country,ref_area,time_period,unit_measure,obs_value) %>% 
  pivot_wider(names_from = unit_measure,
              values_from = obs_value)

vol1<- vol %>% 
  select(-PCH_PRE) %>% 
  filter(NUTS=="1") %>% 
  mutate(ref_area1 = substr(ref_area, start = 1, stop = 2)) %>%
  group_by(country,ref_area1,time_period) %>%
  mutate(share=MIO_NAC/sum(MIO_NAC)) %>% 
  ungroup() %>% 
  select(-ref_area1,-MIO_NAC)
  
vol1_temp<- left_join(vol1,vol) %>% 
  group_by(ref_area) %>% 
  mutate(agg=PCH_PRE*lag(share)) %>% 
  group_by(country,time_period) %>%
  summarise(agg=sum(agg)) %>% 
  rename(ref_area=country) %>% 
  mutate(agg=round(agg,1)) %>% 
  na.omit()

vol1 <-  left_join(vol1_temp,vol) %>% 
  select(ref_area,time_period,agg,PCH_PRE) %>% 
  mutate(diff=agg-PCH_PRE) %>% 
  filter(abs(diff)>= 0.2)

vol2<- vol %>% 
  select(-PCH_PRE) %>% 
  filter(NUTS=="2") %>% 
  mutate(ref_area2 = substr(ref_area, start = 1, stop = 3)) %>%
  group_by(country,ref_area2,time_period) %>%
  mutate(share=MIO_NAC/sum(MIO_NAC)) %>% 
  ungroup() %>% 
  select(-ref_area2,-MIO_NAC)

vol2_temp<- left_join(vol2,vol) %>% 
  group_by(ref_area) %>% 
  mutate(agg=PCH_PRE*lag(share)) %>%
  mutate(ref_area2=substr(ref_area, start = 1, stop = 3)) %>% 
  group_by(ref_area2,time_period) %>%
  summarise(agg=sum(agg)) %>% 
  rename(ref_area=ref_area2) %>% 
  mutate(agg=round(agg,1)) %>% 
  na.omit()

vol2 <-  left_join(vol2_temp,vol) %>% 
  select(ref_area,time_period,agg,PCH_PRE) %>% 
  mutate(diff=agg-PCH_PRE) %>% 
  filter(abs(diff)> 0.2)

vol<- bind_rows(vol1,vol2)


#write file
wb <- createWorkbook() 
modifyBaseFont(wb, fontSize = 12, fontName = "Calibri Light")
if (nrow(NUTS) > 0){
  addWorksheet(wb, "NUTS")
  writeDataTable(wb, "NUTS", NUTS, tableStyle = "TableStyleMedium13")}
if (nrow(NACE) > 0){
  addWorksheet(wb,"NACE")
  writeDataTable(wb, "NACE", NACE,tableStyle = "TableStyleMedium13")}
if (nrow(t1300) > 0){
  addWorksheet(wb,"t1300")
  writeDataTable(wb, "t1300", t1300,tableStyle = "TableStyleMedium13")}
if (nrow(negative) > 0){
  addWorksheet(wb,"negative")
  writeDataTable(wb, "negative", negative,tableStyle = "TableStyleMedium13")}
if (nrow(empsal) > 0){
  addWorksheet(wb,"self")
  writeDataTable(wb, "self", empsal,tableStyle = "TableStyleMedium13")}
if (nrow(vol) > 0){
  addWorksheet(wb,"vol")
  writeDataTable(wb, "vol", vol,tableStyle = "TableStyleMedium13")}

saveWorkbook(wb, paste0("check_eurobase/output/internal_consistency_",format(Sys.time(),"%Y-%m-%d"),
                        ".xlsx"), overwrite = TRUE)

l <- ls()
rm(list = l[sapply(l, function(x) is.data.frame(get(x)))])
