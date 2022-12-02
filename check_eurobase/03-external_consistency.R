# Comparison MA
library(tidyverse)
library(eurostat)
library(openxlsx)

nama <- fread("data/denodo/nama.csv") %>% 
  setnames("obs_value","nama") %>% 
  mutate(unit_measure=str_replace(unit_measure,"XDC","MIO_NAC"),
         unit_measure=str_replace(unit_measure,"PC","PCH_PRE"),
         activity=str_replace(activity,"_T","TOTAL"))
  

nfsa <- fread("data/denodo/nfsa.csv") %>% 
  setnames("obs_value","nfsa")

ma_gdp<- get_annual_NAMA()%>% 
  rename(nama=obs_value) %>% 
  mutate(unit_measure=str_replace(unit_measure,"CP_MPPS_EU27_2020","MIO_PPS_EU27_2020"),
          unit_measure=str_replace(unit_measure,"CP_MEUR","MIO_EUR"))

df_new <- read_parquet("check_eurobase/data/new.parquet") %>% 
  select(-obs_status,-value,-date) %>% 
  filter(country %in% sel_countries) 

gdp2<- df_new %>% 
filter(table =="gdp2" & NUTS ==0) %>% 
  select(ref_area,unit_measure,time_period,obs_value) %>% 
left_join(ma_gdp,.) %>% 
  mutate(diff=round(nama-obs_value),
         diffp=round(diff*100/nama,1)) %>% 
  filter(abs(diff) >2)


##gva_gr ### Later add gdp
gvagr2<- df_new %>% 
  filter(unit_measure=="PCH_PRE" & sto=="B1G" & NUTS =="0") %>% 
  select(ref_area,unit_measure,time_period,obs_value) %>% 
  left_join(.,nama) %>% 
  na.omit() %>% 
  mutate(diff=round(nama-obs_value,2)) %>% 
  filter(abs(diff) >0.1)


##emp3

emp3<- df_new %>% 
  filter(table=="emp3" & NUTS ==0) %>% 
  select(ref_area,sto,activity,unit_measure,time_period,obs_value) %>% 
  left_join(.,nama) %>% 
  na.omit() %>% 
  mutate(diff=round(nama-obs_value),
         diffp=round(diff*100/nama,1)) %>% 
  filter(abs(diff) >2)
  

##b1g3
gva3<- df_new %>% 
  filter(table=="gva3" & NUTS ==0) %>% 
  select(ref_area,sto,activity,unit_measure,time_period,obs_value) %>% 
  left_join(.,nama) %>% 
  na.omit() %>% 
  mutate(diff=round(nama-obs_value),
         diffp=round(diff*100/nama,1)) %>% 
  filter(abs(diff) >2)


##hw2
hw2<- df_new %>% 
  filter(table=="emphw2" & NUTS ==0) %>% 
  select(ref_area,sto,activity,unit_measure,time_period,obs_value) %>% 
  left_join(.,nama) %>% 
  na.omit() %>% 
   mutate(diff=round(nama-obs_value),
         diffp=round(diff*100/nama,1)) %>% 
  filter(abs(diff) >100)


##coe2
coe2<- df_new %>% 
  filter(table=="coe2" & NUTS ==0) %>% 
  select(ref_area,sto,activity,unit_measure,time_period,obs_value) %>% 
  left_join(.,nama) %>% 
  na.omit() %>% 
  mutate(diff=round(nama-obs_value),
         diffp=round(diff*100/nama,1)) %>% 
  filter(abs(diff) >100)



##gfcf2
gfcf2<- df_new %>% 
  filter(table=="gfcf2" & NUTS ==0) %>% 
  select(ref_area,sto,activity,unit_measure,time_period,obs_value) %>% 
  left_join(.,nama) %>% 
  na.omit() %>% 
  mutate(diff=round(nama-obs_value),
         diffp=round(diff*100/nama,1)) %>% 
  filter(abs(diff) >2)

##pop
pop3<- df_new %>% 
  filter(table=="pop3" & NUTS ==0) %>% 
  select(ref_area,sto,activity,unit_measure,time_period,obs_value) %>% 
  left_join(.,nama) %>% 
  na.omit() %>% 
  mutate(diff=round(nama-obs_value),
         diffp=round(diff*100/nama,1)) %>% 
  filter(abs(diff) >2)


##hh2
hh2<- df_new %>% 
  filter(table=="hh2" & NUTS ==0 & unit_measure=="MIO_NAC") %>% 
  select(ref_area,sto,activity,accounting_entry,time_period,obs_value) %>% 
  left_join(.,nfsa) %>% 
  na.omit() %>% 
  mutate(diff=round(nfsa-obs_value),
         diffp=round(diff*100/nfsa,1)) %>% 
  filter(abs(diff) >2)



#write file
wb <- createWorkbook() 
modifyBaseFont(wb, fontSize = 12, fontName = "Calibri Light")
if (nrow(gdp2) > 0){
  addWorksheet(wb, "GDP")
  writeDataTable(wb, "GDP", gdp2, tableStyle = "TableStyleMedium13")}
if (nrow(gvagr2) > 0){
  addWorksheet(wb,"gvagr")
  writeDataTable(wb, "gvagr", gvagr2,tableStyle = "TableStyleMedium13")}
if (nrow(emp3) > 0){
  addWorksheet(wb,"emp3")
  writeDataTable(wb, "emp3", emp3,tableStyle = "TableStyleMedium13")}
if (nrow(gva3) > 0){
  addWorksheet(wb,"gva3")
  writeDataTable(wb, "gva3", gva3,tableStyle = "TableStyleMedium13")}
if (nrow(hw2) > 0){
  addWorksheet(wb,"hw2")
  writeDataTable(wb, "hw2", hw2,tableStyle = "TableStyleMedium13")}
if (nrow(coe2) > 0){
  addWorksheet(wb,"coe2")
  writeDataTable(wb, "coe2", coe2,tableStyle = "TableStyleMedium13")}
if (nrow(gfcf2) > 0){
  addWorksheet(wb,"gfcf2")
  writeDataTable(wb, "gfcf2", gfcf2,tableStyle = "TableStyleMedium13")}
if (nrow(hh2) > 0){
  addWorksheet(wb,"hh2")
  writeDataTable(wb, "hh2", hh2,tableStyle = "TableStyleMedium13")}


saveWorkbook(wb, paste0("check_eurobase/output/external_consistency_",format(Sys.time(),"%Y-%m-%d"),
                        ".xlsx"), overwrite = TRUE)

l <- ls()
rm(list = l[sapply(l, function(x) is.data.frame(get(x)))])