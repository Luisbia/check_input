
ths_int<- 2
ths_per<- 0.1 


# Packages needed
library(rio)
library(tidyverse)
library(openxlsx)
library(regacc)
library(dataregacc)


df_dt <- regacc::load_xml(folder =  "data/xml",
                         country_sel = country_sel,
                         consolidate = FALSE) %>%
  mutate(NUTS=str_length(ref_area)-2,
         country=str_sub(ref_area,1,2)) %>% 
  select(date,
         table_identifier,
         country,
         ref_area,
         NUTS,
         sto,
         accounting_entry,
         activity,
         unit_measure,
         time_period,
         obs_value) %>% 
  arrange(date) %>% 
  group_by (table_identifier,
            country,
            ref_area,
            NUTS,
            sto,
            accounting_entry,
            activity,
            unit_measure,
            time_period) %>% 
  slice_tail(n=1) %>% 
  ungroup() %>% 
  select(-date) %>% 
  na.omit()

# load the nama data
nama <- fread("data/denodo/nama.csv") %>% 
  # change column name
  setnames("obs_value","nama")
# load the nfsa data
nfsa <- fread("data/denodo/nfsa.csv") %>% 
  # change column name
  setnames("obs_value","nfsa")

# T1001 external ----
if ("T1001" %in% unique(df_dt$table_identifier)){
  t1001_reg <- df_dt %>%
    filter(table_identifier =="T1001" & NUTS=="0" & country %in% country_sel) %>% 
    select(ref_area, sto, unit_measure, activity,time_period, obs_value)
  
  t1001_ext <- left_join(t1001_reg, nama) %>% 
    mutate(diff = round(obs_value - nama,1),
           diffp = round(diff * 100/nama,1)) %>% 
    filter(diff !=0)
}

# T1002 external ----
if ("T1002" %in% unique(df_dt$table_identifier)){
  
  t1002_reg <- df_dt %>%
    filter(table_identifier =="T1002" & NUTS=="0" & country %in% country_sel) %>% 
    select(ref_area, sto, unit_measure, activity,time_period, obs_value)
  
  t1002_ext <- left_join(t1002_reg, nama) %>% 
    mutate(diff = round(obs_value - nama,1),
           diffp = round(diff * 100/nama,1)) %>% 
    filter(abs(diff) >ths_int) %>% 
    filter(abs(diffp) >ths_per)
}
# T1200 external ----
if ("T1200" %in% unique(df_dt$table_identifier)){
  
  t1200_reg <- df_dt %>%
    filter(table_identifier =="T1200" & NUTS=="0") %>% 
    select(ref_area, sto, unit_measure, activity,time_period, obs_value)
  
  t1200_ext <- left_join(t1200_reg, nama) %>% 
    mutate(diff = round(obs_value - nama,1),
           diffp = round(diff * 100/nama,1)) %>% 
    filter(abs(diff) >ths_int) %>% 
    filter(abs(diffp) >ths_per)
}
# T1300 external ----
if ("T1300" %in% unique(df_dt$table_identifier)){
  
  t1300_reg <- df_dt %>%
    filter(table_identifier =="T1300" & NUTS=="0") %>% 
    select(ref_area, sto, accounting_entry,time_period, obs_value)
  
  
  t1300_ext <- left_join(t1300_reg, nfsa) %>% 
    mutate(diff = round(nfsa - nfsa,1),
           diffp = round(diff * 100/nfsa,1)) %>% 
    filter(abs(diff) >ths_int) %>% 
    filter(abs(diffp) >ths_per)
}
# ADDITIVITY NACE----

NACE <- df_dt %>%
  filter(table_identifier %in% c("T1002","T1200") & 
           sto %in% c("EMP", "SAL", "B1G", "D1")) %>%
  regacc::check_NACE(ths_abs = ths_int, ths_rel = ths_per)


# Table 13 ----
if ("T1300" %in% unique(df_dt$table_identifier)){
  
  t1300 <- df_dt %>%
    filter(activity =="_Z" &
             unit_measure =="XDC" ) %>% 
    select(-activity,-unit_measure) %>% 
    unite("sto",c(accounting_entry,sto)) %>% 
    na.omit() %>% 
    pivot_wider(names_from=sto, 
                values_from=obs_value) %>% 
    mutate(
      B_B5N_c = round(B_B2A3N + C_D1 + C_D4 - D_D4, digits = 0),
      B_B5N_d = round(B_B5N_c - B_B5N, digits = 0))
  if (country_sel %in% c("AT","BE","CY","DK","EE","FI","HU","LV","MT","PL","PT","RO","SK")){
    t1300<-t1300 %>% 
      mutate(B_B6N_c=round(B_B5N + C_D61 +  C_D62 + C_D7 - D_D5  - D_D61 - D_D62 - D_D7, digits =0))}
  
  if (country_sel %in% c("BG","CZ","DE","EL","ES","FR","IE","IT","LT","LU","NL","NO","RS","SE","SI")){
    t1300<-t1300 %>% 
      mutate(B_B6N_c=round(B_B5N + C_D62 + C_D7 - D_D5  - D_D61  - D_D7, digits =0))}
  
  if (country_sel %in% c("HR")){
    t1300<-t1300 %>% 
      mutate(B_B6N_c=round(B_B5N + C_D61 +  C_D62 + C_D7 - D_D5  - D_D61 - D_D7, digits =0))}
  
  t1300<- t1300 %>% 
    mutate(B_B6N_d = round(B_B6N_c - B_B6N, digits = 0)) %>% 
    filter(if_any(ends_with("_d"), ~ abs(.x) > 3))}

#


### NUTS ----
temp<-df_dt %>% 
  filter(unit_measure !="PC")

NUTS<-regacc::check_NUTS(temp,ths_abs = ths_int, ths_rel = ths_per)


# Negative values ----
negative <- df_dt %>%
  filter(unit_measure !="PC") %>% 
  pivot_wider(names_from = sto,
              values_from = obs_value)

if("EMP" %in% names(negative)){
  negative<-negative %>% 
  mutate(SELF = EMP - SAL )} 

negative<- negative %>% 
  mutate(time_period=as.character(time_period)) %>% 
  pivot_longer(cols = c(where(is.numeric)),
               names_to = "sto",
               values_to = "obs_value") %>% 
  na.omit() %>% 
  filter(obs_value < 0) 

### BTEC ----
BTEC <- df_dt %>%
  filter(sto %in% c("B1G","D1","EMP","SAL","P51G")& activity %in% c("C", "BTE")) %>% 
  pivot_wider(names_from = sto, 
              values_from = obs_value)

if("EMP" %in% names(BTEC)){
  BTEC<-BTEC %>% 
    mutate(SELF = EMP - SAL )} 

BTEC<- BTEC %>% 
  mutate(time_period=as.character(time_period)) %>% 
  pivot_longer(cols = c(where(is.numeric)),
               names_to="sto",
               values_to="obs_value") %>% 
  pivot_wider(names_from = activity, 
              values_from = obs_value) %>%
  mutate(check = round(BTE - C, digits=1)) %>%
  filter(check < 0)

### Volume ----

if("T1001" %in% unique(df_dt$table_identifier) &
   "T1200" %in% unique(df_dt$table_identifier)){
  
  vol<- df_dt %>% 
    filter( NUTS !="3" &
              unit_measure %in% c("XDC","PC") &
              activity=="_T" &
              sto =="B1G" ) %>% 
    select(table_identifier,NUTS,country,ref_area,time_period,unit_measure,obs_value) %>% 
    pivot_wider (names_from = table_identifier,
                 values_from = obs_value) %>% 
    mutate(obs_value = coalesce(T1001,T1200)) %>% 
    select(-T1001, -T1200) %>% 
    pivot_wider(names_from = unit_measure,
                values_from = obs_value)
  
  
  vol1<- vol %>% 
    select(-PC) %>% 
    filter(NUTS=="1") %>% 
    mutate(geo1 = substr(ref_area, start = 1, stop = 2)) %>%
    group_by(country,geo1,time_period) %>%
    mutate(share=XDC/sum(XDC)) %>% 
    ungroup() %>% 
    select(-geo1,-XDC,-NUTS)
  
  
  vol1_temp<- left_join(vol1,vol) %>% 
    group_by(ref_area) %>% 
    arrange(time_period,by_group=TRUE) %>% 
    mutate(agg=PC*lag(share)) %>% 
    group_by(country,time_period) %>%
    summarise(agg=sum(agg)) %>% 
    rename(ref_area=country) %>% 
    na.omit()
  
  vol1 <-  left_join(vol1_temp,vol)
  
  vol2<- vol %>% 
    select(-PC) %>% 
    filter(NUTS=="2") %>% 
    mutate(geo2 = substr(ref_area, start = 1, stop = 3)) %>%
    group_by(country,geo2,time_period) %>%
    mutate(share=XDC/sum(XDC)) %>% 
    ungroup() %>% 
    select(-geo2,-XDC,-NUTS) 
  
  vol2_temp<- left_join(vol2,vol) %>% 
    group_by(ref_area) %>%
    arrange(time_period,by_group=TRUE) %>% 
    mutate(agg=PC*lag(share)) %>%
    mutate(geo2=substr(ref_area, start = 1, stop = 3)) %>% 
    group_by(geo2,time_period) %>%
    summarise(agg=sum(agg)) %>% 
    rename(ref_area=geo2) %>% 
    na.omit()
  
  vol2 <-  left_join(vol2_temp,vol)
  
  NUTS_vol<- bind_rows(vol1,vol2) %>% 
    select(ref_area,time_period,agg,PC) %>% 
    mutate(agg=round(agg,2),
           PC=round(PC,2)) %>% 
    mutate(diff=round(agg-PC,2)) %>% 
    filter(abs(diff)> 0.05)
}

#write file
wb <- createWorkbook() 
modifyBaseFont(wb, fontSize = 12, fontName = "Calibri Light")

# neeed to learn doing that with a function
if (any(ls() %in% "t1001_ext")) {
  if (nrow(t1001_ext)>0){
    addWorksheet(wb,"t1001_ext")
    writeDataTable(wb, "t1001_ext", t1001_ext, tableStyle = "TableStyleMedium13")}}
if (any(ls() %in% "t1002_ext")) {
  if (nrow(t1002_ext)>0){
    addWorksheet(wb,"t1002_ext")
    writeDataTable(wb, "t1002_ext", t1002_ext, tableStyle = "TableStyleMedium13")}}
if (any(ls() %in% "t1200_ext")) {
  if (nrow(t1200_ext)>0){
    addWorksheet(wb,"t1200_ext")
    writeDataTable(wb, "t1200_ext", t1200_ext, tableStyle = "TableStyleMedium13")}}
if (any(ls() %in% "t1300_ext")) {
  if (nrow(t1300_ext)>0){
    addWorksheet(wb,"t1300_ext")
    writeDataTable(wb, "t1300_ext", t1300_ext, tableStyle = "TableStyleMedium13")}}
if (any(ls() %in% "NACE")) {
  if (nrow(NACE)>0){
    addWorksheet(wb,"NACE")
    writeDataTable(wb, "NACE", NACE, tableStyle = "TableStyleMedium13")}}
if (any(ls() %in% "t1300")) {
  if (nrow(t1300)>0){
    addWorksheet(wb,"t1300")
    writeDataTable(wb, "t1300", t1300, tableStyle = "TableStyleMedium13")}}
if (any(ls() %in% "NUTS")) {
  if (nrow(NUTS)>0){
    addWorksheet(wb,"NUTS")
    writeDataTable(wb, "NUTS", NUTS, tableStyle = "TableStyleMedium13")}}
if (any(ls() %in% "negative")) {
  if (nrow(negative)>0){
    addWorksheet(wb,"negative")
    writeDataTable(wb, "negative", negative, tableStyle = "TableStyleMedium13")}}
if (any(ls() %in% "BTEC")) {
  if (nrow(BTEC)>0){
    addWorksheet(wb,"BTEC")
    writeDataTable(wb, "BTEC", BTEC, tableStyle = "TableStyleMedium13")}}
if (any(ls() %in% "NUTS_vol")) {
  if (nrow(NUTS_vol)>0){
    addWorksheet(wb,"NUTS_vol")
    writeDataTable(wb, "NUTS_vol", NUTS_vol, tableStyle = "TableStyleMedium13")}}

# cons <- function(df){
#   if (any(ls() %in% {{df}})) {
#     if (nrow(df)>0){
#       addWorksheet(wb,{{df}})
#       writeDataTable(wb, {{df}}, df, tableStyle = "TableStyleMedium13")}}
# }
# 
# cons("t1001_ext")
# cons("t1002_ext")
# cons("t1200_ext")
# cons("t1300_ext")
# cons("NACE")
# cons("t1300")
# cons("NUTS")
# cons("negative")
# cons("BTEC")
# cons("NUTS_vol")

saveWorkbook(wb, paste0("basic_checks/",country_sel,"_basic_check_xml_",
                        format(Sys.time(),"%Y-%m-%d"),
                        ".xlsx"), overwrite = TRUE)

l <- ls()
rm(list = l[sapply(l, function(x) is.data.frame(get(x)))])
rm(l)

cat("Done")
