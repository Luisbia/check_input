library(data.table)
library(openxlsx)
library(tidyverse)
library(regacc)
library(dataregacc)
ths_int<- 1
ths_hw<-100
ths_per<- 0.1 

df_dt<-list.files(path="data/csv",
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
  filter(type=="T")

nama <- fread("data/denodo/nama.csv") %>% 
  setnames("obs_value","nama")

nfsa <- fread("data/denodo/nfsa.csv") %>% 
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
           diffp = round(diff * 100/nama,1))
  
  temp1<- t1002_ext %>% 
    filter(unit_measure!="HW") %>% 
    filter(abs(diff) >ths_int) %>% 
    filter(abs(diffp) >ths_per)
  
  temp2<- t1002_ext %>% 
    filter(unit_measure=="HW") %>% 
    filter(abs(diff) >ths_hw) %>% 
    filter(abs(diffp) >ths_per)
  
  t1002_ext<- bind_rows(temp1,temp2)
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

NACE1 <- df_dt %>%
  filter(table_identifier %in% c("T1002","T1200") & 
           sto %in% c("EMP", "SAL", "B1G", "D1") & unit_measure !="HW") %>% 
  check_NACE(ths_abs = ths_int, ths_rel = ths_per)

NACE2 <- df_dt %>%
  filter(table_identifier %in% c("T1002") & 
           sto %in% c("EMP", "SAL") & unit_measure =="HW") %>% 
  check_NACE(ths_abs = ths_int, ths_rel = ths_hw)

NACE3<- df_dt %>% 
  filter(table_identifier %in% c("T1002","T1200") & 
           sto %in% c("EMP", "SAL", "B1G", "D1")) %>%
  mutate(activity=str_replace_all(activity,"_T","TOTAL")) %>%
  tidyr::pivot_wider(names_from = activity,
                     values_from = obs_value,
                     values_fill = 0) %>% 
  filter(str_detect(ref_area,paste0(country_sel,"Z"))) %>% 
  dplyr::mutate(
    TOTAL_c = A + BTE + F + GTJ + KTN + OTU,
    GTJ_c = GTI + J,
    KTN_c = K + L + M_N,
    OTU_c = OTQ + RTU,
    TOTAL_d = round(TOTAL_c - TOTAL, digits = 0),
    GTJ_d = round(GTJ_c - GTJ, digits = 0),
    KTN_d = round(KTN_c - KTN, digits = 0),
    OTU_d = round(OTU_c - OTU, digits = 0),
    TOTAL_dp = round((TOTAL_d * 100) / TOTAL, 1),
    GTJ_dp = round((GTJ_d * 100) / GTJ, 1),
    KTN_dp = round((KTN_d * 100) / KTN, 1),
    OTU_dp = round((OTU_d * 100) / OTU, 1)
  ) %>%
  dplyr::filter(if_any(ends_with("_d"), ~ abs(.x) > ths_int)) %>%
  dplyr::filter(if_any(ends_with("_dp"), ~ abs(.x) > ths_per))

NACE<- bind_rows(NACE1,NACE2,NACE3)
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
    filter(if_any(ends_with("_d"), ~ abs(.x) > ths_int))}

#


### NUTS ----
temp<-df_dt %>% 
  filter(!unit_measure %in% c("PC","HW"))

NUTS1<-check_NUTS(temp,ths_abs = ths_int, ths_rel = ths_per)

temp<-df_dt %>% 
  filter(!unit_measure %in% c("HW"))

NUTS2<-check_NUTS(temp,ths_abs = ths_hw, ths_rel = ths_per)

NUTS<- bind_rows(NUTS1,NUTS2)

# Negative values ----
negative <- df_dt %>%
  filter(unit_measure !="PC") %>% 
  pivot_wider(names_from = sto,
              values_from = obs_value) %>% 
  mutate(SELF = EMP - SAL ) %>% 
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
              values_from = obs_value) %>%
  mutate(SELF=EMP-SAL) %>% 
  pivot_longer(cols=c(B1G,D1,EMP,SAL,SELF,P51G),
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

saveWorkbook(wb, paste0("basic_checks/",country_sel,"_basic_check_",
                        format(Sys.time(),"%Y-%m-%d"),
                        ".xlsx"), overwrite = TRUE)

l <- ls()
rm(list = l[sapply(l, function(x) is.data.frame(get(x)))])
rm(l)

cli::cli_alert_success("Done. File created at /basic_checks")
