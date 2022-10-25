
ths_int <- 2


# Packages needed
library(data.table)
library(rio)
library(tidyverse)
library(openxlsx)
library(glue)



# load data
NUTS2021 <- import("data/NUTS2021.xlsx") %>% 
  mutate(NUTS=as.factor(NUTS)) %>% 
  select(-Country)# to be updated to NUTS 2021

load("data/eurobase_cleaned.Rdata")

files <- list.files("data/rds/",
                          pattern= "2021*",
                          full.names=TRUE) %>% 
  map_dfr(.,readRDS) %>% 
  as.data.table()

files<-files[,c("embargo_date","obs_status","conf_status","comment_ts","comment_dset"):= NULL]

files<- files[order(-modification_time),.SD[1], by = .(table,country,NUTS,geo,acc,na_item,nace_r2,unit,time)]

files<- files[,nace_r2 := str_replace(nace_r2,"_T","TOTAL")]
files<-files[,c("modification_time"):= NULL]



# T1001 external ----
  t1001_reg <- left_join(files, NUTS2021) %>%
  filter(table =="T1001" & NUTS=="0") %>% 
   select(geo, na_item, unit, time, values)

 t1001_ext <- left_join(t1001_reg, t1001_nat) %>% 
   mutate(diff = round(values - NatAcc,1),
          diffp = round(diff * 100/NatAcc,1)) %>% 
   filter(diff !=0)
 
 # T1002 external ----
 t1002_reg <- left_join(files, NUTS2021) %>%
   filter(table =="T1002" & NUTS=="0") %>% 
   select(geo, na_item, nace_r2,unit, time, values) 
 
 t1002_ext <- left_join(t1002_reg, t1002_nat) %>% 
   mutate(diff = round(values - NatAcc,1),
          diffp = round(diff * 100/NatAcc,1)) %>% 
   filter(abs(diff) >ths_int)
 
 # T1200 external ----
 t1200_reg <- left_join(files, NUTS2021) %>%
   filter(table =="T1200" & NUTS=="0") %>% 
   select(geo, na_item, nace_r2,unit, time, values) 
 
 t1200_ext <- left_join(t1200_reg, t1200_nat) %>% 
   mutate(diff = round(values - NatAcc,1),
          diffp = round(diff * 100/NatAcc,1)) %>% 
   filter(abs(diff) >ths_int)
 
 # T1300 external ----
 t1300_reg <- left_join(files, NUTS2021) %>%
   filter(table =="T1300" & NUTS=="0") %>% 
   select(geo, acc,na_item, time, values) %>% 
   unite(na_item,c("acc","na_item")) 
 
 
 t1300_ext <- left_join(t1300_reg, t1300_nat) %>% 
   mutate(diff = round(values - SecAcc,1),
          diffp = round(diff * 100/SecAcc,1)) %>% 
   filter(abs(diff) >ths_int)
 
 # ADDITIVITY NACE----
 
    NACE <- full_join(files, NUTS2021) %>%
    filter(table %in% c("T1002","T1200") & 
          na_item %in% c("EMP", "SAL", "B1G", "D1")) %>%
   pivot_wider(names_from = nace_r2, 
               values_from = values) %>%
   mutate(
     TOTAL_c = A + BTE + F + GTJ + KTN + OTU,
     GTJ_c = GTI + J,
     KTN_c = K + L + M_N,
     OTU_c = OTQ + RTU,
     TOTAL_d = round(TOTAL_c - TOTAL, digits = 0),
     GTJ_d = round(GTJ_c - GTJ, digits = 0),
     KTN_d = round(KTN_c - KTN, digits = 0),
     OTU_d = round(OTU_c - OTU, digits = 0)
   )%>% 
   filter(if_any(ends_with("_d"), ~ abs(.x) > ths_int))
 
 

 # Table 13 ----
 t1300_cons <- files %>%
   filter(table == "T1300") %>% 
   unite(na_item,c("acc","na_item")) %>% 
   na.omit() %>% 
   pivot_wider(names_from=na_item, 
               values_from=values) %>%
   mutate(
     B_B5N_c = round(B_B2A3N + C_D1 + C_D4 - D_D4, digits = 0),
     B_B5N_d = round(B_B5N_c - B_B5N, digits = 0)) %>% 
   mutate( B_B6N_c = 
             if("D_D62" %in% colnames(.)) 
               round(B_B5N + C_D61 + C_D62 + C_D7 - D_D5 - D_D61 - D_D62 - D_D7, digits = 0)
           else 
             round(B_B5N + C_D62 + C_D7- D_D5 - D_D61 - D_D7,digits=0)) %>% 
   mutate(B_B6N_d=round(B_B6N_c - B_B6N, digits = 0)) %>% 
   filter(if_any(ends_with("_d"), ~ abs(.x) > ths_int))
 
 
 ## NUTS aggregation -----
 ## NUTS 0----
 temp <- left_join(NUTS2021,files) %>% 
    filter(NUTS=="1" & unit!="PC") %>%
    mutate(geo1 = substr(geo, start = 1, stop = 2)) %>%
    group_by(table, time, na_item, acc, nace_r2, unit, country,geo1) %>%
    mutate(sum = sum(values)) %>%
    select(table, time, na_item, acc, nace_r2, unit, geo1, sum) %>%
    distinct() %>% 
    rename(geo ="geo1") %>% 
    ungroup()
 
 temp1 <- left_join(NUTS2021,files) %>% 
    filter(NUTS=="0" & unit!="PC")
 
    NUTS1 <- left_join(temp, temp1 ) %>%
    select(table, time, na_item, acc, nace_r2, unit, country,geo, sum, values) %>%
    mutate(diff = round(sum - values, digits = 0),
           diffp = round (diff * 100/ values, digits = 1))
    
    ## NUTS 1----
    temp <- left_join(NUTS2021,files) %>% 
       filter(NUTS=="2" & unit!="PC") %>%
       mutate(geo2 = substr(geo, start = 1, stop = 3)) %>%
       group_by(table, time, na_item, acc, nace_r2, unit, country,geo2) %>%
       mutate(sum = sum(values)) %>%
       select(table, time, na_item, acc, nace_r2, unit, country,geo2, sum) %>%
       distinct() %>% 
       rename(geo ="geo2") %>% 
       ungroup()
    
    temp1 <- left_join(NUTS2021,files) %>% 
       filter(NUTS=="1" & unit!="PC")
    
    NUTS2 <- left_join(temp, temp1 ) %>%
       select(table, time, na_item, acc, nace_r2, unit, country,geo, sum, values) %>%
      mutate(diff = round(sum - values, digits = 0),
             diffp = round (diff * 100/ values, digits = 1))
    
    ## NUTS 2----
    temp <- left_join(NUTS2021,files) %>% 
       filter(NUTS=="3" & unit!="PC") %>%
       mutate(geo3 = substr(geo, start = 1, stop = 4)) %>%
       group_by(table, time, na_item, acc, nace_r2, unit, country,geo3) %>%
       mutate(sum = sum(values)) %>%
       select(table, time, na_item, acc, nace_r2, unit, country,geo3, sum) %>%
       distinct() %>% 
       rename(geo ="geo3") %>% 
       ungroup()
    
    temp1 <- left_join(NUTS2021,files) %>% 
       filter(NUTS=="2" & unit!="PC")
    
    NUTS3 <- left_join(temp, temp1 ) %>%
       select(table, time, na_item, acc, nace_r2, unit, country,geo, sum, values) %>%
      mutate(diff = round(sum - values, digits = 0),
             diffp = round (diff * 100/ values, digits = 1))
    
    NUTS <- bind_rows(NUTS1, NUTS2, NUTS3) %>% 
       na.omit() %>% 
      filter(abs(diff) > ths_int)
    
## Negative values ----
    negative <- files %>%
       filter(unit !="PC") %>% 
       pivot_wider(names_from = na_item,
                   values_from = values) %>% 
       mutate(SELF = EMP - SAL ) %>% 
      mutate(tim=as.character(time)) %>% 
       pivot_longer(cols = c(where(is.numeric)),
                    names_to = "na_item",
                    values_to = "values") %>% 
       na.omit() %>% 
       filter(values < 0) 
    
## BTEC ----
    BTEC <- files %>%
       filter(nace_r2 %in% c("C", "BTE")) %>%
       pivot_wider(names_from = nace_r2, 
                   values_from = values) %>%
       mutate(check = round(BTE - C, digits=1)) %>%
       filter(check < 0)
 
    # Volume ----
    
    vol <- left_join(files,NUTS2021) %>% 
       filter( NUTS !="3" & 
               nace_r2 == "TOTAL" &
               na_item == "B1G") %>% 
       select(-acc, - na_item, -nace_r2) %>% 
       pivot_wider (names_from = table,
                    values_from = values) %>% 
       mutate(values = coalesce(T1200,T1001)) %>% 
       select(-T1001, -T1200) %>% 
       pivot_wider(names_from = unit,
                   values_from = values)
       
    temp <- vol %>% 
       filter(NUTS=="1" ) %>%
       mutate(geo1 = substr(geo, start = 1, stop = 2)) %>%
       group_by(geo) %>%
       arrange(time, .by_group = TRUE) %>% 
       mutate(pyp = (1+PC/100) * lag(XDC)) %>% 
      group_by(geo1, time) %>% 
       summarise (pyp = sum(pyp)) %>% 
       rename(geo = geo1)
    
    temp1 <-  vol %>% 
       filter(NUTS=="0" ) %>%
       arrange(time) %>% 
       group_by(geo) %>% 
       arrange(time, .by_group = TRUE) %>% 
       mutate(pyp_original = (1+PC/100) * lag(XDC))
    
    NUTS1_vol <- left_join(temp1,temp) %>% 
      mutate(diff = round(pyp-pyp_original),
             diffp = diff * 100 / pyp_original ) %>% 
      filter(abs(diffp) >0.1) %>% 
      mutate( diffp = round(diffp,1))
    
    
    temp <- vol %>% 
       filter(NUTS=="2" ) %>%
       mutate(geo2 = substr(geo, start = 1, stop = 3)) %>%
       group_by(geo) %>%
       arrange(time, .by_group = TRUE) %>% 
       mutate(pyp = (1+PC/100) * lag(XDC)) %>% 
       group_by(geo2, time) %>% 
       summarise (pyp = sum(pyp)) %>% 
       rename(geo = geo2)
    
    temp1 <-  vol %>% 
       filter(NUTS=="1" ) %>%
       arrange(time) %>% 
       group_by(geo) %>% 
       arrange(time, .by_group = TRUE) %>% 
       mutate(pyp_original = (1+PC/100) * lag(XDC))
    
    NUTS2_vol <- left_join(temp1,temp) %>% 
      mutate(diff = round(pyp-pyp_original),
             diffp = diff * 100 / pyp_original ) %>% 
      filter(abs(diffp) >0.1) %>% 
      mutate( diffp = round(diffp,1))
    
    
    NUTS_vol <- bind_rows(NUTS1_vol, NUTS2_vol) 
       
       
    #write file
    #write file
    wb <- createWorkbook() 
    modifyBaseFont(wb, fontSize = 12, fontName = "Calibri Light")
    if (nrow(t1001_ext) > 0){
      addWorksheet(wb, "t1001_ext")
      writeDataTable(wb, "t1001_ext", t1001_ext, tableStyle = "TableStyleMedium13")}
    if (nrow(t1002_ext) > 0){
      addWorksheet(wb,"t1002_ext")
      writeDataTable(wb, "t1002_ext", t1002_ext,tableStyle = "TableStyleMedium13")}
    if (nrow(t1200_ext) > 0){
      addWorksheet(wb,"t1200_ext")
      writeDataTable(wb, "t1200_ext",t1200_ext,tableStyle = "TableStyleMedium13")}
    if (nrow(t1300_ext) > 0){
      addWorksheet(wb,"t1300_ext")
      writeDataTable(wb, "t1300_ext", t1300_ext, tableStyle = "TableStyleMedium13")}
    if (nrow(NACE) > 0){
      addWorksheet(wb,"NACE")
      writeDataTable(wb, "NACE" ,NACE ,tableStyle = "TableStyleMedium13")}
    if (nrow(t1300_cons) > 0){
      addWorksheet(wb,"t1300_cons")
      writeDataTable(wb, "t1300_cons", t1300_cons, tableStyle = "TableStyleMedium13")}
    if (nrow(NUTS) > 0){
      addWorksheet(wb,"NUTS")
      writeDataTable(wb, "NUTS", NUTS, tableStyle = "TableStyleMedium13")}
    if (nrow(negative) > 0){
      addWorksheet(wb,"Negative")
      writeDataTable(wb, "Negative", negative, tableStyle = "TableStyleMedium13")}
    if (nrow(BTEC) > 0){
      addWorksheet(wb,"BTEC")
      writeDataTable(wb, "BTEC", BTEC, tableStyle = "TableStyleMedium13")}
    if (nrow(NUTS_vol) > 0){
      addWorksheet(wb,"NUTS_vol")
      writeDataTable(wb, "NUTS_vol", NUTS_vol, tableStyle = "TableStyleMedium13")}
    
    saveWorkbook(wb, paste0("basic_checks_",format(Sys.time(),"%Y-%m-%d"),
                            ".xlsx"), overwrite = TRUE)
  