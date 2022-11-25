library(tidyverse)
library(data.table)
library(openxlsx)

country_sel <- c("AT", "BE" ,"BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV","MT","NL", "PL","PT", "RO", "SE", "SI", "SK")
#  c("AL", "AT", "BE" ,"BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "ME", "MK", "MT","NL", "NO", "PL","PT", "RO", "SE", "SI", "SK","TR", "RS", "EU")
table_sel <- c("gdp2","pop3","gva3","gvagr2","emp3","coe2","gfcf2","emphw2","hh2")


df_new <- read_parquet("data/new.parquet") %>% 
  select(-obs_status,-value,-date) %>% 
  filter(country %in% country_sel & table %in% table_sel) %>% 
  rename(new=obs_value)

df_prev <- dataregacc::eurobase %>% 
  select(-obs_status) %>% 
  filter(country %in% country_sel & table %in% table_sel) %>% 
  rename(prev=obs_value) 


df <- full_join(df_prev,df_new) %>% 
  relocate(prev,.before=new) %>% 
  mutate(rev=round(new-prev),
         revp=round(rev*100/prev,1)) %>% 
  mutate(NUTS=as.factor(NUTS)) %>% 
  left_join(.,dataregacc::NUTS_2021) %>% 
  filter(label != "Extra-regio") %>%
  mutate(label = paste0(ref_area, "-", label)) %>% 
  pivot_longer(cols=c(new,prev,rev,revp),
               names_to="type",
               values_to="obs_value")

  extract<- df %>% 
    filter(NUTS %in% c("2") &  
        unit_measure %in% c("MIO_NAC","PS","HW") & 
        type %in% c("rev","revp")  & 
        str_ends(ref_area,"ZZ",negate=TRUE) &
        activity %in% c("TOTAL","_Z") &
        sto %in% c("B1G", "EMP", "POP", "D1", "P51G", "B6N")) 

setDT(extract)

minmax<- copy(extract)

minmax<- minmax[obs_value!=0 & type=="rev", ]
minmax<- minmax[abs(obs_value)>1 & type=="rev", ]


minmax<- minmax[,.(min=min(time_period), max=max(time_period)), .(country,sto,unit_measure)] %>% 
.[,period:=paste0(min,"-",max)] %>% 
  .[,period:= str_replace_all(period,"2020-2020","2020")] %>%
.[,period:= str_replace_all(period,"2019-2019","2019")] %>%
.[,period:= str_replace_all(period,"2018-2018","2018")] %>%
.[,variable:=paste0(sto,unit_measure)] %>% 
.[,":="(sto=NULL,unit_measure=NULL, min=NULL, max=NULL)]

minmax <- dcast(minmax,... ~variable, value.var = "period") 
setcolorder(minmax,c("country","B1GMIO_NAC","EMPPS", "D1MIO_NAC","P51GMIO_NAC", "EMPHW", "B6NMIO_NAC", "POPPS" ))
setnames(minmax, c("country","B1GMIO_NAC","EMPPS", "D1MIO_NAC","P51GMIO_NAC", "EMPHW", "B6NMIO_NAC", "POPPS" ), 
                 c("Country", "Gross Value Added", "Employment (Persons)", "Compensation of Employees", "Gross Fixed Capital Formation", "Employment (Hours Worked)", "Households Net Disposable Income", "Population"))


rev<- copy(extract)
rev <- rev[type=="revp" & time>= 2017,.(avg=round(mean(values,na.rm = TRUE),1)), .(country,na_item,unit)] %>%  
.[,variable:=paste0(na_item,unit)] %>% 
.[,":="(na_item=NULL,unit=NULL)]

rev <- dcast(rev,... ~variable, value.var = "avg") 
setcolorder(rev,c("country","B1GMIO_NAC","EMPPS", "D1MIO_NAC","P51GMIO_NAC", "EMPHW", "B6NMIO_NAC", "POPPS" ))
setnames(rev, c("country","B1GMIO_NAC","EMPPS", "D1MIO_NAC","P51GMIO_NAC", "EMPHW", "B6NMIO_NAC", "POPPS" ), 
         c("Country", "Gross Value Added", "Employment (Persons)", "Compensation of Employees", "Gross Fixed Capital Formatiom", "Employment (Hours Worked)", "Housesolds Net Disposable Income", "Population"))

minmax[is.na(minmax)] <- ":"
rev[is.na(rev)] <- ":"


wb <- createWorkbook() 
modifyBaseFont(wb, fontSize = 12, fontName = "Calibri Light")
  addWorksheet(wb, "Years_revised")
  writeDataTable(wb, "Years_revised", minmax, tableStyle = "TableStyleMedium13")
  addWorksheet(wb,"Average_revision")
  writeDataTable(wb, "Average_revision", rev,tableStyle = "TableStyleMedium13")

saveWorkbook(wb, paste0("output/metadata_",format(Sys.time(),"%Y-%m-%d"),
                        ".xlsx"), overwrite = TRUE)

