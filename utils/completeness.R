## T1001 ----
table_identifier<- "T1001"
ref_area<- NUTS2021 %>% 
  filter(NUTS !="3" & country== country_sel) %>% 
  filter(label!="Extra-regio") %>% 
  select(ref_area) %>% 
  pull()
accounting_entry <- c("B","_Z")
sto <- c ("B1G", "EMP", "POP")
activity<- c("_T", "_Z")
unit_measure <-c ("XDC", "PC", "PS")
country <- country_sel
time_period<- seq(2000L,2021L,by=1L)

df <- expand_grid(table_identifier, country, ref_area, accounting_entry, sto, activity, unit_measure, time_period)

df1<- df %>% 
  filter(  sto =="B1G" &
           activity == "_T" &
           accounting_entry == "B" &
           unit_measure== "XDC" &
           time_period == 2021)

df2<- df %>% 
  filter(  sto =="B1G" &
           activity == "_T" &
           accounting_entry == "_Z" &
           unit_measure == "PC"&
           time_period >=2001 & time_period <=2020)

df3 <- df %>% 
  filter(  sto =="EMP" &
            activity == "_T" &
            accounting_entry == "_Z" &
            unit_measure== "PS" &
            time_period == 2021 )

df4 <- df %>% 
  filter(   sto =="POP" &
            activity == "_Z" &
            accounting_entry == "_Z" &
            unit_measure== "PS" &
            time_period == 2021)

t1001<- bind_rows(df1, df2, df3, df4) %>% 
  mutate(NUTS=as.factor(str_length(ref_area)-2))

## T1002 ----
table_identifier<- "T1002"
ref_area<- NUTS2021 %>% 
  filter(NUTS !="3"  & country== country_sel) %>% 
  filter(label!="Extra-regio") %>% 
  select(ref_area) %>% 
  pull()
accounting_entry <- c("D","_Z")
sto <- c ("D1", "P51G", "EMP", "SAL")
activity<- c("_T", "A", "BTE", "C", "F", "GTI", "GTJ", "J", "KTN", "K", "L", "M_N", "OTU", "OTQ", "RTU")
unit_measure <-c ("XDC","HW")
country <- country_sel
time_period<- seq(2000L,2020L,by=1L)

df <- expand_grid(table_identifier, country, ref_area, accounting_entry, sto, activity, unit_measure, time_period)

df1 <- df %>% 
  filter(accounting_entry == "D" &
           sto %in% c("D1", "P51G") &
           unit_measure == "XDC")

df2 <- df %>% 
  filter(accounting_entry == "_Z" &
           sto %in% c("EMP", "SAL") &
          unit_measure == "HW")

t1002<- bind_rows(df1, df2)%>% 
  mutate(NUTS=as.factor(str_length(ref_area)-2))

## T1200 ----
table_identifier<- "T1200"
ref_area<- NUTS2021 %>% 
  filter(label!="Extra-regio" & country ==country_sel) %>% 
  select(ref_area) %>% 
  pull()
accounting_entry <- c("B","_Z")
sto <- c ("B1G", "POP", "EMP", "SAL")
activity<- c("_Z","_T", "A", "BTE", "C", "F", "GTI", "GTJ", "J", "KTN", "K", "L", "M_N", "OTU", "OTQ", "RTU")
unit_measure <-c ("PS","XDC")
country <- country_sel
time_period<- seq(2000L,2020L,by=1L)


df <- expand_grid(table_identifier, country, ref_area, accounting_entry, sto, activity, unit_measure, time_period)

df1 <- df %>% 
  filter(accounting_entry == "_Z" &
           sto =="POP" &
           activity == "_Z" &
           unit_measure == "PS")

df2 <- df %>% 
  filter(  sto =="B1G" &
           accounting_entry == "B" &
           activity %in% c("_T", "A", "BTE", "C", "F", "GTI", "GTJ", "J", "KTN", "K", "L", "M_N", "OTU", "OTQ", "RTU") &
           unit_measure == "XDC")

df2a<- left_join(df2,NUTS2021) %>% 
  filter (NUTS !="3")

df2b<- left_join(df2,NUTS2021) %>% 
  filter (NUTS =="3" & activity %in% c("_T", "A", "BTE", "F", "GTJ", "KTN",  "OTU"))

df2 <- bind_rows(df2a,df2b)%>% 
  select(-NUTS, -label)

df3 <- df %>% 
  filter(  sto %in% c("EMP", "SAL") &
             accounting_entry == "_Z" &
             activity %in% c("_T", "A", "BTE", "C", "F", "GTI", "GTJ", "J", "KTN", "K", "L", "M_N", "OTU", "OTQ", "RTU") &
             unit_measure == "PS")

df3a<- left_join(df3,NUTS2021) %>% 
  filter (NUTS !="3")

df3b<- left_join(df3,NUTS2021) %>% 
  filter (NUTS =="3" & activity %in% c("_T", "A", "BTE", "C", "F", "GTJ", "KTN",  "OTU"))

df3 <- bind_rows(df3a,df3b) %>% 
  select(-NUTS, -label)


t1200<- bind_rows(df1, df2, df3)%>% 
  mutate(NUTS=as.factor(str_length(ref_area)-2))

## T1300 ----


table_identifier<- "T1300"
ref_area<- NUTS2021 %>% 
  filter(NUTS !="3" & country == country_sel) %>% 
  filter(label!="Extra-regio") %>% 
  select(ref_area) %>% 
  pull()
accounting_entry <- c("B","C","D")
sto <- c ("B5N", "B6N", "B2A3N", "D1", "D4", "D61", "D62", "D7", "D5")
activity<- c("_Z")
unit_measure <-c ("XDC")
country <- country_sel
time_period<- seq(2000L,2020L,by=1L)


df <- expand_grid(table_identifier, country, ref_area, accounting_entry, sto, activity, unit_measure, time_period)

df1<- df %>% 
  filter(accounting_entry == "B" &
           sto %in% c("B5N", "B6N", "B2A3N"))

df2<- df %>% 
  filter(accounting_entry == "C" &
           sto %in% c("D1", "D4", "D62", "D7"))

df3<- df %>% 
  filter(accounting_entry == "D" &
           sto %in% c("D4", "D5", "D61",  "D7"))

t1300<- bind_rows(df1, df2, df3)%>% 
  mutate(NUTS=as.factor(str_length(ref_area)-2))

rm(df,df1,df2,df2a,df2b,df3,df3a,df3b,df4,
   accounting_entry,activity,country,time_period,unit_measure,ref_area,table_identifier, sto)
