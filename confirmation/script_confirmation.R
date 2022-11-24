
df<- read_parquet("data/denodo/all_primary.parquet") %>% 
  filter(country == country_sel)


b1g_emp<- df %>% 
  filter(type =="V" & 
         table_identifier=="T1001_1200" & 
         sto %in% c("B1G", "EMP") & 
         unit_measure %in% c("XDC","PS") & 
         prices !="Y" & NUTS !=3) 

pop<- df %>% 
  filter(type =="V" & 
           table_identifier=="T1001_1200" & 
           sto %in% c("POP") & 
           unit_measure %in% c("PS_B") & NUTS !=3) 


b1gq<- df %>% 
  filter(type =="V" &  
           table_identifier=="T1001_1200" & 
           sto %in% c("B1GQ") &
           NUTS!=3 &
           unit_measure=="PC" &
           transformation =="G1") 

data<- bind_rows(b1g_emp,pop,b1gq) %>% 
  left_join(.,dataregacc::NUTS_2021) %>% 
  #select(country,ref_area,label,NUTS,sto,time_period,obs_value,obs_status) %>% 
  select(country,ref_area,label,NUTS,sto,time_period,obs_value) %>% 
  mutate(obs_value=janitor::round_half_up(obs_value,2)) %>% 
  # mutate(obs_status=str_remove_all(obs_status,"00"),
  #        obs_status=str_replace_all(obs_status,"F0","P"),
  #        obs_status=str_replace_all(obs_status,"G0","E"),
  #        obs_status=str_replace_all(obs_status,"Q0","B"),
  #        obs_status=str_replace_all(obs_status,"J0","D"),
  #        obs_status=str_replace_all(obs_status,"D0","U")) %>%
  # unite("obs_value",c(obs_value,obs_status),sep=" ") %>% 
  pivot_wider(names_from = time_period,
              values_from = obs_value)%>% 
  arrange(sto,ref_area)

#write file

wb <- createWorkbook()  
addWorksheet(wb,paste0(country_sel))
writeDataTable(wb,paste0(country_sel),data,tableStyle = "TableStyleMedium13")

saveWorkbook(wb, paste0("confirmation/",country_sel,".xlsx"), overwrite = TRUE)