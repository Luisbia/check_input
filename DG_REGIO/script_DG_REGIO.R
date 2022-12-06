library(here)
options(scipen=999)
data<- read_parquet(here("data","denodo","all_primary.parquet")) %>% 
  filter(table_identifier=="T1001_1200" & type =="V") 

create_table_REGIO<- function (dat=data, unit_measure, sto, transformation){
  table <- dat %>%
    filter(unit_measure == {{unit_measure}} & sto == {{sto}} & transformation =={{transformation}}) %>%
    select(country,NUTS,ref_area,time_period,obs_value) %>%
    mutate(ref_area=str_replace(ref_area,"B6","EU27_2020"),
           country=str_replace(country,"B6","EU27")) %>% 
    arrange (time_period) %>% 
    pivot_wider(names_from=time_period,
                values_from=obs_value) %>% 
    arrange(country,ref_area) %>% 
    mutate(ref_area=str_replace(ref_area,"B6","EU27_2020")) %>% 
    left_join(dataregacc::NUTS_2021,.)
  
  return(table)
}

df_meur<- create_table_REGIO(dat=data,
                             unit_measure = "EUR",
                             sto = "B1GQ",
                             transformation = "N")  

df_eur_pop<- create_table_REGIO(dat=data,
                             unit_measure = "EUR_R_POP",
                             sto = "B1GQ",
                             transformation = "N")  


df_pps <- create_table_REGIO(dat=data,
                             unit_measure = "PE_B6",
                             sto = "B1GQ",
                             transformation = "N")
  
df_pps_pop <- create_table_REGIO(dat=data,
                                 unit_measure = "PE_B6_R_POP",
                                 sto = "B1GQ",
                                 transformation = "N")

  

df_pps_pop_eu27 <- create_table_REGIO(dat=data,
                                      unit_measure = "PE_B6_R_B6_POP",
                                      sto = "B1GQ",
                                      transformation = "N")

df_pps_pop_eu27_3 <- create_table_REGIO(dat=data,
                                        unit_measure = "PE_B6_R_B6_POP",
                                        sto = "B1GQ",
                                        transformation = "A3")
  

df_pop <- create_table_REGIO(dat=data,
                             unit_measure = "PS_B",
                             sto = "POP",
                             transformation = "N")

# write file
wb <- createWorkbook()

addWorksheet(wb, sheet = "EUR")
addWorksheet(wb, sheet = "EUR_POP")
addWorksheet(wb, sheet = "PPS")
addWorksheet(wb, sheet = "PPS_POP")
addWorksheet(wb, sheet = "PPS_POP_EU27")
addWorksheet(wb, sheet = "PPS_POP_EU27_3y")
addWorksheet(wb, sheet = "POP")


writeData(wb, sheet = "EUR", df_meur)
writeData(wb, sheet = "EUR_POP", df_eur_pop)
writeData(wb, sheet = "PPS", df_pps)
writeData(wb, sheet = "PPS_POP", df_pps_pop)
writeData(wb, sheet = "PPS_POP_EU27", df_pps_pop_eu27)
writeData(wb, sheet = "PPS_POP_EU27_3y", df_pps_pop_eu27_3)
writeData(wb, sheet = "POP", df_pop)

saveWorkbook(wb, "DG_REGIO/DG_REGIO.xlsx", overwrite = TRUE)

