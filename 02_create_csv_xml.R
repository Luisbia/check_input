tem<- regacc::load_xml(folder =  "data/xml",
                          country_sel = country_sel,
                          consolidate = TRUE) %>%
  arrange(date) %>%
  group_by(across(
    c(
      table_identifier,
      ref_area,
      sto,
      accounting_entry,
      activity,
      unit_measure,
      time_period
    )
  )) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(country = str_sub(ref_area, 1, 2),
         NUTS = str_length(ref_area) - 2,
         NUTS=as.factor(NUTS)) %>%
  mutate(type = "T") %>% 
  select(type,table_identifier,country,ref_area,NUTS,accounting_entry,sto,activity,unit_measure,time_period,obs_value) 

val<- dataregacc::validated %>% 
  filter(country==country_sel) %>% 
  mutate(type="V")

gvagr<- val %>% 
  filter(unit_measure=="PC")

lasty<- val %>% 
  filter(table_identifier=="T1001" & time_period>= 2020 & unit_measure=="XDC")# remove GVA in PYP

prevy<- val %>% 
  filter(table_identifier=="T1200" & time_period< 2020 & NUTS!="3" & activity %in% c("_T","_Z")) %>% # remove GVA in PYP
  mutate(table_identifier="T1001")

no_t1001<- val %>% 
  filter(table_identifier!="T1001")

val <- bind_rows(gvagr, lasty, prevy,no_t1001)


regacc<- bind_rows(tem,val) %>% 
  filter(!is.na(obs_value)) %>% 
  pivot_wider(names_from=type,
              values_from=obs_value) %>% 
  mutate(T=coalesce(T,V)) %>% 
  pivot_longer(cols = c(T,V),
               names_to="type",
               values_to="obs_value")

fwrite(regacc,paste0("data/csv/regacc_",country_sel,"_",format(Sys.time(),"%Y-%m-%d"),".csv"))
# List of countries we want to create files
# list<- c("AL", "AT", "BE" ,"BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES",
#          "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "ME", "MK", "MT",
#          "NL", "NO", "PL","PT", "RO", "SE", "SI", "SK","TR", "RS")

# A function to create the csv files
# save_list<- function(x){
#   df_regacc %>%
#     filter(country == x) %>%
#     fwrite(.,paste0("data/denodo/",x,".csv"))
# }
# 
# # iterate the function over the list
# purrr::walk(list,~ save_list(.x))

# unlink("data/denodo/regacc",
#        recursive = TRUE)
#
# write_dataset(df_regacc,"data/denodo/regacc/",
#               partitioning = c("country"))

l <- ls()
rm(list = l[sapply(l, function(x) is.data.frame(get(x)))])
rm(l)

cli::cli_alert_success("Done. File created at data/csv/")
