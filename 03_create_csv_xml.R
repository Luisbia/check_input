tem<- regacc::load_xml(folder =  "data/xml",
                          country_sel = country_sel,
                          consolidate = FALSE) %>%
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

file<-list.files(path="data/denodo",
                 pattern= "_denodo.parquet$",
                 full.names=TRUE) %>% 
  as_tibble() %>% 
  mutate(date=map(value,file.mtime)) %>% 
  unnest(date) %>% 
  arrange(desc(date)) %>% 
  head(1) %>% 
  select(value) %>% 
  pull()

val<- arrow::read_parquet(file) %>% 
  filter(type=="V" & country==country_sel)

regacc<- bind_rows(tem,val) %>% 
  filter(country==country_sel)%>% 
  filter(!is.na(obs_value))

fwrite(regacc,paste0("data/denodo/regacc_",country_sel,"_",format(Sys.time(),"%Y-%m-%d"),".csv"))
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
gc()

cat("Done")
