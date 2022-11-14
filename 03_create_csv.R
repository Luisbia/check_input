
df_regacc<- regacc::get_denodo()
arrow::write_parquet(df_regacc,paste0("data/denodo/regacc_all_",format(Sys.time(),"%Y-%m-%d"),".parquet"))

df_regacc<- df_regacc %>% 
  filter(country == country_sel & !is.na(obs_value))

fwrite(df_regacc,paste0("data/denodo/regacc_",country_sel,"_",format(Sys.time(),"%Y-%m-%d"),".csv"))
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
