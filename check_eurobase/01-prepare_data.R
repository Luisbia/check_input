library(dataregacc)
library(regacc)
library(tidyverse)
check_packages()

list_of_tables <- c("nama_10r_2gdp", "nama_10r_3gdp", "nama_10r_3popgdp", "nama_10r_3gva",
                    "nama_10r_3empers", "nama_10r_2coe", "nama_10r_2gfcf", "nama_10r_2emhrw", 
                    "nama_10r_2hhinc", "nama_10r_2gvagr")

sel_countries <- c("AL", "BE" ,"BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "ME", "MK", "MT","NL", "NO", "PL","PT", "RO", "SE", "SI", "SK","TR", "RS", "EU")

new<- map_dfr(list_of_tables, ~load_sent_eurobase(folder = "check_eurobase/data/new",
                                                  country_sel = sel_countries,
                                                  table_sel= .x))

write_parquet(new,"check_eurobase/data/new.parquet")




