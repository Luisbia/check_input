library(tidyverse)
path<- getwd()
files_local<- list.files(path,
           full.names = T,
           recursive = F,
           pattern= (glob2rx("*.*R*"))) %>% 
  as_tibble() %>% 
  mutate(date_local=file.mtime(value)) %>% 
  mutate(value=str_remove(value,paste0(path,"/")))

files_server<- list.files("U:/03_Regional Accounts/03D_Data Production/2022/R/regacc",
                         full.names = T,
                         recursive = F,
                         pattern= (glob2rx("*.*R*"))) %>% 
  as_tibble() %>% 
  mutate(date_server=file.mtime(value)) %>% 
  mutate(value=str_remove(value,"U:/03_Regional Accounts/03D_Data Production/2022/R/regacc/"))

files<- left_join(files_local,files_server) %>% 
  filter(date_server>date_local) %>% 
  select(value) %>% 
  mutate(value=paste0("U:/03_Regional Accounts/03D_Data Production/2022/R/regacc/", value)) %>% 
  pull()

file.copy(files,path,overwrite=TRUE)


cli::cli_alert_success(paste0(length(files)," files updated"))
