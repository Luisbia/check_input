library(data.table)
library(openxlsx)
library(tidyverse)

full<-list.files(path="data/csv",
                  pattern= glob2rx(paste0("*",country_sel,"*")),
                  full.names=TRUE) %>% 
  as_tibble() %>% 
  mutate(date=map(value,file.mtime)) %>% 
  unnest(date) %>% 
  arrange(desc(date)) %>% 
  head(1) %>% 
  select(value) %>% 
  pull() %>% 
  fread()

full<- dcast(full,... ~ type, value.var = "obs_value") %>% 
  .[,rev:=round(T-V,0)] %>% 
  .[,revp:=round(rev*100/V,1)] %>% 
  .[rev!=0] %>% 
  na.omit()

# Write file
wb <- createWorkbook()
modifyBaseFont(wb, fontSize = 12, fontName = "Calibri Light")
addWorksheet(wb, "Revision")
writeDataTable(wb, "Revision", full, tableStyle = "TableStyleMedium13")
saveWorkbook(wb, paste0("revision/",country_sel,"_revision_",format(Sys.time(),"%Y-%m-%d"),".xlsx"), overwrite = TRUE)

l <- ls()
rm(list = l[sapply(l, function(x) is.data.frame(get(x)))])
rm(l)

cli::cli_alert_success("Done. File created at /revision")