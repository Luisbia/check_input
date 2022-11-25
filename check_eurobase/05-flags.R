
library(tidyverse)
library(openxlsx)

df_new <- read_parquet("data/new.parquet") %>% 
  select(-value,-date) %>% 
  filter(country %in% sel_countries & !is.na(obs_status)) 

#write file
wb <- createWorkbook() 
modifyBaseFont(wb, fontSize = 12, fontName = "Calibri Light")
addWorksheet(wb, "flags")
writeDataTable(wb, "flags", df_new, tableStyle = "TableStyleMedium13")

saveWorkbook(wb, paste0("output/flags_",format(Sys.time(),"%Y-%m-%d"),
                        ".xlsx"), overwrite = TRUE)
l <- ls()
rm(list = l[sapply(l, function(x) is.data.frame(get(x)))])

