
library(tidyverse)
library(openxlsx)

ths<-0.3

df_new <- read_parquet("data/new.parquet") %>% 
  select(-obs_status,-value,-date) %>% 
  filter(country %in% sel_countries) %>% 
mutate(NUTS=as.factor(NUTS))

df_prev <- dataregacc::eurobase %>% 
  rename(prev=obs_value) %>% 
  filter(country %in% sel_countries)


df <- full_join(df_prev,df_new) %>% 
  relocate(prev,.before=obs_value) %>% 
  mutate(rev=round(obs_value-prev),
         revp=round(rev*100/prev,1))%>%
  filter(rev!=0) %>% 
 filter(abs(revp)>ths) 
 

#write file
wb <- createWorkbook() 
modifyBaseFont(wb, fontSize = 12, fontName = "Calibri Light")
  addWorksheet(wb, "revision")
  writeDataTable(wb, "revision", df, tableStyle = "TableStyleMedium13")

saveWorkbook(wb, paste0("output/revision_",format(Sys.time(),"%Y-%m-%d"),
                        ".xlsx"), overwrite = TRUE)

l <- ls()
rm(list = l[sapply(l, function(x) is.data.frame(get(x)))])