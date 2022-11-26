library(odbc)
library(tidyverse)
library(data.table)


vdp_con <- dbConnect(odbc(), "DenodoODBC")

### NFSA
sql_nfsa <- " SELECT * FROM hv_fame_nfsa4regacc "
df_nfsa <- dbGetQuery(vdp_con, sql_nfsa)

dbDisconnect(vdp_con)

df_nfsa <- df_nfsa %>%
  as.data.table  # convert to data.table

df_nfsa<-df_nfsa[,.(name,date,value)] %>% #select three columns
  .[, c("type", "drop16", "drop01", "drop02", "ref_area","drop03","drop04","drop05", "drop06",
        "accounting_entry", "sto", "drop07", "drop08","drop09","drop10","drop11","drop12",
        "drop13","drop14","drop15") := tstrsplit(name, ".", fixed=TRUE)]# split name column by .

drop.cols <- grep("drop", colnames(df_nfsa))# create a variable with columns to remove

df_nfsa[,(drop.cols):= NULL]#remove columns

df_nfsa<- na.omit(df_nfsa)# remove empty rows

df_nfsa<-df_nfsa[,value := as.numeric(value)] %>% #mutate as numeric
  .[,date := as.integer(date)] %>% #mutate as integer
  .[,name:= NULL] %>% #remove column name
  .[date >= 1995,] #filter

df_nfsa <- dcast(df_nfsa, ... ~ type, value.var = "value") %>% #pivot wider
  .[,T:=coalesce(T,V)] %>%
  .[,V:=NULL]%>%
  .[,ref_area:=str_replace_all(ref_area,"GR","EL")]

setnames(df_nfsa, c("date","T"), c("time_period", "obs_value")) # rename

fwrite(df_nfsa,paste0("data/denodo/nfsa_",format(Sys.time(),"%Y-%m-%d"),".csv") )
