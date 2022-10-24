library(odbc)
library(tidyverse)
library(data.table)


vdp_con <- dbConnect(odbc(), "DenodoODBC")

### NAMA

sql_nama <- " SELECT * FROM hv_fame_nama4regacc "
df_nama <- dbGetQuery(vdp_con, sql_nama)
dbDisconnect(vdp_con)
fwrite(df_nama,"data/denodo/nama_orig.csv")

df_nama <- df_nama %>%
  as.data.table

df_nama<-df_nama[,.(name,date,value)] %>%
  .[, c("type", "drop12", "drop01", "drop02", "ref_area","drop03","drop04","drop05",
        "drop06", "sto", "drop7", "activity", "drop08","unit_measure","drop09","transformation","drop10") := tstrsplit(name, ".", fixed=TRUE)]

drop.cols <- grep("drop", colnames(df_nama))

df_nama[,(drop.cols):= NULL]
df_nama[,  name:= NULL]
df_nama<- na.omit(df_nama)

gr<- df_nama %>%
  as.data.table %>%
  .[transformation =="G1",] %>%
  .[,transformation :=NULL] %>%
  .[,unit_measure :="PC"]

df_nama <- df_nama[transformation !="G1",] %>%
  .[,transformation :=NULL]

df_nama<-rbindlist(list(df_nama,gr))


df_nama<-df_nama[,value := as.numeric(value)] %>%
  .[,date := as.integer(date)] %>%
  .[date >= 1995]

df_nama <- dcast(df_nama, ... ~ type, value.var = "value") %>%
  .[,T:=coalesce(T,V)] %>%
  .[,T:=coalesce(T,H)] %>%
  .[,V:=NULL] %>%
  .[,H:=NULL] %>%
  .[,ref_area:=str_replace_all(ref_area,"GR","EL")]


setnames(df_nama, c("date","T"), c("time_period", "obs_value"))


fwrite(df_nama,paste0("data/denodo/nama_",format(Sys.time(),"%Y-%m-%d"),".csv") )
