
data<- get_denodo()


arrow::write_parquet(data,paste0("data/denodo/",
       format(Sys.time(),"%Y-%m-%d"),
       "_denodo.parquet"))

data<- get_denodo(full=TRUE)

arrow::write_parquet(data,paste0("data/denodo/",
                                 format(Sys.time(),"%Y-%m-%d"),
                                 "_denodo_full.parquet"))
       