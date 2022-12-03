

 tables<- get_denodo("hv_fame_regacc4regacc_all_dbs")

  arrow::write_parquet(tables,"data/denodo/all_primary.parquet")


       