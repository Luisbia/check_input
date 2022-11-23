
 t1001<- get_denodo("hv_fame_regacc4regacc_t1001")
 t1002<- get_denodo("hv_fame_regacc4regacc_t1002")
 t1200<- get_denodo("hv_fame_regacc4regacc_t1200")
 t1300<- get_denodo("hv_fame_regacc4regacc_t1300")

 tables <- bind_rows(t1001, t1002, t1200, t1300)

 arrow::write_parquet(tables,"data/denodo/all_primary.parquet")


       