library(dataregacc)
library(regacc)
check_packages()

# Only run if an update is needed
# t1001<- get_denodo("hv_fame_regacc4regacc_t1001")
# t1002<- get_denodo("hv_fame_regacc4regacc_t1002")
# t1200<- get_denodo("hv_fame_regacc4regacc_t1200")
# t1300<- get_denodo("hv_fame_regacc4regacc_t1300")
# 
# tables <- bind_rows(t1001, t1002, t1200, t1300)

# write_parquet(tables,"data/denodo/all_primary.parquet")


### Update data
update_NQR_data(country_sel="DK",
                       input_file = "data/denodo/all_primary.parquet",
                       output_dir = "NQR/data")

### Create report

report_NQR_revision(dat=paste0("NQR/data/",country_sel,".csv"),
                    country_sel=country_sel,
                    output_dir = "NQR")
