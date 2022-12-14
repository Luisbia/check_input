library(dataregacc)
library(regacc)
check_packages()

# Only run if an update is needed
tables<- get_denodo("hv_fame_regacc4regacc_all_dbs")

# write_parquet(tables,"data/denodo/all_primary.parquet")


### Update data
update_NQR_data(country_sel="DK",
                       input_file = "data/denodo/all_primary.parquet",
                       output_dir = "NQR/data")

### Create report

report_NQR_revision(dat=paste0("NQR/data/",country_sel,".csv"),
                    country_sel=country_sel,
                    output_dir = "NQR")
