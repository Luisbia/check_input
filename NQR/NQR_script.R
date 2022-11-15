library(dataregacc)
library(regacc)
check_packages()

### Update data
update_NQR_data(country_sel="DK",
                       input_dir = "data/denodo",
                       output_dir = "NQR")

### Create report

report_NQR_revision(dat="NQR/DK_new.csv",
                    country_sel="DK",
                    output_dir = "NQR")
