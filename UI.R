options(tidyverse.quiet = TRUE)
options(scipen=999)
library(tidyverse)
library(dataregacc)
library(regacc)
library(here)
check_packages()
# select country
country_sel<- "DK"

# bring xml files (/DONE or /INPUT)----
bring_files(folder_sel = "//fame4prod.cc.cec.eu.int/fame-estat/econ/REGACC/DONE",
            country_sel = country_sel,
            folder_out = "data/xml",
            time_min = "2022-10-01")

### Add bring metadata (optional) ----
bring_files(folder_sel = "U:/03_Regional Accounts/03D_Data Production/2022/metadata",
            file_sel = "xlsx",
            country_sel = country_sel,
            folder_out = "metadata")

# basic info and creation of files ----
rmarkdown::render("01_basic_info.Rmd",
                  params = list(report = country_sel),
                  output_file = paste0("basic_info/",country_sel,"_",format(Sys.time(),"%Y-%m-%d"),"_report.html"))

### Update NAMA and NFSA (optional) ----
denodo<- list.files(path="U:/03_Regional Accounts/03D_Data Production/2022/R/check_input/data/denodo",
                    pattern ="csv$",
                    full.names=TRUE)
file.copy(denodo, "data/denodo", overwrite=TRUE, copy.date=TRUE)

### Report in excel. Mind the thresholds inside the file ----
source("02_basic_checks.R")

### Report in html ----
rmarkdown::render("03_report.Rmd", 
                  params = list(report = country_sel),
                  output_file = paste0("report/",
                                       country_sel,"_",
                                       format(Sys.time(),"%Y-%m-%d"),
                                       "_report.html"))

### Revisions.Mind the thresholds inside the file ----
source("04_revision.R")

### Additional scripts (outliers, D1, POP and LFS) ----
source("05_auxiliary.R")


### Visualisations ----

rmarkdown::render("overview/main_indicators.Rmd", 
                  params = list(report = country_sel),
                  output_file = paste0(country_sel,"_",
                                       "_main_indicators.html"))

shiny_t1001 <- function(country_sel) {
  .GlobalEnv$country_sel <- country_sel
  shiny::runApp("app_t1001.R", launch.browser = TRUE)
}
shiny_t1001(country_sel)

shiny_t1002_1200 <- function(country_sel) {
  .GlobalEnv$country_sel <- country_sel
  shiny::runApp("app_t1002_1200_new.R", launch.browser = TRUE)
}
shiny_t1002_1200(country_sel)

shiny_t1300 <- function(country_sel) {
  .GlobalEnv$country_sel <- country_sel
  shiny::runApp("app_t1300.R", launch.browser = TRUE)
}
shiny_t1300(country_sel)


### NQR ----

source("NQR/NQR_script.R")

### GVA PYP ----
source("gva_pyp/gva_pyp.R")


### Confirmation

source("confirmation/script_confirmation.R")



