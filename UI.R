options(tidyverse.quiet = TRUE)
library(tidyverse)
library(luispack)

# select country
country_sel<- "DK"

# bring xml files (/DONE or /INPUT)
bring_files(folder_sel = "//fame2prod.cc.cec.eu.int/fame-estat/econ/REGACC/DONE",
            country_sel = country_sel,
            folder_out = "data/xml",
            time_min = "2022-04-01")

# basic info (/basic_info)
rmarkdown::render("01_basic_info.Rmd",
                  params = list(report = country_sel),
                  output_file = paste0("basic_info/",country_sel,"_",format(Sys.time(),"%Y-%m-%d"),"_report.html"))


# basic checks, default ths=_abs2,ths_per=0.1
source("02_basic_checks_xml.R")

# Extract data from Denodo and create a csv file for the remaining scripts. 
source("03_create_csv.R")
# If you do not need to re-extract from Denodo use the second script
source("03_create_csv_no_extraction.R")
# If you want to create the csv from xml files
source("03_create_csv_xml.R")
# Report in html
rmarkdown::render("04_report.Rmd", 
                  params = list(report = country_sel),
                  output_file = paste0("report/",
                                       country_sel,"_",
                                       format(Sys.time(),"%Y-%m-%d"),
                                       "_report.html"))

# Report in excel. Mind the thresholds inside the file
source("05_basic_checks.R")

# Revisions.Mind the thresholds inside the file
source("06_revision.R")

# Outliers
rmarkdown::render("outliers.Rmd",# new only looks at new data not at not-revised
                  params = list(country = country_sel, 
                                z_score = 3),
                  output_file = paste0("others/",country_sel,"_outlier.html"))

# D1 in T1002 and T1300
rmarkdown::render("D1_nat_dom.Rmd", #/others
                  params = list(report = country_sel),
                  output_file = paste0("others/",country_sel,"_D1_dom_nat.html"))


shiny_t1001 <- function(country_sel) {
  .GlobalEnv$country_sel <- country_sel
  shiny::runApp("app_t1001.R", launch.browser = TRUE)
}
shiny_t1001(country_sel)

shiny_t1002_1200 <- function(country_sel) {
  .GlobalEnv$country_sel <- country_sel
  shiny::runApp("app_t1002_1200.R", launch.browser = TRUE)
}
shiny_t1002_1200(country_sel)

shiny_t1300 <- function(country_sel) {
  .GlobalEnv$country_sel <- country_sel
  shiny::runApp("app_t1300.R", launch.browser = TRUE)
}
shiny_t1300(country_sel)

rmarkdown::render("LFS.Rmd",
                  params = list(report = country_sel),
                  output_file = paste0("others/",country_sel,"_LFS.html"))

rmarkdown::render("POP.Rmd",
                  params = list(report = country_sel),
                  output_file = paste0("others/",country_sel,"_POP.html"))
