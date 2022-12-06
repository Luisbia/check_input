#"AL" "AT" "BE" ,"BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", 
#"IT", "LT", "LU", "LV", "ME", "MK", "MT","NL", "NO", "PL","PT", "RO", "SE", "SI", "SK","TR", "RS", "EU"

# Countries to include

sel_countries <- c("AL", "AT","BE" ,"BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "ME", "MK", "MT","NL", "NO", "PL","PT", "RO", "SE", "SI", "SK","TR", "RS", "EU")
sel_countries <- c("DE")

source("check_eurobase/01-prepare_data.R")# only run if extraction change

source("check_eurobase/02-internal_checks.R")

source("check_eurobase/03-external_consistency.R")

source("check_eurobase/04-revision.R") #3% limit by default

source("check_eurobase/05-flags.R")


country_sel <-c("DE")
 
table_sel <- c("coe2","emphw2","gdp2","gdp3","pop3","gva3","gvagr2","emp3","coe2","gfcf2","emphw2","hh2")


shiny::runApp("check_eurobase/chart_browser.R", launch.browser = TRUE)


source("check_eurobase/metadata.R")
