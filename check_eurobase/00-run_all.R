#"AL" "AT" "BE" ,"BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", 
#"IT", "LT", "LU", "LV", "ME", "MK", "MT","NL", "NO", "PL","PT", "RO", "SE", "SI", "SK","TR", "RS", "EU"

# Countries to include

sel_countries <- c("AL", "BE" ,"BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "ME", "MK", "MT","NL", "NO", "PL","PT", "RO", "SE", "SI", "SK","TR", "RS", "EU")
c("AL", "AT", "BE" ,"BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "ME", "MK", "MT","NL", "NO", "PL","PT", "RO", "SE", "SI", "SK","TR", "RS", "EU")

sel_countries <- c("DE","BE","ES")

source("01-prepare_data.R")# only run if extraction change

source("02-internal_checks.R")

source("03-external_consistency.R")

source("04-revision.R") #3% limit by default

source("05-flags.R")


country_sel <-c("AT","IT","FR","DE")
 
table_sel <- c("coe2","emphw2","gdp2","gdp3","pop3","gva3","gvagr2","emp3","coe2","gfcf2","emphw2","hh2")


shiny::runApp("chart_browser.R", launch.browser = TRUE)


