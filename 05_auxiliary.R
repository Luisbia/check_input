
# Outliers
rmarkdown::render("outliers.Rmd",# new only looks at new data not at not-revised
                  params = list(country = country_sel, 
                                z_score = 3),
                  output_file = paste0("others/",country_sel,"_outlier.html"))

# D1 in T1002 and T1300
rmarkdown::render("D1_nat_dom.Rmd", #/others
                  params = list(report = country_sel),
                  output_file = paste0("others/",country_sel,"_D1_dom_nat.html"))

# LFS
rmarkdown::render("LFS.Rmd",
                  params = list(report = country_sel),
                  output_file = paste0("others/",country_sel,"_LFS.html"))
# POP
rmarkdown::render("POP.Rmd",
                  params = list(report = country_sel),
                  output_file = paste0("others/",country_sel,"_POP.html"))