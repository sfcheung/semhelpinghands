# Adapted from https://www.kloppenborg.ca/2021/06/long-running-vignettes/

base_dir <- getwd()

setwd("vignettes/")
knitr::knit("standardizedSolution_boot_ci.Rmd.original", output = "standardizedSolution_boot_ci.Rmd")
setwd(base_dir)
