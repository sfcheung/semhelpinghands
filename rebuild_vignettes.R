# Adapted from https://www.kloppenborg.ca/2021/06/long-running-vignettes/

base_dir <- getwd()

setwd("vignettes/")
knitr::knit("standardizedSolution_boot_ci.Rmd.original", output = "standardizedSolution_boot_ci.Rmd")
setwd(base_dir)

setwd("vignettes/articles/")
knitr::knit("plot_boot.Rmd.original", output = "plot_boot.Rmd")
setwd(base_dir)


# setwd("vignettes/articles/")
# knitr::knit("plot_boot.Rmd.original", output = "plot_boot.Rmd")
# setwd(base_dir)
