# Generate data
n <- 100
set.seed(4325)
mod_pop <-
"
y1 ~ .2*x1 + .3*x2 + .1*x3
y2 ~ .2*x1 + .2*x3
y3 ~ .2*y2 + .2*x2
"
library(lavaan)
dat <- simulateData(model = mod_pop, sample.nobs = n)
mod <-
"
y1 ~ x1 + x2 + x3
y2 ~ x1 + x3
y3 ~ y2 + x2
"
fit <- sem(mod, dat, fixed.x = FALSE)
parameterEstimates(fit)

dat$gp <- sample(c("gp1", "gp2"), n, replace = TRUE)
fit_gp <- sem(mod, dat, group = "gp")
parameterEstimates(fit_gp)

dvs_ivs <- dat
usethis::use_data(dvs_ivs, overwrite = TRUE)

