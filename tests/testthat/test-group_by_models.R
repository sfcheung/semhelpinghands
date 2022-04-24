skip("WIP")
# skip_on_cran()
# skip_if(!interactive(),
#         message = "standardizedSolution_boot_ci not tested if not interactive")

library(testthat)
library(semhelpinghands)
set.seed(838914)
n <- 500
mod_pop <-
"
y1 ~ .2*x1 + .3*x2 + .1*x3
y2 ~ .2*x1 + .2*x3
y3 ~ .2*y2 + .2*x2
"
library(lavaan)
dat <- simulateData(model = mod_pop, sample.nobs = n)
dat$gp <- sample(c("gp1", "gp2"), n, replace = TRUE)
mod1 <-
"
y1 ~ x1 + x2 + x3
y2 ~ x1 + x3
y3 ~ y2 + x2
"
fit1 <- sem(mod1, dat, group = "gp")
mod2 <-
"
y1 ~ x1 + x2 + x3
y2 ~ x3
"
fit2 <- sem(mod2, dat, group = "gp")

fit1_nogroup <- sem(mod1, dat)
fit2_nogroup <- sem(mod2, dat)

out1 <- merge_model_estimates(list(model1 = fit1,
                                  model2 = fit2),
                             standardized = TRUE,
                             col_names = c("std.all", "se"))
head(dplyr::filter(out1, op == "~"))

out2 <- merge_model_estimates(list(model1 = fit1_nogroup,
                                   model2 = fit2_nogroup),
                             standardized = TRUE,
                             col_names = c("std.all", "se"))
head(dplyr::filter(out2, op == "~"))

out3 <- merge_model_estimates(list(model1 = fit1,
                                  model2 = fit2),
                             standardized = TRUE,
                             col_names = c("std.all", "se"),
                             group_first = TRUE)
head(dplyr::filter(out3, op == "~"))
tail(dplyr::filter(out3, op == "~"))
