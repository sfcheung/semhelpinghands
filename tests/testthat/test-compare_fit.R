library(testthat)
library(semhelpinghands)
library(lavaan)
dat <- dvs_ivs
dat$gp <- rep(c("gp1", "gp2"), each = nrow(dat) / 2)
mod1 <-
"
y1 ~ x1 + x2 + x3
y2 ~ x1 + x3
y3 ~ y2 + x2
"
fit1 <- sem(mod1, dat, group = "gp")
est1 <- parameterEstimates(fit1)
std1 <- standardizedSolution(fit1)
fit1_mlr <- sem(mod1, dat, group = "gp", estimator = "MLR")
datm <- dat
datm[1, 1:2] <- NA
datm[99:100, 2:4] <- NA
fit1_fiml <- sem(mod1, datm, group = "gp", missing = "fiml.x")
fit1_list <- sem(mod1, datm, group = "gp")

mod2 <-
"
y1 ~ x1 + x2 + x3
y2 ~ x3
"
fit2 <- sem(mod2, dat, group = "gp")
fit2_mlr <- sem(mod2, dat, group = "gp", estimator = "MLR")

fit1_nogroup <- sem(mod1, dat)
fit2_nogroup <- sem(mod2, dat)

fit1_nogroup_mlr <- sem(mod2, dat, group = "gp", estimator = "MLR")
fit2_nogroup_gls <- sem(mod2, dat, group = "gp", estimator = "GLS")

fit_list <- list(fit1 = fit1, fit2_mlr = fit2_mlr)

test_that("fitMeasures_by_models", {
  tmp <- fitMeasures_by_models(list(fit1 = fit1,
                                    fit2_mlr = fit2_mlr))
  expect_true(all(c("fit1", "fit2_mlr") %in% colnames(tmp)))
  expect_true("rmsea.robust" %in% tmp$short_name)
  tmp <- fitMeasures_by_models(list(fit1 = fit1,
                                    fit2 = fit2))
  expect_false("rmsea.robust" %in% tmp$short_name)
  tmp2 <- capture.output(print(tmp, measures_compact = c("cfi", "rmsea")))
  expect_true(length(tmp2) == 3)
  expect_true(!any(grepl("TLI", tmp2)))
  expect_output(print(tmp, nd = 5), "0.00000")
  tmp <- fitMeasures_by_models(list(fit1 = fit1,
                                    fit2 = fit2),
                               c("cfi", "bic"))
  expect_true(length(setdiff(c("bic", "cfi"), tmp$short_name)) == 0)
  tmp <- fitMeasures_by_models(list(fit1 = fit1,
                                    fit2 = fit2),
                               output = "text")
  expect_true(length(dim(tmp)) == 2)
})
