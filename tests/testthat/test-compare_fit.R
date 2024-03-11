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

summary(fit1, fit.measures = TRUE, estimates = FALSE)
summary(fit2, fit.measures = TRUE, estimates = FALSE)
summary(fit1_nogroup, fit.measures = TRUE, estimates = FALSE)
summary(fit2_nogroup, fit.measures = TRUE, estimates = FALSE)
summary(fit1_mlr, fit.measures = TRUE, estimates = FALSE)
summary(fit2_mlr, fit.measures = TRUE, estimates = FALSE)
summary(fit1_nogroup_mlr, fit.measures = TRUE, estimates = FALSE)
summary(fit2_nogroup_gls, fit.measures = TRUE, estimates = FALSE)

fit_list <- list(fit1 = fit1, fit2_mlr = fit2_mlr)


tmp <- fitMeasures_by_models(list(fit1 = fit1,
                                  fit2_mlr = fit2_mlr))
tmp
tmp <- fitMeasures_by_models(list(fit1 = fit1,
                                  fit2 = fit2))
tmp
print(tmp, remove_all_na = FALSE)

tmp <- fitMeasures_by_models(list(fit1 = fit1,
                                  fit2_mlr = fit2_mlr))
tmp
tmp <- fitMeasures_by_models(list(fit1 = fit1_mlr,
                                  fit2 = fit2_mlr))
tmp

print(tmp, remove_all_na = FALSE)

print(tmp, measures_compact = c("chisq", "cfi"))

print(tmp, measures_compact = c("baseline.chisq", "cfi"))

