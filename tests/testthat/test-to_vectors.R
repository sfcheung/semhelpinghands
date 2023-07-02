library(testthat)

library(lavaan)

cfa.model <- '
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
'
fit_cfa <- cfa(cfa.model,
               data = HolzingerSwineford1939)
fit_cfa_mg <- cfa(cfa.model,
                  data = HolzingerSwineford1939,
                  group = "school")

cfa.model.2 <- '
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9 + x2
'
fit_cfa2 <- cfa(cfa.model.2,
               data = HolzingerSwineford1939)
fit_cfa2_mg <- cfa(cfa.model.2,
                  data = HolzingerSwineford1939,
                  group = "school")

cfa.model.3 <- '
visual  =~ x1 + x2 + x3 + x6
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9 + x2
'
fit_cfa3 <- cfa(cfa.model.3,
               data = HolzingerSwineford1939)
fit_cfa3_mg <- cfa(cfa.model.3,
                  data = HolzingerSwineford1939,
                  group = "school")

sem.model <- '
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
speed ~ a*textual
visual ~ b*speed
ab := a * b
'
fit_sem <- sem(sem.model,
               data = HolzingerSwineford1939)

sem.model_mg <- '
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
speed ~ c(a1, a2)*textual
visual ~ c(b1, b2)*speed
ab1 := a1 * b1
ab2 := a2 * b2
'
fit_sem_mg <- cfa(sem.model_mg,
                  data = HolzingerSwineford1939,
                  group = "school")

test_that("rsquare", {
    expect_equal(vec_rsquare(fit_cfa),
                 lavInspect(fit_cfa, "r2"),
                 ignore_attr = TRUE)
    expect_equal(vec_rsquare(fit_cfa_mg),
                 unlist(lavInspect(fit_cfa_mg, "r2")),
                 ignore_attr = TRUE)
    expect_equal(vec_rsquare(fit_sem),
                 lavInspect(fit_sem, "r2"),
                 ignore_attr = TRUE)
    expect_equal(vec_rsquare(fit_sem_mg),
                 unlist(lavInspect(fit_sem_mg, "r2")),
                 ignore_attr = TRUE)
  })

test_that("Sample vcov", {
    expect_equal(vec_sample_vcov(fit_cfa),
                 lav_matrix_vech(lavInspect(fit_cfa, "sampstats")$cov),
                 ignore_attr = TRUE)
    expect_equal(vec_sample_vcov(fit_cfa_mg),
                 c(lav_matrix_vech(lavInspect(fit_cfa_mg, "sampstats")[[1]]$cov),
                   lav_matrix_vech(lavInspect(fit_cfa_mg, "sampstats")[[2]]$cov)),
                 ignore_attr = TRUE)
    expect_equal(vec_sample_vcov(fit_sem),
                 lav_matrix_vech(lavInspect(fit_sem, "sampstats")$cov),
                 ignore_attr = TRUE)
    expect_equal(vec_sample_vcov(fit_sem_mg),
                 c(lav_matrix_vech(lavInspect(fit_sem_mg, "sampstats")[[1]]$cov),
                   lav_matrix_vech(lavInspect(fit_sem_mg, "sampstats")[[2]]$cov)),
                 ignore_attr = TRUE)
  })

test_that("Sample variances", {
    expect_equal(vec_sample_var(fit_cfa),
                 diag(lavInspect(fit_cfa, "sampstats")$cov),
                 ignore_attr = TRUE)
    expect_equal(vec_sample_var(fit_cfa_mg),
                 c(diag(lavInspect(fit_cfa_mg, "sampstats")[[1]]$cov),
                   diag(lavInspect(fit_cfa_mg, "sampstats")[[2]]$cov)),
                 ignore_attr = TRUE)
    expect_equal(vec_sample_var(fit_sem),
                 diag(lavInspect(fit_sem, "sampstats")$cov),
                 ignore_attr = TRUE)
    expect_equal(vec_sample_var(fit_sem_mg),
                 c(diag(lavInspect(fit_sem_mg, "sampstats")[[1]]$cov),
                   diag(lavInspect(fit_sem_mg, "sampstats")[[2]]$cov)),
                 ignore_attr = TRUE)
  })

test_that("Sampling variances of estimates", {
    expect_equal(vec_est_var(fit_cfa),
                 diag(lavInspect(fit_cfa, "vcov")),
                 ignore_attr = TRUE)
    expect_equal(vec_est_var(fit_cfa_mg),
                 diag(lavInspect(fit_cfa_mg, "vcov")),
                 ignore_attr = TRUE)
    expect_equal(vec_est_var(fit_sem),
                 diag(lavInspect(fit_sem, "vcov")),
                 ignore_attr = TRUE)
    expect_equal(vec_est_var(fit_sem_mg),
                 diag(lavInspect(fit_sem_mg, "vcov")),
                 ignore_attr = TRUE)
  })


test_that("Sampling variances of estimates", {
    expect_equal(vec_est_se(fit_cfa),
                 sqrt(diag(lavInspect(fit_cfa, "vcov"))),
                 ignore_attr = TRUE)
    expect_equal(vec_est_se(fit_cfa_mg),
                 sqrt(diag(lavInspect(fit_cfa_mg, "vcov"))),
                 ignore_attr = TRUE)
    expect_equal(vec_est_se(fit_sem),
                 sqrt(diag(lavInspect(fit_sem, "vcov"))),
                 ignore_attr = TRUE)
    expect_equal(vec_est_se(fit_sem_mg),
                 sqrt(diag(lavInspect(fit_sem_mg, "vcov"))),
                 ignore_attr = TRUE)
  })

test_that("Sanity check", {
    expect_error(vec_def_var(fit_cfa))
    expect_error(vec_def_var(fit_cfa_mg))
  })


test_that("Sampling variances of user-defined parameters", {
    expect_equal(vec_def_var(fit_sem),
                 lavInspect(fit_sem, "vcov.def"),
                 ignore_attr = TRUE)
    expect_equal(vec_def_var(fit_sem_mg),
                 diag(lavInspect(fit_sem_mg, "vcov.def")),
                 ignore_attr = TRUE)
  })

test_that("Sanity check", {
    expect_error(vec_def_se(fit_cfa))
    expect_error(vec_def_se(fit_cfa_mg))
  })

test_that("SEs of user-defined parameters", {
    expect_equal(vec_def_se(fit_sem),
                 sqrt(lavInspect(fit_sem, "vcov.def")),
                 ignore_attr = TRUE)
    expect_equal(vec_def_se(fit_sem_mg),
                 sqrt(diag(lavInspect(fit_sem_mg, "vcov.def"))),
                 ignore_attr = TRUE)
  })

tmp1 <- lavTestLRT(fit_cfa3, fit_cfa2, fit_cfa)
tmp2 <- lavTestLRT(fit_cfa2, fit_cfa)
tmp3 <- lavTestLRT(fit_cfa3, fit_cfa)

test_that("LR Tests", {
    expect_equal(vec_lavTestLRT(fit_cfa, fit_cfa2, fit_cfa3,
                                model.names = c("M1", "M2", "M3"))[c(3, 6)],
                 tmp1[2:3, "Pr(>Chisq)"],
                 ignore_attr = TRUE)
    expect_equal(vec_lavTestLRT(fit_cfa, fit_cfa2,
                                model.names = c("M1", "M2"))[c(3)],
                 tmp2[2, "Pr(>Chisq)"],
                 ignore_attr = TRUE)
    expect_equal(vec_lavTestLRT(fit_cfa, fit_cfa3,
                                model.names = c("M1", "M3"))[c(3)],
                 tmp3[2, "Pr(>Chisq)"],
                 ignore_attr = TRUE)
  })

test_that("Score tests", {
    expect_equal(vec_lavTestScore(fit_cfa, add = c("visual =~ x9\ntextual =~ x3"))[1],
                 lavTestScore(fit_cfa, add = c("visual =~ x9\ntextual =~ x3"))$test[1, "X2"],
                 ignore_attr = TRUE)
    expect_equal(vec_lavTestScore(fit_cfa, add = c("visual =~ x9"))[1],
                 lavTestScore(fit_cfa, add = c("visual =~ x9"))$test[1, "X2"],
                 ignore_attr = TRUE)
    expect_equal(length(vec_lavTestScore(fit_cfa, add = c("visual =~ x9\ntextual =~ x3"), univariate = FALSE)),
                 3,
                 ignore_attr = TRUE)
  })

test_that("Wald tests", {
    expect_equal(vec_lavTestWald(fit_sem_mg, "a1 == a2\nb1 == b2", prefix = "Con1"),
                 unlist(lavTestWald(fit_sem_mg, "a1 == a2\nb1 == b2")[1:3]),
                 ignore_attr = TRUE)
    expect_false(any(grepl("Con1", names(vec_lavTestWald(fit_sem_mg, "a1 == a2\nb1 == b2")))))
  })

skip_if_not_installed("semTools")
library(semTools)
test_that("compRelSEM", {
    expect_equal(vec_compRelSEM(fit_cfa),
                 compRelSEM(fit_cfa),
                 ignore_attr = TRUE)
    expect_equal(vec_compRelSEM(fit_cfa, return.total = TRUE),
                 compRelSEM(fit_cfa, return.total = TRUE),
                 ignore_attr = TRUE)
    expect_equal(sort(vec_compRelSEM(fit_cfa_mg)),
                 sort(unlist(compRelSEM(fit_cfa_mg)[, -1])),
                 ignore_attr = TRUE)
    expect_equal(sort(vec_compRelSEM(fit_cfa_mg, return.total = TRUE)),
                 sort(unlist(compRelSEM(fit_cfa_mg, return.total = TRUE)[, -1])),
                 ignore_attr = TRUE)
  })

test_that("Sanity check", {
    expect_error(vec_compRelSEM(fit_cfa, return.df = TRUE))
  })
