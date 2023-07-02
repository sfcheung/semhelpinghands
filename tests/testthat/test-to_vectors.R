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

get_rsquare(fit_cfa)
get_rsquare(fit_cfa_mg)
get_rsquare(fit_sem)
get_rsquare(fit_sem_mg)

get_sample_vcov(fit_cfa)
get_sample_vcov(fit_cfa_mg)
get_sample_vcov(fit_sem)
get_sample_vcov(fit_sem_mg)

get_sample_var(fit_cfa)
get_sample_var(fit_cfa_mg)
get_sample_var(fit_sem)
get_sample_var(fit_sem_mg)

get_est_var(fit_cfa)
get_est_var(fit_cfa_mg)
get_est_var(fit_sem)
get_est_var(fit_sem_mg)

get_est_se(fit_cfa)
get_est_se(fit_cfa_mg)
get_est_se(fit_sem)
get_est_se(fit_sem_mg)

test_that("Sanity check", {
    expect_error(get_def_var(fit_cfa))
    expect_error(get_def_var(fit_cfa_mg))
  })
get_def_var(fit_sem)
get_def_var(fit_sem_mg)

test_that("Sanity check", {
    expect_error(get_def_se(fit_cfa))
    expect_error(get_def_se(fit_cfa_mg))
  })
get_def_se(fit_sem)
get_def_se(fit_sem_mg)


get_lavTestLRT(fit_cfa, fit_cfa2, fit_cfa3,
               model.names = c("M1", "M2", "M3"))
get_lavTestLRT(fit_cfa, fit_cfa2,
               model.names = c("M1", "M2"))
get_lavTestLRT(fit_cfa, fit_cfa3,
               model.names = c("M1", "M3"))

get_lavTestScore(fit_cfa, add = c("visual =~ x9\ntextual =~ x3"))
get_lavTestScore(fit_cfa, add = c("visual =~ x9"))
get_lavTestScore(fit_cfa, add = c("visual =~ x9\ntextual =~ x3"), univariate = FALSE)

get_lavTestWald(fit_sem_mg, "a1 == a2\nb1 == b2", prefix = "Con1")
get_lavTestWald(fit_sem_mg, "a1 == a2\nb1 == b2")

get_compRelSEM(fit_cfa)
get_compRelSEM(fit_cfa, return.total = TRUE)
get_compRelSEM(fit_cfa_mg)
get_compRelSEM(fit_cfa_mg, return.total = TRUE)
test_that("Sanity check", {
    expect_error(get_compRelSEM(fit_cfa, return.df = TRUE))
  })
