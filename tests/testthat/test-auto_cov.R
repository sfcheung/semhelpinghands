library(lavaan)

mod_check <-
"
m1 ~ x1 + w1 + w1x1 + x2 + c1 + c2
m2 ~ m1 + c1 + c2
m3 ~ x2 + x1 + c1 + c2
y1 ~ m2 + w2 + w2m2 + x1 + x2 + m3 + c1 + c2
y2 ~ m3 + x2 + x1 + m2 + c1 + c2
# Covariances for the error term of m2
m2 ~~ w2 + w2m2
# Covariances between all exogenous variables
w2   ~~ w2m2 + x1 + w1 + w1x1 + x2 + c1 + c2
w2m2 ~~ x1 + w1 + w1x1 + x2 + c1 + c2
x1   ~~ w1 + w1x1 + x2 + c1 + c2
w1   ~~ w1x1 + x2 + c1 + c2
w1x1 ~~ x2 + c1 + c2
x2   ~~ c1 + c2
c1   ~~ c2
"

fit_check <- sem(model = mod_check, meanstructure = TRUE,
           fixed.x = FALSE, do.fit = FALSE)

mod <-
"
m1 ~ x1 + w1 + w1x1 + x2 + c1 + c2
m2 ~ m1 + c1 + c2
m3 ~ x2 + x1 + c1 + c2
y1 ~ m2 + w2 + w2m2 + x1 + x2 + m3 + c1 + c2
y2 ~ m3 + x2 + x1 + m2 + c1 + c2
# Covariances for the error term of m2
m2 ~~ w2 + w2m2
"

fit <- sem(model = mod, meanstructure = TRUE,
           fixed.x = FALSE, do.fit = FALSE)

mod_auto <- add_exo_cov(mod)
cat(mod_auto)

fit_auto <- sem(model = c(mod, auto_exo_cov(mod, print = FALSE)),
           meanstructure = TRUE,
           fixed.x = FALSE, do.fit = FALSE)

test_that("auto_cov", {
    expect_identical(lavInspect(fit_auto)$psi != 0,
                     lavInspect(fit_check)$psi != 0)
  })
