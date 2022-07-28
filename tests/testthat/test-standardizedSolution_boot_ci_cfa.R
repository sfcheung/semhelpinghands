# skip_on_cran()
# skip_if(!interactive(),
#         message = "standardizedSolution_boot_ci not tested if not interactive")

library(testthat)
library(semhelpinghands)

# Example from https://lavaan.ugent.be/tutorial/cfa.html
options(width = 132)

library(lavaan)
data(HolzingerSwineford1939)
model <-
"
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
"

set.seed(1234)
system.time(fit <- cfa(model,
                       data = HolzingerSwineford1939,
                       se = "boot",
                       bootstrap = 100,
                       warn = FALSE))

ci_boot <- standardizedSolution_boot_ci(fit, save_boot_est_std = TRUE)

get_std <- function(object) {
    lavaan::standardizedSolution(object)$est.std
  }
fit2 <- cfa(model,
            data = HolzingerSwineford1939,
            se = "none",
            bootstrap = 100)
set.seed(1234)
boot_ci_test <- suppressWarnings(bootstrapLavaan(fit2, R = 100,
                                FUN = get_std))

test_that("Compare boot estimates directly", {
    expect_equal(
        attr(ci_boot, "boot_est_std"),
        boot_ci_test,
        ignore_attr = TRUE
      )
  })
