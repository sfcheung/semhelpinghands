skip("WIP")
# skip_on_cran()
# skip_if(!interactive(),
#         message = "standardizedSolution_boot_ci not tested if not interactive")

library(testthat)
library(semhelpinghands)

# Example from https://lavaan.ugent.be/tutorial/mediation.html

library(lavaan)
set.seed(1234)
n <- 1000
X <- runif(n) - .5
M <- 0.20*X + rnorm(n)
Y <- 0.17*M + rnorm(n)
Data <- data.frame(X = X, Y = Y, M = M)
model <- ' # direct effect
             Y ~ c*X
           # mediator
             M ~ a*X
             Y ~ b*M
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '
set.seed(1234)
# One bootstrap replication failed. Kept intentionally.
suppressWarnings(system.time(fit <- sem(model,
                       data = Data,
                       se = "boot",
                       bootstrap = 100)))

ci_boot <- standardizedSolution_boot_ci(fit, save_boot_est_std = TRUE)

summary_fit <- summary(fit)
pe_attrib <- attributes(summary_fit$pe)
ptable <- parameterTable(fit)
est0 <- parameterEstimates(fit)
est1 <- est0
est1$id <- seq_len(nrow(est1))
est1 <- merge(est1, ci_boot[, c("lhs", "op", "rhs", "est.std", "boot.ci.lower", "boot.ci.upper", "boot.se")])
est1 <- merge(est1, ptable)
est1 <- est1[order(est1$id), ]
est1$user <- NULL
est1$free <- NULL
est1$ustart <- NULL
est1$plabel <- NULL
est1$start <- NULL
est1$id <- NULL
tmp <- colnames(est1)
tmp[tmp == "est.std"] <- "std"
tmp[tmp == "boot.ci.lower"] <- "std.ci.lower"
tmp[tmp == "boot.ci.upper"] <- "std.ci.upper"
class(est1) <- class(est0)
est1
tmp <- !(names(pe_attrib) %in% names(attributes(est1)))
attributes(est1) <- c(attributes(est1),
                      pe_attrib[tmp])
class(est1) <- c("lavaan.parameterEstimates", class(est1))
est1

est2 <- est1
est2$est <- est2$est.std
est2$ci.lower <- est2$boot.ci.lower
est2$ci.upper <- est2$boot.ci.upper
est2$se <- est2$boot.se
est2$boot.se <- NULL
est2$z <- NULL
est2$pvalue <- NULL
est2$est.std <- NULL
est2$boot.ci.lower <- NULL
est2$boot.ci.upper <- NULL
est2

get_std <- function(object) {
    lavaan::standardizedSolution(object)$est.std
  }
# fit2 <- update(fit, se = "none")
fit2 <- sem(model,
            data = Data,
            se = "none",
            bootstrap = 100)
set.seed(1234)
suppressWarnings(boot_ci_test <- bootstrapLavaan(fit2, R = 100,
                                FUN = get_std))

# For lavaan 0.9-13
boot_ci_test_error_idx <- attr(boot_ci_test, "error.idx")
if (!is.null(boot_ci_test_error_idx)) {
    if (length(boot_ci_test_error_idx) > 0) {
        boot_ci_test <- boot_ci_test[-boot_ci_test_error_idx, ]
      }
  }

test_that("Compare boot estimates directly", {
    expect_equal(
        attr(ci_boot, "boot_est_std"),
        boot_ci_test,
        ignore_attr = TRUE
      )
  })
