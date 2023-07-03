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

print(ci_boot, nd = 5)
print(ci_boot, output = "text")
print(ci_boot, output = "text", standardized_only = FALSE)

print.std_solution_boot <- function(x,
                                    ...,
                                    output = "table",
                                    standardized_only = TRUE,
                                    nd = 3) {
    if (output == "table") {
        NextMethod()
        return(x)
      }
    ptable <- attr(x, "partable")
    est0 <- attr(x, "est")
    est1 <- est0
    est1$id <- seq_len(nrow(est1))
    i0 <- colnames(x) %in% c("se", "z", "pvalue",
                             "ci.lower", "ci.upper")
    est1 <- merge(est1,
                  x[, !i0])
    i0 <- colnames(ptable) %in% c("est", "se",
                                  "user", "free",
                                  "ustart", "plabel",
                                  "start",
                                  "id")
    est1 <- merge(est1, ptable[, !i0])
    est1 <- est1[order(est1$id), ]
    est1$id <- NULL
    tmp <- colnames(est1)
    tmp[tmp == "est.std"] <- "std"
    tmp[tmp == "boot.ci.lower"] <- "std.ci.lower"
    tmp[tmp == "boot.ci.upper"] <- "std.ci.upper"
    class(est1) <- class(est0)
    tmp <- !(names(pe_attrib) %in% names(attributes(est1)))
    attributes(est1) <- c(attributes(est1),
                          pe_attrib[tmp])
    class(est1) <- c("lavaan.parameterEstimates", class(est1))
    if (!standardized_only) {
        print(est1, ..., nd = nd)
        return(invisible(x))
      } else {
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
        out <- capture.output(print(est2, nd = nd))
        i <- grepl("Parameter Estimates:", out, fixed = TRUE)
        out[i] <- "Standardized Estimates"
        i <- grepl("  Standard errors  ", out, fixed = TRUE)
        j <- unlist(gregexpr("Bootstrap", out[i]))[1]
        tmp <- "  Confidence interval"
        st1 <- paste0(tmp,
                      paste0(rep(" ", j - nchar(tmp) - 1),
                             collapse = ""),
                      "Bootstrap")
        j <- nchar(out[i])
        tmp <- "  Confidence Level"
        tmp2 <- paste0(formatC(level * 100, digits = 1, format = "f"),
                       "%")
        st2 <- paste0(tmp,
                      paste0(rep(" ", j - nchar(tmp) - nchar(tmp2)),
                             collapse = ""),
                      tmp2)
        out <- c(out[seq_len(which(i))],
                 st1,
                 st2,
                 out[-seq_len(which(i))])
        out <- gsub("    Estimate  Std.Err",
                    "Standardized  Std.Err",
                    out)
        cat(out, sep = "\n")
        return(invisible(x))
      }
  }

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
