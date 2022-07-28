#' @title Add Significant Test Results
#'
#' @description Insert columns to denote whether a
#'  parameter is significant.
#'
#' @details The function calls [lavaan::parameterEstimates()] or
#'  [lavaan::standardizedSolution()] and check the columns
#'  `pvalue` and/or `ci.lower` and `ci.upper` and
#'  then insert columns to denote for each parameter estimate
#'  whether it is significant.
#'
#'
#' @return The output of [lavaan::parameterEstimates()] or
#'  [lavaan::standardizedSolution()], with one or two columns
#'  inserted after the parameter estimates to denote the
#'  significant test results.
#'
#' @param object A [lavaan-class] object.
#' @param ... Optional arguments to be passed to
#'  [lavaan::parameterEstimates()] or
#'  [lavaan::standardizedSolution()].
#' @param standardized Whether standardized solution
#'  is needed. If `TRUE`, [lavaan::standardizedSolution()]
#'  will be called. If `FALSE`, the default,
#'  [lavaan::parameterEstimates()] will be called.
#' @param na_str The string to be used for parameters
#'  with no significant tests. For example, fixed
#'  parameters. Default is `""`.
#' @param use A character vector of one or two
#'  elements. If one of them is `"pvalue"`,
#'  *p*-values will be used. If one of them is
#'  `"ci"`, confidence intervals will be used.
#'  Can use both by `c("pvalue", "ci")`.
#'
#' @author Shu Fai Cheung (shufai.cheung@gmail.com)
#'
#' @examples
#'
#' library(lavaan)
#' set.seed(5478374)
#' n <- 50
#' x <- runif(n) - .5
#' m <- .40 * x + rnorm(n, 0, sqrt(1 - .40))
#' y <- .30 * m + rnorm(n, 0, sqrt(1 - .30))
#' dat <- data.frame(x = x, y = y, m = m)
#' model <-
#' '
#' m ~ a*x
#' y ~ b*m
#' ab := a*b
#' '
#' fit <- sem(model, data = dat, fixed.x = FALSE)
#'
#' add_sig(fit)
#' add_sig(fit, standardized = TRUE)
#' add_sig(fit, use = "ci")
#' add_sig(fit, standardized = TRUE, use = "ci")
#' add_sig(fit, standardized = TRUE, use = c("ci", "pvalue"))
#'
#' @export

add_sig <- function(object,
                    ...,
                    standardized = FALSE,
                    na_str = "",
                    use = "pvalue") {
    use <- match.arg(use, choices = c("pvalue", "ci"), several.ok = TRUE)
    args0 <- list(...)
    if (inherits(object, "lavaan")) {
        if (standardized) {
            out <- lavaan::standardizedSolution(object, ...)
          } else {
            out <- lavaan::parameterEstimates(object, ...)
          }
      } else {
        out <- object
        if ("est.std" %in% colnames(out)) {
            standardized <- TRUE
          } else {
            standardized <- FALSE
          }
      }
    if ("pvalue" %in% use) {
        pvalue <- out$pvalue
        psig <- stats::symnum(pvalue,
                            corr = FALSE,
                            na = na_str,
                            cutpoints = c(0 , .001, .01, .05, 1),
                            symbols = c("***", "**", "*", " "))
      } else {
        psig <- NULL
      }
    if ("ci" %in% use) {
        ci_chk <- ((out$ci.lower > 0) | (out$ci.upper < 0)) &
                  (out$ci.lower != out$ci.upper)
        if ("level" %in% names(args0)) {
            level = args0$level
          } else {
            level = .95
          }
        csig <- ifelse(ci_chk, "Sig", "  ")
      } else {
        csig <- NULL
      }
    if (standardized) {
        k <- which(colnames(out) == "est.std")
      } else {
        k <- which(colnames(out) == "est")
      }
    if (identical(use, "pvalue")) {
        out0 <- data.frame(sig = format(psig))
        k0 <- 1
      } else if (identical(use, "ci")) {
        out0 <- data.frame(ci.sig = format(csig))
        k0 <- 1
      } else {
        out0 <- data.frame(sig = format(psig),
                           ci.sig = format(csig))
        k0 <- 2
      }
    out2 <- data.frame(out[c(1:k)],
                        out0,
                        out[c((k + k0):ncol(out))])
    class(out2) <- class(out)
    out2
  }