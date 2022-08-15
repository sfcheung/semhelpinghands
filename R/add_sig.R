#' @title Add Significant Test Results
#'
#' @description Insert columns to denote whether a
#'  parameter is significant.
#'
#' @details The function calls [lavaan::parameterEstimates()] or
#'  [lavaan::standardizedSolution()] and checks the columns
#'  `pvalue`, `ci.lower` and `ci.upper`, and `boot.ci.lower`
#'  and `boot.ci.upper` and
#'  then insert columns to denote for each parameter estimate
#'  whether it is significant based on the
#'  requested criteria.
#'
#' @return The output of [lavaan::parameterEstimates()] or
#'  [lavaan::standardizedSolution()], with one or two columns
#'  inserted after the parameter estimates to denote the
#'  significant test results.
#'
#' @param object A [lavaan-class] object or the output
#'               of [lavaan::parameterEstimates()] or
#'               [lavaan::standardizedSolution()].
#' @param ... Optional arguments to be passed to
#'  [lavaan::parameterEstimates()] or
#'  [lavaan::standardizedSolution()].
#' @param standardized Whether standardized solution
#'  is needed. If `TRUE`, [lavaan::standardizedSolution()]
#'  will be called. If `FALSE`, the default,
#'  [lavaan::parameterEstimates()] will be called.
#'  Ignored if a table if estimates is supplied.
#' @param na_str The string to be used for parameters
#'  with no significant tests. For example, fixed
#'  parameters. Default is `""`.
#' @param use A character vector of one or more
#'  strings. If `"pvalue"` is in the vector,
#'  *p*-values will be used. If
#'  `"ci"` is in the vector, confidence intervals
#'  appeared on `ci.lower` and `ci.uppwr`
#'   will be used.
#'  If `"boot.ci"` is in the vector and the columns
#'  `boot.ci.lower` and `boot.ci.upper` are available,
#'  these columns will be used.
#'  Note that `ci.lower` and `ci.upper` can also
#'  be bootstrap confidence intervals in some tables if
#'  `se = "boot"` is used.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [lavaan::parameterEstimates()]
#'          and [lavaan::standardizedSolution()]
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
    use <- match.arg(use, choices = c("pvalue", "ci", "boot.ci"), several.ok = TRUE)
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
    if (("pvalue" %in% use) && !is.null(out$pvalue)) {
        pvalue <- out$pvalue
        psig <- stats::symnum(pvalue,
                            corr = FALSE,
                            na = na_str,
                            cutpoints = c(0 , .001, .01, .05, 1),
                            symbols = c("***", "**", "*", " "))
      } else {
        psig <- NULL
      }
    if (("ci" %in% use) && !is.null(out$ci.lower) &&
                           !is.null(out$ci.upper)) {
        ci_chk <- ((out$ci.lower > 0) | (out$ci.upper < 0)) &
                  (out$ci.lower != out$ci.upper)
        csig <- ifelse(ci_chk, "Sig", "  ")
      } else {
        csig <- NULL
      }
    if (("boot.ci" %in% use) && !is.null(out$boot.ci.lower) &&
                                !is.null(out$boot.ci.upper)) {
        bci_chk <- ((out$boot.ci.lower > 0) | (out$boot.ci.upper < 0)) &
                   (out$boot.ci.lower != out$boot.ci.upper)
        bcsig <- ifelse(bci_chk, "Sig", "  ")
      } else {
        bcsig <- NULL
      }
    if (standardized) {
        k <- which(colnames(out) == "est.std")
      } else {
        k <- which(colnames(out) == "est")
      }
    out0 <- NULL
    if ("pvalue" %in% use) {
        out0 <- cbind(out0, sig = format(psig))
      }
    if ("ci" %in% use) {
        out0 <- cbind(out0, ci = format(csig))
      }
    if ("boot.ci" %in% use) {
        out0 <- cbind(out0, boot.ci = format(bcsig))
      }
    k0 <- ncol(out0)
    k1 <- ncol(out)
    if (!is.null(k0)) {
        out2 <- out
        tmp <- cbind(out0, out[, (k + 1):k1, drop = FALSE])
        out2[, (k + 1):(k1 + k0)] <- tmp
        colnames(out2)[(k + 1):(k1 + k0)] <- colnames(tmp)
      } else {
        out2 <- out
      }
    # class(out2) <- class(out)
    out2
  }
