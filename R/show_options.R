#' @title Show Major Options in an
#' Output of 'lavaan'
#'
#' @description Display the values of
#' major options in a model fitted
#' by [lavaan::lavaan()] or its
#' wrappers (e.g., [lavaan::sem()] or
#' [lavaan::cfa()]).
#'
#' @details It extracts the values of
#' major options in the output of
#' [lavaan::lavaan()] or its wrappers
#' (e.g., [lavaan::sem()] or
#' [lavaan::cfa()].
#'
#' It checks the actual values, not
#' the call used. This is useful for
#' understanding how a prepackaged
#' estimator such as `ML`, `MLM`, and
#' `MLR` set other options. It
#' supports the following options:
#'
#' - Estimator (`estimator`)
#' - Standard error (`se`)
#' - Model chi-square test(s) (`test`)
#' - Missing data method (`missing`)
#' - Information matrix used for
#'    computing standard errors
#'    (`information`)
#' - Information matrix used for
#'    computing model chi-square
#'    (`information`)
#' - Whether the mean structure is
#'    included.
#'
#' @return
#' A `show_options`-class object with a
#' print method that formats the output.
#'
#' @param fit An output of
#' [lavaan::lavaan()] or its wrappers
#' (e.g., [lavaan::cfa()] and
#' [lavaan::sem()])
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#' library(lavaan)
#'
#' # From the help page of lavaan::cfa().
#'
#' HS.model <- '
#' visual  =~ x1 + x2 + x3
#' textual =~ x4 + x5 + x6
#' speed   =~ x7 + x8 + x9
#' '
#' fit <- cfa(HS.model, data = HolzingerSwineford1939)
#'
#' tmp <- show_options(fit)
#' tmp
#'
#' fit <- cfa(HS.model, data = HolzingerSwineford1939, estimator = "MLR")
#' show_options(fit)
#' fit <- cfa(HS.model, data = HolzingerSwineford1939, estimator = "MLM")
#' show_options(fit)
#'
#' @export
#'
#' @order 1

show_options <- function(fit) {
    call0 <- lavaan::lavInspect(fit, "call")
    call0_estimator <- call0$estimator
    if (is.null(call0_estimator)) {
        call0_estimator <- lavaan::lavOptions("estimator")$estimator
        call0_estimator <- paste(call0_estimator, collapse = ", ")
      }
    call0_se <- call0$se
    if (is.null(call0_se)) {
        call0_se <- lavaan::lavOptions("se")$se
      }
    call0_missing <- call0$missing
    if (is.null(call0_missing)) {
        call0_missing <- lavaan::lavOptions("missing")$missing
      }
    call0_test <- call0$test
    if (is.null(call0_test)) {
        call0_test <- lavaan::lavOptions("test")$test
      }
    call0_mimic <- call0$mimic
    if (is.null(call0_mimic)) {
        call0_mimic <- lavaan::lavOptions("mimic")$mimic
      }
    call0_model_type <- call0$model.type
    if (is.null(call0_model_type)) {
        call0_model_type <- lavaan::lavOptions("model.type")$model.type
      }
    call0_information <- call0$information
    if (is.null(call0_information)) {
        call0_information <- lavaan::lavOptions("information")$information
      }
    if (length(call0_information) == 1) {
        call0_information1 <- call0_information2 <- call0_information
      } else {
        call0_information1 <- call0_information[1]
        call0_information2 <- call0_information[2]
      }
    call0_meanstructure <- call0$meanstructure
    if (is.null(call0_meanstructure)) {
        call0_meanstructure <- lavaan::lavOptions("meanstructure")$meanstructure
        call0_meanstructure <- ifelse(is.logical(call0_meanstructure),
                                      ifelse(call0_meanstructure, "Yes", "No"),
                                      call0_meanstructure)
      }

    fit_opt <- fit@Options
    opt_estimator <- fit_opt$estimator
    opt_se <- fit_opt$se
    opt_test <- paste(fit_opt$test, collapse = ", ")
    opt_missing <- fit_opt$missing
    opt_mimic <- fit_opt$mimic
    opt_information1 <- fit_opt$information[1]
    opt_information2 <- fit_opt$information[2]
    opt_meanstructure <- ifelse(fit_opt$meanstructure, "Yes", "No")
    call_arg <- c(call0_estimator,
                  call0_se,
                  call0_test,
                  call0_missing,
                  call0_information1,
                  call0_information2,
                  call0_meanstructure)
    opt_arg <- c(opt_estimator,
                 opt_se,
                 opt_test,
                 opt_missing,
                 opt_information1,
                 opt_information2,
                 opt_meanstructure)
    opt_names <- c("Estimator(s)",
                   "Standard Error",
                   "Model Test Statistic(s)",
                   "Missing Data",
                   "Information Matrix (for SE)",
                   "Information Matrix (for Model Test)",
                   "Mean Structure")
    out <- data.frame(Options = opt_names,
                      Call = call_arg,
                      Actual = opt_arg)
    class(out) <- c("show_options", class(out))
    out
  }

#' @param x The output of [show_options()].
#' @param ... Additional arguments. Ignored.
#' @export
#' @describeIn show_options The print method of the output of [show_options()].
#' @order 2

print.show_options <- function(x, ...) {
    class(x) <- class(x)[-1]
    NextMethod(print, x, quote = FALSE, right = FALSE,
               row.names = FALSE, ...)
  }