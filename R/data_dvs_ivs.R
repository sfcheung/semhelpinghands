#' @title Sample Dataset: 3 Predictors and 3 Outcomes
#'
#' @description A path model with three predictors
#' and three outcomes, for illustration.
#'
#' @format A data frame with 100 rows and 7 variables:
#' \describe{
#'   \item{y1}{Outcome variable 1. Numeric.}
#'   \item{y2}{Outcome variable 2. Numeric.}
#'   \item{y3}{Outcome variable 3. Numeric.}
#'   \item{x1}{Predictor 1. Numeric.}
#'   \item{x2}{Predictor 2. Numeric.}
#'   \item{x3}{Predictor 3. Numeric.}
#'   \item{gp}{Group variable: "gp1" or "gp2". String.}
#' }
#'
#' @examples
#' library(lavaan)
#' mod <-
#' "
#' y1 ~ x1 + x2 + x3
#' y2 ~ x1 + x3
#' y3 ~ y2 + x2
#' "
#' fit <- sem(mod, dat)
#' parameterEstimates(fit)
#' fit_gp <- sem(mod, dat, group = "gp")
#' parameterEstimates(fit_gp)
#'
"dvs_ivs"