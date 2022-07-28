#' @title Refit a 'lavaan'-Model by Several Estimators
#'
#' @description Refit a model in 'lavaan' by several
#'  lavaan-supported estimators
#'
#' @details The function simply use [lapply()] and
#'  [update()] to rerun the analysis once for each
#'  of the estimator using `update(object, estimator = "x"`,
#'  `x` being the estimator.
#'
#' The results can then be compared using [group_by_models()].
#'
#' @return A list of `lavaan` outputs, each of them is
#'  an update of the original output using one of the
#'  estimator.
#'
#' @param object A [lavaan-class] object.
#'
#' @param estimators A character vector of the estimator
#'  supported by the `estimator` argument of
#'  [lavaan::lavaan()] and its wrappers.
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
#'
#' fit <- sem(model, data = dat, fixed.x = FALSE)
#' fit_more <- compare_estimators(fit, estimator = c("GLS", "MLR", "ML"))
#' # Use group_by_models to compare the estimates
#' group_by_models(fit_more, col_names = c("est", "pvalue"))
#'
#'
#' @export

compare_estimators <- function(object,
                               estimators = NULL) {
    outs <- lapply(estimators,
                   function(x) lavaan::update(object, estimator = x))
    names(outs) <- estimators
    outs
  }