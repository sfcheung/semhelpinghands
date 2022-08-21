#' @title Refit a 'lavaan'-Model by
#' Several Estimators
#'
#' @description Refit a model in
#' 'lavaan' by several lavaan-supported
#' estimators
#'
#' @details The function simply uses
#' [lapply()] and [update()] to rerun
#' the analysis once for each of the
#' estimator using `update(object,
#' estimator = "x"`, `x` being the
#' estimator.
#'
#' The results can then be compared
#' using [group_by_models()].
#'
#' @return A list of `lavaan` outputs,
#' each of them is an update of the
#' original output using one of the
#' estimators.
#'
#' @param object A [lavaan-class]
#' object.
#'
#' @param estimators A character vector
#' of the estimator supported by the
#' `estimator` argument of
#' [lavaan::lavaan()] and its wrappers,
#' such as [lavaan::sem()] and
#' [lavaan::cfa()].
#'
#' @seealso [group_by_models()]
#'
#' @author Shu Fai Cheung
#' <https://orcid.org/0000-0002-9871-9448>.
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
#'
#' # Refit the model by three different estimators
#' fit_more <- compare_estimators(fit, estimator = c("GLS", "MLR", "ML"))
#'
#' # Use group_by_models to compare the estimates
#' group_by_models(fit_more, col_names = c("est", "pvalue"))
#'
#' # Use se_ratios to compare standard errors
#' se_ratios(fit_more, reference = "ML")
#'
#' @name compare_estimators
NULL

#' @describeIn compare_estimators Refit the model with different estimators.
#' @order 1
#' @export

compare_estimators <- function(object,
                               estimators = NULL) {
    if (is.null(estimators)) {
        stop("'estimators' cannot be NULL.")
      }
    if (!is.character(estimators)) {
        stop("'estimators' must be a numeric vector.")
      }
    if (!inherits(object, "lavaan")) {
        stop("'object' is not a lavaan-class object.")
      }
    call0 <- stats::getCall(object)
    call1 <- call0
    for (i in 2:length(call0)) {
        call1[[i]] <- eval(call0[[i]], parent.frame())
      }
    outs <- lapply(estimators, function(x) {
                      call1$estimator <- x
                      eval(call1)
                    })
    names(outs) <- estimators
    outs
  }
#' @param fit_list The output
#' of [compare_estimators()].
#'
#' @param reference The name
#' of the reference method
#' (ratios will be equal to one).
#' Must be one of the estimator
#' used on [compare_estimators()].
#' If `NULL`, the first estimator
#' will be used.
#'
#' @describeIn compare_estimators A wrapper of [group_by_models()] that
#' computes the ratios of standard errors of different methods
#' to those of one method.
#' @order 2
#' @export

se_ratios <- function(fit_list, reference = NULL) {
    fit_names <- names(fit_list)
    if (is.null(reference)) {
        reference <- fit_names[1]
        reference_id <- 1
      } else {
        reference_id <- match(reference, fit_names)
        if (is.na(reference_id)) {
            stop("'reference' not one of the estimators.")
          }
      }
    out <- group_by_models(fit_list, col_names = "se")
    se_names <- paste0("se_", fit_names)
    i <- match(se_names[1], colnames(out))
    tmp <- out[, -seq(1, i - 1)]
    tmp2 <- tmp / tmp[, reference_id]
    colnames(tmp2) <- paste0("ratio_", fit_names)
    out[colnames(tmp2)] <- tmp2
    out
  }