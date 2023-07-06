#' @title Store Bootstrap Estimates of User-Defined Parameters
#'
#' @description It receives a
#' [lavaan::lavaan-class] object fitted
#' with bootstrapping standard errors
#' requested, computes the user-defined
#' parameters in each bootstrap samples,
#' and returns [lavaan::lavaan-class] object
#' with the estimates stored.
#'
#' @details
#' [lavaan::lavaan()] and its wrappers,
#' such as [lavaan::sem()] and
#' [lavaan::cfa()], stores the estimates
#' of free parameters in each bootstrap
#' sample if bootstrapping is requested.
#' However, if a model has user-defined
#' parameters, their values in each
#' bootstrap sample are not stored.
#;
#' [store_boot_def()] computes the
#' retrieves the stored bootstrap
#' estimates and computes the values
#' of user-defined parameters. The
#' values are then stored in the slot
#' `external` of the object,
#' in the element `shh_boot_def`.
#' The bootstrap estimates can then be
#' used by other functions for diagnostics
#' purposes.
#'
#' [get_boot_def()] extracts the
#' bootstrap estimates of user-defined
#' parameters from a [lavaan-class]
#' object. If none is stored, `NULL`
#' is returned.
#'
#' @return [store_boot_def()] returns
#' the fit object set to
#' `object`, with the bootstrap values
#' of user-defined parameters in the
#' bootstrap samples, as a matrix,
#' stored in the
#' slot `external` of `object` under
#' the name
#' `shh_boot_def`.
#'
#' [get_boot_def()] returns a matrix
#' of the stored bootstrap estimates
#' of user-defined parameters
#'
#' @param object A [lavaan-class]
#' object, fitted with 'se = "boot"'.
#'
#' @param force_run If `TRUE`, will skip
#' checks and run models without
#' checking the estimates. For internal
#' use. Default is `FALSE`.
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
#' # Should set bootstrap to at least 2000 in real studies
#' fit <- sem(model, data = dat, fixed.x = FALSE,
#'            se = "boot",
#'            bootstrap = 100)
#' summary(fit)
#'
#' fit_with_boot_def <- store_boot_def(fit)
#' get_boot_def(fit_with_boot_def)
#'
#' @name store_boot_def
NULL

#' @rdname store_boot_def
#' @export

store_boot_def <- function(object,
                           force_run = FALSE) {
    if (!inherits(object, "lavaan")) {
        stop("The object must be a lavaan-class object.")
      }
    if (!force_run) {
      }

    out_all <- boot_def(object = object)
    object@external$shh_boot_def <- out_all
    return(object)
  }

#' @rdname store_boot_def
#' @export

get_boot_def <- function(object) {
    return(object@external$shh_boot_def)
  }

#' Compute bootstrap estimates of user-defined parameters
#' @noRd

boot_def <- function(object) {
    # For lavaan 0.6-13
    # Remove bootstrap replications with error
    if (!(":=" %in% lavaan::parameterTable(object)$op)) {
        return(NULL)
      }
    boot_est0 <- try(lavaan::lavTech(object, "boot"), silent = TRUE)
    if (inherits(boot_est0, "try-error")) {
        stop("Bootstrapping estimates not found. Was se = 'boot' or 'bootstrap'?")
      }
    boot_error_idx <- attr(boot_est0, "error.idx")
    if (!is.null(boot_error_idx)) {
        if (length(boot_error_idx) > 0) {
            boot_est0 <- boot_est0[-boot_error_idx, ]
          }
      }
    boot_est <- split(boot_est0, row(boot_est0))
    out_all <- lapply(boot_est, object@Model@def.function)
    out_all <- do.call(rbind, out_all)
    return(out_all)
  }
