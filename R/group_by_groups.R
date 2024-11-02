#' @title Group Estimates By Groups
#'
#' @description Groups parameter
#' estimates or other information such
#' as *p*-values into a table with
#' groups as columns and parameters as
#' rows.
#'
#' @return A data-frame-like object of
#' the class `est_table`.
#'
#' @param object A 'lavaan'-class object
#' or the output of
#' [lavaan::parameterEstimates()] or
#' [lavaan::standardizedSolution()].
#'
#' @param ... Optional arguments to be
#' passed to
#' [lavaan::parameterEstimates()].
#' Ignored if object is an output of
#' [lavaan::parameterEstimates()] or
#' [lavaan::standardizedSolution()].
#'
#' @param col_names A vector of the
#' column names in the parameter
#' estimate tables to be included.
#' Default is `"est"`.
#'
#' @param group_first If `TRUE`, the
#' columns will be grouped by groups
#' first and then by columns in the
#' parameter estimates tables. Default
#' is `TRUE`.
#'
#' @param group_labels A character
#' vector of group labels. Will be
#' assigned to group id = 1, 2, 3, etc.
#' If not provided. will try to be
#' retrieved from `object` if it is a
#' [lavaan::lavaan-class] object.
#'
#' @param fit Optional. A
#' [lavaan::lavaan-class] object.
#' If `object` is a parameter estimates
#' table and `group_labels` is `NULL`,
#' it will try to retrieve the group
#' labels from `fit` is supplied.
#'
#' @param use_standardizedSolution If `TRUE`
#' and `object` is not an
#' estimates table,
#' then [lavaan::standardizedSolution()]
#' will be used to generate the table.
#' If `FALSE`, the default, then
#' [lavaan::parameterEstimates()] will
#' be used if necessary.
#'
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' library(lavaan)
#' set.seed(5478374)
#' n <- 100
#' x <- runif(n) - .5
#' m <- .40 * x + rnorm(n, 0, sqrt(1 - .40))
#' y <- .30 * m + rnorm(n, 0, sqrt(1 - .30))
#' city <- sample(c("City Alpha", "City Beta"), 100,
#'                replace = TRUE)
#' dat <- data.frame(x = x, y = y, m = m, city = city)
#' model <-
#' '
#' m ~ c(a1, a2)*x
#' y ~ c(b1, b2)*m
#' a1b1 := a1*b1
#' a2b2 := a2*b2
#' '
#' fit <- sem(model, data = dat, fixed.x = FALSE,
#'            group = "city")
#' (est <- parameterEstimates(fit))
#'
#' # Group them by groups
#' group_by_groups(fit)
#'
#' # Can also work on a parameter estimates table
#' # To have group labels, need to supply the fit object
#' group_by_groups(est, fit = fit)
#'
#' # Can be used with some other functions in semhelpinghands
#' # when used on a parameter estimates table
#' group_by_groups(filter_by(est, op = "~"), fit = fit)
#'
#' # Also support piping
#' est |> filter_by(op = "~") |>
#'        group_by_groups(fit = fit)
#'
#' @export

group_by_groups <- function(object,
                         ...,
                         col_names = "est",
                         group_first = TRUE,
                         group_labels = NULL,
                         fit = NULL,
                         use_standardizedSolution = FALSE) {
    object_type <- check_lavaan_type(object)
    if (is.na(object_type)) {
        stop("object is not of the accepted types.")
      }
    if (object_type == "lavaan") {
        if (use_standardizedSolution) {
            p_est <- lavaan::standardizedSolution(object,
                                                ...)
          } else {
            p_est <- lavaan::parameterEstimates(object,
                                                ...)
          }
      } else {
        p_est <- object
      }
    grouped <- is_grouped(object)
    if (!grouped) stop("No groups found")
    p_est_org <- p_est
    p_est <- p_est[p_est$group != 0, ]
    p_est_gp <- p_est$group
    p_est <- p_est[, -which(colnames(p_est) == "group")]
    p_est_grouped <- split(p_est, p_est_gp)
    if (!is.null(group_labels)) {
        names(p_est_grouped) <- group_labels
      } else {
        if (object_type == "lavaan") {
            names(p_est_grouped) <- lavaan::lavInspect(object, "group.label")
          } else if (inherits(fit, "lavaan")) {
            names(p_est_grouped) <- lavaan::lavInspect(fit, "group.label")
          }
      }
    group_by_models(p_est_grouped,
                    col_names = col_names,
                    model_first = group_first)
  }
