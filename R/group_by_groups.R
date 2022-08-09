#' @title Group Estimates By Groups
#'
#' @description Groups parameter estimates or other information such as
#'               p-values into a table with groups as columns
#'               and parameters as rows.
#'
#' @return A data-frame-like object of the class `est_table`.
#'
#' @param object A [lavaan-class] object or the output of
#'                [lavaan::parameterEstimates()] or
#'                [lavaan::standardizedSolution()].
#'
#' @param ... Optional arguments to be passed to
#'            [lavaan::parameterEstimates()]. Ignored if object is an
#'            output of [lavaan::parameterEstimates()] or
#'                [lavaan::standardizedSolution()].
#'
#' @param col_names A vector of the column names in the
#'                  parameter estimate tables to be included.
#'                  Default is `"est"`.
#'
#' @param group_first If `TRUE`, the columns will be grouped
#'                    by groups first and then by
#'                    columns in the parameter estimates
#'                    tables. Default is `TRUE`.
#'
#' @param group_labels A character vector of group labels.
#'                     Will be assigned to group id = 1, 3, 3,
#'                     etc. If not provided. will try to be
#'                     retrieved from `object` if it is
#'                     a [lavaan::lavaan-class] object.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' # TODO: Amend
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
#' parameterEstimates(fit)
#' group_by_dvs(fit)
#' group_by_ivs(fit)
#'
#' @export

group_by_groups <- function(object,
                         ...,
                         col_names = "est",
                         group_first = TRUE,
                         group_labels = NULL) {
    object_type <- check_lavaan_type(object)
    if (is.na(object_type)) {
        stop("object is not of the accepted types.")
      }
    if (object_type == "lavaan") {
        p_est <- lavaan::parameterEstimates(object,
                                            ...)
      } else {
        p_est <- object
      }
    grouped <- is_grouped(object)
    if (!grouped) stop("No groups found")
    p_est_org <- p_est
    p_est <- p_est[p_est$group != 0, ]
    p_est <- p_est[, -which(colnames(p_est) == "group")]
    p_est_grouped <- split(p_est, p_est_org$group)
    if (!is.null(group_labels)) {
        names(p_est_grouped) <- group_labels
      } else {
        if (object_type == "lavaan") {
            names(p_est_grouped) <- lavaan::lavInspect(object, "group.label")
          }
      }
    group_by_models(p_est_grouped,
                    col_names = col_names,
                    model_first = group_first)
  }
