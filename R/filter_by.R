#' @title Filter a Parameter Estimates
#' Table
#'
#' @description Filter parameter
#' estimates table and similar tables
#' in`lavaan` by common fields such as
#' `op` (operator).
#'
#' @details This functions accepts the
#'  output of
#'  [lavaan::parameterEstimates()] and
#'  [lavaan::standardizedSolution()] and
#'  filter the rows by commonly used
#'  field.
#'
#' @return The filtered version of the
#' input object.
#'
#' @param object The output of
#' [lavaan::parameterEstimates()],
#' [lavaan::standardizedSolution()], or
#' a `lavaan.data.frame` object.
#' May also work on an `est_table`-class
#' object returned by functions
#' like [group_by_dvs()] but there
#' is no guarantee.
#'
#' @param op A character vector of the
#' operators (`op`) for filtering.
#' Common operators are `"~"`, `"~~"`,
#' `"=~"`, `":="`, and "`~1`".
#'
#' @param lhs A character vector of
#' names in the `lhs` column.
#'
#' @param rhs A character vector of
#' names in the `rhs` column.
#'
#' @param group A vector of either the
#' group numbers in the `group` column
#' of the labels of the groups. If
#' labels are supplied, the original fit
#' object must be supplied for
#' extracting the group labels.
#'
#' @param fit The original fit object.
#'  Usd when `group` is a vector of the
#'  group labels.
#'
#' @author Shu Fai Cheung
#' <https://orcid.org/0000-0002-9871-9448>
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
#' model_gp <-
#' '
#' m ~ c(a1, a2)*x
#' y ~ c(b1, b2)*m
#' a1b1 := a1*b1
#' a2b2 := a2*b2
#' '
#' dat$gp <- sample(c("gp1", "gp2"), n, replace = TRUE)
#' fit_gp <- sem(model_gp, dat, group = "gp", warn = FALSE)
#'
#' est <- parameterEstimates(fit)
#' est_gp <- parameterEstimates(fit_gp)
#'
#' filter_by(est, op = "~")
#'
#' filter_by(est, op = "~", lhs = "y")
#'
#' filter_by(est, rhs = c("m", "x"), op = "~")
#'
#' filter_by(est_gp, group = 2)
#'
#' # If the fit object is supplied, can filter
#' # by group label
#' filter_by(est_gp, group = "gp2", fit = fit_gp)
#' filter_by(est_gp, group = "gp2", fit = fit_gp, op = "~")
#'
#' # Select user-defined parameters
#' filter_by(est_gp, op = ":=")
#'
#' # Can be used with some other functions in semhelpinghands
#' # Piping can also be used
#' est_gp |> filter_by(op = "~", group = "gp2", fit = fit_gp) |>
#'           add_sig()
#'
#'
#' @export

filter_by <- function(object,
                      op = NULL,
                      lhs = NULL,
                      rhs = NULL,
                      group = NULL,
                      fit = NULL) {
    out <- object
    if (!is.null(op)) {
        i_op <- out$op %in% op
      } else {
        i_op <- TRUE
      }
    if (!is.null(lhs)) {
        i_lhs <- out$lhs %in% lhs
      } else {
        i_lhs <- TRUE
      }
    if (!is.null(rhs)) {
        i_rhs <- out$rhs %in% rhs
      } else {
        i_rhs <- TRUE
      }
    if (!is.null(group) && !is.null(out$group)) {
        i_gp <- out$group %in% group
        if (!any(i_gp) && is.character(group)) {
            if (missing(fit)) {
                stop("group is characater but fit is not supplied.")
              }
            gpl <- lavaan::lavInspect(fit, "group.label")
            if (length(gpl) > 0) {
                group_id <- which(gpl %in% group)
                i_gp <- out$group %in% group_id
              }
          }
      } else {
        i_gp <- TRUE
      }
    out <- out[i_op & i_lhs & i_rhs & i_gp, ]
    class(out) <- class(object)
    out
  }