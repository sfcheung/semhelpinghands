#' @title Sort a Parameter Estimates
#' Table
#'
#' @description Sort a parameter
#' estimates table or a similar table
#' in`lavaan` by common fields such as
#' `op` (operator) and `lhs` (left-
#' hand side).
#'
#' @details This functions accepts the
#'  output of
#'  [lavaan::parameterEstimates()] and
#'  [lavaan::standardizedSolution()] and
#'  filter the rows by commonly used
#'  field.
#'
#' @return The sorted version of the
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
#' @param by A character vector of the
#' columns for filtering. Default
#' is c("op", "lhs").
#'
#' @param op_priority How rows are
#' sorted by `op`. Default is `c("=~",
#' "~", "~~", ":=", "~1", "|", "~*~")`.
#'
#' @param number_rows Whether the row
#' names will be set to row numbers
#' after sorting *if* the row names
#' of `object` is equal to row numbers.
#' Default is `TRUE`.
#'
#' @author Shu Fai Cheung
#' <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' # TODO: Update
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

sort_by <- function(object,
                    by = c("op", "lhs", "rhs"),
                    op_priority = c("=~", "~", "~~", ":=", "~1", "|", "~*~"),
                    number_rows = TRUE) {
    op_priority <- match.arg(op_priority, several.ok = TRUE)
    grouped <- "group" %in% colnames(object)
    op_priority0 <- match(object$op, op_priority, nomatch = -999)
    by0 <- by
    tmp <- object
    tmp[, "op"] <- op_priority0
    if (grouped) {
        if (is.unsorted(object$group)) {
            tmp$group <- match(tmp$group, unique(object$group))
            tmp2 <- sapply(c(by, "group"), function(x) tmp[, x],
                          simplify = FALSE)
          } else {
            tmp$group <- match(tmp$group, unique(object$group))
            tmp2 <- sapply(c("group", by), function(x) tmp[, x],
                          simplify = FALSE)
          }
      } else {
        tmp2 <- sapply(by, function(x) tmp[, x],
                       simplify = FALSE)
      }
    out_order <- do.call(order, c(tmp2))
    out <- object[out_order, ]
    if (!is.unsorted(as.numeric(rownames(object))) && number_rows) {
        rownames(out) <- seq_len(nrow(out))
      }
    class(out) <- class(object)
    out
  }