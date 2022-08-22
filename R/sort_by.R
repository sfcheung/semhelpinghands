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
#' is `c("op", "lhs", "rhs")`.
#'
#' @param op_priority How rows are
#' sorted by `op`. Default is `c("=~",
#' "~", "~~", ":=", "~1", "|", "~*~")`.
#' Can set only a few of the operators,
#' e.g., `c("~", "~~")`. Other operators
#' will be placed to the end with orders
#' not changed.
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
#' library(lavaan)
#' set.seed(5478374)
#' n <- 50
#' x <- runif(n) - .5
#' m <- .40 * x + rnorm(n, 0, sqrt(1 - .40))
#' y <- .30 * m + rnorm(n, 0, sqrt(1 - .30))
#' dat <- data.frame(x = x, y = y, m = m)
#' model1 <-
#' '
#' m ~ a*x
#' y ~ b*m
#' ab := a*b
#' '
#' fit1 <- sem(model1, data = dat, fixed.x = FALSE)
#' model2 <-
#' '
#' m ~ a*x
#' y ~ b*m + x
#' ab := a*b
#' '
#' fit2 <- sem(model2, data = dat, fixed.x = FALSE)
#' parameterEstimates(fit1)
#' parameterEstimates(fit2)
#' out <- group_by_models(list(no_direct = fit1,
#'                             direct = fit2),
#'                         col_names = c("est", "pvalue"))
#' out
#' sort_by(out)
#' sort_by(out, op_priority = c("~", ":="))
#' sort_by(out, by = c("op", "rhs"))
#'
#'
#' @export

sort_by <- function(object,
                    by = c("op", "lhs", "rhs"),
                    op_priority = c("=~", "~", "~~", ":=", "~1", "|", "~*~"),
                    number_rows = TRUE) {
    op_priority <- match.arg(op_priority, several.ok = TRUE)
    grouped <- "group" %in% colnames(object)
    op_priority0 <- match(object$op, op_priority, nomatch = 999)
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