#' @title Print an 'est_table' Object
#'
#' @description Print method for an 'est_table' object
#'
#' @param x Object of the class `est_table`.
#' @param ... Optional arguments to be passed to [print()] methods.
#' @param nd Number of digits to be printed. Default is 3.
#'           (Scientific
#'           notation will never be used.)
#' @param empty_cells String to be printed for empty cells
#'                    or cells with no values. Default is `"--"`.
#' @param group_first If the object has a column for groups,
#'                    whether group will be printed before
#'                    the row variables. Default is `FALSE`.
#'
#' @export

print.est_table <- function(x,
                            ...,
                            nd = 3,
                            empty_cells = "--",
                            group_first = FALSE) {
    if (is.null(attr(x, "v_ind"))) {
        class(x) <- class(x)[which(class(x) != "est_table")]
        NextMethod()
        return(invisible(x))
      }
    grouped <- attr(x, "grouped")
    group_first <- attr(x, "group_first")
    by_ivs <- attr(x, "by_iv")
    if (isTRUE(grouped) & !isTRUE(by_ivs)) {
        out <- format_est_mat(x,
                              v_ind = attr(x, "v_ind"),
                              gp_ind = attr(x, "gp_ind"),
                              group_first = group_first,
                              nd = nd,
                              na_print = empty_cells)
      } else {
        out <- format_est_mat(x,
                              v_ind = 1,
                              nd = nd,
                              na_print = empty_cells)
      }
    x <- out
    NextMethod()
    return(invisible(x))
  }
