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
    grouped <- attr(x, "grouped")
    group_first <- attr(x, "group_first")
    if (grouped) {
        out <- format_est_mat(x,
                              v_ind = attr(x, "v_ind"),
                              gp_ind = attr(x, "gp_ind"),
                              group_first = group_first,
                              nd = nd,
                              na_print = empty_cells)
      } else {
        out <- format_est_mat(x,
                              v_ind = attr(x, "v_ind"),
                              nd = nd,
                              na_print = empty_cells)
      }
    x <- out
    NextMethod()
  }
