#' @title Expand Reduced Parameters to Original Parameters
#'
#' @return
#' If the input is a vector, the output is a vector.
#' If the input is a matrix, the output is a matrix.
#'
#' @noRd

lav_par_expand <- function(x, fit) {
    is_vec <- is.vector(x)
    if (is_vec) {
        p <- length(x)
        x <- matrix(x, nrow = 1, ncol = p)
      } else {
        p <- ncol(x)
      }
    lav_model <- fit@Model
    if (lav_model@eq.constraints) {
        K <- lav_model@eq.constraints.K
        k0 <- lav_model@eq.constraints.k0
        p0 <- length(k0)
        xout <-  x %*% t(K) + matrix(k0, nrow(x), p0, byrow = TRUE)
      } else {
        xout <- x
      }
    if (is_vec) {
        return(as.vector(xout))
      } else {
        return(xout)
      }
  }
