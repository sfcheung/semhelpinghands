check_lavaan_type <- function(x) {
  if (inherits(x, "lavaan")) return("lavaan")
  if (inherits(x, "lavaan.data.frame") && ("est" %in% colnames(x))) return("est")
  if (inherits(x, "lavaan.data.frame") && ("est.std" %in% colnames(x))) return("std")
  return(NA)
}

check_lavaan_type_list <- function(x) {
    out <- sapply(x, check_lavaan_type)
    out
  }

all_lavaan <- function(x) {
    all(check_lavaan_type(x) %in% "lavaan")
  }

all_est <- function(x) {
    all(check_lavaan_type(x) %in% "est")
  }

all_std <- function(x) {
    all(check_lavaan_type(x) %in% "std")
  }

all_type <- function(x) {
    out0 <- unique(check_lavaan_type_list(x))
    if (length(out0) != 1) return(NA)
    return(out0)
  }

is_grouped <- function(x) {
    output_type <- check_lavaan_type(x)
    if (output_type == "lavaan") {
        grouped <- lavaan::lavInspect(x, "ngroups") > 1
      } else {
        if (!("group" %in% colnames(x))) {
            grouped <- FALSE
          } else {
            if (length(unique(x$group)) > 1) {
                grouped <- TRUE
              } else {
                grouped = FALSE
              }
          }
      }
    grouped
  }