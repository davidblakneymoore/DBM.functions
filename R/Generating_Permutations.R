#' Generating Permutations
#'
#' This function generates all possible permutations for a given number of items.
#'
#' `Generating_Permutations` generates all possible permutations for a given number of items.
#'
#' @param Number_of_Items a positive integer for which permutations will be calculated.
#'
#' @return This function returns a list of all the possible permutations of a given number of items.
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' Generating_Permutations(0)
#' Generating_Permutations(1)
#' Generating_Permutations(4)
#'
#' @export
Generating_Permutations <- function (Number_of_Items) {
  if (length(Number_of_Items) != 1) {
    stop ("The 'Number_of_Items' argument must be of length 1.")
  }
  if (is.na(Number_of_Items)) {
    stop ("The 'Number_of_Items' argument must not be missing.")
  }
  if (!is.numeric(Number_of_Items)) {
    stop ("The 'Number_of_Items' argument must be numeric.")
  }
  if ((Number_of_Items %% 1) != 0) {
    stop ("The 'Number_of_Items' argument must be a whole number.")
  }
  if (Number_of_Items < 1) {
    stop ("The 'Number_of_Items' argument must be positive.")
  }
  if (Number_of_Items == 1) {
    Final_Output <- as.list(1)
  } else if (Number_of_Items > 1) {
    w <- lapply(seq_len(Number_of_Items), function (x) {
      u <- x
      v <- seq_len(Number_of_Items)[which(!(seq_len(Number_of_Items) %in% u))]
      list(u, v)
    })
    if (unique(sapply(w, function (x) {
      length(x[[2]])
    })) == 1) {
      Final_Output <- lapply(w, function (x) {
        c(x[[1]], x[[2]])
      })
    } else if (unique(sapply(w, function (x) {
      length(x[[2]])
    })) > 1) {
      Recursive_Function <- function (z) {
        w <- unlist(lapply(z, function (x) {
          lapply(x[[2]], function (y) {
            u <- c(x[[1]], y)
            v <- seq_len(Number_of_Items)[which(!(seq_len(Number_of_Items) %in% u))]
            list(u, v)
          })
        }), recursive = F)
        if (unique(sapply(w, function (x) {
          length(x[[2]])
        })) == 1) {
          Final_Output <- lapply(w, function (x) {
            c(x[[1]], x[[2]])
          })
        } else if (unique(sapply(w, function (x) {
          length(x[[2]])
        })) > 1) {
          Final_Output <- Recursive_Function(w)
        }
        Final_Output
      }
      Final_Output <- Recursive_Function(w)
    }
  }
  Final_Output
}