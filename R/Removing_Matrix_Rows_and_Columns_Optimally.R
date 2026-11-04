#' Removing Matrix Rows and Columns Optimally
#'
#' This function removes missing or non-finite values from matrices by means of row and column deletions. These deletions are performed in a way that ensures that the maximum number of good (non-missing or finite) values are retained in the resulting matrix.
#'
#' `Removing_Matrix_Rows_and_Columns_Optimally` removes missing or non-finite values from matrices by means of row and column deletions. These deletions are performed in a way that ensures that the maximum number of good (non-missing or finite) values are retained in the resulting matrix.
#'
#' @param Matrix a matrix from which missing or non-finite values are to be removed.
#' @param Values_to_Remove one of either "Missing Values", "Non-Finite Values", or "Both Missing and Non-Finite Values"; indicates which types of values should be removed. For non-numeric matrices, only "Missing Values" may be provided.
#'
#' @return This function returns a matrix whose missing values or non-finite values have been removed by means of row and column deletion.
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' # Example 1 - a numeric matrix with both missing and non-finite values
#' set.seed(412)
#' Matrix_1 <- matrix(1:216, nrow = 12)
#' rownames(Matrix_1) <- paste(LETTERS[seq_len(nrow(Matrix_1))])
#' colnames(Matrix_1) <- paste(letters[seq_len(ncol(Matrix_1))])
#' Proportion_of_Missing_Values <- 0.2
#' for (i in seq_len(round(Proportion_of_Missing_Values * nrow(Matrix_1) * ncol(Matrix_1)))) {
#'   Matrix_1[sample(seq_len(nrow(Matrix_1)), 1), sample(seq_len(ncol(Matrix_1)), 1)] <- sample(c(NA, Inf, -Inf), 1, T, c(0.5, 0.25, 0.25))
#' }
#' Matrix_1
#' (Output_1 <- Removing_Matrix_Rows_and_Columns_Optimally(Matrix_1))
#' 
#' # Example 2 - a matrix of character strings with missing values
#' set.seed(410)
#' Matrix_2 <- matrix(LETTERS[1:24], ncol = 3)
#' for (i in seq_len(round(Proportion_of_Missing_Values * nrow(Matrix_2) * ncol(Matrix_2)))) {
#'   Matrix_2[sample(seq_len(nrow(Matrix_2)), 1), sample(seq_len(ncol(Matrix_2)), 1)] <- NA
#' }
#' rownames(Matrix_2) <- paste("Row", seq_len(nrow(Matrix_2)), sep = "_")
#' colnames(Matrix_2) <- paste("Column", seq_len(ncol(Matrix_2)), sep = "_")
#' Matrix_2
#' (Output_2 <- Removing_Matrix_Rows_and_Columns_Optimally(Matrix_2))
#'
#' @export
Removing_Matrix_Rows_and_Columns_Optimally <- function (Matrix, Values_to_Remove = "Missing Values") {
  if (!is.matrix(Matrix)) {
    stop ("The 'Matrix' argument must be a matrix.")
  }
  if (is.null(rownames(Matrix))) {
    stop ("Please make sure the matrix has row names; otherwise, it may be difficult to identify the rows in the output matrix.")
  }
  if (is.null(colnames(Matrix))) {
    stop ("Please make sure the matrix has column names; otherwise, it may be difficult to identify the columns in the output matrix.")
  }
  if (length(Values_to_Remove) != 1) {
    stop ("The 'Values_to_Remove' argument must be of length 1.")
  }
  if (!is.character(Values_to_Remove)) {
    stop ("The 'Values_to_Remove' argument must be a character string.")
  }
  if (!(Values_to_Remove %in% c("Missing Values", "Non-Finite Values", "Both Missing and Non-Finite Values"))) {
    stop ("The 'Values_to_Remove' argument must be either 'Missing Values', 'Non-Finite Values', or 'Both Missing and Non-Finite Values'.")
  }
  if (Values_to_Remove != "Missing Values") {
    if (!is.numeric(Matrix)) {
      stop ("For non-numeric matrices, only missing values can be removed - there will be no non-finite values. Therefore, please specify 'Missing Values' for the 'Values_to_Remove' argument.")
    }
  }
  if (Values_to_Remove == "Missing Values") {
    if (!any(is.na(Matrix))) {
      return (list(Matrix))
    } else if (any(is.na(Matrix))) {
      Proportion_of_Bad_Values_in_Each_Row <- apply(Matrix, 1, function (x) {
        length(which(is.na(x))) / ncol(Matrix)
      })
      Maximum_Proportion_of_Bad_Values_in_Each_Row <- max(Proportion_of_Bad_Values_in_Each_Row)
      Proportion_of_Bad_Values_in_Each_Column <- apply(Matrix, 2, function (x) {
        length(which(is.na(x))) / nrow(Matrix)
      })
      Maximum_Proportion_of_Bad_Values_in_Each_Column <- max(Proportion_of_Bad_Values_in_Each_Column)
      if (Maximum_Proportion_of_Bad_Values_in_Each_Row > Maximum_Proportion_of_Bad_Values_in_Each_Column) {
        List_of_Possible_Matrices <- setNames(lapply(which(Proportion_of_Bad_Values_in_Each_Row == Maximum_Proportion_of_Bad_Values_in_Each_Row), function (x) {
          Matrix[-c(x), , drop = F]
        }), NULL)
      } else if (Maximum_Proportion_of_Bad_Values_in_Each_Row < Maximum_Proportion_of_Bad_Values_in_Each_Column) {
        List_of_Possible_Matrices <- setNames(lapply(which(Proportion_of_Bad_Values_in_Each_Column == Maximum_Proportion_of_Bad_Values_in_Each_Column), function (x) {
          Matrix[, -c(x), drop = F]
        }), NULL)
      } else if (Maximum_Proportion_of_Bad_Values_in_Each_Row == Maximum_Proportion_of_Bad_Values_in_Each_Column) {
        List_of_Possible_Matrices_1 <- setNames(lapply(which(Proportion_of_Bad_Values_in_Each_Row == Maximum_Proportion_of_Bad_Values_in_Each_Row), function (x) {
          Matrix[-c(x), , drop = F]
        }), NULL)
        List_of_Possible_Matrices_2 <- setNames(lapply(which(Proportion_of_Bad_Values_in_Each_Column == Maximum_Proportion_of_Bad_Values_in_Each_Column), function (x) {
          Matrix[, -c(x), drop = F]
        }), NULL)
        List_of_Possible_Matrices <- c(List_of_Possible_Matrices_1, List_of_Possible_Matrices_2)
      }
      if (any(duplicated(List_of_Possible_Matrices))) {
        List_of_Possible_Matrices <- List_of_Possible_Matrices[-c(which(duplicated(List_of_Possible_Matrices)))]
      }
      if (any(!sapply(List_of_Possible_Matrices, function (x) {
        any(is.na(x))
      }))
      ) {
        List_of_Possible_Matrices <- List_of_Possible_Matrices[which(!sapply(List_of_Possible_Matrices, function (x) {
          any(is.na(x))
        }))]
        Numbers_of_Entries <- sapply(List_of_Possible_Matrices, function (x) {
          nrow(x) * ncol(x)
        })
        List_of_Possible_Matrices <- List_of_Possible_Matrices[which(Numbers_of_Entries == max(Numbers_of_Entries))]
        List_of_Possible_Matrices <- List_of_Possible_Matrices[which(!duplicated(List_of_Possible_Matrices))]
        return (List_of_Possible_Matrices)
      } else if (all(sapply(List_of_Possible_Matrices, function (x) {
        any(is.na(x))
      }))
      ) {
        List_of_Possible_Matrices <- unlist(lapply(List_of_Possible_Matrices, Removing_Matrix_Rows_and_Columns_Optimally), recursive = F)
        Numbers_of_Entries <- sapply(List_of_Possible_Matrices, function (x) {
          nrow(x) * ncol(x)
        })
        List_of_Possible_Matrices <- List_of_Possible_Matrices[which(Numbers_of_Entries == max(Numbers_of_Entries))]
        List_of_Possible_Matrices[which(!duplicated(List_of_Possible_Matrices))]
      }
    }
  } else if (Values_to_Remove == "Non-Finite Values") {
    if (!any(!is.finite(Matrix))) {
      return (list(Matrix))
    } else if (any(!is.finite(Matrix))) {
      Proportion_of_Bad_Values_in_Each_Row <- apply(Matrix, 1, function (x) {
        length(which(!is.finite(x))) / ncol(Matrix)
      })
      Maximum_Proportion_of_Bad_Values_in_Each_Row <- max(Proportion_of_Bad_Values_in_Each_Row)
      Proportion_of_Bad_Values_in_Each_Column <- apply(Matrix, 2, function (x) {
        length(which(!is.finite(x))) / nrow(Matrix)
      })
      Maximum_Proportion_of_Bad_Values_in_Each_Column <- max(Proportion_of_Bad_Values_in_Each_Column)
      if (Maximum_Proportion_of_Bad_Values_in_Each_Row > Maximum_Proportion_of_Bad_Values_in_Each_Column) {
        List_of_Possible_Matrices <- setNames(lapply(which(Proportion_of_Bad_Values_in_Each_Row == Maximum_Proportion_of_Bad_Values_in_Each_Row), function (x) {
          Matrix[-c(x), , drop = F]
        }), NULL)
      } else if (Maximum_Proportion_of_Bad_Values_in_Each_Row < Maximum_Proportion_of_Bad_Values_in_Each_Column) {
        List_of_Possible_Matrices <- setNames(lapply(which(Proportion_of_Bad_Values_in_Each_Column == Maximum_Proportion_of_Bad_Values_in_Each_Column), function (x) {
          Matrix[, -c(x), drop = F]
        }), NULL)
      } else if (Maximum_Proportion_of_Bad_Values_in_Each_Row == Maximum_Proportion_of_Bad_Values_in_Each_Column) {
        List_of_Possible_Matrices_1 <- setNames(lapply(which(Proportion_of_Bad_Values_in_Each_Row == Maximum_Proportion_of_Bad_Values_in_Each_Row), function (x) {
          Matrix[-c(x), , drop = F]
        }), NULL)
        List_of_Possible_Matrices_2 <- setNames(lapply(which(Proportion_of_Bad_Values_in_Each_Column == Maximum_Proportion_of_Bad_Values_in_Each_Column), function (x) {
          Matrix[, -c(x), drop = F]
        }), NULL)
        List_of_Possible_Matrices <- c(List_of_Possible_Matrices_1, List_of_Possible_Matrices_2)
      }
      if (any(duplicated(List_of_Possible_Matrices))) {
        List_of_Possible_Matrices <- List_of_Possible_Matrices[-c(which(duplicated(List_of_Possible_Matrices)))]
      }
      if (any(!sapply(List_of_Possible_Matrices, function (x) {
        any(!is.finite(x))
      }))
      ) {
        List_of_Possible_Matrices <- List_of_Possible_Matrices[which(!sapply(List_of_Possible_Matrices, function (x) {
          any(is.na(x))
        }))]
        Numbers_of_Entries <- sapply(List_of_Possible_Matrices, function (x) {
          nrow(x) * ncol(x)
        })
        List_of_Possible_Matrices <- List_of_Possible_Matrices[which(Numbers_of_Entries == max(Numbers_of_Entries))]
        List_of_Possible_Matrices <- List_of_Possible_Matrices[which(!duplicated(List_of_Possible_Matrices))]
        return (List_of_Possible_Matrices)
      } else if (all(sapply(List_of_Possible_Matrices, function (x) {
        any(!is.finite(x))
      }))
      ) {
        List_of_Possible_Matrices <- unlist(lapply(List_of_Possible_Matrices, Removing_Matrix_Rows_and_Columns_Optimally), recursive = F)
        Numbers_of_Entries <- sapply(List_of_Possible_Matrices, function (x) {
          nrow(x) * ncol(x)
        })
        List_of_Possible_Matrices <- List_of_Possible_Matrices[which(Numbers_of_Entries == max(Numbers_of_Entries))]
        List_of_Possible_Matrices[which(!duplicated(List_of_Possible_Matrices))]
      }
    }
  } else if (Values_to_Remove == "Both Missing and Non-Finite Values") {
    if (!any(is.na(Matrix)) | !any(!is.finite(Matrix))) {
      return (list(Matrix))
    } else if (any(is.na(Matrix)) | any(!is.finite(Matrix))) {
      Proportion_of_Bad_Values_in_Each_Row <- apply(Matrix, 1, function (x) {
        length(which(is.na(x) | !is.finite(x))) / ncol(Matrix)
      })
      Maximum_Proportion_of_Bad_Values_in_Each_Row <- max(Proportion_of_Bad_Values_in_Each_Row)
      Proportion_of_Bad_Values_in_Each_Column <- apply(Matrix, 2, function (x) {
        length(which(is.na(x) | !is.finite(x))) / nrow(Matrix)
      })
      Maximum_Proportion_of_Bad_Values_in_Each_Column <- max(Proportion_of_Bad_Values_in_Each_Column)
      if (Maximum_Proportion_of_Bad_Values_in_Each_Row > Maximum_Proportion_of_Bad_Values_in_Each_Column) {
        List_of_Possible_Matrices <- setNames(lapply(which(Proportion_of_Bad_Values_in_Each_Row == Maximum_Proportion_of_Bad_Values_in_Each_Row), function (x) {
          Matrix[-c(x), , drop = F]
        }), NULL)
      } else if (Maximum_Proportion_of_Bad_Values_in_Each_Row < Maximum_Proportion_of_Bad_Values_in_Each_Column) {
        List_of_Possible_Matrices <- setNames(lapply(which(Proportion_of_Bad_Values_in_Each_Column == Maximum_Proportion_of_Bad_Values_in_Each_Column), function (x) {
          Matrix[, -c(x), drop = F]
        }), NULL)
      } else if (Maximum_Proportion_of_Bad_Values_in_Each_Row == Maximum_Proportion_of_Bad_Values_in_Each_Column) {
        List_of_Possible_Matrices_1 <- setNames(lapply(which(Proportion_of_Bad_Values_in_Each_Row == Maximum_Proportion_of_Bad_Values_in_Each_Row), function (x) {
          Matrix[-c(x), , drop = F]
        }), NULL)
        List_of_Possible_Matrices_2 <- setNames(lapply(which(Proportion_of_Bad_Values_in_Each_Column == Maximum_Proportion_of_Bad_Values_in_Each_Column), function (x) {
          Matrix[, -c(x), drop = F]
        }), NULL)
        List_of_Possible_Matrices <- c(List_of_Possible_Matrices_1, List_of_Possible_Matrices_2)
      }
      if (any(duplicated(List_of_Possible_Matrices))) {
        List_of_Possible_Matrices <- List_of_Possible_Matrices[-c(which(duplicated(List_of_Possible_Matrices)))]
      }
      if (any(!sapply(List_of_Possible_Matrices, function (x) {
        any(is.na(x) | !is.finite(x))
      }))
      ) {
        List_of_Possible_Matrices <- List_of_Possible_Matrices[which(!sapply(List_of_Possible_Matrices, function (x) {
          any(is.na(x) | !is.finite(x))
        }))]
        Numbers_of_Entries <- sapply(List_of_Possible_Matrices, function (x) {
          nrow(x) * ncol(x)
        })
        List_of_Possible_Matrices <- List_of_Possible_Matrices[which(Numbers_of_Entries == max(Numbers_of_Entries))]
        List_of_Possible_Matrices <- List_of_Possible_Matrices[which(!duplicated(List_of_Possible_Matrices))]
        return (List_of_Possible_Matrices)
      } else if (all(sapply(List_of_Possible_Matrices, function (x) {
        any(is.na(x) | !is.finite(x))
      }))
      ) {
        List_of_Possible_Matrices <- unlist(lapply(List_of_Possible_Matrices, Removing_Matrix_Rows_and_Columns_Optimally), recursive = F)
        Numbers_of_Entries <- sapply(List_of_Possible_Matrices, function (x) {
          nrow(x) * ncol(x)
        })
        List_of_Possible_Matrices <- List_of_Possible_Matrices[which(Numbers_of_Entries == max(Numbers_of_Entries))]
        List_of_Possible_Matrices[which(!duplicated(List_of_Possible_Matrices))]
      }
    }
  }
}