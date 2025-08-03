#' Checking Layout Matrix Viability
#'
#' This function determines if a potential layout matrix (for plotting) is viable by determining if any plot numbers are skipped and if all potential plotting regions are single, contiguous, perfect rectangles.
#'
#' `Checking_Layout_Matrix_Viability` determines if a potential layout matrix (for plotting) is viable by determining if all potential plotting regions are rectangular and contiguous.
#' @param Matrix the matrix whose viability as a layout matrix you wish to check.
#' @param Should_Skipping_Plot_Numbers_Be_Allowed whether layout matrices that skip plot numbers should be allowed. If skipping numbers is allowed, some plots will not appear in the final figure, but it could be intentional - perhaps a throwaway plot was made for some other purpose. The default to this argument is `FALSE`.
#'
#' @return This function returns a list. The first list element, entitled `"Numbering_Stuff"`, contains information pertaining to if the matrix ???
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' (Example_Matrix_1 <- matrix(c(1, 1, 1, 1, 2, 2, 3, 3, 4, 5, 6, 7), byrow = T, ncol = 4))
#' (Example_Matrix_2 <- matrix(c(1, 1, 1, 2, 0, 2, 3, 4, 5), byrow = T, ncol = 3))
#' (Example_Matrix_3 <- matrix(c(1, 1, 1, 1, 1, 2, 3, 3, 3, 4, 5, 6), byrow = T, ncol = 3))
#' (Example_Matrix_4 <- matrix(c(1, 1, 1, 1, 1, 1, 3, 3, 3, 4, 5, 6), byrow = T, ncol = 3))
#' Checking_Layout_Matrix_Viability(Example_Matrix_1)
#' Checking_Layout_Matrix_Viability(Example_Matrix_2)
#' Checking_Layout_Matrix_Viability(Example_Matrix_3)
#' Checking_Layout_Matrix_Viability(Example_Matrix_4)
#' Checking_Layout_Matrix_Viability(Example_Matrix_4, Should_Skipping_Plot_Numbers_Be_Allowed = TRUE)
#'
#' @export
Checking_Layout_Matrix_Viability <- function (Matrix, Should_Skipping_Plot_Numbers_Be_Allowed = FALSE) {
  if (is.null(Matrix)) {
    stop ("The 'Matrix' argument must be provided")
  }
  if (!is.matrix(Matrix)) {
    stop ("The 'Matrix' argument must be a matrix.")
  }
  if (!all((Matrix %% 1) == 0)) {
    stop ("The 'Matrix' argument must contain whole numbers.")
  }
  if (!all(Matrix >= 0)) {
    stop ("The 'Matrix' argument must contain only nonnegative numbers.")
  }
  if (any(is.na(Matrix))) {
    stop ("The 'Matrix' object must not contain any missing values.")
  }
  if (Should_Skipping_Plot_Numbers_Be_Allowed == T) {
    Are_All_Numbers_Present_in_the_Matrix <- NA
    Error_Message_1 <- NULL
  } else if (Should_Skipping_Plot_Numbers_Be_Allowed == F) {
    Layout_Matrix_Numbers <- unique(as.vector(Matrix))
    Missing_Numbers <- which(!(seq_len(max(as.vector(Matrix))) %in% Layout_Matrix_Numbers))
    if (length(Missing_Numbers) == 0) {
      Are_All_Numbers_Present_in_the_Matrix <- TRUE
      Error_Message_1 <- NULL
    } else if (length(Missing_Numbers) > 0) {
      Are_All_Numbers_Present_in_the_Matrix <- FALSE
      if (length(Missing_Numbers) == 1) {
        Error_Message_1 <- paste0("The number ", Missing_Numbers, " is missing.")
      } else if (length(Missing_Numbers) == 2) {
        Error_Message_1 <- paste0("The numbers ", Reduce(function (a, b) {
          paste0(a, " and ", b)
        }, Missing_Numbers), " are missing.")
      } else if (length(Missing_Numbers) > 2) {
        Error_Message_1 <- paste0("The numbers ", paste0(Reduce(function (a, b) {
          paste0(a, ", ", b, ", and ")
        }, Missing_Numbers[seq_len((length(Missing_Numbers) - 1))]), Missing_Numbers[length(Missing_Numbers)]), " are missing.")
      }
    }
  }
  Output <- !sapply(unique(as.vector(Matrix))[order(unique(as.vector(Matrix)))][which(unique(as.vector(Matrix))[order(unique(as.vector(Matrix)))] != 0)], function (i) {
    Row_Indices <- which(sapply(seq_len(nrow(Matrix)), function (x) {
      any(Matrix[x, ] == i)
    }))
    Column_Indices <- which(sapply(seq_len(ncol(Matrix)), function (y) {
      any(Matrix[, y] == i)
    }))
    all(Matrix[min(Row_Indices):max(Row_Indices), min(Column_Indices):max(Column_Indices)] == i)
  })
  Are_All_Numbers_Present_in_Single_Contiguous_Perfect_Rectangles <- !any(Output)
  Indices <- which(Output)
  if (length(Indices) == 0) {
    Error_Message_2 <- NULL
  } else if (length(Indices) == 1) {
    Error_Message_2 <- paste0("The number ", as.character(Indices), " does not appear in a single, contiguous, perfect rectangle.")
  } else if (length(Indices) == 2) {
    Error_Message_2 <- paste0("The numbers ", Reduce(function (a, b) {
      paste0(a, " and ", b)
    }, Indices), " do not appear in single, contiguous, perfect rectangles.")
  } else if (length(Indices) > 2) {
    Error_Message_2 <- paste0("The numbers ", paste0(Reduce(function (a, b) {
      paste0(a, ", ", b, ", and ")
    }, Indices[seq_len((length(Indices) - 1))]), Indices[length(Indices)]), " do not appear in single, contiguous, perfect rectangles.")
  }
  list(Numbering_Stuff = list(Are_All_Numbers_Present_in_the_Matrix = Are_All_Numbers_Present_in_the_Matrix, Error_Message = Error_Message_1), Rectangle_Stuff = list(Are_All_Numbers_Present_in_Single_Contiguous_Perfect_Rectangles = Are_All_Numbers_Present_in_Single_Contiguous_Perfect_Rectangles, Error_Message = Error_Message_2))
}
