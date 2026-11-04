#' Finding the Full Diagonal of a Matrix
#'
#' This function finds the full diagonal of a matrix. A full diagonal of a matrix is the diagonal of the matrix that is the line segment that starts at the entry in the first row and the first column of the matrix and that ends at the entry in the last row and the last column of the matrix.
#'
#' `Finding_the_Full_Diagonal_of_a_Matrix` finds the full diagonal of a matrix. A full diagonal of a matrix is the diagonal of the matrix that is the line segment that starts at the entry in the first row and the first column of the matrix and that ends at the entry in the last row and the last column of the matrix. If the numbers of rows and columns are coprime, the only two matrix entries that fall perfectly along the full diagonal are its two endpoints; when the numbers of rows and coliumns are not coprime, there will be more than two elements of the full diagonal. For square matrices, the [main diagonal](https://en.wikipedia.org/wiki/Main_diagonal) and the full diagonal are identical, but for non-square matrices, the main diagonal and the full diagonal are not identical.
#'
#' @param Matrix the matrix for which the full diagonal will be calculated.
#'
#' @return This function returns the vector of matrix entries that fall on the full diagonal of a matrix (`Full_Diagonal`), a matrix containing the distances of each entry to the full diagonal (`Distances_From_the_Full_Diagonal`), and the original matrix (`Original_Matrix`).
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' # An example where the number of columns is greater than the number of rows
#' Matrix_1 <- matrix(1:24, nrow = 4)
#' Finding_the_Full_Diagonal_of_a_Matrix(Matrix_1)
#' 
#' # An example where the number of rows is greater than the number of columns
#' Matrix_2 <- matrix(1:24, ncol = 4)
#' Finding_the_Full_Diagonal_of_a_Matrix(Matrix_2)
#' 
#' # A non-numeric example where the number of columns equals the number of rows
#' Matrix_3 <- matrix(LETTERS[1:16], ncol = 4)
#' Finding_the_Full_Diagonal_of_a_Matrix(Matrix_3)
#' 
#' # An example where the matrix only has one row
#' Matrix_4 <- matrix(letters, nrow = 1)
#' Finding_the_Full_Diagonal_of_a_Matrix(Matrix_4)
#' 
#' # An example where the matrix only has one column
#' Matrix_5 <- matrix(letters, ncol = 1)
#' Finding_the_Full_Diagonal_of_a_Matrix(Matrix_5)
#' 
#' # An example where the matrix has no rows and no columns
#' Matrix_6 <- matrix(NA, nrow = 0, ncol = 0)
#' Finding_the_Full_Diagonal_of_a_Matrix(Matrix_6)
#'
#' @export
Finding_the_Full_Diagonal_of_a_Matrix <- function (Matrix) {
  if (!is.matrix(Matrix)) {
    stop ("'Matrix' must be a matrix.")
  }
  if ((nrow(Matrix) == 0) | (ncol(Matrix) == 0)) {
    Full_Diagonal <- NULL
    Matrix_of_Distances_to_the_Full_Diagonal <- matrix(NA, nrow = 0, ncol = 0)
  } else if ((nrow(Matrix) == 1) | (ncol(Matrix) == 1)) {
    Full_Diagonal <- as.vector(Matrix)
    Matrix_of_Distances_to_the_Full_Diagonal <- matrix(rep(0, (nrow(Matrix) * ncol(Matrix))), nrow = nrow(Matrix), ncol = ncol(Matrix))
  } else if ((nrow(Matrix) > 1) & ncol(Matrix) > 1) {
    if (nrow(Matrix) <= ncol(Matrix)) {
      Rise_Over_Run <- (nrow(Matrix) - 1) / (ncol(Matrix) - 1)
      Possible_Combinations <- expand.grid(seq_len(nrow(Matrix)), seq_len(ncol(Matrix)))
      colnames(Possible_Combinations) <- c("Row_Number", "Column_Number")
      Indices <- sapply(seq_len(nrow(Possible_Combinations)), function (y) {
        (Possible_Combinations$Row_Number[y] - 1) == ((Possible_Combinations$Column_Number[y] - 1) * Rise_Over_Run)
      })
      Full_Diagonal_Positions <- Possible_Combinations[which(Indices), ]
      rownames(Full_Diagonal_Positions) <- NULL
      Full_Diagonal <- sapply(seq_len(nrow(Full_Diagonal_Positions)), function (y) {
        Matrix[Full_Diagonal_Positions$Row_Number[y], Full_Diagonal_Positions$Column_Number[y]]
      })
    } else if (nrow(Matrix) > ncol(Matrix)) {
      Run_Over_Rise <- (ncol(Matrix) - 1) / (nrow(Matrix) - 1)
      Possible_Combinations <- expand.grid(seq_len(nrow(Matrix)), seq_len(ncol(Matrix)))
      colnames(Possible_Combinations) <- c("Row_Number", "Column_Number")
      Indices <- sapply(seq_len(nrow(Possible_Combinations)), function (y) {
        ((Possible_Combinations$Row_Number[y] - 1) * Run_Over_Rise) == (Possible_Combinations$Column_Number[y] - 1)
      })
      Full_Diagonal_Positions <- Possible_Combinations[which(Indices), ]
      rownames(Full_Diagonal_Positions) <- NULL
      Full_Diagonal <- sapply(seq_len(nrow(Full_Diagonal_Positions)), function (y) {
        Matrix[Full_Diagonal_Positions$Row_Number[y], Full_Diagonal_Positions$Column_Number[y]]
      })
    }
    Slope <- -((nrow(Matrix) - 1) / (ncol(Matrix) - 1))
    Possible_Combinations$Distance_From_the_Full_Diagonal <- sapply(seq_len(nrow(Possible_Combinations)), function (i) {
      abs((Slope * (Possible_Combinations$Column_Number - 1)[i]) + ((-1) * ((-1) * (Possible_Combinations$Row_Number - 1))[i])) / sqrt(((Slope) ^ 2) + ((-1) ^ 2))
    })
    Matrix_of_Distances_to_the_Full_Diagonal <- matrix(NA, nrow = nrow(Matrix), ncol = ncol(Matrix))
    for (i in seq_len(nrow(Possible_Combinations))) {
      Matrix_of_Distances_to_the_Full_Diagonal[Possible_Combinations$Row_Number[i], Possible_Combinations$Column_Number[i]] <- Possible_Combinations$Distance_From_the_Full_Diagonal[i]
    }
  }
  list(Full_Diagonal = Full_Diagonal, Distances_From_the_Full_Diagonal = Matrix_of_Distances_to_the_Full_Diagonal, Original_Matrix = Matrix)
}