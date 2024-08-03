#' Attempting to Diagonalize a Matrix Based on Its Full Diagonal
#'
#' This function performs row and column exchanges on a matrix and generates the matrix that is optimally diagonalized based on its full diagonal (not its main diagonal). The resulting matrix has its greatest values near the full diagonal and its least values as far as possible from the full diagonal. This function is useful for generating bipartite interaction matrix visualizations, such as ones created by the 'bipartite::plotweb()' function. In the case of these bipartite interaction matrix visualizations, attempting to diagonalizwe the matrix of interactions between the higher and lower trophic levels (based on the full, not the main, diagonal) will allow for figures with the least amount of crossing edges.
#'
#' `Attempting_to_Diagonalize_a_Matrix_Based_on_Its_Full_Diagonal` performs row and column exchanges on a matrix and generates the matrix that is optimally diagonalized based on its full diagonal (not its main diagonal). The resulting matrix has its greatest values near the full diagonal and its least values as far as possible from the full diagonal. This function is useful for generating bipartite interaction matrix visualizations, such as ones created by the 'bipartite::plotweb()' function. In the case of these bipartite interaction matrix visualizations, attempting to diagonalizwe the matrix of interactions between the higher and lower trophic levels (based on the full, not the main, diagonal) will allow for figures with the least amount of crossing edges.
#'
#' This function should be used with numeric matrices that only contain nonnegative entries and that do not contain any missing values.
#'
#' This function relies on another function from the 'DBM.functions' package - the 'Finding_the_Full_Diagonal_of_a_Matrix()' function - as well as the 'permutations()' function from the 'gtools' package.
#'
#' @param Matrix the matrix for which an attempt will be made to diagonalize based on the full (not the main) diagonal.
#'
#' @return This function returns the optimally ordered matrix which has been roughly diagonalized (`Optimally_Reordered_Matrix`), the original matrix (`Original_Matrix`), and a template matrix showing the relative proximities between each entry and the full diagonal, where larger values represent proximity to the full diagonal and smaller values represent distance from the full diagonal (`Template_Matrix`).
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' Matrix_10 <- matrix(rchisq(24, 5), ncol = 4)
#' Attempting_to_Diagonalize_a_Matrix_Based_on_Its_Full_Diagonal(Matrix_10)
#'
#' @export
Attempting_to_Diagonalize_a_Matrix_Based_on_Its_Full_Diagonal <- function (Matrix) {
  if (!is.matrix(Matrix)) {
    stop ("The 'Matrix' argument must be a matrix.")
  }
  if (!is.numeric(Matrix)) {
    stop ("The 'Matrix' argument must be numeric.")
  }
  if (any(is.na(Matrix))) {
    stop ("The matrix must not contain any missing values.")
  }
  if (is.null(rownames(Matrix))) {
    rownames(Matrix) <- as.character(seq_len(nrow(Matrix)))
  }
  if (is.null(colnames(Matrix))) {
    colnames(Matrix) <- as.character(seq_len(ncol(Matrix)))
  }
  Rescaled_Matrix <- Matrix / max(Matrix)
  Output <- DBM.functions::Finding_the_Full_Diagonal_of_a_Matrix(Matrix)
  Distances_From_the_Full_Diagonal <- Output$Distances_From_the_Full_Diagonal
  Template_Matrix <- 1 - (Distances_From_the_Full_Diagonal / max(Distances_From_the_Full_Diagonal))
  rownames(Template_Matrix) <- rownames(Matrix)
  colnames(Template_Matrix) <- colnames(Matrix)
  Row_Permutations <- gtools::permutations(n = nrow(Matrix), r = nrow(Matrix), v = rownames(Matrix))
  Row_Permutations <- sapply(seq_len(nrow(Row_Permutations)), function (x) {
    Reduce(function (y, z) {
      paste(y, z, sep = ", ")
    }, Row_Permutations[x, ])
  })
  Column_Permutations <- gtools::permutations(n = ncol(Matrix), r = ncol(Matrix), v = colnames(Matrix))
  Column_Permutations <- sapply(seq_len(nrow(Column_Permutations)), function (x) {
    Reduce(function (y, z) {
      paste(y, z, sep = ", ")
    }, Column_Permutations[x, ])
  })
  All_Possibilities <- expand.grid(Row_Permutations, Column_Permutations)
  colnames(All_Possibilities) <- c("Row_Names", "Column_Names")
  All_Possibilities <- lapply(All_Possibilities, as.character)
  Row_Orders <- as.list(All_Possibilities$Row_Names)
  Column_Orders <- as.list(All_Possibilities$Column_Names)
  Row_Orders <- lapply(Row_Orders, function (x) {
    unlist(strsplit(x, ", "))
  })
  Column_Orders <- lapply(Column_Orders, function (x) {
    unlist(strsplit(x, ", "))
  })
  Reordered_Original_Matrices <- mapply(function (x, y) {
    Matrix[match(x, rownames(Matrix)), match(y, colnames(Matrix))]
  }, x = Row_Orders, y = Column_Orders, SIMPLIFY = F)
  Scores <- mapply(function (x, y) {
    z <- Rescaled_Matrix[match(x, rownames(Rescaled_Matrix)), match(y, colnames(Rescaled_Matrix))]
    sum(abs(z - Template_Matrix))
  }, x = Row_Orders, y = Column_Orders)
  Optimally_Reordered_Matrix <- Reordered_Original_Matrices[[which.min(Scores)]]
  list(Optimally_Reordered_Matrix = Optimally_Reordered_Matrix, Original_Matrix = Matrix, Template_Matrix = Template_Matrix)
}