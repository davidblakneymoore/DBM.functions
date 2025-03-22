#' Making Replicable Layout Matrices for Complicated Figures
#'
#' This function is to be used when creating a plotting layout matrix where each plot in this layout matrix is itself comprised of multiple plots that were themselves created using a layout matrix. In other words, it's used when a figure comprised of multiple plots is itself to be replicated to create a more complex figure.
#'
#' `Making_Replicable_Layout_Matrices_for_Complicated_Figures` is to be used when creating a plotting layout matrix where each plot in this layout matrix is itself comprised of multiple plots that were themselves created using a layout matrix. In other words, it's used when a figure comprised of multiple plots is itself to be replicated to create a more complex figure.
#' @param Individual_Layout_Matrix the layout matrix for each individual figure that is to be replicated.
#' @param Overall_Layout_Matrix the layout matrix depicting how each individual figure is to be replicated.
#'
#' @return This function returns a layout matrix that replicated smaller layout matrices to allow for complex figures to be plotted.
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' Individual_Layout_Matrix <- matrix(c(1, 1, 2, 3), byrow = T, ncol = 2)
#' Overall_Layout_Matrix <- matrix(c(0, 1, 1, 0, 2, 2, 3, 3), byrow = T, ncol = 4)
#' (Layout_Matrix <- Making_Replicable_Layout_Matrices_for_Complicated_Figures(Individual_Layout_Matrix, Overall_Layout_Matrix))
#' layout(Layout_Matrix, heights = rep(c(0.15, 0.85), 2))
#' lapply(unique(iris$Species), function (x) {
#'   par(mar = c(1, 1, 3, 1))
#'   plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
#'   text(0, 0, paste0("Species: ", as.character(x)), cex = 2.5, xpd = T)
#'   par(mar = c(5, 4, 4, 2) + 0.1)
#'   plot(Sepal.Width ~ Sepal.Length, iris[which(iris$Species == x), ], xlab = "Sepal Length", ylab = "Sepal Width")
#'   plot(Petal.Width ~ Petal.Length, iris[which(iris$Species == x), ], xlab = "Petal Length", ylab = "Petal Width")
#' })
#'
#' @export
Making_Replicable_Layout_Matrices_for_Complicated_Figures <- function(Individual_Layout_Matrix, Overall_Layout_Matrix) {
  if (is.null(Individual_Layout_Matrix)) {
    stop ("The 'Individual_Layout_Matrix' argument must be provided")
  }
  if (!is.matrix(Individual_Layout_Matrix)) {
    stop ("The 'Individual_Layout_Matrix' argument must be a matrix.")
  }
  if (!all((Individual_Layout_Matrix %% 1) == 0)) {
    stop ("The 'Individual_Layout_Matrix' argument must contain whole numbers.")
  }
  if (!all(Individual_Layout_Matrix >= 0)) {
    stop ("The 'Individual_Layout_Matrix' argument must contain only nonnegative numbers.")
  }
  if (any(is.na(Individual_Layout_Matrix))) {
    stop ("The 'Individual_Layout_Matrix' object must not contain any missing values.")
  }
  if (is.null(Overall_Layout_Matrix)) {
    stop ("The 'Overall_Layout_Matrix' argument must be provided")
  }
  if (!is.matrix(Overall_Layout_Matrix)) {
    stop ("The 'Overall_Layout_Matrix' argument must be a matrix.")
  }
  if (!all((Overall_Layout_Matrix %% 1) == 0)) {
    stop ("The 'Overall_Layout_Matrix' argument must contain whole numbers.")
  }
  if (!all(Overall_Layout_Matrix >= 0)) {
    stop ("The 'Overall_Layout_Matrix' argument must contain only nonnegative numbers.")
  }
  if (any(is.na(Overall_Layout_Matrix))) {
    stop ("The 'Overall_Layout_Matrix' object must not contain any missing values.")
  }
  Individual_Layout_Matrix_Viability <- Checking_Layout_Matrix_Viability(Individual_Layout_Matrix)
  Overall_Layout_Matrix_Viability <- Checking_Layout_Matrix_Viability(Overall_Layout_Matrix)
  if (is.na(Individual_Layout_Matrix_Viability$Numbering_Stuff$Are_All_Numbers_Present_in_the_Matrix)) {
    if (Individual_Layout_Matrix_Viability$Numbering_Stuff$Are_All_Numbers_Present_in_the_Matrix == F) {
      stop (paste0("The 'Individual_Layout_Matrix' matrix is not viable: ", gsub("^t", "T", Individual_Layout_Matrix_Viability$Numbering_Stuff$Error_Message)))
    }
  }
  if (is.na(Overall_Layout_Matrix_Viability$Numbering_Stuff$Are_All_Numbers_Present_in_the_Matrix)) {
    if (Overall_Layout_Matrix_Viability$Numbering_Stuff$Are_All_Numbers_Present_in_the_Matrix == F) {
      stop (paste0("The 'Overall_Layout_Matrix' matrix is not viable: ", gsub("^t", "T", Overall_Layout_Matrix_Viability$Numbering_Stuff$Error_Message)))
    }
  }
  if (Individual_Layout_Matrix_Viability$Rectangle_Stuff$Are_All_Numbers_Present_in_Single_Contiguous_Perfect_Rectangles == F) {
    stop (paste0("The 'Individual_Layout_Matrix' matrix is not viable: ", gsub("^t", "T", Individual_Layout_Matrix_Viability$Rectangle_Stuff$Error_Message)))
  }
  if (Overall_Layout_Matrix_Viability$Rectangle_Stuff$Are_All_Numbers_Present_in_Single_Contiguous_Perfect_Rectangles == F) {
    stop (paste0("The 'Overall_Layout_Matrix' matrix is not viable: ", gsub("^t", "T", Overall_Layout_Matrix_Viability$Rectangle_Stuff$Error_Message)))
  }
  Individual_Layout_Matrix_Dimensions <- dim(Individual_Layout_Matrix)
  Number_of_Plots_in_Each_Individual_Layout_Matrix <- max(Individual_Layout_Matrix)
  Final_Matrix <- Intermediate_Matrix <- kronecker(Overall_Layout_Matrix, array(1, Individual_Layout_Matrix_Dimensions))
  for (i in unique(Overall_Layout_Matrix[Overall_Layout_Matrix != 0])) {
    Indices <- which(Intermediate_Matrix == i, arr.ind = TRUE)
    Dimensions_1 <- apply(Indices, 2, function (x) {
      diff(range(x)) + 1
    })
    Dimensions_2 <- Dimensions_1 / Individual_Layout_Matrix_Dimensions
    New_Individual_Layout_Matrix <- Individual_Layout_Matrix[rep(seq_len(Individual_Layout_Matrix_Dimensions[1]), each = Dimensions_2[1]), rep(seq_len(Individual_Layout_Matrix_Dimensions[2]), each = Dimensions_2[2])] + ((i - 1) * Number_of_Plots_in_Each_Individual_Layout_Matrix)
    Final_Matrix[Indices] <- New_Individual_Layout_Matrix
  }
  Final_Matrix
}
