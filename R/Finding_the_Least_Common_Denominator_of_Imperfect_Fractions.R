#' Finding the Least Common Denominator of Imperfect Fractions
#'
#' This function takes, as its input, a vector of imperfect fractions and returns a number which, when multiplied by each element of the input vector, returns a vector of integers. This function was originally developed to turn vectors of heights and widths for a plot layout matrix into a layout matrix where each entry in the layout matrix represents a region in the plot layout matrix of equal size (in other words, it can be used to create a plot layout matrix for which heights and widths are not required because they are implied by the plot layout matrix). (It was specifically developed with this use in mind for the `Generating_Figures_Containing_Lower_Trophic_Level_Phenology_Plots_a_Bipartite_Interaction_Matrix_Visualization_Plot_and_Upper_Trophic_Level_Phenology_Plots` function in the `DBM.functions` package.) The examples provide some clarification.
#'
#' `Finding_the_Least_Common_Denominator_of_Imperfect_Fractions` takes, as its input, a vector of imperfect fractions and returns a number which, when multiplied by each element of the input vector, returns a vector of integers. This function was originally developed to turn vectors of heights and widths for a plot layout matrix into a layout matrix where each entry in the layout matrix represents a region in the plot layout matrix of equal size (in other words, it can be used to create a plot layout matrix for which heights and widths are not required because they are implied by the plot layout matrix). (It was specifically developed with this use in mind for the `Making_Replicable_Layout_Matrices_for_Complicated_Figures` and `Generating_Figures_Containing_Lower_Trophic_Level_Phenology_Plots_a_Bipartite_Interaction_Matrix_Visualization_Plot_and_Upper_Trophic_Level_Phenology_Plots` functions in the `DBM.functions` package.) The examples provide some clarification.
#' @param Vector the vector of imperfect fractions you wish to turn into integers (using the same multiplier - the output of this function - for each vector element).
#'
#' @return This function returns a number that, when multiplied by each element of the input vector, returns a vector of integers.
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' # Example 1: A Simple Example
#' (Practice <- c(0.1, 0.9, 0.5, (1 / 3), 0.5344, (2 / 7)))
#' (Function_Output <- Finding_the_Least_Common_Denominator_of_Imperfect_Fractions(Practice))
#' Function_Output * Practice
#'
#' # Example 2: Generating a Plot Layout Matrix
#' Heights <- c(0.075, 0.925)
#' Widths <- rep((1 / 3), 3)
#' (Original_Plot_Layout_Matrix <- matrix(c(1, 1, 1, 2, 3, 4), byrow = T, ncol = 3))
#' (New_Heights <- Finding_the_Least_Common_Denominator_of_Imperfect_Fractions(Heights) * Heights)
#' (New_Widths <- Finding_the_Least_Common_Denominator_of_Imperfect_Fractions(Widths) * Widths)
#' Intermediate_Matrix <- do.call("rbind", lapply(seq_len(nrow(Original_Plot_Layout_Matrix)), function (x) {
#'   unlist(sapply(seq_along(Original_Plot_Layout_Matrix[x, ]), function (y) {
#'     rep(Original_Plot_Layout_Matrix[x, y], each = New_Widths[y])
#'   }))
#' }))
#' (Final_Layout_Matrix <- do.call("cbind", lapply(seq_len(ncol(Intermediate_Matrix)), function (x) {
#'   unlist(sapply(seq_along(Intermediate_Matrix[, x]), function (y) {
#'     rep(Intermediate_Matrix[y, x], each = New_Heights[y])
#'   }))
#' })))
#' # Without specifying any heights or widths for this layout matrix, the resulting figure will have the intended appearance.
#' layout(Final_Layout_Matrix)
#' par(mar = c(1, 1, 1, 1))
#' plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
#' text(0, 0, expression(paste(bold("Example Figure Using the `iris` Data Set"))), cex = 2)
#' par(mar = c(5, 4, 4, 2) + 0.1)
#' lapply(unique(iris$Species), function (x) {
#'   plot(Sepal.Width ~ Sepal.Length, iris[which(iris$Species == x), ], main = paste("Species:", x), xlab = "Sepal Length", ylab = "Sepal Width")
#' })
#'
#' @export
Finding_the_Least_Common_Denominator_of_Imperfect_Fractions <- function (Vector) {
  Multiplier <- 1
  repeat {
    if (all((Vector * Multiplier) %% 1 == 0)) {
      break
    }
    Multiplier <- Multiplier + 1
  }
  Multiplier
}
