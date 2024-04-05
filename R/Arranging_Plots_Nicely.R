#' Arranging Plots Nicely
#'
#' This function generates a plot layout matrix that is as square as possible - in other words, it generates a plot layout matrix whose number of rows and number of columns differ by either `0` (if possible) or `1` (as a last resort).
#'
#' `Arranging_Plots_Nicely` generates a plot layout matrix that is as square as possible - in other words, it generates a plot layout matrix whose number of rows and number of columns differ by either `0` (if possible) or `1` (as a last resort). This function works best when each individual plot in the figure is desired to be equal in size and in importance.
#'
#' @param Number_of_Plots the number of plots to be arranged.
#' @param Arrangement_Option whether a slightly wider or a slightly taller arrangement should be favored. Possibilities for this argument include `"Wide"` and `"Tall"`, with `"Wide"` being the default. This argument is only truly relevant if the numbers of rows and columns of the plot layout matrix differ by `1` - if the number of rows and the number of columns are equal, this argument has no bearing on the output.
#' @param Row_for_Plot_Title a logical argument indicating whether an extra row (an extra plot) should be included at the top of the figure for a figure title. By default, this extra row is included.
#' @param Row_for_Plot_Legend a logical argument indicating whether an extra row (an extra plot) should be included at the bottom of the figure for a legend. By default, this extra row is included.
#'
#' @return This function returns a plot layout matrix.
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' # Use a Subset of the 'mtcars' Data Frame
#' ?mtcars
#' Data_Frame <- mtcars[, which(colnames(mtcars) != "vs")]
#'
#' # Generate the Layout Matrix
#' (Layout_Matrix <- DBM.functions::Arranging_Plots_Nicely(Number_of_Plots = (ncol(mtcars) - 3), Arrangement_Option = "Tall"))
#'
#' # Make the Figure
#' jpeg("Example Plot.jpeg", height = 1000, width = 1000)
#' layout(Layout_Matrix, heights = c(1, rep(exp(1), (nrow(Layout_Matrix) - 2)), 1))
#' par(mar = c(1, 1, 5, 1))
#' plot(0, type = "n", axes = FALSE, xlab = "", ylab = "", main = "\nPlotting the 'mpg' Column Against Other Columns From the 'mtcars' Data Frame", cex.main = 2.5)
#' par(mar = c(5, 4, 2, 2) + 0.1)
#' lapply(seq_len(ncol(Data_Frame) - 2), function (x) {
#'   plot(Data_Frame$mpg ~ Data_Frame[, which(!(colnames(Data_Frame) %in% c("mpg", "am")))[x]], xlab = colnames(Data_Frame)[which(!(colnames(Data_Frame) %in% c("mpg", "am")))[x]], ylab = "mpg", type = "n")
#'   lapply(seq_len(length(unique(Data_Frame$am))), function (y) {
#'     points(Data_Frame[which(Data_Frame$am == unique(Data_Frame$am)[y]), ]$mpg ~ Data_Frame[which(Data_Frame$am == unique(Data_Frame$am)[y]), which(!(colnames(Data_Frame) %in% c("mpg", "am")))[x]], pch = 19, col = y)
#'   })
#' })
#' par(mar = c(1, 1, 1, 1))
#' plot(0, type = "n", axes = FALSE, xlab = "", ylab = "")
#' legend("top", horiz = TRUE, title = expression(paste(bold("'am' Column Value"))), legend = unique(Data_Frame$am), col = 1:2, pch = 19)
#' dev.off()
#'
#' @export
Arranging_Plots_Nicely <- function (Number_of_Plots, Arrangement_Option = c("Wide", "Tall"), Row_for_Plot_Title = TRUE, Row_for_Plot_Legend = TRUE) {
  if (missing(Number_of_Plots) | is.null(Number_of_Plots) | is.na(Number_of_Plots)) {
    stop ("The 'Number_of_Plots' argument must be provided.")
  }
  if (!(is.finite(Number_of_Plots))) {
    stop ("The 'Number_of_Plots' argument must be finite.")
  }
  if (!(is.numeric(Number_of_Plots))) {
    stop ("The 'Number_of_Plots' argument must be numeric.")
  }
  if (length(Number_of_Plots) != 1) {
    stop ("The 'Number_of_Plots' argument must be of length 1.")
  }
  if (any(is.null(Arrangement_Option)) | any(is.na(Arrangement_Option[1]))) {
    stop ("The 'Arrangement_Option' argument must be provided.")
  }
  if (!(is.character(Arrangement_Option))) {
    stop ("The 'Arrangement_Option' argument must be of class 'character'.")
  }
  if (!(all((Arrangement_Option == "Wide") | (Arrangement_Option == "Tall")))) {
    stop ("The 'Arrangement_Option' argument must be either 'Wide' or 'Tall'.")
  }
  if (is.null(Row_for_Plot_Title) | is.na(Row_for_Plot_Title)) {
    stop ("The 'Row_for_Plot_Title' argument must be provided.")
  }
  if (!(is.logical(Row_for_Plot_Title))) {
    stop ("The 'Row_for_Plot_Title' argument must be of class 'logical'.")
  }
  if (length(Row_for_Plot_Title) != 1) {
    stop ("The 'Row_for_Plot_Title' argument must be of length 1.")
  }
  if (is.null(Row_for_Plot_Legend) | is.na(Row_for_Plot_Legend)) {
    stop ("The 'Row_for_Plot_Legend' argument must be provided.")
  }
  if (!(is.logical(Row_for_Plot_Legend))) {
    stop ("The 'Row_for_Plot_Legend' argument must be of class 'logical'.")
  }
  if (length(Row_for_Plot_Legend) != 1) {
    stop ("The 'Row_for_Plot_Legend' argument must be of length 1.")
  }
  if (Arrangement_Option[1] == "Wide") {
    Number_of_Rows <- ceiling(Number_of_Plots / ceiling(sqrt(Number_of_Plots)))
    Number_of_Columns <- ceiling(sqrt(Number_of_Plots))
  } else if (Arrangement_Option[1] == "Tall") {
    Number_of_Rows <- ceiling(sqrt(Number_of_Plots))
    Number_of_Columns <- ceiling(Number_of_Plots / ceiling(sqrt(Number_of_Plots)))
  }
  Plot_Layout_Numbering <- rep(seq_len(Number_of_Plots), each = 2)
  if ((Number_of_Plots %% Number_of_Columns) == 0) {
    Plot_Layout_Numbering_Matrix <- t(sapply(seq_len(Number_of_Rows), function (x) {
      rep(seq_len(Number_of_Columns) + (Number_of_Columns * (x - 1)), each = 2)
    }))
  } else if ((Number_of_Plots %% Number_of_Columns) != 0) {
    Number_of_Plots_in_the_Last_Row <- Number_of_Plots %% Number_of_Columns
    Plots_in_the_Last_Row <- seq_len(Number_of_Plots)[(Number_of_Plots - Number_of_Plots_in_the_Last_Row + 1):Number_of_Plots]
    Plot_Layout_Numbering_Without_the_Last_Row <- t(sapply(seq_len(Number_of_Rows - 1), function (x) {
      rep(seq_len(Number_of_Columns) + (Number_of_Columns * (x - 1)), each = 2)
    }))
    Initial_Last_Row_Plot_Layout_Numbering <- Plot_Layout_Numbering[which(Plot_Layout_Numbering %in% Plots_in_the_Last_Row)]
    Final_Last_Row_Plot_Layout_Numbering <- c(rep(0, (((Number_of_Columns * 2) - length(Initial_Last_Row_Plot_Layout_Numbering)) / 2)), Initial_Last_Row_Plot_Layout_Numbering, rep(0, (((Number_of_Columns * 2) - length(Initial_Last_Row_Plot_Layout_Numbering)) / 2)))
    Plot_Layout_Numbering_Matrix <- rbind(Plot_Layout_Numbering_Without_the_Last_Row, Final_Last_Row_Plot_Layout_Numbering)
  }
  if (Row_for_Plot_Title == TRUE) {
    Plot_Layout_Numbering_Matrix <- ifelse(Plot_Layout_Numbering_Matrix == 0, 0, Plot_Layout_Numbering_Matrix + 1)
    Title_Row <- rep(1, Number_of_Columns)
    Plot_Layout_Numbering_Matrix <- rbind(Title_Row, Plot_Layout_Numbering_Matrix)
  }
  if (Row_for_Plot_Legend == TRUE) {
    Legend_Row <- rep((max(Plot_Layout_Numbering_Matrix) + 1), Number_of_Columns)
    Plot_Layout_Numbering_Matrix <- rbind(Plot_Layout_Numbering_Matrix, Legend_Row)
  }
  rownames(Plot_Layout_Numbering_Matrix) <- NULL
  Plot_Layout_Numbering_Matrix
}
