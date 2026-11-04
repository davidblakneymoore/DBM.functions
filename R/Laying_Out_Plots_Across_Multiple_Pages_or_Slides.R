#' Laying Out Plots Across Multiple Pages or Slides
#'
#' This function generates plot layout matrices that allow the user to break up extensive numbers of plots neatly across multiple figures across multiple pages of a manuscript or multiple slides in a presentation.
#'
#' `Laying_Out_Plots_Across_Multiple_Pages_or_Slides` generates plot layout matrices that allow the user to break up extensive numbers of plots neatly across multiple figures across multiple pages of a manuscript or multiple slides in a presentation.
#'
#' @param Number_of_Plots the number of plots to be arranged across multiple figures (across multiple pages or slides).
#' @param Number_of_Rows the number of rows of plots within each figure.
#' @param Number_of_Columns the number of columns of plots within each figure.
#' @param Add_a_Row_for_a_Figure_Title a logical argument indicating if a row should be added at the top of each layout matrix for a title plot. The default to this argument, `TRUE`, adds a row at the top of each layout matrix for a title plot.
#' @param Add_a_Row_for_a_Figure_Legend a logical argument indicating if a row should be added at the bottom of each layout matrix for a legend plot. The default to this argument, `TRUE`, adds a row at the bottom of each layout matrix for a legend plot.
#' @param Should_Blank_Space_Be_Left_at_the_Bottom_of_the_Last_Figure a logical argument indicating if blank space should be left at the bottom of the last layout matrix if the last layout matrix contains fewer rows than the previous layout matrix or layout matrices to ensure that the last layout matrix is the same size as the previous layout matrix or layout matrices. When creating figures within R, it's best if this argument is `TRUE`, whereas when creating figures that will be stored to the hard drive, it's best if this argument is `FALSE` (see the examples for more details). The default to this argument, `TRUE`, adds blank space if necessary to ensure that all layout matrices are the exact same size (which, again, is particularly nice when printing figures in R).
#'
#' @return This function returns a list of plot layout matrices (`List_of_Layout_Matrices`) where each list element corresponds with a plot layout matrix for a new figure (a new page of a manuscript or a new slide of a presentation). This function also returns a vector (`Indices`) that should be used to assign plots to figures (see the examples for more details).
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' Data_Frame <- as.data.frame(matrix(rnorm(384), ncol = 32))
#' colnames(Data_Frame) <- paste("Response_Variable", seq_len(ncol(Data_Frame)), sep = "_")
#' Data_Frame$Group <- rep(LETTERS[1:4], each = 3)
#' Data_Frame <- Data_Frame[, c(which(colnames(Data_Frame) == "Group"), which(colnames(Data_Frame) != "Group"))]
#' Relative_Title_Plot_Height <- 1
#' Relative_Boxplot_Plot_Height <- 4
#' 
#' # Example 1: Printing Figures in R
#' Output <- Laying_Out_Plots_Across_Multiple_Pages_or_Slides(Number_of_Plots = ncol(Data_Frame[, grep("Response_Variable", colnames(Data_Frame))]), Number_of_Rows = 4, Number_of_Columns = 3, Add_a_Row_for_a_Figure_Legend = F)
#' lapply(seq_along(Output$List_of_Layout_Matrices), function (i) {
#'   Data_Subset <- Data_Frame[, c(which(colnames(Data_Frame) == "Group"), grep("Response_Variable", colnames(Data_Frame))[which(Output$Indices == i)])]
#'   layout(Output$List_of_Layout_Matrices[[i]], heights = c(Relative_Title_Plot_Height, rep(Relative_Boxplot_Plot_Height, (nrow(Output$List_of_Layout_Matrices[[i]]) - 1))))
#'   par(mar = c(1, 1, 1, 1))
#'   plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
#'   if (i == 1) {
#'     text(0, 0, expression(paste(bold("Boxplots"))), cex = 2.5)
#'   } else if (i > 1) {
#'     text(0, 0, expression(paste(bold("Boxplots, continued"))), cex = 2.5)
#'   }
#'   par(mar = c(5, 5, 4, 2))
#'   lapply(seq_along(grep("Response_Variable", colnames(Data_Subset))), function (j) {
#'     boxplot(Data_Subset[, grep("Response_Variable", colnames(Data_Subset))[j]] ~ Data_Subset$Group, main = gsub("_", " ", colnames(Data_Subset)[grep("Response_Variable", colnames(Data_Subset))[j]]), xlab = "Group", ylab = "Response", cex.axis = 1.5, cex.lab = 1.5, cex.main = 2)
#'   })
#' })
#' 
#' # Example 2: Saving Figures to the Hard Drive
#' # Caution - this chunk of code will actually store figures in your working directory on your hard drive named 'Boxplot Figure 1', 'Boxplot Figure 2', and 'Boxplot Figure 3'!
#' Output <- Laying_Out_Plots_Across_Multiple_Pages_or_Slides(Number_of_Plots = ncol(Data_Frame[, grep("Response_Variable", colnames(Data_Frame))]), Number_of_Rows = 4, Number_of_Columns = 3, Add_a_Row_for_a_Figure_Legend = F, Should_Blank_Space_Be_Left_at_the_Bottom_of_the_Last_Figure = F)
#' Horizontal_Figure_Stretching_Constant <- 75
#' Vertical_Figure_Stretching_Constant <- 75
#' lapply(seq_along(Output$List_of_Layout_Matrices), function (i) {
#'   Data_Subset <- Data_Frame[, c(which(colnames(Data_Frame) == "Group"), grep("Response_Variable", colnames(Data_Frame))[which(Output$Indices == i)])]
#'   jpeg(paste0("Boxplot Figure ", i, ".jpeg"), height = ((Vertical_Figure_Stretching_Constant * (nrow(Output$List_of_Layout_Matrices[[i]]) - 1) * Relative_Boxplot_Plot_Height) + (Vertical_Figure_Stretching_Constant * Relative_Title_Plot_Height)), width = (Horizontal_Figure_Stretching_Constant * ncol(Output$List_of_Layout_Matrices[[i]]) * Relative_Boxplot_Plot_Height / 2))
#'   layout(Output$List_of_Layout_Matrices[[i]], heights = c(Relative_Title_Plot_Height, rep(Relative_Boxplot_Plot_Height, (nrow(Output$List_of_Layout_Matrices[[i]]) - 1))))
#'   par(mar = c(1, 1, 1, 1))
#'   plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
#'   if (i == 1) {
#'     text(0, 0, expression(paste(bold("Boxplots"))), cex = 2.5)
#'   } else if (i > 1) {
#'     text(0, 0, expression(paste(bold("Boxplots, continued"))), cex = 2.5)
#'   }
#'   par(mar = c(5, 5, 4, 2))
#'   lapply(seq_along(grep("Response_Variable", colnames(Data_Subset))), function (j) {
#'     boxplot(Data_Subset[, grep("Response_Variable", colnames(Data_Subset))[j]] ~ Data_Subset$Group, main = gsub("_", " ", colnames(Data_Subset)[grep("Response_Variable", colnames(Data_Subset))[j]]), xlab = "Group", ylab = "Response", cex.axis = 1.5, cex.lab = 1.5, cex.main = 2)
#'   })
#'   dev.off()
#' })
#'
#' @export
Laying_Out_Plots_Across_Multiple_Pages_or_Slides <- function (Number_of_Plots, Number_of_Rows, Number_of_Columns, Add_a_Row_for_a_Figure_Title = T, Add_a_Row_for_a_Figure_Legend = T, Should_Blank_Space_Be_Left_at_the_Bottom_of_the_Last_Figure = T) {
  if (length(Number_of_Plots) != 1) {
    stop ("The 'Number_of_Plots' argument must be of length 1.")
  }
  if (is.na(Number_of_Plots)) {
    stop ("The 'Number_of_Plots' argument must not be missing.")
  }
  if (!is.numeric(Number_of_Plots)) {
    stop ("The 'Number_of_Plots' argument must be numeric.")
  }
  if ((Number_of_Plots %% 1) != 0) {
    stop ("The 'Number_of_Plots' argument must be a whole number.")
  }
  if (Number_of_Plots < 1) {
    stop ("The 'Number_of_Plots' argument must be positive.")
  }
  if (length(Number_of_Rows) != 1) {
    stop ("The 'Number_of_Rows' argument must be of length 1.")
  }
  if (is.na(Number_of_Rows)) {
    stop ("The 'Number_of_Rows' argument must not be missing.")
  }
  if (!is.numeric(Number_of_Rows)) {
    stop ("The 'Number_of_Rows' argument must be numeric.")
  }
  if ((Number_of_Rows %% 1) != 0) {
    stop ("The 'Number_of_Rows' argument must be a whole number.")
  }
  if (Number_of_Rows < 1) {
    stop ("The 'Number_of_Rows' argument must be positive.")
  }
  if (length(Number_of_Columns) != 1) {
    stop ("The 'Number_of_Columns' argument must be of length 1.")
  }
  if (is.na(Number_of_Columns)) {
    stop ("The 'Number_of_Columns' argument must not be missing.")
  }
  if (!is.numeric(Number_of_Columns)) {
    stop ("The 'Number_of_Columns' argument must be numeric.")
  }
  if ((Number_of_Columns %% 1) != 0) {
    stop ("The 'Number_of_Columns' argument must be a whole number.")
  }
  if (Number_of_Columns < 1) {
    stop ("The 'Number_of_Columns' argument must be positive.")
  }
  if (length(Add_a_Row_for_a_Figure_Title) != 1) {
    stop ("The 'Add_a_Row_for_a_Figure_Title' argument must be of length 1.")
  }
  if (!is.logical(Add_a_Row_for_a_Figure_Title)) {
    stop ("The 'Add_a_Row_for_a_Figure_Title' argument must be of class 'logical'.")
  }
  if (is.na(Add_a_Row_for_a_Figure_Title)) {
    stop ("The 'Add_a_Row_for_a_Figure_Title' argument must not be missing.")
  }
  if (length(Add_a_Row_for_a_Figure_Legend) != 1) {
    stop ("The 'Add_a_Row_for_a_Figure_Legend' argument must be of length 1.")
  }
  if (!is.logical(Add_a_Row_for_a_Figure_Legend)) {
    stop ("The 'Add_a_Row_for_a_Figure_Legend' argument must be of class 'logical'.")
  }
  if (is.na(Add_a_Row_for_a_Figure_Legend)) {
    stop ("The 'Add_a_Row_for_a_Figure_Legend' argument must not be missing.")
  }
  if (length(Should_Blank_Space_Be_Left_at_the_Bottom_of_the_Last_Figure) != 1) {
    stop ("The 'Should_Blank_Space_Be_Left_at_the_Bottom_of_the_Last_Figure' argument must be of length 1.")
  }
  if (!is.logical(Should_Blank_Space_Be_Left_at_the_Bottom_of_the_Last_Figure)) {
    stop ("The 'Should_Blank_Space_Be_Left_at_the_Bottom_of_the_Last_Figure' argument must be of class 'logical'.")
  }
  if (is.na(Should_Blank_Space_Be_Left_at_the_Bottom_of_the_Last_Figure)) {
    stop ("The 'Should_Blank_Space_Be_Left_at_the_Bottom_of_the_Last_Figure' argument must not be missing.")
  }
  Number_of_Cells <- Number_of_Rows * Number_of_Columns
  Vector_1 <- seq_len(Number_of_Plots) %/% Number_of_Cells
  Vector_1 <- c(0, Vector_1[-length(Vector_1)]) + 1
  Vector_2 <- seq_len(Number_of_Plots) %% Number_of_Cells
  Vector_2[which(Vector_2 == 0)] <- Number_of_Cells
  Vector_2_in_List_Form <- lapply(unique(Vector_1), function (x) {
    Vector_2[which(Vector_1 == x)]
  })
  List_of_Matrices <- lapply(seq_along(Vector_2_in_List_Form), function (i) {
    if (i < length(Vector_2_in_List_Form)) {
      Layout_Matrix <- matrix(unlist(lapply(seq_len(Number_of_Cells), function (x) {
        rep(x, 2)
      })), ncol = Number_of_Columns * 2, byrow = T)
    } else if (i == length(Vector_2_in_List_Form)) {
      if (length(Vector_2_in_List_Form[[i]]) == Number_of_Cells) {
        Layout_Matrix <- matrix(unlist(lapply(seq_len(Number_of_Cells), function (x) {
          rep(x, 2)
        })), ncol = Number_of_Columns * 2, byrow = T)
      } else if (length(Vector_2_in_List_Form[[i]]) < Number_of_Cells) {
        Vector_3 <- Vector_2_in_List_Form[[i]]
        Vector_4 <- Vector_3 %/% Number_of_Columns
        Vector_4 <- c(0, Vector_4[-length(Vector_4)]) + 1
        Layout_Matrix <- do.call("rbind", lapply(seq_along(unique(Vector_4)), function (j) {
          if (j < length(unique(Vector_4))) {
            unlist(lapply(Vector_3[which(Vector_4 == j)], function (y) {
              rep(y, 2)
            }))
          } else if (j == length(unique(Vector_4))) {
            Number_of_Zeroes <- Number_of_Columns - length(Vector_3[which(Vector_4 == j)])
            c(rep(0, Number_of_Zeroes), unlist(lapply(Vector_3[which(Vector_4 == j)], function (y) {
              rep(y, 2)
            })), rep(0, Number_of_Zeroes))
          }
        }))
        if (Should_Blank_Space_Be_Left_at_the_Bottom_of_the_Last_Figure == T) {
          Layout_Matrix <- rbind(Layout_Matrix, do.call("rbind", lapply(seq_len(Number_of_Rows - nrow(Layout_Matrix)), function (k) {
            rep(0, ncol(Layout_Matrix))
          })))
        }
      }
    }
    Layout_Matrix
  })
  if (Add_a_Row_for_a_Figure_Title == T) {
    List_of_Matrices <- lapply(List_of_Matrices, function (x) {
      x[which(x != 0)] <- x[which(x != 0)] + 1
      rbind(rep(1, ncol(x)), x)
    })
  }
  if (Add_a_Row_for_a_Figure_Legend == T) {
    List_of_Matrices <- lapply(List_of_Matrices, function (x) {
      rbind(x, rep(max(x), ncol(x)))
    })
  }
  list(List_of_Layout_Matrices = List_of_Matrices, Indices = Vector_1)
}
