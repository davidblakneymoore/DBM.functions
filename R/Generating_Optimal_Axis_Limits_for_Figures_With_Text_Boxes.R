#' Generating Optimal Axis Limits for Figures With Text Boxes
#'
#' This function may be used to generate axis limits for plots when both text and points (or lines) are present. It was developed specifically for ordinations (such as [principal component analysis](https://en.wikipedia.org/wiki/Principal_component_analysis) or [non-metric multidimensional scaling](https://en.wikipedia.org/wiki/Multidimensional_scaling#Non-metric_multidimensional_scaling_(NMDS))) when data points are plotted alongside loadings vectors and when these loadings vectors are identified with text on the plot.
#'
#' Use this function only when you've already got the plotting window's desired dimensions set. Don't use the function, resize the plotting window, and then use the output of the function to plot - it won't work. The function uses the current size of the plotting region in its calculations.
#'
#' Unfortunately, this function cannot handle formatted text (such as bold, italicized, or underlined text) at this time.
#' 
#' Also, unfortunately, this function cannot handle figures with multiple plots because the function turns off the null device and creates a lot of plots internally to generate the output. In the future, a version of this function that may be used with complicated plot arrangements may become available.
#'
#' `Generating_Optimal_Axis_Limits_for_Figures_With_Text_Boxes` may be used to generate axis limits for plots when both text and points (or lines) are present. It was developed specifically for ordinations (such as [principal component analysis](https://en.wikipedia.org/wiki/Principal_component_analysis) or [non-metric multidimensional scaling](https://en.wikipedia.org/wiki/Multidimensional_scaling#Non-metric_multidimensional_scaling_(NMDS))) when data points are plotted alongside loadings vectors and when these loadings vectors are identified with text on the plot.
#' @param Points_Horizontal_Axis_Coordinates the numeric variables comprising the coordinates from the first ordination axis (which will become the horizontal axis).
#' @param Points_Vertical_Axis_Coordinates the numeric variables comprising the coordinates from the second ordination axis (which will become the vertical axis).
#' @param Texts the vector of loadings names.
#' @param Texts_Horizontal_Axis_Coordinates the values you wish to align across the vertical axes. The default for this argument is a vector of `0`s. This argument must be a numeric vector and it must contain the same number of elements that there are variables being aligned.
#' @param Texts_Vertical_Axis_Coordinates the weights assigned to each variable. To prevent certain variables from being crowded near the top or the bottom of the plot, a greater weight can be assigned to these variables, which ensures that these variables will take up more of the plotting region (at the expense of other variables, of course). The default for this argument is to assign all the variables the same weight. This argument must be a numeric vector, all the entries must be finite and nonnegative, and it must contain the same number of elements as there are variables being aligned.
#' @param Character_Expansion the relative sizes of the names of the loadings labels. This argument is passed to the `"cex"` argument of all the relevant graphing functions. The default is `1`. This argument must be a nonnegative number.
#' @param Spacing_Constant the relative amount of space between the loadings labels and the tips of the loadings vectors. The default is `1` (which corresponds to the size of a gap between two lines in a text box). Larger numbers for this argument lead to larger spacing. This argument must be a nonnegative number. Horizontal and vertical spacing is equal as all calculations are done in inches and take into account the dimensions of the plotting region.
#' @param Text_Shifting_Method the method for shifting text boxes away from loading vector tips. The `"None"` method centers text boxes at the tip of the loading vectors. The `"Only_Horizontally_With_Leftward_Tendancy"` method shifts text boxes horizontally away from loading vector tips, but if there are no horizontal components to loading vectors, shifts to the left are performed. The `"Only_Horizontally_With_Rightward_Tendancy"` method shifts text boxes horizontally away from loading vector tips, but if there are no horizontal components to loading vectors, shifts to the right are performed. The `"Mainly_Horizontally_With_Leftward_Tendancy"` method shifts text boxes horizontally away from loading vector tips. If there are no horizontal components to loading vectors, vertical shifts are performed if there are vertical components, and shifts to the left are performed if there are no vertical components. The `"Mainly_Horizontally_With_Rightward_Tendancy"` method shifts text boxes horizontally away from loading vector tips. If there are no horizontal components to loading vectors, vertical shifts are performed if there are vertical components, and shifts to the right are performed if there are no vertical components. The `"Only_Vertically_With_Upward_Tendancy"` method shifts text boxes vertically away from loading vector tips, but if there are no vertical components to loading vectors, shifts up are performed. The `"Only_Vertically_With_Downward_Tendancy"` method shifts text boxes vertically away from loading vector tips, but if there are no vertical components to loading vectors, shifts down are performed. The `"Mainly_Vertically_With_Upward_Tendancy"` method shifts text boxes vertically away from loading vector tips. If there are no vertical components to loading vectors, horizontal shifts are performed if there are horizontal components, and shifts up are performed if there are no horizontal components. The `"Mainly_Vertically_With_Downward_Tendancy"` method shifts text boxes vertically away from loading vector tips. If there are no vertical components to loading vectors, horizontal shifts are performed if there are horizontal components, and shifts down are performed if there are no horizontal components. The `"Mainly_Diagonally_Using_Text_Box_Corners_at_45_Degree_Angles_With_Leftward_Tendancy"` method places text box corners at the tips of the loading vectors at an angle of 45 ˚. For vectors with no horizontal vector components, text boxes are shifted vertically; for vectors with no vertical vector components, text boxes are shifted horizontally. For vectors with neither vertical nor horizontal components, text boxes are shifted to the left. The `"Mainly_Diagonally_Using_Text_Box_Corners_at_45_Degree_Angles_With_Upward_Tendancy"` method places text box corners at the tips of the loading vectors at an angle of 45 ˚. For vectors with no horizontal vector components, text boxes are shifted vertically; for vectors with no vertical vector components, text boxes are shifted horizontally. For vectors with neither vertical nor horizontal components, text boxes are shifted up. The `"Mainly_Diagonally_Using_Text_Box_Corners_at_45_Degree_Angles_With_Rightward_Tendancy"` method places text box corners at the tips of the loading vectors at an angle of 45 ˚. For vectors with no horizontal vector components, text boxes are shifted vertically; for vectors with no vertical vector components, text boxes are shifted horizontally. For vectors with neither vertical nor horizontal components, text boxes are shifted to the right. The `"Mainly_Diagonally_Using_Text_Box_Corners_at_45_Degree_Angles_With_Downward_Tendancy"` method places text box corners at the tips of the loading vectors at an angle of 45 ˚. For vectors with no horizontal vector components, text boxes are shifted vertically; for vectors with no vertical vector components, text boxes are shifted horizontally. For vectors with neither vertical nor horizontal components, text boxes are shifted down. The `"Mainly_Diagonally_Using_Text_Box_Corners_at_the_Same_Angles_as_the_Associated_Vectors_With_Leftward_Tendancy"` method places text box corners at the tips of the loading vectors at the same angle as the loading vectors. For vectors with no horizontal vector components, text boxes are shifted vertically; for vectors with no vertical vector components, text boxes are shifted horizontally. For vectors with neither vertical nor horizontal components, text boxes are shifted to the left. The `"Mainly_Diagonally_Using_Text_Box_Corners_at_the_Same_Angles_as_the_Associated_Vectors_With_Upward_Tendancy"` method places text box corners at the tips of the loading vectors at the same angle as the loading vectors. For vectors with no horizontal vector components, text boxes are shifted vertically; for vectors with no vertical vector components, text boxes are shifted horizontally. For vectors with neither vertical nor horizontal components, text boxes are shifted up. The `"Mainly_Diagonally_Using_Text_Box_Corners_at_the_Same_Angles_as_the_Associated_Vectors_With_Rightward_Tendancy"` method places text box corners at the tips of the loading vectors at the same angle as the loading vectors. For vectors with no horizontal vector components, text boxes are shifted vertically; for vectors with no vertical vector components, text boxes are shifted horizontally. For vectors with neither vertical nor horizontal components, text boxes are shifted to the right. The `"Mainly_Diagonally_Using_Text_Box_Corners_at_the_Same_Angles_as_the_Associated_Vectors_With_Downward_Tendancy"` method places text box corners at the tips of the loading vectors at the same angle as the loading vectors. For vectors with no horizontal vector components, text boxes are shifted vertically; for vectors with no vertical vector components, text boxes are shifted horizontally. For vectors with neither vertical nor horizontal components, text boxes are shifted down. The `"Using_Vector_Directions_With_Leftward_Tendancy"` method places text box centers in lne with vectors such that the minimum spacing between text boxes and vector tips is the spacing set by the user. For vectors with neither vertical nor horizontal components, text boxes are shifted to the left. The `"Using_Vector_Directions_With_Upward_Tendancy"` method places text box centers in lne with vectors such that the minimum spacing between text boxes and vector tips is the spacing set by the user. For vectors with neither vertical nor horizontal components, text boxes are shifted up. The `"Using_Vector_Directions_With_Rightward_Tendancy"` method places text box centers in lne with vectors such that the minimum spacing between text boxes and vector tips is the spacing set by the user. For vectors with neither vertical nor horizontal components, text boxes are shifted to the right. The `"Using_Vector_Directions_With_Downward_Tendancy"` method places text box centers in lne with vectors such that the minimum spacing between text boxes and vector tips is the spacing set by the user. For vectors with neither vertical nor horizontal components, text boxes are shifted down. The default is `"Using_Vector_Directions_With_Rightward_Tendancy"`.
#' @param Should_Text_Box_Polygons_Shade_out_Points A logical indicator which specifies whether text boxes are to be shaded prior to adding the text. If shading is to occur, a small additional amount of space is left around the outside of the plotting regions so that the shaded areas do not get cut off by the edge of the plot. The default is `FALSE`.
#'
#' @return This function returns the new axis limits to ensure all points and loadings labels are visible in the plotting region, and it also returns the coordinates for the loadings labels (taking into account the size of these labels). Finally, if text boxes are to be shaded, it will also return the coordinates of the corners of the text boxes.
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' set.seed(121)
#' Number_of_Data_Points <- 75
#' Ordination_Data <- data.frame(Ordination_Axis_1_Coordinates = rnorm(Number_of_Data_Points), Ordination_Axis_2_Coordinates = rnorm(Number_of_Data_Points))
#' Loadings <- data.frame(Variable = paste("Variable", seq_len(5), sep = "_"), Ordination_Axis_1_Coordinate = c(-0.1, -0.3, 1.2, 3.3, 1.1), Ordination_Axis_2_Coordinate = c(-1.1, -0.1, 0.5, -2.2, 1.7))
#' plot(Ordination_Data$Ordination_Axis_1_Coordinates, Ordination_Data$Ordination_Axis_2_Coordinates, type = "n", xlab = "Ordination Axis 1", ylab = "Ordination Axis 2", main = "Example Figure 1:\nOnly Points Are Considered When Determining Axis Limits")
#' points(Ordination_Data$Ordination_Axis_1_Coordinates, Ordination_Data$Ordination_Axis_2_Coordinates, pch = 19, col = 2)
#' arrows(rep(0, nrow(Loadings)), rep(0, nrow(Loadings)), Loadings$Ordination_Axis_1_Coordinate, Loadings$Ordination_Axis_2_Coordinate, length = 0.1)
#' plot(c(Ordination_Data$Ordination_Axis_1_Coordinates, Loadings$Ordination_Axis_1_Coordinate), c(Ordination_Data$Ordination_Axis_2_Coordinates, Loadings$Ordination_Axis_2_Coordinate), type = "n", xlab = "Ordination Axis 1", ylab = "Ordination Axis 2", main = "Example Figure 2:\nPoints and Loading Vectors Are Considered When Determining Axis Limits")
#' points(Ordination_Data$Ordination_Axis_1_Coordinates, Ordination_Data$Ordination_Axis_2_Coordinates, pch = 19, col = 2)
#' arrows(rep(0, nrow(Loadings)), rep(0, nrow(Loadings)), Loadings$Ordination_Axis_1_Coordinate, Loadings$Ordination_Axis_2_Coordinate, length = 0.1)
#' (Output_1 <- Generating_Optimal_Axis_Limits_for_Figures_With_Text_Boxes(Ordination_Data$Ordination_Axis_1_Coordinates, Ordination_Data$Ordination_Axis_2_Coordinates, Loadings$Variable, Loadings$Ordination_Axis_1_Coordinate, Loadings$Ordination_Axis_2_Coordinate, Character_Expansion = 1.125, Spacing_Constant = 0.875))
#' plot(0, type = "n", xlab = "Ordination Axis 1", ylab = "Ordination Axis 2", main = "Example Figure 3:\nOptimal Axis Limits Are Generated to Include Loading Vector Variable Names", xlim = Output_1$Axis_Limits$Horizontal_Axis_Limits, ylim = Output_1$Axis_Limits$Vertical_Axis_Limits)
#' points(Ordination_Data$Ordination_Axis_1_Coordinates, Ordination_Data$Ordination_Axis_2_Coordinates, pch = 19, col = 2)
#' arrows(rep(0, nrow(Loadings)), rep(0, nrow(Loadings)), Loadings$Ordination_Axis_1_Coordinate, Loadings$Ordination_Axis_2_Coordinate, length = 0.1)
#' text(Output_1$New_Texts_Axis_Coordinates$New_Texts_Horizontal_Axis_Coordinate, Output_1$New_Texts_Axis_Coordinates$New_Texts_Vertical_Axis_Coordinate, Output_1$New_Texts_Axis_Coordinates$Text, cex = 1.125)
#' (Output_2 <- Generating_Optimal_Axis_Limits_for_Figures_With_Text_Boxes(Ordination_Data$Ordination_Axis_1_Coordinates, Ordination_Data$Ordination_Axis_2_Coordinates, Loadings$Variable, Loadings$Ordination_Axis_1_Coordinate, Loadings$Ordination_Axis_2_Coordinate, Character_Expansion = 1.125, Spacing_Constant = 0.875, Should_Text_Box_Polygons_Shade_out_Points = TRUE))
#' plot(0, type = "n", xlab = "Ordination Axis 1", ylab = "Ordination Axis 2", main = "Example Figure 4:\nOptimal Axis Limits Are Generated to Include Loading Vector Variable Names With Background Shading", xlim = Output_2$Axis_Limits$Horizontal_Axis_Limits, ylim = Output_2$Axis_Limits$Vertical_Axis_Limits)
#' points(Ordination_Data$Ordination_Axis_1_Coordinates, Ordination_Data$Ordination_Axis_2_Coordinates, pch = 19, col = 2)
#' lapply(split(Output_2$Text_Box_Corner_Data_Frame, Output_2$Text_Box_Corner_Data_Frame$Text), function (x) {
#'   polygon(x = c(x$Horizontal_Coordinates, x$Horizontal_Coordinates[1]), y = c(x$Vertical_Coordinates, x$Vertical_Coordinates[1]), col = "grey90", border = NA)
#' })
#' arrows(rep(0, nrow(Loadings)), rep(0, nrow(Loadings)), Loadings$Ordination_Axis_1_Coordinate, Loadings$Ordination_Axis_2_Coordinate, length = 0.1)
#' text(Output_2$New_Texts_Axis_Coordinates$New_Texts_Horizontal_Axis_Coordinate, Output_2$New_Texts_Axis_Coordinates$New_Texts_Vertical_Axis_Coordinate, Output_2$New_Texts_Axis_Coordinates$Text, cex = 1.125)
#'
#' @export
Generating_Optimal_Axis_Limits_for_Figures_With_Text_Boxes <- function (Points_Horizontal_Axis_Coordinates, Points_Vertical_Axis_Coordinates, Texts, Texts_Horizontal_Axis_Coordinates, Texts_Vertical_Axis_Coordinates, Character_Expansion = 1, Spacing_Constant = 1, Text_Shifting_Method = "Using_Vector_Directions_With_Rightward_Tendancy", Should_Text_Box_Polygons_Shade_out_Points = FALSE) {
  if (!is.numeric(Points_Horizontal_Axis_Coordinates)) {
    stop ("The 'Points_Horizontal_Axis_Coordinates' argument must be of class 'numeric'.")
  }
  if (any(is.na(Points_Horizontal_Axis_Coordinates))) {
    stop("The 'Points_Horizontal_Axis_Coordinates' argument may not contain any missing values.")
  }
  if (!is.numeric(Points_Vertical_Axis_Coordinates)) {
    stop ("The 'Points_Vertical_Axis_Coordinates' argument must be of class 'numeric'.")
  }
  if (any(is.na(Points_Vertical_Axis_Coordinates))) {
    stop("The 'Points_Vertical_Axis_Coordinates' argument may not contain any missing values.")
  }
  if (length (Points_Horizontal_Axis_Coordinates) != length(Points_Vertical_Axis_Coordinates)) {
    stop ("The 'Points_Horizontal_Axis_Coordinates' and 'Points_Vertical_Axis_Coordinates' arguments must be of the same length.")
  }
  if (!is.character(Texts)) {
    stop ("The 'Texts' argument must be of class 'character'.")
  }
  if (any(is.na(Texts))) {
    stop("The 'Texts' argument may not contain any missing values.")
  }
  if (!is.numeric(Texts_Horizontal_Axis_Coordinates)) {
    stop ("The 'Texts_Horizontal_Axis_Coordinates' argument must be of class 'numeric'.")
  }
  if (any(is.na(Texts_Horizontal_Axis_Coordinates))) {
    stop("The 'Texts_Horizontal_Axis_Coordinates' argument may not contain any missing values.")
  }
  if (!is.numeric(Texts_Vertical_Axis_Coordinates)) {
    stop ("The 'Texts_Vertical_Axis_Coordinates' argument must be of class 'numeric'.")
  }
  if (any(is.na(Texts_Vertical_Axis_Coordinates))) {
    stop("The 'Texts_Vertical_Axis_Coordinates' argument may not contain any missing values.")
  }
  if (!all(c(length(Texts) == length(Texts_Horizontal_Axis_Coordinates), length(Texts) == length(Texts_Vertical_Axis_Coordinates), length(Texts_Horizontal_Axis_Coordinates) == length(Texts_Vertical_Axis_Coordinates)))) {
    stop ("The 'Texts', 'Texts_Horizontal_Axis_Coordinates', and 'Texts_Vertical_Axis_Coordinates' arguments must all be of the same length.")
  }
  if (!is.numeric(Character_Expansion)) {
    stop ("The 'Character_Expansion' argument must be of class 'numeric'.")
  }
  if (is.na(Character_Expansion)) {
    stop("The 'Character_Expansion' argument may not be missing.")
  }
  if (length(Character_Expansion) != 1) {
    stop ("The 'Character_Expansion' argument must be of length '1'.")
  }
  if (Character_Expansion < 0) {
    stop ("The 'Character_Expansion' argument must be a nonnegative number.")
  }
  if (!is.numeric(Spacing_Constant)) {
    stop ("The 'Spacing_Constant' argument must be of class 'numeric'.")
  }
  if (is.na(Spacing_Constant)) {
    stop("The 'Spacing_Constant' argument may not be missing.")
  }
  if (length(Spacing_Constant) != 1) {
    stop ("The 'Spacing_Constant' argument must be of length '1'.")
  }
  if (Spacing_Constant < 0) {
    stop ("The 'Spacing_Constant' argument must be a nonnegative number.")
  }
  if (!is.character(Text_Shifting_Method)) {
    stop ("The 'Text_Shifting_Method' argument must be of class 'character'.")
  }
  if (is.na(Text_Shifting_Method)) {
    stop("The 'Text_Shifting_Method' argument may not be missing.")
  }
  if (length(Text_Shifting_Method) != 1) {
    stop ("The 'Text_Shifting_Method' argument must be of length '1'.")
  }
  if (!(Text_Shifting_Method %in% c("None", "Only_Horizontally_With_Leftward_Tendancy", "Only_Horizontally_With_Rightward_Tendancy", "Mainly_Horizontally_With_Leftward_Tendancy", "Mainly_Horizontally_With_Rightward_Tendancy", "Only_Vertically_With_Upward_Tendancy", "Only_Vertically_With_Downward_Tendancy", "Mainly_Vertically_With_Upward_Tendancy", "Mainly_Vertically_With_Downward_Tendancy", "Mainly_Diagonally_Using_Text_Box_Corners_at_45_Degree_Angles_With_Leftward_Tendancy", "Mainly_Diagonally_Using_Text_Box_Corners_at_45_Degree_Angles_With_Upward_Tendancy", "Mainly_Diagonally_Using_Text_Box_Corners_at_45_Degree_Angles_With_Rightward_Tendancy", "Mainly_Diagonally_Using_Text_Box_Corners_at_45_Degree_Angles_With_Downward_Tendancy", "Mainly_Diagonally_Using_Text_Box_Corners_at_the_Same_Angles_as_the_Associated_Vectors_With_Leftward_Tendancy", "Mainly_Diagonally_Using_Text_Box_Corners_at_the_Same_Angles_as_the_Associated_Vectors_With_Upward_Tendancy", "Mainly_Diagonally_Using_Text_Box_Corners_at_the_Same_Angles_as_the_Associated_Vectors_With_Rightward_Tendancy", "Mainly_Diagonally_Using_Text_Box_Corners_at_the_Same_Angles_as_the_Associated_Vectors_With_Downward_Tendancy", "Using_Vector_Directions_With_Leftward_Tendancy", "Using_Vector_Directions_With_Upward_Tendancy", "Using_Vector_Directions_With_Rightward_Tendancy", "Using_Vector_Directions_With_Downward_Tendancy"))) {
    stop ("The 'Text_Shifting_Method' argument must be either 'None', 'Only_Horizontally_With_Leftward_Tendancy', 'Only_Horizontally_With_Rightward_Tendancy', 'Mainly_Horizontally_With_Leftward_Tendancy', 'Mainly_Horizontally_With_Rightward_Tendancy', 'Only_Vertically_With_Upward_Tendancy', 'Only_Vertically_With_Downward_Tendancy', 'Mainly_Vertically_With_Upward_Tendancy', 'Mainly_Vertically_With_Downward_Tendancy', 'Mainly_Diagonally_Using_Text_Box_Corners_at_45_Degree_Angles_With_Leftward_Tendancy', 'Mainly_Diagonally_Using_Text_Box_Corners_at_45_Degree_Angles_With_Upward_Tendancy', 'Mainly_Diagonally_Using_Text_Box_Corners_at_45_Degree_Angles_With_Rightward_Tendancy', 'Mainly_Diagonally_Using_Text_Box_Corners_at_45_Degree_Angles_With_Downward_Tendancy', 'Mainly_Diagonally_Using_Text_Box_Corners_at_the_Same_Angles_as_the_Associated_Vectors_With_Leftward_Tendancy', 'Mainly_Diagonally_Using_Text_Box_Corners_at_the_Same_Angles_as_the_Associated_Vectors_With_Upward_Tendancy', 'Mainly_Diagonally_Using_Text_Box_Corners_at_the_Same_Angles_as_the_Associated_Vectors_With_Rightward_Tendancy', 'Mainly_Diagonally_Using_Text_Box_Corners_at_the_Same_Angles_as_the_Associated_Vectors_With_Downward_Tendancy', 'Using_Vector_Directions_With_Leftward_Tendancy', 'Using_Vector_Directions_With_Upward_Tendancy', 'Using_Vector_Directions_With_Rightward_Tendancy', or 'Using_Vector_Directions_With_Downward_Tendancy'.")
  }
  Angles <- atan2(Texts_Vertical_Axis_Coordinates, Texts_Horizontal_Axis_Coordinates)
  tryCatch (dev.off(), error = function (e) {
    NULL
  })
  plot(c(Points_Vertical_Axis_Coordinates, Texts_Vertical_Axis_Coordinates) ~ c(Points_Horizontal_Axis_Coordinates, Texts_Horizontal_Axis_Coordinates), type = "n", axes = F, xlab = "", ylab = "")
  Axis_Limits <- par("usr")
  Widths <- strwidth(Texts) * Character_Expansion
  Heights <- strheight(Texts) * Character_Expansion
  Axis_Limits <- par("usr")
  Axis_Ranges <- c(diff(Axis_Limits[1:2]), diff(Axis_Limits[3:4]))
  if (Text_Shifting_Method == "None") {
    New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
      New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
      New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
      c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
    }))
  } else if (Text_Shifting_Method == "Only_Horizontally_With_Leftward_Tendancy") {
    Plotting_Window_Dimensions <- par("pin")
    Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
    Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
    Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
    New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
      if (Texts_Horizontal_Axis_Coordinates[x] <= 0) {
        New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
      } else if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
        New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
      }
      New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
      c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
    }))
  } else if (Text_Shifting_Method == "Only_Horizontally_With_Rightward_Tendancy") {
    Plotting_Window_Dimensions <- par("pin")
    Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
    Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
    Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
    New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
      if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
        New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
      } else if (Texts_Horizontal_Axis_Coordinates[x] >= 0) {
        New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
      }
      New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
      c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
    }))
  } else if (Text_Shifting_Method == "Mainly_Horizontally_With_Leftward_Tendancy") {
    Plotting_Window_Dimensions <- par("pin")
    Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
    Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
    Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
    New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
      if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
        New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
      } else if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
        New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
      } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
        if (Texts_Vertical_Axis_Coordinates[x] != 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
        } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
        }
      }
      if (Texts_Horizontal_Axis_Coordinates[x] != 0) {
        New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
      } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
        if (Texts_Vertical_Axis_Coordinates[x] > 0) {
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
        } else if (Texts_Vertical_Axis_Coordinates[x] < 0) {
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height  (Heights[x] / 2)
        } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        }
      }
      c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
    }))
  } else if (Text_Shifting_Method == "Mainly_Horizontally_With_Rightward_Tendancy") {
    Plotting_Window_Dimensions <- par("pin")
    Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
    Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
    Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
    New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
      if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
        New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
      } else if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
        New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
      } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
        if (Texts_Vertical_Axis_Coordinates[x] != 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
        } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
        }
      }
      if (Texts_Horizontal_Axis_Coordinates[x] != 0) {
        New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
      } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
        if (Texts_Vertical_Axis_Coordinates[x] > 0) {
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
        } else if (Texts_Vertical_Axis_Coordinates[x] < 0) {
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height  (Heights[x] / 2)
        } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        }
      }
      c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
    }))
  } else if (Text_Shifting_Method == "Only_Vertically_With_Upward_Tendancy") {
    Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
    New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
      New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
      if (Texts_Vertical_Axis_Coordinates[x] < 0) {
        New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
      } else if (Texts_Vertical_Axis_Coordinates[x] >= 0) {
        New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
      }
      c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
    }))
  } else if (Text_Shifting_Method == "Only_Vertically_With_Downward_Tendancy") {
    Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
    New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
      New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
      if (Texts_Vertical_Axis_Coordinates[x] <= 0) {
        New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
      } else if (Texts_Vertical_Axis_Coordinates[x] > 0) {
        New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
      }
      c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
    }))
  } else if (Text_Shifting_Method == "Mainly_Vertically_With_Upward_Tendancy") {
    Plotting_Window_Dimensions <- par("pin")
    Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
    Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
    Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
    New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
      if (Texts_Vertical_Axis_Coordinates[x] != 0) {
        New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
      } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
        }
      }
      if (Texts_Vertical_Axis_Coordinates[x] < 0) {
        New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
      } else if (Texts_Vertical_Axis_Coordinates[x] > 0) {
        New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
      } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] != 0) {
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
        }
      }
      c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
    }))
  } else if (Text_Shifting_Method == "Mainly_Vertically_With_Downward_Tendancy") {
    Plotting_Window_Dimensions <- par("pin")
    Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
    Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
    Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
    New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
      if (Texts_Vertical_Axis_Coordinates[x] != 0) {
        New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
      } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
        }
      }
      if (Texts_Vertical_Axis_Coordinates[x] < 0) {
        New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
      } else if (Texts_Vertical_Axis_Coordinates[x] > 0) {
        New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
      } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] != 0) {
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
        }
      }
      c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
    }))
  } else if (Text_Shifting_Method == "Mainly_Diagonally_Using_Text_Box_Corners_at_45_Degree_Angles_With_Leftward_Tendancy") {
    Plotting_Window_Dimensions <- par("pin")
    Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
    Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
    Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
    New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
      if (Texts_Vertical_Axis_Coordinates[x] < 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(7 * pi / 4)) + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(7 * pi / 4)) - (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(5 * pi / 4)) - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(5 * pi / 4)) - (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
        }
      } else if (Texts_Vertical_Axis_Coordinates[x] > 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(pi / 4)) + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(pi / 4)) + (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(3 * pi / 4)) - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(3 * pi / 4)) + (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
        }
      } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        }
      }
      c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
    }))
  } else if (Text_Shifting_Method == "Mainly_Diagonally_Using_Text_Box_Corners_at_45_Degree_Angles_With_Upward_Tendancy") {
    Plotting_Window_Dimensions <- par("pin")
    Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
    Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
    Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
    New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
      if (Texts_Vertical_Axis_Coordinates[x] < 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(7 * pi / 4)) + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(7 * pi / 4)) - (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(5 * pi / 4)) - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(5 * pi / 4)) - (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
        }
      } else if (Texts_Vertical_Axis_Coordinates[x] > 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(pi / 4)) + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(pi / 4)) + (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(3 * pi / 4)) - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(3 * pi / 4)) + (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
        }
      } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
        }
      }
      c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
    }))
  } else if (Text_Shifting_Method == "Mainly_Diagonally_Using_Text_Box_Corners_at_45_Degree_Angles_With_Rightward_Tendancy") {
    Plotting_Window_Dimensions <- par("pin")
    Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
    Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
    Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
    New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
      if (Texts_Vertical_Axis_Coordinates[x] < 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(7 * pi / 4)) + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(7 * pi / 4)) - (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(5 * pi / 4)) - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(5 * pi / 4)) - (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
        }
      } else if (Texts_Vertical_Axis_Coordinates[x] > 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(pi / 4)) + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(pi / 4)) + (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(3 * pi / 4)) - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(3 * pi / 4)) + (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
        }
      } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        }
      }
      c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
    }))
  } else if (Text_Shifting_Method == "Mainly_Diagonally_Using_Text_Box_Corners_at_45_Degree_Angles_With_Downward_Tendancy") {
    Plotting_Window_Dimensions <- par("pin")
    Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
    Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
    Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
    New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
      if (Texts_Vertical_Axis_Coordinates[x] < 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(7 * pi / 4)) + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(7 * pi / 4)) - (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(5 * pi / 4)) - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(5 * pi / 4)) - (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
        }
      } else if (Texts_Vertical_Axis_Coordinates[x] > 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(pi / 4)) + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(pi / 4)) + (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(3 * pi / 4)) - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(3 * pi / 4)) + (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
        }
      } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
        }
      }
      c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
    }))
  } else if (Text_Shifting_Method == "Mainly_Diagonally_Using_Text_Box_Corners_at_the_Same_Angles_as_the_Associated_Vectors_With_Leftward_Tendancy") {
    Plotting_Window_Dimensions <- par("pin")
    Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
    Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
    Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
    New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
      if (Texts_Vertical_Axis_Coordinates[x] < 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) - (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) - (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
        }
      } else if (Texts_Vertical_Axis_Coordinates[x] > 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) + (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) + (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
        }
      } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        }
      }
      c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
    }))
  } else if (Text_Shifting_Method == "Mainly_Diagonally_Using_Text_Box_Corners_at_the_Same_Angles_as_the_Associated_Vectors_With_Upward_Tendancy") {
    Plotting_Window_Dimensions <- par("pin")
    Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
    Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
    Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
    New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
      if (Texts_Vertical_Axis_Coordinates[x] < 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) - (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) - (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
        }
      } else if (Texts_Vertical_Axis_Coordinates[x] > 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) + (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) + (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
        }
      } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
        }
      }
      c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
    }))
  } else if (Text_Shifting_Method == "Mainly_Diagonally_Using_Text_Box_Corners_at_the_Same_Angles_as_the_Associated_Vectors_With_Rightward_Tendancy") {
    Plotting_Window_Dimensions <- par("pin")
    Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
    Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
    Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
    New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
      if (Texts_Vertical_Axis_Coordinates[x] < 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) - (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) - (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
        }
      } else if (Texts_Vertical_Axis_Coordinates[x] > 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) + (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) + (Heights[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
        }
      } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
        if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        }
      }
      c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
    }))
  } else if (Text_Shifting_Method == "Mainly_Diagonally_Using_Text_Box_Corners_at_the_Same_Angles_as_the_Associated_Vectors_With_Downward_Tendancy") {
    Plotting_Window_Dimensions <- par("pin")
    Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
    Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
    Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
    New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
      if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
        if (Texts_Vertical_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) + (Heights[x] / 2)
        } else if (Texts_Vertical_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) - (Heights[x] / 2)
        } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
        }
      } else if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
        if (Texts_Vertical_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) + (Heights[x] / 2)
        } else if (Texts_Vertical_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) - (Heights[x] / 2)
        } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
        }
      } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
        if (Texts_Vertical_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
        } else if (Texts_Vertical_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
        } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
        }
      }
      c(New_Texts_Horizontal_Axis_Coordinate = New_Texts_Horizontal_Axis_Coordinate, New_Texts_Vertical_Axis_Coordinate = New_Texts_Vertical_Axis_Coordinate)
    }))
  } else if (Text_Shifting_Method == "Using_Vector_Directions_With_Leftward_Tendancy") {
    Plotting_Window_Dimensions <- par("pin")
    Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
    Horizontal_Axis_Conversion_Factor <- Plotting_Window_Dimensions[1] / Axis_Ranges[1]
    Vertical_Axis_Conversion_Factor <- Plotting_Window_Dimensions[2] / Axis_Ranges[2]
    Gap_Length_in_Inches <- Gap_Height * Vertical_Axis_Conversion_Factor
    Gap_Width <- Gap_Length_in_Inches / Horizontal_Axis_Conversion_Factor
    New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
      if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
        New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
        New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
      } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
        New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
        New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
      } else if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
        New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
        New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
      } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
        New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
        New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
      } else if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
        New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
        New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
      } else if ((Texts_Horizontal_Axis_Coordinates[x] != 0) & (Texts_Vertical_Axis_Coordinates[x] != 0)) {
        if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
          Temporary_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
          Temporary_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
          Temporary_Angle <- Angles[x]
        } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
          Temporary_Horizontal_Axis_Coordinate <- -Texts_Horizontal_Axis_Coordinates[x]
          Temporary_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
          Temporary_Angle <- (pi / 2) - (Angles[x] - (pi / 2))
        } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
          Temporary_Horizontal_Axis_Coordinate <- -Texts_Horizontal_Axis_Coordinates[x]
          Temporary_Vertical_Axis_Coordinate <- -Texts_Vertical_Axis_Coordinates[x]
          Temporary_Angle <- Angles[x] + pi
        } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
          Temporary_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
          Temporary_Vertical_Axis_Coordinate <- -Texts_Vertical_Axis_Coordinates[x]
          Temporary_Angle <- abs(Angles[x])
        }
        Rectangle_Angle <- atan2((Heights[x] / 2), (Widths[x] / 2))
        Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner <- atan2((Temporary_Vertical_Axis_Coordinate + Gap_Height + (Heights[x] / 2)), (Temporary_Horizontal_Axis_Coordinate + (Widths[x] / 2)))
        Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner <- atan2((Temporary_Vertical_Axis_Coordinate + (Heights[x] / 2)), (Temporary_Horizontal_Axis_Coordinate + Gap_Width + (Widths[x] / 2)))
        if (Temporary_Angle >= Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner) {
          New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + (Gap_Height / tan(Temporary_Angle)) + ((Heights[x] / 2) / tan(Temporary_Angle))
          New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Gap_Height + (Heights[x] / 2)
        } else if ((Temporary_Angle < Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner) & (Temporary_Angle > Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner)) {
          if (Rectangle_Angle < Temporary_Angle) {
            Text_Box_Width_in_Inches <- Horizontal_Axis_Conversion_Factor * Widths[x]
            Text_Box_Height_in_Inches <- Vertical_Axis_Conversion_Factor * Heights[x]
            New_Angle <- Temporary_Angle + asin(((Text_Box_Width_in_Inches / 2) - (Text_Box_Height_in_Inches / 2) / tan(Temporary_Angle)) * sin(Temporary_Angle) / Gap_Length_in_Inches)
            Horizontal_Component_in_Inches <- Gap_Length_in_Inches * cos(New_Angle)
            Vertical_Component_in_Inches <- Gap_Length_in_Inches * sin(New_Angle)
            Horizontal_Component <- Horizontal_Component_in_Inches / Horizontal_Axis_Conversion_Factor
            Vertical_Component <- Vertical_Component_in_Inches / Vertical_Axis_Conversion_Factor
            New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Horizontal_Component + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Vertical_Component + (Heights[x] / 2)
          } else if (Rectangle_Angle == Temporary_Angle) {
            New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + (Gap_Width * cos(Temporary_Angle)) + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + (Gap_Height * sin(Temporary_Angle)) + (Heights[x] / 2)
          } else if (Rectangle_Angle > Temporary_Angle) {
            Text_Box_Width_in_Inches <- Horizontal_Axis_Conversion_Factor * Widths[x]
            Text_Box_Height_in_Inches <- Vertical_Axis_Conversion_Factor * Heights[x]
            Other_Angle <- asin(((Text_Box_Height_in_Inches / 2) - (Text_Box_Width_in_Inches * tan(Temporary_Angle) / 2)) * sin((pi / 2) - Temporary_Angle) / Gap_Length_in_Inches)
            New_Angle <- ((pi / 2) - Temporary_Angle) + Other_Angle
            Horizontal_Component_in_Inches <- Gap_Length_in_Inches * sin(New_Angle)
            Vertical_Component_in_Inches <- Gap_Length_in_Inches * cos(New_Angle)
            Horizontal_Component <- Horizontal_Component_in_Inches / Horizontal_Axis_Conversion_Factor
            Vertical_Component <- Vertical_Component_in_Inches / Vertical_Axis_Conversion_Factor
            New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Horizontal_Component + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Vertical_Component + (Heights[x] / 2)
          }
        } else if (Temporary_Angle <= Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner) {
          New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Gap_Width + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + (Gap_Width * tan(Temporary_Angle)) + ((Widths[x] / 2) * tan(Temporary_Angle))
        }
        if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- New_Texts_Horizontal_Axis_Coordinate
          New_Texts_Vertical_Axis_Coordinate <- New_Texts_Vertical_Axis_Coordinate
        } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- -New_Texts_Horizontal_Axis_Coordinate
          New_Texts_Vertical_Axis_Coordinate <- New_Texts_Vertical_Axis_Coordinate
        } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- -New_Texts_Horizontal_Axis_Coordinate
          New_Texts_Vertical_Axis_Coordinate <- -New_Texts_Vertical_Axis_Coordinate
        } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- New_Texts_Horizontal_Axis_Coordinate
          New_Texts_Vertical_Axis_Coordinate <- -New_Texts_Vertical_Axis_Coordinate
        }
      }
      c(New_Texts_Horizontal_Axis_Coordinate = New_Texts_Horizontal_Axis_Coordinate, New_Texts_Vertical_Axis_Coordinate = New_Texts_Vertical_Axis_Coordinate)
    }))
  } else if (Text_Shifting_Method == "Using_Vector_Directions_With_Upward_Tendancy") {
    Plotting_Window_Dimensions <- par("pin")
    Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
    Horizontal_Axis_Conversion_Factor <- Plotting_Window_Dimensions[1] / Axis_Ranges[1]
    Vertical_Axis_Conversion_Factor <- Plotting_Window_Dimensions[2] / Axis_Ranges[2]
    Gap_Length_in_Inches <- Gap_Height * Vertical_Axis_Conversion_Factor
    Gap_Width <- Gap_Length_in_Inches / Horizontal_Axis_Conversion_Factor
    New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
      if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
        New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
        New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
      } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
        New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
        New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
      } else if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
        New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
        New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
      } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
        New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
        New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
      } else if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
        New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
        New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
      } else if ((Texts_Horizontal_Axis_Coordinates[x] != 0) & (Texts_Vertical_Axis_Coordinates[x] != 0)) {
        if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
          Temporary_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
          Temporary_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
          Temporary_Angle <- Angles[x]
        } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
          Temporary_Horizontal_Axis_Coordinate <- -Texts_Horizontal_Axis_Coordinates[x]
          Temporary_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
          Temporary_Angle <- (pi / 2) - (Angles[x] - (pi / 2))
        } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
          Temporary_Horizontal_Axis_Coordinate <- -Texts_Horizontal_Axis_Coordinates[x]
          Temporary_Vertical_Axis_Coordinate <- -Texts_Vertical_Axis_Coordinates[x]
          Temporary_Angle <- Angles[x] + pi
        } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
          Temporary_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
          Temporary_Vertical_Axis_Coordinate <- -Texts_Vertical_Axis_Coordinates[x]
          Temporary_Angle <- abs(Angles[x])
        }
        Rectangle_Angle <- atan2((Heights[x] / 2), (Widths[x] / 2))
        Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner <- atan2((Temporary_Vertical_Axis_Coordinate + Gap_Height + (Heights[x] / 2)), (Temporary_Horizontal_Axis_Coordinate + (Widths[x] / 2)))
        Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner <- atan2((Temporary_Vertical_Axis_Coordinate + (Heights[x] / 2)), (Temporary_Horizontal_Axis_Coordinate + Gap_Width + (Widths[x] / 2)))
        if (Temporary_Angle >= Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner) {
          New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + (Gap_Height / tan(Temporary_Angle)) + ((Heights[x] / 2) / tan(Temporary_Angle))
          New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Gap_Height + (Heights[x] / 2)
        } else if ((Temporary_Angle < Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner) & (Temporary_Angle > Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner)) {
          if (Rectangle_Angle < Temporary_Angle) {
            Text_Box_Width_in_Inches <- Horizontal_Axis_Conversion_Factor * Widths[x]
            Text_Box_Height_in_Inches <- Vertical_Axis_Conversion_Factor * Heights[x]
            New_Angle <- Temporary_Angle + asin(((Text_Box_Width_in_Inches / 2) - (Text_Box_Height_in_Inches / 2) / tan(Temporary_Angle)) * sin(Temporary_Angle) / Gap_Length_in_Inches)
            Horizontal_Component_in_Inches <- Gap_Length_in_Inches * cos(New_Angle)
            Vertical_Component_in_Inches <- Gap_Length_in_Inches * sin(New_Angle)
            Horizontal_Component <- Horizontal_Component_in_Inches / Horizontal_Axis_Conversion_Factor
            Vertical_Component <- Vertical_Component_in_Inches / Vertical_Axis_Conversion_Factor
            New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Horizontal_Component + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Vertical_Component + (Heights[x] / 2)
          } else if (Rectangle_Angle == Temporary_Angle) {
            New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + (Gap_Width * cos(Temporary_Angle)) + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + (Gap_Height * sin(Temporary_Angle)) + (Heights[x] / 2)
          } else if (Rectangle_Angle > Temporary_Angle) {
            Text_Box_Width_in_Inches <- Horizontal_Axis_Conversion_Factor * Widths[x]
            Text_Box_Height_in_Inches <- Vertical_Axis_Conversion_Factor * Heights[x]
            Other_Angle <- asin(((Text_Box_Height_in_Inches / 2) - (Text_Box_Width_in_Inches * tan(Temporary_Angle) / 2)) * sin((pi / 2) - Temporary_Angle) / Gap_Length_in_Inches)
            New_Angle <- ((pi / 2) - Temporary_Angle) + Other_Angle
            Horizontal_Component_in_Inches <- Gap_Length_in_Inches * sin(New_Angle)
            Vertical_Component_in_Inches <- Gap_Length_in_Inches * cos(New_Angle)
            Horizontal_Component <- Horizontal_Component_in_Inches / Horizontal_Axis_Conversion_Factor
            Vertical_Component <- Vertical_Component_in_Inches / Vertical_Axis_Conversion_Factor
            New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Horizontal_Component + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Vertical_Component + (Heights[x] / 2)
          }
        } else if (Temporary_Angle <= Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner) {
          New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Gap_Width + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + (Gap_Width * tan(Temporary_Angle)) + ((Widths[x] / 2) * tan(Temporary_Angle))
        }
        if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- New_Texts_Horizontal_Axis_Coordinate
          New_Texts_Vertical_Axis_Coordinate <- New_Texts_Vertical_Axis_Coordinate
        } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- -New_Texts_Horizontal_Axis_Coordinate
          New_Texts_Vertical_Axis_Coordinate <- New_Texts_Vertical_Axis_Coordinate
        } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- -New_Texts_Horizontal_Axis_Coordinate
          New_Texts_Vertical_Axis_Coordinate <- -New_Texts_Vertical_Axis_Coordinate
        } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- New_Texts_Horizontal_Axis_Coordinate
          New_Texts_Vertical_Axis_Coordinate <- -New_Texts_Vertical_Axis_Coordinate
        }
      }
      c(New_Texts_Horizontal_Axis_Coordinate = New_Texts_Horizontal_Axis_Coordinate, New_Texts_Vertical_Axis_Coordinate = New_Texts_Vertical_Axis_Coordinate)
    }))
  } else if (Text_Shifting_Method == "Using_Vector_Directions_With_Rightward_Tendancy") {
    Plotting_Window_Dimensions <- par("pin")
    Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
    Horizontal_Axis_Conversion_Factor <- Plotting_Window_Dimensions[1] / Axis_Ranges[1]
    Vertical_Axis_Conversion_Factor <- Plotting_Window_Dimensions[2] / Axis_Ranges[2]
    Gap_Length_in_Inches <- Gap_Height * Vertical_Axis_Conversion_Factor
    Gap_Width <- Gap_Length_in_Inches / Horizontal_Axis_Conversion_Factor
    New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
      if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
        New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
        New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
      } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
        New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
        New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
      } else if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
        New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
        New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
      } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
        New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
        New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
      } else if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
        New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
        New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
      } else if ((Texts_Horizontal_Axis_Coordinates[x] != 0) & (Texts_Vertical_Axis_Coordinates[x] != 0)) {
        if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
          Temporary_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
          Temporary_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
          Temporary_Angle <- Angles[x]
        } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
          Temporary_Horizontal_Axis_Coordinate <- -Texts_Horizontal_Axis_Coordinates[x]
          Temporary_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
          Temporary_Angle <- (pi / 2) - (Angles[x] - (pi / 2))
        } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
          Temporary_Horizontal_Axis_Coordinate <- -Texts_Horizontal_Axis_Coordinates[x]
          Temporary_Vertical_Axis_Coordinate <- -Texts_Vertical_Axis_Coordinates[x]
          Temporary_Angle <- Angles[x] + pi
        } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
          Temporary_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
          Temporary_Vertical_Axis_Coordinate <- -Texts_Vertical_Axis_Coordinates[x]
          Temporary_Angle <- abs(Angles[x])
        }
        Rectangle_Angle <- atan2((Heights[x] / 2), (Widths[x] / 2))
        Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner <- atan2((Temporary_Vertical_Axis_Coordinate + Gap_Height + (Heights[x] / 2)), (Temporary_Horizontal_Axis_Coordinate + (Widths[x] / 2)))
        Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner <- atan2((Temporary_Vertical_Axis_Coordinate + (Heights[x] / 2)), (Temporary_Horizontal_Axis_Coordinate + Gap_Width + (Widths[x] / 2)))
        if (Temporary_Angle >= Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner) {
          New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + (Gap_Height / tan(Temporary_Angle)) + ((Heights[x] / 2) / tan(Temporary_Angle))
          New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Gap_Height + (Heights[x] / 2)
        } else if ((Temporary_Angle < Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner) & (Temporary_Angle > Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner)) {
          if (Rectangle_Angle < Temporary_Angle) {
            Text_Box_Width_in_Inches <- Horizontal_Axis_Conversion_Factor * Widths[x]
            Text_Box_Height_in_Inches <- Vertical_Axis_Conversion_Factor * Heights[x]
            New_Angle <- Temporary_Angle + asin(((Text_Box_Width_in_Inches / 2) - (Text_Box_Height_in_Inches / 2) / tan(Temporary_Angle)) * sin(Temporary_Angle) / Gap_Length_in_Inches)
            Horizontal_Component_in_Inches <- Gap_Length_in_Inches * cos(New_Angle)
            Vertical_Component_in_Inches <- Gap_Length_in_Inches * sin(New_Angle)
            Horizontal_Component <- Horizontal_Component_in_Inches / Horizontal_Axis_Conversion_Factor
            Vertical_Component <- Vertical_Component_in_Inches / Vertical_Axis_Conversion_Factor
            New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Horizontal_Component + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Vertical_Component + (Heights[x] / 2)
          } else if (Rectangle_Angle == Temporary_Angle) {
            New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + (Gap_Width * cos(Temporary_Angle)) + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + (Gap_Height * sin(Temporary_Angle)) + (Heights[x] / 2)
          } else if (Rectangle_Angle > Temporary_Angle) {
            Text_Box_Width_in_Inches <- Horizontal_Axis_Conversion_Factor * Widths[x]
            Text_Box_Height_in_Inches <- Vertical_Axis_Conversion_Factor * Heights[x]
            Other_Angle <- asin(((Text_Box_Height_in_Inches / 2) - (Text_Box_Width_in_Inches * tan(Temporary_Angle) / 2)) * sin((pi / 2) - Temporary_Angle) / Gap_Length_in_Inches)
            New_Angle <- ((pi / 2) - Temporary_Angle) + Other_Angle
            Horizontal_Component_in_Inches <- Gap_Length_in_Inches * sin(New_Angle)
            Vertical_Component_in_Inches <- Gap_Length_in_Inches * cos(New_Angle)
            Horizontal_Component <- Horizontal_Component_in_Inches / Horizontal_Axis_Conversion_Factor
            Vertical_Component <- Vertical_Component_in_Inches / Vertical_Axis_Conversion_Factor
            New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Horizontal_Component + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Vertical_Component + (Heights[x] / 2)
          }
        } else if (Temporary_Angle <= Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner) {
          New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Gap_Width + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + (Gap_Width * tan(Temporary_Angle)) + ((Widths[x] / 2) * tan(Temporary_Angle))
        }
        if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- New_Texts_Horizontal_Axis_Coordinate
          New_Texts_Vertical_Axis_Coordinate <- New_Texts_Vertical_Axis_Coordinate
        } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- -New_Texts_Horizontal_Axis_Coordinate
          New_Texts_Vertical_Axis_Coordinate <- New_Texts_Vertical_Axis_Coordinate
        } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- -New_Texts_Horizontal_Axis_Coordinate
          New_Texts_Vertical_Axis_Coordinate <- -New_Texts_Vertical_Axis_Coordinate
        } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- New_Texts_Horizontal_Axis_Coordinate
          New_Texts_Vertical_Axis_Coordinate <- -New_Texts_Vertical_Axis_Coordinate
        }
      }
      c(New_Texts_Horizontal_Axis_Coordinate = New_Texts_Horizontal_Axis_Coordinate, New_Texts_Vertical_Axis_Coordinate = New_Texts_Vertical_Axis_Coordinate)
    }))
  } else if (Text_Shifting_Method == "Using_Vector_Directions_With_Downward_Tendancy") {
    Plotting_Window_Dimensions <- par("pin")
    Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
    Horizontal_Axis_Conversion_Factor <- Plotting_Window_Dimensions[1] / Axis_Ranges[1]
    Vertical_Axis_Conversion_Factor <- Plotting_Window_Dimensions[2] / Axis_Ranges[2]
    Gap_Length_in_Inches <- Gap_Height * Vertical_Axis_Conversion_Factor
    Gap_Width <- Gap_Length_in_Inches / Horizontal_Axis_Conversion_Factor
    New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
      if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
        New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
        New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
      } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
        New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
        New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
      } else if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
        New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
        New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
      } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
        New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
        New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
      } else if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
        New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
        New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
      } else if ((Texts_Horizontal_Axis_Coordinates[x] != 0) & (Texts_Vertical_Axis_Coordinates[x] != 0)) {
        if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
          Temporary_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
          Temporary_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
          Temporary_Angle <- Angles[x]
        } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
          Temporary_Horizontal_Axis_Coordinate <- -Texts_Horizontal_Axis_Coordinates[x]
          Temporary_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
          Temporary_Angle <- (pi / 2) - (Angles[x] - (pi / 2))
        } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
          Temporary_Horizontal_Axis_Coordinate <- -Texts_Horizontal_Axis_Coordinates[x]
          Temporary_Vertical_Axis_Coordinate <- -Texts_Vertical_Axis_Coordinates[x]
          Temporary_Angle <- Angles[x] + pi
        } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
          Temporary_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
          Temporary_Vertical_Axis_Coordinate <- -Texts_Vertical_Axis_Coordinates[x]
          Temporary_Angle <- abs(Angles[x])
        }
        Rectangle_Angle <- atan2((Heights[x] / 2), (Widths[x] / 2))
        Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner <- atan2((Temporary_Vertical_Axis_Coordinate + Gap_Height + (Heights[x] / 2)), (Temporary_Horizontal_Axis_Coordinate + (Widths[x] / 2)))
        Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner <- atan2((Temporary_Vertical_Axis_Coordinate + (Heights[x] / 2)), (Temporary_Horizontal_Axis_Coordinate + Gap_Width + (Widths[x] / 2)))
        if (Temporary_Angle >= Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner) {
          New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + (Gap_Height / tan(Temporary_Angle)) + ((Heights[x] / 2) / tan(Temporary_Angle))
          New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Gap_Height + (Heights[x] / 2)
        } else if ((Temporary_Angle < Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner) & (Temporary_Angle > Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner)) {
          if (Rectangle_Angle < Temporary_Angle) {
            Text_Box_Width_in_Inches <- Horizontal_Axis_Conversion_Factor * Widths[x]
            Text_Box_Height_in_Inches <- Vertical_Axis_Conversion_Factor * Heights[x]
            New_Angle <- Temporary_Angle + asin(((Text_Box_Width_in_Inches / 2) - (Text_Box_Height_in_Inches / 2) / tan(Temporary_Angle)) * sin(Temporary_Angle) / Gap_Length_in_Inches)
            Horizontal_Component_in_Inches <- Gap_Length_in_Inches * cos(New_Angle)
            Vertical_Component_in_Inches <- Gap_Length_in_Inches * sin(New_Angle)
            Horizontal_Component <- Horizontal_Component_in_Inches / Horizontal_Axis_Conversion_Factor
            Vertical_Component <- Vertical_Component_in_Inches / Vertical_Axis_Conversion_Factor
            New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Horizontal_Component + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Vertical_Component + (Heights[x] / 2)
          } else if (Rectangle_Angle == Temporary_Angle) {
            New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + (Gap_Width * cos(Temporary_Angle)) + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + (Gap_Height * sin(Temporary_Angle)) + (Heights[x] / 2)
          } else if (Rectangle_Angle > Temporary_Angle) {
            Text_Box_Width_in_Inches <- Horizontal_Axis_Conversion_Factor * Widths[x]
            Text_Box_Height_in_Inches <- Vertical_Axis_Conversion_Factor * Heights[x]
            Other_Angle <- asin(((Text_Box_Height_in_Inches / 2) - (Text_Box_Width_in_Inches * tan(Temporary_Angle) / 2)) * sin((pi / 2) - Temporary_Angle) / Gap_Length_in_Inches)
            New_Angle <- ((pi / 2) - Temporary_Angle) + Other_Angle
            Horizontal_Component_in_Inches <- Gap_Length_in_Inches * sin(New_Angle)
            Vertical_Component_in_Inches <- Gap_Length_in_Inches * cos(New_Angle)
            Horizontal_Component <- Horizontal_Component_in_Inches / Horizontal_Axis_Conversion_Factor
            Vertical_Component <- Vertical_Component_in_Inches / Vertical_Axis_Conversion_Factor
            New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Horizontal_Component + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Vertical_Component + (Heights[x] / 2)
          }
        } else if (Temporary_Angle <= Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner) {
          New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Gap_Width + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + (Gap_Width * tan(Temporary_Angle)) + ((Widths[x] / 2) * tan(Temporary_Angle))
        }
        if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- New_Texts_Horizontal_Axis_Coordinate
          New_Texts_Vertical_Axis_Coordinate <- New_Texts_Vertical_Axis_Coordinate
        } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- -New_Texts_Horizontal_Axis_Coordinate
          New_Texts_Vertical_Axis_Coordinate <- New_Texts_Vertical_Axis_Coordinate
        } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- -New_Texts_Horizontal_Axis_Coordinate
          New_Texts_Vertical_Axis_Coordinate <- -New_Texts_Vertical_Axis_Coordinate
        } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- New_Texts_Horizontal_Axis_Coordinate
          New_Texts_Vertical_Axis_Coordinate <- -New_Texts_Vertical_Axis_Coordinate
        }
      }
      c(New_Texts_Horizontal_Axis_Coordinate = New_Texts_Horizontal_Axis_Coordinate, New_Texts_Vertical_Axis_Coordinate = New_Texts_Vertical_Axis_Coordinate)
    }))
  }
  rownames(New_Texts_Axis_Coordinates) <- Texts
  Text_Box_Corner_Data_Frame <- do.call("rbind", lapply(seq_len(length(Texts)), function (x) {
    if (Should_Text_Box_Polygons_Shade_out_Points == T) {
      Horizontal_Coordinates <- c((New_Texts_Axis_Coordinates[x, "New_Texts_Horizontal_Axis_Coordinate"] - Gap_Width - (Widths[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Horizontal_Axis_Coordinate"] + Gap_Width + (Widths[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Horizontal_Axis_Coordinate"] + Gap_Width + (Widths[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Horizontal_Axis_Coordinate"] - Gap_Width - (Widths[x] / 2)))
      Vertical_Coordinates <- c((New_Texts_Axis_Coordinates[x, "New_Texts_Vertical_Axis_Coordinate"] + Gap_Height + (Heights[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Vertical_Axis_Coordinate"] + Gap_Height + (Heights[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Vertical_Axis_Coordinate"] - Gap_Height - (Heights[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Vertical_Axis_Coordinate"] - Gap_Height - (Heights[x] / 2)))
    } else if (Should_Text_Box_Polygons_Shade_out_Points == F) {
      Horizontal_Coordinates <- c((New_Texts_Axis_Coordinates[x, "New_Texts_Horizontal_Axis_Coordinate"] - (Widths[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Horizontal_Axis_Coordinate"] + (Widths[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Horizontal_Axis_Coordinate"] + (Widths[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Horizontal_Axis_Coordinate"] - (Widths[x] / 2)))
      Vertical_Coordinates <- c((New_Texts_Axis_Coordinates[x, "New_Texts_Vertical_Axis_Coordinate"] + (Heights[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Vertical_Axis_Coordinate"] + (Heights[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Vertical_Axis_Coordinate"] - (Heights[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Vertical_Axis_Coordinate"] - (Heights[x] / 2)))
    }
    Data_Frame <- data.frame(Horizontal_Coordinates = Horizontal_Coordinates, Vertical_Coordinates = Vertical_Coordinates)
    Data_Frame$Text <- rep(Texts[x], nrow(Data_Frame))
    Data_Frame$Corner <- c("Upper Left", "Upper Right", "Lower Right", "Lower Left")
    Data_Frame[, c(which(colnames(Data_Frame) == "Text"), which(colnames(Data_Frame) == "Corner"), which(colnames(Data_Frame) == "Horizontal_Coordinates"), which(colnames(Data_Frame) == "Vertical_Coordinates"))]
  }))
  plot(c(Points_Vertical_Axis_Coordinates, Texts_Vertical_Axis_Coordinates, Text_Box_Corner_Data_Frame$Vertical_Coordinates) ~ c(Points_Horizontal_Axis_Coordinates, Texts_Horizontal_Axis_Coordinates, Text_Box_Corner_Data_Frame$Horizontal_Coordinates), type = "n", axes = F, xlab = "", ylab = "")
  New_Axis_Limits <- par("usr")
  Plotting_Text_Box_Corners_Iteratively <- function () {
    Widths <- strwidth(Texts) * Character_Expansion
    Heights <- strheight(Texts) * Character_Expansion
    Axis_Limits <- par("usr")
    Axis_Ranges <- c(diff(Axis_Limits[1:2]), diff(Axis_Limits[3:4]))
    if (Text_Shifting_Method == "None") {
      New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
        New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
        New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
      }))
    } else if (Text_Shifting_Method == "Only_Horizontally_With_Leftward_Tendancy") {
      Plotting_Window_Dimensions <- par("pin")
      Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
      Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
      Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
      New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
        if (Texts_Horizontal_Axis_Coordinates[x] <= 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
        }
        New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
      }))
    } else if (Text_Shifting_Method == "Only_Horizontally_With_Rightward_Tendancy") {
      Plotting_Window_Dimensions <- par("pin")
      Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
      Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
      Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
      New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
        if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] >= 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
        }
        New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
      }))
    } else if (Text_Shifting_Method == "Mainly_Horizontally_With_Leftward_Tendancy") {
      Plotting_Window_Dimensions <- par("pin")
      Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
      Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
      Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
      New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
        if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          if (Texts_Vertical_Axis_Coordinates[x] != 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
          } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
          }
        }
        if (Texts_Horizontal_Axis_Coordinates[x] != 0) {
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          if (Texts_Vertical_Axis_Coordinates[x] > 0) {
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
          } else if (Texts_Vertical_Axis_Coordinates[x] < 0) {
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height  (Heights[x] / 2)
          } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
          }
        }
        c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
      }))
    } else if (Text_Shifting_Method == "Mainly_Horizontally_With_Rightward_Tendancy") {
      Plotting_Window_Dimensions <- par("pin")
      Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
      Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
      Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
      New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
        if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          if (Texts_Vertical_Axis_Coordinates[x] != 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
          } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
          }
        }
        if (Texts_Horizontal_Axis_Coordinates[x] != 0) {
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          if (Texts_Vertical_Axis_Coordinates[x] > 0) {
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
          } else if (Texts_Vertical_Axis_Coordinates[x] < 0) {
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height  (Heights[x] / 2)
          } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
          }
        }
        c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
      }))
    } else if (Text_Shifting_Method == "Only_Vertically_With_Upward_Tendancy") {
      Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
      New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
        New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
        if (Texts_Vertical_Axis_Coordinates[x] < 0) {
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
        } else if (Texts_Vertical_Axis_Coordinates[x] >= 0) {
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
        }
        c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
      }))
    } else if (Text_Shifting_Method == "Only_Vertically_With_Downward_Tendancy") {
      Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
      New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
        New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
        if (Texts_Vertical_Axis_Coordinates[x] <= 0) {
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
        } else if (Texts_Vertical_Axis_Coordinates[x] > 0) {
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
        }
        c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
      }))
    } else if (Text_Shifting_Method == "Mainly_Vertically_With_Upward_Tendancy") {
      Plotting_Window_Dimensions <- par("pin")
      Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
      Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
      Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
      New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
        if (Texts_Vertical_Axis_Coordinates[x] != 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
        } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
          }
        }
        if (Texts_Vertical_Axis_Coordinates[x] < 0) {
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
        } else if (Texts_Vertical_Axis_Coordinates[x] > 0) {
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
        } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] != 0) {
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
          }
        }
        c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
      }))
    } else if (Text_Shifting_Method == "Mainly_Vertically_With_Downward_Tendancy") {
      Plotting_Window_Dimensions <- par("pin")
      Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
      Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
      Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
      New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
        if (Texts_Vertical_Axis_Coordinates[x] != 0) {
          New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
        } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
          }
        }
        if (Texts_Vertical_Axis_Coordinates[x] < 0) {
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
        } else if (Texts_Vertical_Axis_Coordinates[x] > 0) {
          New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
        } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] != 0) {
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
          }
        }
        c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
      }))
    } else if (Text_Shifting_Method == "Mainly_Diagonally_Using_Text_Box_Corners_at_45_Degree_Angles_With_Leftward_Tendancy") {
      Plotting_Window_Dimensions <- par("pin")
      Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
      Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
      Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
      New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
        if (Texts_Vertical_Axis_Coordinates[x] < 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(7 * pi / 4)) + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(7 * pi / 4)) - (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(5 * pi / 4)) - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(5 * pi / 4)) - (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
          }
        } else if (Texts_Vertical_Axis_Coordinates[x] > 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(pi / 4)) + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(pi / 4)) + (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(3 * pi / 4)) - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(3 * pi / 4)) + (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
          }
        } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
          } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
          }
        }
        c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
      }))
    } else if (Text_Shifting_Method == "Mainly_Diagonally_Using_Text_Box_Corners_at_45_Degree_Angles_With_Upward_Tendancy") {
      Plotting_Window_Dimensions <- par("pin")
      Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
      Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
      Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
      New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
        if (Texts_Vertical_Axis_Coordinates[x] < 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(7 * pi / 4)) + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(7 * pi / 4)) - (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(5 * pi / 4)) - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(5 * pi / 4)) - (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
          }
        } else if (Texts_Vertical_Axis_Coordinates[x] > 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(pi / 4)) + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(pi / 4)) + (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(3 * pi / 4)) - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(3 * pi / 4)) + (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
          }
        } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
          } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
          }
        }
        c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
      }))
    } else if (Text_Shifting_Method == "Mainly_Diagonally_Using_Text_Box_Corners_at_45_Degree_Angles_With_Rightward_Tendancy") {
      Plotting_Window_Dimensions <- par("pin")
      Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
      Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
      Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
      New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
        if (Texts_Vertical_Axis_Coordinates[x] < 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(7 * pi / 4)) + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(7 * pi / 4)) - (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(5 * pi / 4)) - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(5 * pi / 4)) - (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
          }
        } else if (Texts_Vertical_Axis_Coordinates[x] > 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(pi / 4)) + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(pi / 4)) + (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(3 * pi / 4)) - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(3 * pi / 4)) + (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
          }
        } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
          } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
          }
        }
        c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
      }))
    } else if (Text_Shifting_Method == "Mainly_Diagonally_Using_Text_Box_Corners_at_45_Degree_Angles_With_Downward_Tendancy") {
      Plotting_Window_Dimensions <- par("pin")
      Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
      Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
      Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
      New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
        if (Texts_Vertical_Axis_Coordinates[x] < 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(7 * pi / 4)) + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(7 * pi / 4)) - (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(5 * pi / 4)) - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(5 * pi / 4)) - (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
          }
        } else if (Texts_Vertical_Axis_Coordinates[x] > 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(pi / 4)) + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(pi / 4)) + (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(3 * pi / 4)) - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(3 * pi / 4)) + (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
          }
        } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
          } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
          }
        }
        c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
      }))
    } else if (Text_Shifting_Method == "Mainly_Diagonally_Using_Text_Box_Corners_at_the_Same_Angles_as_the_Associated_Vectors_With_Leftward_Tendancy") {
      Plotting_Window_Dimensions <- par("pin")
      Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
      Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
      Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
      New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
        if (Texts_Vertical_Axis_Coordinates[x] < 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) - (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) - (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
          }
        } else if (Texts_Vertical_Axis_Coordinates[x] > 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) + (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) + (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
          }
        } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
          } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
          }
        }
        c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
      }))
    } else if (Text_Shifting_Method == "Mainly_Diagonally_Using_Text_Box_Corners_at_the_Same_Angles_as_the_Associated_Vectors_With_Upward_Tendancy") {
      Plotting_Window_Dimensions <- par("pin")
      Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
      Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
      Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
      New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
        if (Texts_Vertical_Axis_Coordinates[x] < 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) - (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) - (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
          }
        } else if (Texts_Vertical_Axis_Coordinates[x] > 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) + (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) + (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
          }
        } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
          } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
          }
        }
        c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
      }))
    } else if (Text_Shifting_Method == "Mainly_Diagonally_Using_Text_Box_Corners_at_the_Same_Angles_as_the_Associated_Vectors_With_Rightward_Tendancy") {
      Plotting_Window_Dimensions <- par("pin")
      Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
      Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
      Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
      New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
        if (Texts_Vertical_Axis_Coordinates[x] < 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) - (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) - (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
          }
        } else if (Texts_Vertical_Axis_Coordinates[x] > 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) + (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) + (Heights[x] / 2)
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x]
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
          }
        } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
          if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
          } else if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
          } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinates <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates[x]
          }
        }
        c(New_Texts_Horizontal_Axis_Coordinates = New_Texts_Horizontal_Axis_Coordinates, New_Texts_Vertical_Axis_Coordinates = New_Texts_Vertical_Axis_Coordinates)
      }))
    } else if (Text_Shifting_Method == "Mainly_Diagonally_Using_Text_Box_Corners_at_the_Same_Angles_as_the_Associated_Vectors_With_Downward_Tendancy") {
      Plotting_Window_Dimensions <- par("pin")
      Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
      Gap_Length_in_Inches <- Gap_Height * (Plotting_Window_Dimensions[1] / Axis_Ranges[1])
      Gap_Width <- Gap_Length_in_Inches * (Axis_Ranges[2] / Plotting_Window_Dimensions[2])
      New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
        if (Texts_Horizontal_Axis_Coordinates[x] < 0) {
          if (Texts_Vertical_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) + (Heights[x] / 2)
          } else if (Texts_Vertical_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) - (Heights[x] / 2)
          } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
          }
        } else if (Texts_Horizontal_Axis_Coordinates[x] > 0) {
          if (Texts_Vertical_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) + (Heights[x] / 2)
          } else if (Texts_Vertical_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] + (Gap_Width * cos(Angles[x])) + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] + (Gap_Height * sin(Angles[x])) - (Heights[x] / 2)
          } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
          }
        } else if (Texts_Horizontal_Axis_Coordinates[x] == 0) {
          if (Texts_Vertical_Axis_Coordinates[x] > 0) {
            New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
            New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
          } else if (Texts_Vertical_Axis_Coordinates[x] < 0) {
            New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
            New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
          } else if (Texts_Vertical_Axis_Coordinates[x] == 0) {
            New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
            New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
          }
        }
        c(New_Texts_Horizontal_Axis_Coordinate = New_Texts_Horizontal_Axis_Coordinate, New_Texts_Vertical_Axis_Coordinate = New_Texts_Vertical_Axis_Coordinate)
      }))
    } else if (Text_Shifting_Method == "Using_Vector_Directions_With_Leftward_Tendancy") {
      Plotting_Window_Dimensions <- par("pin")
      Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
      Horizontal_Axis_Conversion_Factor <- Plotting_Window_Dimensions[1] / Axis_Ranges[1]
      Vertical_Axis_Conversion_Factor <- Plotting_Window_Dimensions[2] / Axis_Ranges[2]
      Gap_Length_in_Inches <- Gap_Height * Vertical_Axis_Conversion_Factor
      Gap_Width <- Gap_Length_in_Inches / Horizontal_Axis_Conversion_Factor
      New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
        if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
        } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
        } else if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
        } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
        } else if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
        } else if ((Texts_Horizontal_Axis_Coordinates[x] != 0) & (Texts_Vertical_Axis_Coordinates[x] != 0)) {
          if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
            Temporary_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
            Temporary_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
            Temporary_Angle <- Angles[x]
          } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
            Temporary_Horizontal_Axis_Coordinate <- -Texts_Horizontal_Axis_Coordinates[x]
            Temporary_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
            Temporary_Angle <- (pi / 2) - (Angles[x] - (pi / 2))
          } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
            Temporary_Horizontal_Axis_Coordinate <- -Texts_Horizontal_Axis_Coordinates[x]
            Temporary_Vertical_Axis_Coordinate <- -Texts_Vertical_Axis_Coordinates[x]
            Temporary_Angle <- Angles[x] + pi
          } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
            Temporary_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
            Temporary_Vertical_Axis_Coordinate <- -Texts_Vertical_Axis_Coordinates[x]
            Temporary_Angle <- abs(Angles[x])
          }
          Rectangle_Angle <- atan2((Heights[x] / 2), (Widths[x] / 2))
          Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner <- atan2((Temporary_Vertical_Axis_Coordinate + Gap_Height + (Heights[x] / 2)), (Temporary_Horizontal_Axis_Coordinate + (Widths[x] / 2)))
          Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner <- atan2((Temporary_Vertical_Axis_Coordinate + (Heights[x] / 2)), (Temporary_Horizontal_Axis_Coordinate + Gap_Width + (Widths[x] / 2)))
          if (Temporary_Angle >= Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner) {
            New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + (Gap_Height / tan(Temporary_Angle)) + ((Heights[x] / 2) / tan(Temporary_Angle))
            New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Gap_Height + (Heights[x] / 2)
          } else if ((Temporary_Angle < Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner) & (Temporary_Angle > Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner)) {
            if (Rectangle_Angle < Temporary_Angle) {
              Text_Box_Width_in_Inches <- Horizontal_Axis_Conversion_Factor * Widths[x]
              Text_Box_Height_in_Inches <- Vertical_Axis_Conversion_Factor * Heights[x]
              New_Angle <- Temporary_Angle + asin(((Text_Box_Width_in_Inches / 2) - (Text_Box_Height_in_Inches / 2) / tan(Temporary_Angle)) * sin(Temporary_Angle) / Gap_Length_in_Inches)
              Horizontal_Component_in_Inches <- Gap_Length_in_Inches * cos(New_Angle)
              Vertical_Component_in_Inches <- Gap_Length_in_Inches * sin(New_Angle)
              Horizontal_Component <- Horizontal_Component_in_Inches / Horizontal_Axis_Conversion_Factor
              Vertical_Component <- Vertical_Component_in_Inches / Vertical_Axis_Conversion_Factor
              New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Horizontal_Component + (Widths[x] / 2)
              New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Vertical_Component + (Heights[x] / 2)
            } else if (Rectangle_Angle == Temporary_Angle) {
              New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + (Gap_Width * cos(Temporary_Angle)) + (Widths[x] / 2)
              New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + (Gap_Height * sin(Temporary_Angle)) + (Heights[x] / 2)
            } else if (Rectangle_Angle > Temporary_Angle) {
              Text_Box_Width_in_Inches <- Horizontal_Axis_Conversion_Factor * Widths[x]
              Text_Box_Height_in_Inches <- Vertical_Axis_Conversion_Factor * Heights[x]
              Other_Angle <- asin(((Text_Box_Height_in_Inches / 2) - (Text_Box_Width_in_Inches * tan(Temporary_Angle) / 2)) * sin((pi / 2) - Temporary_Angle) / Gap_Length_in_Inches)
              New_Angle <- ((pi / 2) - Temporary_Angle) + Other_Angle
              Horizontal_Component_in_Inches <- Gap_Length_in_Inches * sin(New_Angle)
              Vertical_Component_in_Inches <- Gap_Length_in_Inches * cos(New_Angle)
              Horizontal_Component <- Horizontal_Component_in_Inches / Horizontal_Axis_Conversion_Factor
              Vertical_Component <- Vertical_Component_in_Inches / Vertical_Axis_Conversion_Factor
              New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Horizontal_Component + (Widths[x] / 2)
              New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Vertical_Component + (Heights[x] / 2)
            }
          } else if (Temporary_Angle <= Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner) {
            New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Gap_Width + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + (Gap_Width * tan(Temporary_Angle)) + ((Widths[x] / 2) * tan(Temporary_Angle))
          }
          if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
            New_Texts_Horizontal_Axis_Coordinate <- New_Texts_Horizontal_Axis_Coordinate
            New_Texts_Vertical_Axis_Coordinate <- New_Texts_Vertical_Axis_Coordinate
          } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
            New_Texts_Horizontal_Axis_Coordinate <- -New_Texts_Horizontal_Axis_Coordinate
            New_Texts_Vertical_Axis_Coordinate <- New_Texts_Vertical_Axis_Coordinate
          } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
            New_Texts_Horizontal_Axis_Coordinate <- -New_Texts_Horizontal_Axis_Coordinate
            New_Texts_Vertical_Axis_Coordinate <- -New_Texts_Vertical_Axis_Coordinate
          } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
            New_Texts_Horizontal_Axis_Coordinate <- New_Texts_Horizontal_Axis_Coordinate
            New_Texts_Vertical_Axis_Coordinate <- -New_Texts_Vertical_Axis_Coordinate
          }
        }
        c(New_Texts_Horizontal_Axis_Coordinate = New_Texts_Horizontal_Axis_Coordinate, New_Texts_Vertical_Axis_Coordinate = New_Texts_Vertical_Axis_Coordinate)
      }))
    } else if (Text_Shifting_Method == "Using_Vector_Directions_With_Upward_Tendancy") {
      Plotting_Window_Dimensions <- par("pin")
      Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
      Horizontal_Axis_Conversion_Factor <- Plotting_Window_Dimensions[1] / Axis_Ranges[1]
      Vertical_Axis_Conversion_Factor <- Plotting_Window_Dimensions[2] / Axis_Ranges[2]
      Gap_Length_in_Inches <- Gap_Height * Vertical_Axis_Conversion_Factor
      Gap_Width <- Gap_Length_in_Inches / Horizontal_Axis_Conversion_Factor
      New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
        if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
        } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
        } else if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
        } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
        } else if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
        } else if ((Texts_Horizontal_Axis_Coordinates[x] != 0) & (Texts_Vertical_Axis_Coordinates[x] != 0)) {
          if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
            Temporary_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
            Temporary_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
            Temporary_Angle <- Angles[x]
          } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
            Temporary_Horizontal_Axis_Coordinate <- -Texts_Horizontal_Axis_Coordinates[x]
            Temporary_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
            Temporary_Angle <- (pi / 2) - (Angles[x] - (pi / 2))
          } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
            Temporary_Horizontal_Axis_Coordinate <- -Texts_Horizontal_Axis_Coordinates[x]
            Temporary_Vertical_Axis_Coordinate <- -Texts_Vertical_Axis_Coordinates[x]
            Temporary_Angle <- Angles[x] + pi
          } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
            Temporary_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
            Temporary_Vertical_Axis_Coordinate <- -Texts_Vertical_Axis_Coordinates[x]
            Temporary_Angle <- abs(Angles[x])
          }
          Rectangle_Angle <- atan2((Heights[x] / 2), (Widths[x] / 2))
          Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner <- atan2((Temporary_Vertical_Axis_Coordinate + Gap_Height + (Heights[x] / 2)), (Temporary_Horizontal_Axis_Coordinate + (Widths[x] / 2)))
          Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner <- atan2((Temporary_Vertical_Axis_Coordinate + (Heights[x] / 2)), (Temporary_Horizontal_Axis_Coordinate + Gap_Width + (Widths[x] / 2)))
          if (Temporary_Angle >= Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner) {
            New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + (Gap_Height / tan(Temporary_Angle)) + ((Heights[x] / 2) / tan(Temporary_Angle))
            New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Gap_Height + (Heights[x] / 2)
          } else if ((Temporary_Angle < Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner) & (Temporary_Angle > Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner)) {
            if (Rectangle_Angle < Temporary_Angle) {
              Text_Box_Width_in_Inches <- Horizontal_Axis_Conversion_Factor * Widths[x]
              Text_Box_Height_in_Inches <- Vertical_Axis_Conversion_Factor * Heights[x]
              New_Angle <- Temporary_Angle + asin(((Text_Box_Width_in_Inches / 2) - (Text_Box_Height_in_Inches / 2) / tan(Temporary_Angle)) * sin(Temporary_Angle) / Gap_Length_in_Inches)
              Horizontal_Component_in_Inches <- Gap_Length_in_Inches * cos(New_Angle)
              Vertical_Component_in_Inches <- Gap_Length_in_Inches * sin(New_Angle)
              Horizontal_Component <- Horizontal_Component_in_Inches / Horizontal_Axis_Conversion_Factor
              Vertical_Component <- Vertical_Component_in_Inches / Vertical_Axis_Conversion_Factor
              New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Horizontal_Component + (Widths[x] / 2)
              New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Vertical_Component + (Heights[x] / 2)
            } else if (Rectangle_Angle == Temporary_Angle) {
              New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + (Gap_Width * cos(Temporary_Angle)) + (Widths[x] / 2)
              New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + (Gap_Height * sin(Temporary_Angle)) + (Heights[x] / 2)
            } else if (Rectangle_Angle > Temporary_Angle) {
              Text_Box_Width_in_Inches <- Horizontal_Axis_Conversion_Factor * Widths[x]
              Text_Box_Height_in_Inches <- Vertical_Axis_Conversion_Factor * Heights[x]
              Other_Angle <- asin(((Text_Box_Height_in_Inches / 2) - (Text_Box_Width_in_Inches * tan(Temporary_Angle) / 2)) * sin((pi / 2) - Temporary_Angle) / Gap_Length_in_Inches)
              New_Angle <- ((pi / 2) - Temporary_Angle) + Other_Angle
              Horizontal_Component_in_Inches <- Gap_Length_in_Inches * sin(New_Angle)
              Vertical_Component_in_Inches <- Gap_Length_in_Inches * cos(New_Angle)
              Horizontal_Component <- Horizontal_Component_in_Inches / Horizontal_Axis_Conversion_Factor
              Vertical_Component <- Vertical_Component_in_Inches / Vertical_Axis_Conversion_Factor
              New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Horizontal_Component + (Widths[x] / 2)
              New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Vertical_Component + (Heights[x] / 2)
            }
          } else if (Temporary_Angle <= Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner) {
            New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Gap_Width + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + (Gap_Width * tan(Temporary_Angle)) + ((Widths[x] / 2) * tan(Temporary_Angle))
          }
          if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
            New_Texts_Horizontal_Axis_Coordinate <- New_Texts_Horizontal_Axis_Coordinate
            New_Texts_Vertical_Axis_Coordinate <- New_Texts_Vertical_Axis_Coordinate
          } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
            New_Texts_Horizontal_Axis_Coordinate <- -New_Texts_Horizontal_Axis_Coordinate
            New_Texts_Vertical_Axis_Coordinate <- New_Texts_Vertical_Axis_Coordinate
          } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
            New_Texts_Horizontal_Axis_Coordinate <- -New_Texts_Horizontal_Axis_Coordinate
            New_Texts_Vertical_Axis_Coordinate <- -New_Texts_Vertical_Axis_Coordinate
          } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
            New_Texts_Horizontal_Axis_Coordinate <- New_Texts_Horizontal_Axis_Coordinate
            New_Texts_Vertical_Axis_Coordinate <- -New_Texts_Vertical_Axis_Coordinate
          }
        }
        c(New_Texts_Horizontal_Axis_Coordinate = New_Texts_Horizontal_Axis_Coordinate, New_Texts_Vertical_Axis_Coordinate = New_Texts_Vertical_Axis_Coordinate)
      }))
    } else if (Text_Shifting_Method == "Using_Vector_Directions_With_Rightward_Tendancy") {
      Plotting_Window_Dimensions <- par("pin")
      Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
      Horizontal_Axis_Conversion_Factor <- Plotting_Window_Dimensions[1] / Axis_Ranges[1]
      Vertical_Axis_Conversion_Factor <- Plotting_Window_Dimensions[2] / Axis_Ranges[2]
      Gap_Length_in_Inches <- Gap_Height * Vertical_Axis_Conversion_Factor
      Gap_Width <- Gap_Length_in_Inches / Horizontal_Axis_Conversion_Factor
      New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
        if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
        } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
        } else if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
        } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
        } else if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
        } else if ((Texts_Horizontal_Axis_Coordinates[x] != 0) & (Texts_Vertical_Axis_Coordinates[x] != 0)) {
          if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
            Temporary_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
            Temporary_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
            Temporary_Angle <- Angles[x]
          } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
            Temporary_Horizontal_Axis_Coordinate <- -Texts_Horizontal_Axis_Coordinates[x]
            Temporary_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
            Temporary_Angle <- (pi / 2) - (Angles[x] - (pi / 2))
          } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
            Temporary_Horizontal_Axis_Coordinate <- -Texts_Horizontal_Axis_Coordinates[x]
            Temporary_Vertical_Axis_Coordinate <- -Texts_Vertical_Axis_Coordinates[x]
            Temporary_Angle <- Angles[x] + pi
          } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
            Temporary_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
            Temporary_Vertical_Axis_Coordinate <- -Texts_Vertical_Axis_Coordinates[x]
            Temporary_Angle <- abs(Angles[x])
          }
          Rectangle_Angle <- atan2((Heights[x] / 2), (Widths[x] / 2))
          Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner <- atan2((Temporary_Vertical_Axis_Coordinate + Gap_Height + (Heights[x] / 2)), (Temporary_Horizontal_Axis_Coordinate + (Widths[x] / 2)))
          Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner <- atan2((Temporary_Vertical_Axis_Coordinate + (Heights[x] / 2)), (Temporary_Horizontal_Axis_Coordinate + Gap_Width + (Widths[x] / 2)))
          if (Temporary_Angle >= Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner) {
            New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + (Gap_Height / tan(Temporary_Angle)) + ((Heights[x] / 2) / tan(Temporary_Angle))
            New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Gap_Height + (Heights[x] / 2)
          } else if ((Temporary_Angle < Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner) & (Temporary_Angle > Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner)) {
            if (Rectangle_Angle < Temporary_Angle) {
              Text_Box_Width_in_Inches <- Horizontal_Axis_Conversion_Factor * Widths[x]
              Text_Box_Height_in_Inches <- Vertical_Axis_Conversion_Factor * Heights[x]
              New_Angle <- Temporary_Angle + asin(((Text_Box_Width_in_Inches / 2) - (Text_Box_Height_in_Inches / 2) / tan(Temporary_Angle)) * sin(Temporary_Angle) / Gap_Length_in_Inches)
              Horizontal_Component_in_Inches <- Gap_Length_in_Inches * cos(New_Angle)
              Vertical_Component_in_Inches <- Gap_Length_in_Inches * sin(New_Angle)
              Horizontal_Component <- Horizontal_Component_in_Inches / Horizontal_Axis_Conversion_Factor
              Vertical_Component <- Vertical_Component_in_Inches / Vertical_Axis_Conversion_Factor
              New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Horizontal_Component + (Widths[x] / 2)
              New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Vertical_Component + (Heights[x] / 2)
            } else if (Rectangle_Angle == Temporary_Angle) {
              New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + (Gap_Width * cos(Temporary_Angle)) + (Widths[x] / 2)
              New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + (Gap_Height * sin(Temporary_Angle)) + (Heights[x] / 2)
            } else if (Rectangle_Angle > Temporary_Angle) {
              Text_Box_Width_in_Inches <- Horizontal_Axis_Conversion_Factor * Widths[x]
              Text_Box_Height_in_Inches <- Vertical_Axis_Conversion_Factor * Heights[x]
              Other_Angle <- asin(((Text_Box_Height_in_Inches / 2) - (Text_Box_Width_in_Inches * tan(Temporary_Angle) / 2)) * sin((pi / 2) - Temporary_Angle) / Gap_Length_in_Inches)
              New_Angle <- ((pi / 2) - Temporary_Angle) + Other_Angle
              Horizontal_Component_in_Inches <- Gap_Length_in_Inches * sin(New_Angle)
              Vertical_Component_in_Inches <- Gap_Length_in_Inches * cos(New_Angle)
              Horizontal_Component <- Horizontal_Component_in_Inches / Horizontal_Axis_Conversion_Factor
              Vertical_Component <- Vertical_Component_in_Inches / Vertical_Axis_Conversion_Factor
              New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Horizontal_Component + (Widths[x] / 2)
              New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Vertical_Component + (Heights[x] / 2)
            }
          } else if (Temporary_Angle <= Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner) {
            New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Gap_Width + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + (Gap_Width * tan(Temporary_Angle)) + ((Widths[x] / 2) * tan(Temporary_Angle))
          }
          if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
            New_Texts_Horizontal_Axis_Coordinate <- New_Texts_Horizontal_Axis_Coordinate
            New_Texts_Vertical_Axis_Coordinate <- New_Texts_Vertical_Axis_Coordinate
          } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
            New_Texts_Horizontal_Axis_Coordinate <- -New_Texts_Horizontal_Axis_Coordinate
            New_Texts_Vertical_Axis_Coordinate <- New_Texts_Vertical_Axis_Coordinate
          } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
            New_Texts_Horizontal_Axis_Coordinate <- -New_Texts_Horizontal_Axis_Coordinate
            New_Texts_Vertical_Axis_Coordinate <- -New_Texts_Vertical_Axis_Coordinate
          } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
            New_Texts_Horizontal_Axis_Coordinate <- New_Texts_Horizontal_Axis_Coordinate
            New_Texts_Vertical_Axis_Coordinate <- -New_Texts_Vertical_Axis_Coordinate
          }
        }
        c(New_Texts_Horizontal_Axis_Coordinate = New_Texts_Horizontal_Axis_Coordinate, New_Texts_Vertical_Axis_Coordinate = New_Texts_Vertical_Axis_Coordinate)
      }))
    } else if (Text_Shifting_Method == "Using_Vector_Directions_With_Downward_Tendancy") {
      Plotting_Window_Dimensions <- par("pin")
      Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Spacing_Constant
      Horizontal_Axis_Conversion_Factor <- Plotting_Window_Dimensions[1] / Axis_Ranges[1]
      Vertical_Axis_Conversion_Factor <- Plotting_Window_Dimensions[2] / Axis_Ranges[2]
      Gap_Length_in_Inches <- Gap_Height * Vertical_Axis_Conversion_Factor
      Gap_Width <- Gap_Length_in_Inches / Horizontal_Axis_Conversion_Factor
      New_Texts_Axis_Coordinates <- t(sapply(seq_len(length(Texts)), function (x) {
        if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
        } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] - Gap_Width - (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
        } else if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] + Gap_Height + (Heights[x] / 2)
        } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] == 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x] + Gap_Width + (Widths[x] / 2)
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
        } else if ((Texts_Horizontal_Axis_Coordinates[x] == 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
          New_Texts_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
          New_Texts_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x] - Gap_Height - (Heights[x] / 2)
        } else if ((Texts_Horizontal_Axis_Coordinates[x] != 0) & (Texts_Vertical_Axis_Coordinates[x] != 0)) {
          if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
            Temporary_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
            Temporary_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
            Temporary_Angle <- Angles[x]
          } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
            Temporary_Horizontal_Axis_Coordinate <- -Texts_Horizontal_Axis_Coordinates[x]
            Temporary_Vertical_Axis_Coordinate <- Texts_Vertical_Axis_Coordinates[x]
            Temporary_Angle <- (pi / 2) - (Angles[x] - (pi / 2))
          } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
            Temporary_Horizontal_Axis_Coordinate <- -Texts_Horizontal_Axis_Coordinates[x]
            Temporary_Vertical_Axis_Coordinate <- -Texts_Vertical_Axis_Coordinates[x]
            Temporary_Angle <- Angles[x] + pi
          } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
            Temporary_Horizontal_Axis_Coordinate <- Texts_Horizontal_Axis_Coordinates[x]
            Temporary_Vertical_Axis_Coordinate <- -Texts_Vertical_Axis_Coordinates[x]
            Temporary_Angle <- abs(Angles[x])
          }
          Rectangle_Angle <- atan2((Heights[x] / 2), (Widths[x] / 2))
          Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner <- atan2((Temporary_Vertical_Axis_Coordinate + Gap_Height + (Heights[x] / 2)), (Temporary_Horizontal_Axis_Coordinate + (Widths[x] / 2)))
          Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner <- atan2((Temporary_Vertical_Axis_Coordinate + (Heights[x] / 2)), (Temporary_Horizontal_Axis_Coordinate + Gap_Width + (Widths[x] / 2)))
          if (Temporary_Angle >= Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner) {
            New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + (Gap_Height / tan(Temporary_Angle)) + ((Heights[x] / 2) / tan(Temporary_Angle))
            New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Gap_Height + (Heights[x] / 2)
          } else if ((Temporary_Angle < Threshold_Angle_Involving_the_Point_Below_the_Lower_Left_Text_Box_Corner) & (Temporary_Angle > Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner)) {
            if (Rectangle_Angle < Temporary_Angle) {
              Text_Box_Width_in_Inches <- Horizontal_Axis_Conversion_Factor * Widths[x]
              Text_Box_Height_in_Inches <- Vertical_Axis_Conversion_Factor * Heights[x]
              New_Angle <- Temporary_Angle + asin(((Text_Box_Width_in_Inches / 2) - (Text_Box_Height_in_Inches / 2) / tan(Temporary_Angle)) * sin(Temporary_Angle) / Gap_Length_in_Inches)
              Horizontal_Component_in_Inches <- Gap_Length_in_Inches * cos(New_Angle)
              Vertical_Component_in_Inches <- Gap_Length_in_Inches * sin(New_Angle)
              Horizontal_Component <- Horizontal_Component_in_Inches / Horizontal_Axis_Conversion_Factor
              Vertical_Component <- Vertical_Component_in_Inches / Vertical_Axis_Conversion_Factor
              New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Horizontal_Component + (Widths[x] / 2)
              New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Vertical_Component + (Heights[x] / 2)
            } else if (Rectangle_Angle == Temporary_Angle) {
              New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + (Gap_Width * cos(Temporary_Angle)) + (Widths[x] / 2)
              New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + (Gap_Height * sin(Temporary_Angle)) + (Heights[x] / 2)
            } else if (Rectangle_Angle > Temporary_Angle) {
              Text_Box_Width_in_Inches <- Horizontal_Axis_Conversion_Factor * Widths[x]
              Text_Box_Height_in_Inches <- Vertical_Axis_Conversion_Factor * Heights[x]
              Other_Angle <- asin(((Text_Box_Height_in_Inches / 2) - (Text_Box_Width_in_Inches * tan(Temporary_Angle) / 2)) * sin((pi / 2) - Temporary_Angle) / Gap_Length_in_Inches)
              New_Angle <- ((pi / 2) - Temporary_Angle) + Other_Angle
              Horizontal_Component_in_Inches <- Gap_Length_in_Inches * sin(New_Angle)
              Vertical_Component_in_Inches <- Gap_Length_in_Inches * cos(New_Angle)
              Horizontal_Component <- Horizontal_Component_in_Inches / Horizontal_Axis_Conversion_Factor
              Vertical_Component <- Vertical_Component_in_Inches / Vertical_Axis_Conversion_Factor
              New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Horizontal_Component + (Widths[x] / 2)
              New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + Vertical_Component + (Heights[x] / 2)
            }
          } else if (Temporary_Angle <= Threshold_Angle_Involving_the_Point_to_the_Left_of_the_Lower_Left_Text_Box_Corner) {
            New_Texts_Horizontal_Axis_Coordinate <- Temporary_Horizontal_Axis_Coordinate + Gap_Width + (Widths[x] / 2)
            New_Texts_Vertical_Axis_Coordinate <- Temporary_Vertical_Axis_Coordinate + (Gap_Width * tan(Temporary_Angle)) + ((Widths[x] / 2) * tan(Temporary_Angle))
          }
          if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
            New_Texts_Horizontal_Axis_Coordinate <- New_Texts_Horizontal_Axis_Coordinate
            New_Texts_Vertical_Axis_Coordinate <- New_Texts_Vertical_Axis_Coordinate
          } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] > 0)) {
            New_Texts_Horizontal_Axis_Coordinate <- -New_Texts_Horizontal_Axis_Coordinate
            New_Texts_Vertical_Axis_Coordinate <- New_Texts_Vertical_Axis_Coordinate
          } else if ((Texts_Horizontal_Axis_Coordinates[x] < 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
            New_Texts_Horizontal_Axis_Coordinate <- -New_Texts_Horizontal_Axis_Coordinate
            New_Texts_Vertical_Axis_Coordinate <- -New_Texts_Vertical_Axis_Coordinate
          } else if ((Texts_Horizontal_Axis_Coordinates[x] > 0) & (Texts_Vertical_Axis_Coordinates[x] < 0)) {
            New_Texts_Horizontal_Axis_Coordinate <- New_Texts_Horizontal_Axis_Coordinate
            New_Texts_Vertical_Axis_Coordinate <- -New_Texts_Vertical_Axis_Coordinate
          }
        }
        c(New_Texts_Horizontal_Axis_Coordinate = New_Texts_Horizontal_Axis_Coordinate, New_Texts_Vertical_Axis_Coordinate = New_Texts_Vertical_Axis_Coordinate)
      }))
    }
    rownames(New_Texts_Axis_Coordinates) <- Texts
    Text_Box_Corner_Data_Frame <- do.call("rbind", lapply(seq_len(length(Texts)), function (x) {
      if (Should_Text_Box_Polygons_Shade_out_Points == T) {
        Horizontal_Coordinates <- c((New_Texts_Axis_Coordinates[x, "New_Texts_Horizontal_Axis_Coordinate"] - Gap_Width - (Widths[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Horizontal_Axis_Coordinate"] + Gap_Width + (Widths[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Horizontal_Axis_Coordinate"] + Gap_Width + (Widths[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Horizontal_Axis_Coordinate"] - Gap_Width - (Widths[x] / 2)))
        Vertical_Coordinates <- c((New_Texts_Axis_Coordinates[x, "New_Texts_Vertical_Axis_Coordinate"] + Gap_Height + (Heights[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Vertical_Axis_Coordinate"] + Gap_Height + (Heights[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Vertical_Axis_Coordinate"] - Gap_Height - (Heights[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Vertical_Axis_Coordinate"] - Gap_Height - (Heights[x] / 2)))
      } else if (Should_Text_Box_Polygons_Shade_out_Points == F) {
        Horizontal_Coordinates <- c((New_Texts_Axis_Coordinates[x, "New_Texts_Horizontal_Axis_Coordinate"] - (Widths[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Horizontal_Axis_Coordinate"] + (Widths[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Horizontal_Axis_Coordinate"] + (Widths[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Horizontal_Axis_Coordinate"] - (Widths[x] / 2)))
        Vertical_Coordinates <- c((New_Texts_Axis_Coordinates[x, "New_Texts_Vertical_Axis_Coordinate"] + (Heights[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Vertical_Axis_Coordinate"] + (Heights[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Vertical_Axis_Coordinate"] - (Heights[x] / 2)), (New_Texts_Axis_Coordinates[x, "New_Texts_Vertical_Axis_Coordinate"] - (Heights[x] / 2)))
      }
      Data_Frame <- data.frame(Horizontal_Coordinates = Horizontal_Coordinates, Vertical_Coordinates = Vertical_Coordinates)
      Data_Frame$Text <- rep(Texts[x], nrow(Data_Frame))
      Data_Frame$Corner <- c("Upper Left", "Upper Right", "Lower Right", "Lower Left")
      Data_Frame[, c(which(colnames(Data_Frame) == "Text"), which(colnames(Data_Frame) == "Corner"), which(colnames(Data_Frame) == "Horizontal_Coordinates"), which(colnames(Data_Frame) == "Vertical_Coordinates"))]
    }))
    plot(c(Points_Vertical_Axis_Coordinates, Texts_Vertical_Axis_Coordinates, Text_Box_Corner_Data_Frame$Vertical_Coordinates) ~ c(Points_Horizontal_Axis_Coordinates, Texts_Horizontal_Axis_Coordinates, Text_Box_Corner_Data_Frame$Horizontal_Coordinates), type = "n", axes = F, xlab = "", ylab = "")
    New_Axis_Limits <- par("usr")
    if (all(Axis_Limits == New_Axis_Limits)) {
      New_Texts_Axis_Coordinates <- as.data.frame(New_Texts_Axis_Coordinates)
      New_Texts_Axis_Coordinates$Text <- rownames(New_Texts_Axis_Coordinates)
      New_Texts_Axis_Coordinates_Data_Frame <- New_Texts_Axis_Coordinates[, c(which(colnames(New_Texts_Axis_Coordinates) == "Text"), which(colnames(New_Texts_Axis_Coordinates) != "Text"))]
      if (Should_Text_Box_Polygons_Shade_out_Points == T) {
        list(Axis_Limits = list(Horizontal_Axis_Limits = New_Axis_Limits[1:2], Vertical_Axis_Limits = New_Axis_Limits[3:4]), New_Texts_Axis_Coordinates_Data_Frame = New_Texts_Axis_Coordinates_Data_Frame, Text_Box_Corner_Data_Frame = Text_Box_Corner_Data_Frame)
      } else if (Should_Text_Box_Polygons_Shade_out_Points == F) {
        list(Axis_Limits = list(Horizontal_Axis_Limits = New_Axis_Limits[1:2], Vertical_Axis_Limits = New_Axis_Limits[3:4]), New_Texts_Axis_Coordinates_Data_Frame = New_Texts_Axis_Coordinates_Data_Frame)
      }
    } else if (!all(Axis_Limits == New_Axis_Limits)) {
      Axis_Limits <- New_Axis_Limits
      Plotting_Text_Box_Corners_Iteratively()
    }
  }
  if (all(Axis_Limits == New_Axis_Limits)) {
    New_Texts_Axis_Coordinates <- as.data.frame(New_Texts_Axis_Coordinates)
    New_Texts_Axis_Coordinates$Text <- rownames(New_Texts_Axis_Coordinates)
    New_Texts_Axis_Coordinates_Data_Frame <- New_Texts_Axis_Coordinates[, c(which(colnames(New_Texts_Axis_Coordinates) == "Text"), which(colnames(New_Texts_Axis_Coordinates) != "Text"))]
    if (Should_Text_Box_Polygons_Shade_out_Points == T) {
      list(Axis_Limits = list(Horizontal_Axis_Limits = New_Axis_Limits[1:2], Vertical_Axis_Limits = New_Axis_Limits[3:4]), New_Texts_Axis_Coordinates_Data_Frame = New_Texts_Axis_Coordinates_Data_Frame, Text_Box_Corner_Data_Frame = Text_Box_Corner_Data_Frame)
    } else if (Should_Text_Box_Polygons_Shade_out_Points == F) {
      list(Axis_Limits = list(Horizontal_Axis_Limits = New_Axis_Limits[1:2], Vertical_Axis_Limits = New_Axis_Limits[3:4]), New_Texts_Axis_Coordinates_Data_Frame = New_Texts_Axis_Coordinates_Data_Frame)
    }
  } else if (!all(Axis_Limits == New_Axis_Limits)) {
    Axis_Limits <- New_Axis_Limits
    Plotting_Text_Box_Corners_Iteratively()
  }
}