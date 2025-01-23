#' Generating Axis Limits for Figures With Text
#'
#' This function may be used to generate nice axis limits for figures that have both points and text.
#'
#' `Generating_Axis_Limits_for_Figures_With_Text_Function` generates nice axis limits for figures that have both points and text. It was designed with multidimensional ordinations in mind, where points are plotted alongside labeled loadings vectors. Occasionally, the loadings vector's labels fall outside of the plotting region or too close to the edge of the plot, and this function generates the optimal axis limits in these cases.
#' @param Points_Horizontal_Axis_Coordinates the numeric variables comprising the coordinates from the first ordination axis (which will become the horizontal axis).
#' @param Points_Vertical_Axis_Coordinates the numeric variables comprising the coordinates from the second ordination axis (which will become the vertical axis).
#' @param Texts the vector of loadings names.
#' @param Texts_Horizontal_Axis_Coordinates the values you wish to align across the vertical axes. The default for this argument is a vector of `0`s. This argument must be a numeric vector and it must contain the same number of elements that there are variables being aligned.
#' @param Texts_Vertical_Axis_Coordinates the weights assigned to each variable. To prevent certain variables from being crowded near the top or the bottom of the plot, a greater weight can be assigned to these variables, which ensures that these variables will take up more of the plotting region (at the expense of other variables, of course). The default for this argument is to assign all the variables the same weight. This argument must be a numeric vector, all the entries must be finite and nonnegative, and it must contain the same number of elements as there are variables being aligned.
#' @param Character_Expansion the relative sizes of the names of the loadings labels. This argument is passed to the `"cex"` argument of all the relevant graphing functions. The default is `1`. This argument must be a nonnegative number.
#' @param Vertical_Text_Spacing_Constant the relative amount of space vertically between the loadings labels and the tips of the loadings vectors. The default is `1`. Larger numbers for this argument lead to larger spacing. This argument must be a nonnegative number.
#'
#' @return This function returns the new axis limits to ensure all points and loadings labels are visible in the plotting region, and it also returns the coordinates for the loadings labels (taking into account the size of these labels).
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' # An Example
#' set.seed(16)
#' Number_of_Data_Points <- 50
#' Ordination_Data <- data.frame(Ordination_Axis_1_Coordinates = rnorm(Number_of_Data_Points), Ordination_Axis_2_Coordinates = rnorm(Number_of_Data_Points))
#' Number_of_Variables <- 5
#' Loadings <- data.frame(Variable = paste("Variable", seq_len(Number_of_Variables), sep = "_"), Ordination_Axis_1_Coordinate = rnorm(Number_of_Variables), Ordination_Axis_2_Coordinate = rnorm(Number_of_Variables))
#' Character_Expansion <- 1.5
#' Vertical_Text_Spacing_Constant <- 0.75
#' (Output <- Generating_Axis_Limits_for_Figures_With_Text_Function(Points_Horizontal_Axis_Coordinates = Ordination_Data$Ordination_Axis_1_Coordinates, Points_Vertical_Axis_Coordinates = Ordination_Data$Ordination_Axis_2_Coordinates, Texts = Loadings$Variable, Texts_Horizontal_Axis_Coordinates = Loadings$Ordination_Axis_1_Coordinate, Texts_Vertical_Axis_Coordinates = Loadings$Ordination_Axis_2_Coordinate, Character_Expansion = Character_Expansion, Vertical_Text_Spacing_Constant = Vertical_Text_Spacing_Constant))
#' par(mar = c(4, 4, 6, 2), mfrow = c(1, 3))
#' plot(Ordination_Data$Ordination_Axis_2_Coordinates ~ Ordination_Data$Ordination_Axis_1_Coordinates, pch = 19, xlab = "", ylab = "", main = "Figure With Default Axis Limits:\nAll Points Are Visible But The Tips\nOf Some Loading Vectors Are Not", col = 2, cex.main = 1.5) # 141:241
#' mtext("Ordination Axis 1", 1, line = 2.5)
#' mtext("Ordination Axis 2", 2, line = 2.5)
#' arrows(rep(0, nrow(Loadings)), rep(0, nrow(Loadings)), Loadings$Ordination_Axis_1_Coordinate, Loadings$Ordination_Axis_2_Coordinate, lwd = 1.25, length = 0.1)
#' text(Output$Text_Coordinate_Data_Frame$Horizontal_Axis_Coordinate, Output$Text_Coordinate_Data_Frame$Vertical_Axis_Coordinate, gsub("_", " ", Loadings$Variable), cex = Character_Expansion)
#' plot(c(Ordination_Data$Ordination_Axis_2_Coordinates, Loadings$Ordination_Axis_2_Coordinate) ~ c(Ordination_Data$Ordination_Axis_1_Coordinates, Loadings$Ordination_Axis_1_Coordinate), type = "n", xlab = "", ylab = "", main = "Figure With Better Axis Limits: All\nPoints And Loading Vectors Are Visible But\nSome Loading Vector Names Are Cut Off", cex.main = 1.5) # 227:343
#' mtext("Ordination Axis 1", 1, line = 2.5)
#' mtext("Ordination Axis 2", 2, line = 2.5)
#' points(Ordination_Data$Ordination_Axis_1_Coordinates, Ordination_Data$Ordination_Axis_2_Coordinates, pch = 19, col = 2)
#' arrows(rep(0, nrow(Loadings)), rep(0, nrow(Loadings)), Loadings$Ordination_Axis_1_Coordinate, Loadings$Ordination_Axis_2_Coordinate, lwd = 1.25, length = 0.1)
#' text(Output$Text_Coordinate_Data_Frame$Horizontal_Axis_Coordinate, Output$Text_Coordinate_Data_Frame$Vertical_Axis_Coordinate, gsub("_", " ", Loadings$Variable), cex = Character_Expansion)
#' plot(Ordination_Data$Ordination_Axis_2_Coordinates ~ Ordination_Data$Ordination_Axis_1_Coordinates, pch = 19, xlim = Output$Axis_Limits$Horizontal_Axis_Limits, ylim = Output$Axis_Limits$Vertical_Axis_Limits, xlab = "", ylab = "", main = "Figure With Optimized Axis Limits: All Points,\nLoading Vectors, And Loading Vector Names\nAre Visible And There Is No Wasted Space", col = 2, cex.main = 1.5) # 239:368
#' mtext("Ordination Axis 1", 1, line = 2.5)
#' mtext("Ordination Axis 2", 2, line = 2.5)
#' arrows(rep(0, nrow(Loadings)), rep(0, nrow(Loadings)), Loadings$Ordination_Axis_1_Coordinate, Loadings$Ordination_Axis_2_Coordinate, lwd = 1.25, length = 0.1)
#' text(Output$Text_Coordinate_Data_Frame$Horizontal_Axis_Coordinate, Output$Text_Coordinate_Data_Frame$Vertical_Axis_Coordinate, gsub("_", " ", Loadings$Variable), cex = Character_Expansion)
#'
#' @export
Generating_Axis_Limits_for_Figures_With_Text_Function <- function (Points_Horizontal_Axis_Coordinates, Points_Vertical_Axis_Coordinates, Texts, Texts_Horizontal_Axis_Coordinates, Texts_Vertical_Axis_Coordinates, Character_Expansion = 1, Vertical_Text_Spacing_Constant = 1, Shift_Text_Vertically_Away_From_Their_Coordinates = T) {
  if (!is.numeric(Points_Horizontal_Axis_Coordinates)) {
    stop ("The 'Points_Horizontal_Axis_Coordinates' argument must be of class 'numeric'.")
  }
  if (!is.numeric(Points_Vertical_Axis_Coordinates)) {
    stop ("The 'Points_Vertical_Axis_Coordinates' argument must be of class 'numeric'.")
  }
  if (length (Points_Horizontal_Axis_Coordinates) != length(Points_Vertical_Axis_Coordinates)) {
    stop ("The 'Points_Horizontal_Axis_Coordinates' and 'Points_Vertical_Axis_Coordinates' arguments must be of the same length.")
  }
  if (!is.character(Texts)) {
    stop ("The 'Texts' argument must be of class 'character'.")
  }
  if (!is.numeric(Texts_Horizontal_Axis_Coordinates)) {
    stop ("The 'Texts_Horizontal_Axis_Coordinates' argument must be of class 'numeric'.")
  }
  if (!is.numeric(Texts_Vertical_Axis_Coordinates)) {
    stop ("The 'Texts_Vertical_Axis_Coordinates' argument must be of class 'numeric'.")
  }
  if (!all(c(length(Texts) == length(Texts_Horizontal_Axis_Coordinates), length(Texts) == length(Texts_Vertical_Axis_Coordinates), length(Texts_Horizontal_Axis_Coordinates) == length(Texts_Vertical_Axis_Coordinates)))) {
    stop ("The 'Texts', 'Texts_Horizontal_Axis_Coordinates', and 'Texts_Vertical_Axis_Coordinates' arguments must all be of the same length.")
  }
  if (!is.numeric(Character_Expansion)) {
    stop ("The 'Character_Expansion' argument must be of class 'numeric'.")
  }
  if (length(Character_Expansion) != 1) {
    stop ("The 'Character_Expansion' argument must be of length '1'.")
  }
  if (Character_Expansion < 0) {
    stop ("The 'Character_Expansion' argument must be a nonnegative number.")
  }
  if (!is.numeric(Vertical_Text_Spacing_Constant)) {
    stop ("The 'Vertical_Text_Spacing_Constant' argument must be of class 'numeric'.")
  }
  if (length(Vertical_Text_Spacing_Constant) != 1) {
    stop ("The 'Vertical_Text_Spacing_Constant' argument must be of length '1'.")
  }
  if (Vertical_Text_Spacing_Constant < 0) {
    stop ("The 'Vertical_Text_Spacing_Constant' argument must be a nonnegative number.")
  }
  if (!is.logical(Shift_Text_Vertically_Away_From_Their_Coordinates)) {
    stop ("The 'Shift_Text_Vertically_Away_From_Their_Coordinates' argument must be of class 'logical'.")
  }
  if (length(Shift_Text_Vertically_Away_From_Their_Coordinates) != 1) {
    stop ("The 'Shift_Text_Vertically_Away_From_Their_Coordinates' argument must be of length '1'.")
  }
  plot(c(Points_Vertical_Axis_Coordinates, Texts_Vertical_Axis_Coordinates) ~ c(Points_Horizontal_Axis_Coordinates, Texts_Horizontal_Axis_Coordinates), type = "n", axes = F, xlab = "", ylab = "")
  Widths <- strwidth(Texts) * Character_Expansion
  Heights <- strheight(Texts) * Character_Expansion
  Text <- rep(Texts, each = 4)
  Corner <- rep(c("Upper Left", "Upper Right", "Lower Right", "Lower Left"), length.out = length(Text))
  if (Shift_Text_Vertically_Away_From_Their_Coordinates == T) {
    Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Vertical_Text_Spacing_Constant
    New_Texts_Vertical_Axis_Coordinates <- ifelse(Texts_Vertical_Axis_Coordinates >= 0, Texts_Vertical_Axis_Coordinates + Gap_Height + (Heights / 2), Texts_Vertical_Axis_Coordinates - Gap_Height - (Heights / 2))
  } else if (Shift_Text_Vertically_Away_From_Their_Coordinates == F) {
    New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates
  }
  Horizontal_Axis_Coordinates <- as.vector(sapply(seq_len(length(Texts)), function (x) {
    c(Texts_Horizontal_Axis_Coordinates[x] - (Widths[x] / 2), Texts_Horizontal_Axis_Coordinates[x] + (Widths[x] / 2), Texts_Horizontal_Axis_Coordinates[x] + (Widths[x] / 2), Texts_Horizontal_Axis_Coordinates[x] - (Widths[x] / 2))
  }))
  Vertical_Axis_Coordinates <- as.vector(sapply(seq_len(length(Texts)), function (x) {
    c(New_Texts_Vertical_Axis_Coordinates[x] + (Heights[x] / 2), New_Texts_Vertical_Axis_Coordinates[x] + (Heights[x] / 2), New_Texts_Vertical_Axis_Coordinates[x] - (Heights[x] / 2), New_Texts_Vertical_Axis_Coordinates[x] - (Heights[x] / 2))
  }))
  Text_Corner_Data_Frame <- data.frame(Text = Text, Corner = Corner, Horizontal_Axis_Coordinates = Horizontal_Axis_Coordinates, Vertical_Axis_Coordinates = Vertical_Axis_Coordinates)
  Iterative_Text_Corner_Plotting_Function <- function (Text_Corner_Data_Frame) {
    plot(c(Points_Vertical_Axis_Coordinates, Texts_Vertical_Axis_Coordinates, Text_Corner_Data_Frame$Vertical_Axis_Coordinates) ~ c(Points_Horizontal_Axis_Coordinates, Texts_Horizontal_Axis_Coordinates, Text_Corner_Data_Frame$Horizontal_Axis_Coordinates), type = "n", axes = F, xlab = "", ylab = "")
    Widths <- strwidth(Texts) * Character_Expansion
    Heights <- strheight(Texts) * Character_Expansion
    Text <- rep(Texts, each = 4)
    Corner <- rep(c("Upper Left", "Upper Right", "Lower Right", "Lower Left"), length.out = length(Text))
    if (Shift_Text_Vertically_Away_From_Their_Coordinates == T) {
      Gap_Height <- (strheight("X\nX") - (2 * strheight("X"))) * Character_Expansion * Vertical_Text_Spacing_Constant
      New_Texts_Vertical_Axis_Coordinates <- ifelse(Texts_Vertical_Axis_Coordinates >= 0, Texts_Vertical_Axis_Coordinates + Gap_Height + (Heights / 2), Texts_Vertical_Axis_Coordinates - Gap_Height - (Heights / 2))
    } else if (Shift_Text_Vertically_Away_From_Their_Coordinates == F) {
      New_Texts_Vertical_Axis_Coordinates <- Texts_Vertical_Axis_Coordinates
    }
    Horizontal_Axis_Coordinates <- as.vector(sapply(seq_len(length(Texts)), function (x) {
      c(Texts_Horizontal_Axis_Coordinates[x] - (Widths[x] / 2), Texts_Horizontal_Axis_Coordinates[x] + (Widths[x] / 2), Texts_Horizontal_Axis_Coordinates[x] + (Widths[x] / 2), Texts_Horizontal_Axis_Coordinates[x] - (Widths[x] / 2))
    }))
    Vertical_Axis_Coordinates <- as.vector(sapply(seq_len(length(Texts)), function (x) {
      c(New_Texts_Vertical_Axis_Coordinates[x] + (Heights[x] / 2), New_Texts_Vertical_Axis_Coordinates[x] + (Heights[x] / 2), New_Texts_Vertical_Axis_Coordinates[x] - (Heights[x] / 2), New_Texts_Vertical_Axis_Coordinates[x] - (Heights[x] / 2))
    }))
    New_Text_Corners <- data.frame(Text = Text, Corner = Corner, Horizontal_Axis_Coordinates = Horizontal_Axis_Coordinates, Vertical_Axis_Coordinates = Vertical_Axis_Coordinates)
    if (identical(Text_Corner_Data_Frame, New_Text_Corners)) {
      Text_Corner_Data_Frame <- New_Text_Corners
      return (Text_Corner_Data_Frame)
      break
    } else if (!identical(Text_Corner_Data_Frame, New_Text_Corners)) {
      Text_Corner_Data_Frame <- New_Text_Corners
      Iterative_Text_Corner_Plotting_Function(Text_Corner_Data_Frame)
    }
  }
  Text_Corners <- Iterative_Text_Corner_Plotting_Function(Text_Corner_Data_Frame)
  All_Horizontal_Axis_Coordinates <- c(Points_Horizontal_Axis_Coordinates, Texts_Horizontal_Axis_Coordinates, Text_Corners$Horizontal_Axis_Coordinates)
  All_Vertical_Axis_Coordinates <- c(Points_Vertical_Axis_Coordinates, Texts_Vertical_Axis_Coordinates, Text_Corners$Vertical_Axis_Coordinates)
  plot(All_Vertical_Axis_Coordinates ~ All_Horizontal_Axis_Coordinates, type = "n", axes = F, xlab = "", ylab = "")
  Axis_Limits <- par("usr")
  Horizontal_Axis_Limits <- Axis_Limits[1:2]
  Vertical_Axis_Limits <- Axis_Limits[3:4]
  Axis_Limits <- data.frame(Limit = c("Lower", "Upper"), Horizontal_Axis_Limits = Horizontal_Axis_Limits, Vertical_Axis_Limits = Vertical_Axis_Limits)
  Text_Coordinate_Data_Frame <- setNames(as.data.frame(t(sapply(split(Text_Corners, Text_Corners$Text), function (x) {
    c(mean(x$Horizontal_Axis_Coordinates), mean(x$Vertical_Axis_Coordinates))
  }))), c("Horizontal_Axis_Coordinate", "Vertical_Axis_Coordinate"))
  Text_Coordinate_Data_Frame$Text <- rownames(Text_Coordinate_Data_Frame)
  rownames(Text_Coordinate_Data_Frame) <- NULL
  Text_Coordinate_Data_Frame <- Text_Coordinate_Data_Frame[, c(which(colnames(Text_Coordinate_Data_Frame) == "Text"), which(colnames(Text_Coordinate_Data_Frame) != "Text"))]
  list(Text_Coordinate_Data_Frame = Text_Coordinate_Data_Frame, Axis_Limits = Axis_Limits)
}