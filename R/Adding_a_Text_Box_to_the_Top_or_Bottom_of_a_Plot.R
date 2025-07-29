#' Adding a Text Box to the Top or Bottom of a Plot
#'
#' This function may be used to determine optimal vertical-axis limits of plots when a text box is to be added at the top or the bottom of the plotting region. This function minimizes unused plotting space in the plotting window and maximizes the amount of space allocated toward the text and the points or lines.
#'
#' `Adding_a_Text_Box_to_the_Top_or_Bottom_of_a_Plot` may be used to determine optimal vertical-axis limits of plots when a text box is to be added at the top or the bottom of the plotting region. This function minimizes unused plotting space in the plotting window and maximizes the amount of space allocated toward the text and the points or lines.
#'
#' This function works by plotting the provided data, extracting the vertical-axis limits of the plot, determining how large the desired text is based on the existing plotting region, and then calculating new vertical-axis limits based on the range of the response variable and the text height. Then, these new vertical-axis limits are used to make another plot using the data, the text size is again determined based on this new plotting area, and another new set of vertical-axis limits are determined using the same criterion from the previous iteration. This iterative process repeats until it converges to a particular set of vertical-axis limits.
#' @param Horizontal_Coordinates The horizontal-axis values of the plot.
#' @param Vertical_Coordinates The vertical-axis values of the plot.
#' @param Text The text to be added to the histogram. This argument may be of class `'character'`, `'language'`, `'expression'`, or `'call'`.
#' @param Text_Box_Location The position of the text box. The possible values for this argument are `"Top"` and `"Bottom"`. The default for this argument is `"Top"`.
#' @param Text_Size The size of the desired text in the text box. The default for this argument is `1`.
#' @param Plot_Margins The plot margins, which are passed to the `mar` argument of the `par()` function. The default value for this argument is the default plot margins, `c(5, 4, 4, 2) + 0.1`.
#' @param Tolerance How close together vertical-axis limits from consecutive iterations must be for the algorithm to converge to a solution. The default value for this argument is `sqrt(.Machine$double.eps)`, where `.Machine$double.eps` is the smallest number R recognizes.
#'
#' @return This function returns a list with three elements. The first element, `Vertical_Axis_Limits`, contains the vertical-axis limits for the plot; these vertical-axis limits optimize the space allocated to the text box - they minimize unused plotting area and they prevent text from falling on top of the points or outside of the plotting area. The second element, `Text_Vertical_Coordinate`, is the vertical-axis coordinate for the text box (it is assumed that the horizontal-axis coordinate for the text box will be the horizontal center of the histogram, which can be determined using `mean(par("usr")[1:2])`). The third element, `Text_Box_Border_Coordinates`, contains both the horizontal-axis and vertical-axis coordinates of the border of the text box, which may be plotted using the `polygon()` function if it's desired.
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' set.seed(41)
#' Horizontal_Coordinates <- rnorm(10)
#' Vertical_Coordinates <- rnorm(10)
#' Text <- bquote(atop("Predictor Variable Mean:" ~ .(round(mean(Horizontal_Coordinates), 3)) ~ cm ^ 2, "Response Variable Mean:" ~ .(round(mean(Vertical_Coordinates), 3)) ~ cm ^ 2))
#' Text_Size <- 1.125
#' Plot_Margins <- c(5, 5, 4, 1)
#' Add_a_Text_Box_Border <- T
#' (Output <- Adding_a_Text_Box_to_the_Top_or_Bottom_of_a_Plot(Horizontal_Coordinates, Vertical_Coordinates, Text, Text_Size = Text_Size, Plot_Margins = Plot_Margins))
#' par(mar = Plot_Margins)
#' plot(Horizontal_Coordinates, Vertical_Coordinates, ylim = Output$Vertical_Axis_Limits, xlab = expression(paste("Area 1 (cm" ^ "2" * ")")), , ylab = expression(paste("Area 2 (cm" ^ "2" * ")")), main = "Example Plot With a Text Box")
#' text(mean(par("usr")[1:2]), Output$Text_Vertical_Coordinate, Text, cex = Text_Size)
#' if (Add_a_Text_Box_Border == T) {
#'   polygon(Output$Text_Box_Border_Coordinates$Horizontal_Axis_Coordinates, Output$Text_Box_Border_Coordinates$Vertical_Axis_Coordinates)
#' }
#'
#' @export
Adding_a_Text_Box_to_the_Top_or_Bottom_of_a_Plot <- function (Horizontal_Coordinates, Vertical_Coordinates, Text, Text_Box_Location = "Top", Text_Size = 1, Plot_Margins = c(5, 4, 4, 2) + 0.1, Tolerance = sqrt(.Machine$double.eps)) {
  if (!is.numeric(Horizontal_Coordinates)) {
    stop ("The 'Horizontal_Coordinates' argument must be of class 'numeric'.")
  }
  if (length(Horizontal_Coordinates) == 0) {
    stop ("The 'Horizontal_Coordinates' argument must contain at least one value.")
  }
  if (all(is.na(Horizontal_Coordinates))) {
    stop ("The 'Horizontal_Coordinates' argument must contain at least sone nonmissing values.")
  }
  if (!is.numeric(Vertical_Coordinates)) {
    stop ("The 'Vertical_Coordinates' argument must be of class 'numeric'.")
  }
  if (length(Vertical_Coordinates) == 0) {
    stop ("The 'Vertical_Coordinates' argument must contain at least one value.")
  }
  if (all(is.na(Vertical_Coordinates))) {
    stop ("The 'Vertical_Coordinates' argument must contain at least some nonmissing values.")
  }
  if (length(Horizontal_Coordinates) != length(Vertical_Coordinates)) {
    stop ("The 'Horizontal_Coordinates' and 'Vertical_Coordinates' arguments must contain the same number of elements.")
  }
  if (!all((is.character(Text)) | (is.language(Text)) | (is.expression(Text)) | (is.call(Text)))) {
    stop ("The 'Text' argument must contain something of class 'character', 'language', 'expression', or 'call'.")
  }
  if (!is.numeric(Text_Size)) {
    stop ("The 'Text_Size' argument must be of class 'numeric'.")
  }
  if (length(Text_Size) != 1) {
    stop ("The 'Text_Size' argument must contain exactly one thing.")
  }
  if (is.na(Text_Size)) {
    stop ("The 'Text_Size' argument must not be missing.")
  }
  if (!is.character(Text_Box_Location)) {
    stop ("The 'Text_Box_Location' argument must be of class 'character'.")
  }
  if (length(Text_Box_Location) != 1) {
    stop ("The 'Text_Box_Location' argument must be of length 1.")
  }
  if (is.na(Text_Box_Location)) {
    stop ("The 'Text_Box_Location' argument must not be missing.")
  }
  if (!(Text_Box_Location %in% c("Top", "Bottom"))) {
    stop ("The 'Text_Box_Location' argument must be either 'Top' or 'Bottom'.")
  }
  Recursive_Helper_Function <- function (New_Vertical_Axis_Limits) {
    plot(Horizontal_Coordinates, Vertical_Coordinates, ylim = New_Vertical_Axis_Limits, type = "n", axes = F, xlab = "", ylab = "")
    Vertical_Axis_Limits <- New_Vertical_Axis_Limits
    Text_Height <- strheight(Text) * Text_Size
    Total_Vertical_Span <- (diff(range(Vertical_Coordinates)) + Text_Height) / (1 - (3 * Proportion_of_Space_Around_the_Plot_Top_and_Bottom))
    Extra_Vertical_Axis_Space <- Proportion_of_Space_Around_the_Plot_Top_and_Bottom * Total_Vertical_Span
    if (Text_Box_Location == "Top") {
      New_Vertical_Axis_Limits <- c(min(Vertical_Coordinates), max(Vertical_Coordinates) + Extra_Vertical_Axis_Space + Text_Height)
    } else if (Text_Box_Location == "Bottom") {
      New_Vertical_Axis_Limits <- c(min(Vertical_Coordinates) - Extra_Vertical_Axis_Space - Text_Height, max(Vertical_Coordinates))
    }
    if (as.character(all.equal(Vertical_Axis_Limits, New_Vertical_Axis_Limits, tolerance = Tolerance)) == "TRUE") {
      if (Text_Box_Location == "Top") {
        list(Vertical_Axis_Limits = New_Vertical_Axis_Limits, Text_Vertical_Coordinate = (New_Vertical_Axis_Limits[2] - (Text_Height / 2)), Text_Box_Border_Coordinates = list(Horizontal_Axis_Coordinates = c((mean(par("usr")[1:2]) - (Text_Width / 2)), (mean(par("usr")[1:2]) + (Text_Width / 2)), (mean(par("usr")[1:2]) + (Text_Width / 2)), (mean(par("usr")[1:2]) - (Text_Width / 2)), (mean(par("usr")[1:2]) - (Text_Width / 2))), Vertical_Axis_Coordinates = c(New_Vertical_Axis_Limits[2], New_Vertical_Axis_Limits[2], (New_Vertical_Axis_Limits[2] - Text_Height), (New_Vertical_Axis_Limits[2] - Text_Height), New_Vertical_Axis_Limits[2])))
      } else if (Text_Box_Location == "Bottom") {
        list(Vertical_Axis_Limits = New_Vertical_Axis_Limits, Text_Vertical_Coordinate = (New_Vertical_Axis_Limits[1] + (Text_Height / 2)), Text_Box_Border_Coordinates = list(Horizontal_Axis_Coordinates = c((mean(par("usr")[1:2]) - (Text_Width / 2)), (mean(par("usr")[1:2]) + (Text_Width / 2)), (mean(par("usr")[1:2]) + (Text_Width / 2)), (mean(par("usr")[1:2]) - (Text_Width / 2)), (mean(par("usr")[1:2]) - (Text_Width / 2))), Vertical_Axis_Coordinates = c((New_Vertical_Axis_Limits[1] + Text_Height), (New_Vertical_Axis_Limits[1] + Text_Height), New_Vertical_Axis_Limits[1], New_Vertical_Axis_Limits[1], (New_Vertical_Axis_Limits[1] + Text_Height))))
      }
    } else if (!(as.character(all.equal(Vertical_Axis_Limits, New_Vertical_Axis_Limits, tolerance = Tolerance)) == "TRUE")) {
      Recursive_Helper_Function(New_Vertical_Axis_Limits)
    }
  }
  par(mar = Plot_Margins)
  plot(Horizontal_Coordinates, Vertical_Coordinates, type = "n", axes = F, xlab = "", ylab = "")
  Initial_Vertical_Axis_Limits <- par("usr")[3:4]
  Proportion_of_Space_Around_the_Plot_Top_and_Bottom <- (((diff(Initial_Vertical_Axis_Limits) - diff(range(Vertical_Coordinates))) / diff(Initial_Vertical_Axis_Limits))) / 2
  Text_Width <- strwidth(Text) * Text_Size
  if (Text_Width > diff(range(Horizontal_Coordinates))) {
    stop ("The plotting window is too narrow and this algorithm will not converge. Please make the plotting window wider or reduce the size of the text and try again.")
  }
  Text_Height <- strheight(Text) * Text_Size
  Total_Vertical_Span <- (diff(range(Vertical_Coordinates)) + Text_Height) / (1 - (3 * Proportion_of_Space_Around_the_Plot_Top_and_Bottom))
  Extra_Vertical_Axis_Space <- Proportion_of_Space_Around_the_Plot_Top_and_Bottom * Total_Vertical_Span
  if ((Text_Height + Extra_Vertical_Axis_Space) >= diff(range(Vertical_Coordinates))) {
    stop ("The plotting window is too short and this algorithm will not converge. Please make the plotting window taller or reduce the size of the text and try again.")
  }
  if (Text_Box_Location == "Top") {
    New_Vertical_Axis_Limits <- c(min(Vertical_Coordinates), max(Vertical_Coordinates) + Extra_Vertical_Axis_Space + Text_Height)
  } else if (Text_Box_Location == "Bottom") {
    New_Vertical_Axis_Limits <- c(min(Vertical_Coordinates) - Extra_Vertical_Axis_Space - Text_Height, max(Vertical_Coordinates))
  }
  plot(Horizontal_Coordinates, Vertical_Coordinates, ylim = New_Vertical_Axis_Limits, type = "n", axes = F, xlab = "", ylab = "")
  Vertical_Axis_Limits <- New_Vertical_Axis_Limits
  Text_Height <- strheight(Text) * Text_Size
  Total_Vertical_Span <- (diff(range(Vertical_Coordinates)) + Text_Height) / (1 - (3 * Proportion_of_Space_Around_the_Plot_Top_and_Bottom))
  Extra_Vertical_Axis_Space <- Proportion_of_Space_Around_the_Plot_Top_and_Bottom * Total_Vertical_Span
  if (Text_Box_Location == "Top") {
    New_Vertical_Axis_Limits <- c(min(Vertical_Coordinates), max(Vertical_Coordinates) + Extra_Vertical_Axis_Space + Text_Height)
  } else if (Text_Box_Location == "Bottom") {
    New_Vertical_Axis_Limits <- c(min(Vertical_Coordinates) - Extra_Vertical_Axis_Space - Text_Height, max(Vertical_Coordinates))
  }
  if (as.character(all.equal(Vertical_Axis_Limits, New_Vertical_Axis_Limits, tolerance = Tolerance)) == "TRUE") {
    if (Text_Box_Location == "Top") {
      list(Vertical_Axis_Limits = New_Vertical_Axis_Limits, Text_Vertical_Coordinate = (New_Vertical_Axis_Limits[2] - (Text_Height / 2)), Text_Box_Border_Coordinates = list(Horizontal_Axis_Coordinates = c((mean(par("usr")[1:2]) - (Text_Width / 2)), (mean(par("usr")[1:2]) + (Text_Width / 2)), (mean(par("usr")[1:2]) + (Text_Width / 2)), (mean(par("usr")[1:2]) - (Text_Width / 2)), (mean(par("usr")[1:2]) - (Text_Width / 2))), Vertical_Axis_Coordinates = c(New_Vertical_Axis_Limits[2], New_Vertical_Axis_Limits[2], (New_Vertical_Axis_Limits[2] - Text_Height), (New_Vertical_Axis_Limits[2] - Text_Height), New_Vertical_Axis_Limits[2])))
    } else if (Text_Box_Location == "Bottom") {
      list(Vertical_Axis_Limits = New_Vertical_Axis_Limits, Text_Vertical_Coordinate = (New_Vertical_Axis_Limits[1] + (Text_Height / 2)), Text_Box_Border_Coordinates = list(Horizontal_Axis_Coordinates = c((mean(par("usr")[1:2]) - (Text_Width / 2)), (mean(par("usr")[1:2]) + (Text_Width / 2)), (mean(par("usr")[1:2]) + (Text_Width / 2)), (mean(par("usr")[1:2]) - (Text_Width / 2)), (mean(par("usr")[1:2]) - (Text_Width / 2))), Vertical_Axis_Coordinates = c((New_Vertical_Axis_Limits[1] + Text_Height), (New_Vertical_Axis_Limits[1] + Text_Height), New_Vertical_Axis_Limits[1], New_Vertical_Axis_Limits[1], (New_Vertical_Axis_Limits[1] + Text_Height))))
    }
  } else if (!(as.character(all.equal(Vertical_Axis_Limits, New_Vertical_Axis_Limits, tolerance = Tolerance)) == "TRUE")) {
    Recursive_Helper_Function(New_Vertical_Axis_Limits)
  }
}
