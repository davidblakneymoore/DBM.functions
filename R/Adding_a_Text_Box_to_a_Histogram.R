#' Adding a Text Box to a Histogram
#'
#' This function may be used to determine optimal vertical-axis limits of histograms when a text box is to be added above all the histogram bars and within the plotting area. Using the `Text` and `Text_Size` arguments, this function minimizes unused plotting space in the plotting window.
#'
#' `Adding_a_Text_Box_to_a_Histogram` may be used to determine optimal vertical-axis limits of histograms when a text box is to be added above all the histogram bars and within the plotting area. Using the `Text` and `Text_Size` arguments, this function minimizes unused plotting space in the plotting window.
#' @param Values The values to use to generate the histogram.
#' @param Text The text to be added to the histogram. This argument may be of class `'character'`, `'language'`, `'expression'`, or `'call'`.
#' @param Text_Size W. The default for this argument is `1`.
#' @param Frequency_or_Density The vertical-axis variable of the histogram. The default to this argument is `"Frequency"`.
#'
#' @return This function returns a list with two elements. The first element, `Vertical_Axis_Limits`, contains the vertical-axis limits for the histogram; these vertical axis limits optimize the space allocated to the text box - they minimize unused plotting area and they prevent text from falling on top of the histogram bars or outside the plotting area. The second element, `Text_Vertical_Coordinate`, is the vertical-axis coordinate for the text box (it is assumed that the horizontal-axis coordinate for the text box will be the horizontal center of the histogram, which can be determined using `mean(par("usr")[1:2])`).
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' set.seed(42)
#' Values <- rlnorm(100)
#' Text <- bquote(atop("Mean:" ~ .(round(mean(Values), 3)) ~ cm ^ 2, "Standard Deviation:" ~ .(round(sd(Values), 3)) ~ cm ^ 2))
#' Text_Size <- 1.25
#' Frequency_or_Density <- "Density" # This argument may be either "Frequency" or "Density" ("Frequency" is the default)
#' (Output <- Adding_a_Text_Box_to_a_Histogram(Values, Text, Text_Size = 1.125, Frequency_or_Density = "Density"))
#' hist(Values, freq = F, ylim = Output$Vertical_Axis_Limits)
#' text(mean(par("usr")[1:2]), Output$Text_Vertical_Coordinate, Text)
#'
#' @export
Adding_a_Text_Box_to_a_Histogram <- function (Values, Text, Text_Size = 1, Frequency_or_Density = "Frequency") {
  if (!is.numeric(Values)) {
    stop ("The 'Values' argument must be of class 'numeric'.")
  }
  if (length(Values) == 0) {
    stop ("The 'Values' argument must contain at least one value.")
  }
  if (all(is.na(Values))) {
    stop ("The 'Values' argument must contain at least sone nonmissing values.")
  }
  if (!all((is.character(Text)) | (is.language(Text)) | (is.expression(Text)) | (is.call(Text)))) {
    stop ("The 'Text' argument must contain something that may be plotted in a text box on a plot (such as a character string or an expression).")
  }
  if (length(Text) != 0) {
    stop ("The 'Text' argument must contain exactly one thing.")
  }
  if (is.na(Text_Size)) {
    stop ("The 'Text' argument must not be missing.")
  }
  if (!is.numeric(Text_Size)) {
    stop ("The 'Values' argument must be of class 'numeric'.")
  }
  if (length(Text_Size) != 1) {
    stop ("The 'Text_Size' argument must contain exactly one thing.")
  }
  if (is.na(Text_Size)) {
    stop ("The 'Text_Size' argument must not be missing.")
  }
  if (!is.character(Frequency_or_Density)) {
    stop ("The 'Frequency_or_Density' argument must be of class 'numeric'.")
  }
  if (length(Frequency_or_Density) != 1) {
    stop ("The 'Frequency_or_Density' argument must contain exactly one thing.")
  }
  if (Frequency_or_Density %in% c("Frequency", "Density")) {
    stop ("The 'Frequency_or_Density' argument must be either 'Frequency' or 'Density'.")
  }
  
  Recursive_Helper_Function <- function (New_Vertical_Axis_Limits) {
    if (Frequency_or_Density == "Frequency") {
      Histogram_Information <- hist(Values, ylim = New_Vertical_Axis_Limits)
    } else if (Frequency_or_Density == "Density") {
      Histogram_Information <- hist(Values, freq = F, ylim = New_Vertical_Axis_Limits)
    }
    Vertical_Axis_Limits <- New_Vertical_Axis_Limits
    Text_Height <- strheight(Text) * Text_Size
    Total_Vertical_Span <- (max(Histogram_Information$density) + Text_Height) / (1 - (3 * Proportion_of_Space_Around_the_Histogram_Top_and_Bottom))
    Extra_Space <- Proportion_of_Space_Around_the_Histogram_Top_and_Bottom * Total_Vertical_Span
    New_Vertical_Axis_Limits <- c(0, max(Histogram_Information$density) + Extra_Space + Text_Height)
    if (identical(Vertical_Axis_Limits, New_Vertical_Axis_Limits)) {
      list(Vertical_Axis_Limits = New_Vertical_Axis_Limits, Text_Vertical_Coordinate = New_Vertical_Axis_Limits[2] - (Text_Height / 2))
    } else if (!identical(Vertical_Axis_Limits, New_Vertical_Axis_Limits)) {
      Recursive_Helper_Function(New_Vertical_Axis_Limits)
    }
  }
  tryCatch (dev.off(), error = function (e) {
    NULL
  })
  if (Frequency_or_Density == "Frequency") {
    Histogram_Information <- hist(Values)
  } else if (Frequency_or_Density == "Density") {
    Histogram_Information <- hist(Values, freq = F)
  }
  Initial_Vertical_Axis_Limits <- par("usr")[3:4]
  Proportion_of_Space_Around_the_Histogram_Top_and_Bottom <- ((diff(Initial_Vertical_Axis_Limits) - max(Histogram_Information$density)) / diff(Initial_Vertical_Axis_Limits)) / 2
  Text_Height <- strheight(Text) * Text_Size
  Total_Vertical_Span <- (max(Histogram_Information$density) + Text_Height) / (1 - (3 * Proportion_of_Space_Around_the_Histogram_Top_and_Bottom))
  Extra_Space <- Proportion_of_Space_Around_the_Histogram_Top_and_Bottom * Total_Vertical_Span
  New_Vertical_Axis_Limits <- c(0, max(Histogram_Information$density) + Extra_Space + Text_Height)
  Histogram_Information <- hist(Values, freq = F, ylim = New_Vertical_Axis_Limits)
  Vertical_Axis_Limits <- New_Vertical_Axis_Limits
  Text_Height <- strheight(Text) * Text_Size
  Total_Vertical_Span <- (max(Histogram_Information$density) + Text_Height) / (1 - (3 * Proportion_of_Space_Around_the_Histogram_Top_and_Bottom))
  Extra_Space <- Proportion_of_Space_Around_the_Histogram_Top_and_Bottom * Total_Vertical_Span
  New_Vertical_Axis_Limits <- c(-Extra_Space, max(Histogram_Information$density) + Text_Height + (2 * Extra_Space))
  if (identical(Vertical_Axis_Limits, New_Vertical_Axis_Limits)) {
    list(Vertical_Axis_Limits = New_Vertical_Axis_Limits, Text_Vertical_Coordinate = New_Vertical_Axis_Limits[2] - (Text_Height / 2))
  } else if (!identical(Vertical_Axis_Limits, New_Vertical_Axis_Limits)) {
    if (Frequency_or_Density == "Frequency") {
      Histogram_Information <- hist(Values, ylim = New_Vertical_Axis_Limits)
    } else if (Frequency_or_Density == "Density") {
      Histogram_Information <- hist(Values, freq = F, ylim = New_Vertical_Axis_Limits)
    }
    Vertical_Axis_Limits <- New_Vertical_Axis_Limits
    Text_Height <- strheight(Text) * Text_Size
    Total_Vertical_Span <- (max(Histogram_Information$density) + Text_Height) / (1 - (3 * Proportion_of_Space_Around_the_Histogram_Top_and_Bottom))
    Extra_Space <- Proportion_of_Space_Around_the_Histogram_Top_and_Bottom * Total_Vertical_Span
    New_Vertical_Axis_Limits <- c(0, max(Histogram_Information$density) + Extra_Space + Text_Height)
    if (identical(Vertical_Axis_Limits, New_Vertical_Axis_Limits)) {
      list(Vertical_Axis_Limits = New_Vertical_Axis_Limits, Text_Vertical_Coordinate = New_Vertical_Axis_Limits[2] - (Text_Height / 2))
    } else if (!identical(Vertical_Axis_Limits, New_Vertical_Axis_Limits)) {
      Recursive_Helper_Function(New_Vertical_Axis_Limits)
    }
  }
}