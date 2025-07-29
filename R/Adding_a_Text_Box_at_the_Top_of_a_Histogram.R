#' Adding a Text Box at the Top of a Histogram
#'
#' This function may be used to determine optimal vertical-axis limits of histograms when a text box is to be added above all the histogram bars and within the plotting area. Using the `Text` and `Text_Size` arguments, this function minimizes unused plotting space in the plotting window.
#'
#' `Adding_a_Text_Box_at_the_Top_of_a_Histogram` may be used to determine optimal vertical-axis limits of histograms when a text box is to be added above all the histogram bars and within the plotting area. Using the `Text` and `Text_Size` arguments, this function minimizes unused plotting space in the plotting window.
#'
#' This function works by using the data provided to make a histogram, extracting the vertical-axis limits of the histogram, determining how large the desired text is based on the existing plotting region, and then calculating new vertical-axis limits based on the range of the response variable and the text height. Then, these new vertical-axis limits are used to plot the data again, the text size is again determined based on this new plotting area, and another new set of vertical-axis limits are determined using the same criterion from the previous iteration. This iterative process repeats until it converges to a particular set of vertical-axis limits.
#' @param Values The values to use to generate the histogram.
#' @param Text The text to be added to the histogram. This argument may be of class `'character'`, `'language'`, `'expression'`, or `'call'`.
#' @param Text_Size The size of the desired text in the text box. The default for this argument is `1`.
#' @param Plot_Margins The plot margins, which are passed to the `mar` argument of the `par()` function. The default value for this argument is the default plot margins, `c(5, 4, 4, 2) + 0.1`.
#' @param Tolerance How close together vertical-axis limits from consecutive iterations must be for the algorithm to converge to a solution. The default value for this argument is `sqrt(.Machine$double.eps)`, where `.Machine$double.eps` is the smallest number R recognizes.
#' @param ... Other arguments (such as `breaks`, `freq`, and `xlim`) which are passed to the `hist()` function within this function. Please note that this function allows for the use of any of the primary arguments of the `hist()` function, but it does not support additional arguments of the `hist()` function accessible through its `...` argument (for more information, run `help(hist)`).
#'
#' @return This function returns a list with two elements. The first element, `Vertical_Axis_Limits`, contains the vertical-axis limits for the histogram; these vertical axis limits optimize the space allocated to the text box - they minimize unused plotting area and they prevent text from falling on top of the histogram bars or outside the plotting area. The second element, `Text_Vertical_Coordinate`, is the vertical-axis coordinate for the text box (it is assumed that the horizontal-axis coordinate for the text box will be the horizontal center of the histogram, which can be determined using `mean(par("usr")[1:2])`).
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' set.seed(42)
#' Areas <- rlnorm(100)
#' Text <- bquote(atop("Mean:" ~ .(round(mean(Areas), 3)) ~ cm ^ 2, "Standard Deviation:" ~ .(round(sd(Areas), 3)) ~ cm ^ 2))
#' Text_Size <- 1.125
#' Plot_Margins <- c(5, 5, 4, 1)
#' (Output <- Adding_a_Text_Box_at_the_Top_of_a_Histogram(Areas, Text, Text_Size = Text_Size, Plot_Margins = Plot_Margins))
#' par(mar = Plot_Margins)
#' hist(Areas, ylim = Output$Vertical_Axis_Limits, xlab = expression(paste("Area (cm" ^ "2" * ")")), main = "Example Histogram With a Text Box")
#' text(mean(par("usr")[1:2]), Output$Text_Vertical_Coordinate, Text, cex = Text_Size)
#'
#' @export
Adding_a_Text_Box_at_the_Top_of_a_Histogram <- function (Values, Text, Text_Size = 1, Plot_Margins = c(5, 4, 4, 2) + 0.1, Tolerance = sqrt(.Machine$double.eps), ...) {
  if (!is.numeric(Values)) {
    stop ("The 'Values' argument must be of class 'numeric'.")
  }
  if (length(Values) == 0) {
    stop ("The 'Values' argument must contain at least one value.")
  }
  if (all(is.na(Values))) {
    stop ("The 'Values' argument must contain at least some nonmissing values.")
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
  Extra_Arguments <- list(...)
  Valid_Arguments <- names(formals(hist.default))
  Valid_Arguments <- Valid_Arguments[which(Valid_Arguments != '...')]
  Invalid_Arguments <- setdiff(names(Extra_Arguments), Valid_Arguments)
  if (length(Invalid_Arguments) == 1) {
    stop (paste0("'", Invalid_Arguments, "' is not a valid argument."))
  } else if (length(Invalid_Arguments) == 2) {
    stop (paste0("'", Invalid_Arguments[1], "' and '", Invalid_Arguments[2], "' are not valid arguments."))
  } else if (length(Invalid_Arguments) > 2) {
    stop (paste0(Reduce(function (a, b) {
      paste0("'", b, "', '", b, "'")
    }, Invalid_Arguments[seq_len(length(Invalid_Arguments) - 1)]), ", and '", Invalid_Arguments[length(Invalid_Arguments)], "' are not valid arguments."))
  }
  if ("breaks" %in% names(Extra_Arguments)) {
    if (is.numeric(Extra_Arguments$breaks)) {
      if (length(Extra_Arguments$breaks) == 1) {
        if (is.na(Extra_Arguments$breaks)) {
          stop("The 'breaks' argument, if specified, must not be missing.")
        }
        if ((Extra_Arguments$breaks < 1) | ((Extra_Arguments$breaks %% 1) != 0)) {
          stop ("If the 'breaks' argument is a single number, it must be a positive integer.")
        }
      } else if (length(Extra_Arguments$breaks) > 1) {
        if (any(is.na(Extra_Arguments$breaks))) {
          stop ("The 'breaks' argument, if specified, must not contain any missing values.")
        }
        if ((min(Values, na.rm = T) < min(Extra_Arguments$breaks)) | (max(Values, na.rm = T) > max(Extra_Arguments$breaks))) {
          stop ("The 'breaks' argument does not span the range of the values being plotted.")
        }
      }
    }
    if (is.character(Extra_Arguments$breaks)) {
      if (length(Extra_Arguments$breaks) > 1) {
        stop ("If the 'breaks' argument is of class 'character', it must contain only one item.")
      }
      if (is.na(Extra_Arguments$breaks)) {
        stop ("The 'breaks' argument, if specified, must not be missing.")
      }
      if (!(Extra_Arguments$breaks %in% c("Sturges", "Scott", "FD", "Freedman-Diaconis"))) {
        stop ("If the 'breaks' argument is of class 'character', it must be either 'Sturges', 'Scott', 'FD', or 'Freedman-Diaconis'.")
      }
    }
    if (is.function(Extra_Arguments$breaks)) {
      Breaks <- Extra_Arguments$breaks(x)
      if (!is.numeric(Breaks)) {
        stop ("The function provided for the 'breaks' argument does not return an object of class 'numeric'.")
      } else if (is.numeric(Breaks)) {
        if (length(Breaks) == 1) {
          if (is.na(Breaks)) {
            stop("The function provided for the 'breaks' argument, if specified, must not return a missing value.")
          }
          if ((Breaks < 1) | ((Breaks %% 1) != 0)) {
            stop ("If the function provided for the 'breaks' argument returns a single number, it must be a positive integer.")
          }
        } else if (length(Breaks) > 1) {
          if (any(is.na(Breaks))) {
            stop ("The function provided for the 'breaks' argument must not return any missing values.")
          }
          if ((min(Values, na.rm = T) < min(Breaks)) | (max(Values, na.rm = T) > max(Breaks))) {
            stop ("The function provided for the 'breaks' argument does not return an object that spans the range of the values being plotted.")
          }
        }
      }
    }
  }
  if ("freq" %in% names(Extra_Arguments)) {
    if (!is.logical(Extra_Arguments$freq)) {
      stop ("The 'freq' argument must be of class 'logical'.")
    }
    if (length(Extra_Arguments$freq) > 1) {
      stop ("The 'freq' argument must be of length 1.")
    }
    if (is.na(Extra_Arguments$freq)) {
      stop ("The 'freq' argument, if specified, must not be missing.")
    }
  }
  if ("probability" %in% names(Extra_Arguments)) {
    if (!is.logical(Extra_Arguments$probability)) {
      stop ("The 'probability' argument must be of class 'logical'.")
    }
    if (length(Extra_Arguments$probability) > 1) {
      stop ("The 'probability' argument must be of length 1.")
    }
    if (is.na(Extra_Arguments$probability)) {
      stop ("The 'probability' argument, if specified, must not be missing.")
    }
    if (Extra_Arguments$probability != !Extra_Arguments$freq) {
      stop ("The 'probability' argument must be the opposite of the 'freq' argument.")
    }
  }
  if ("include.lowest" %in% names(Extra_Arguments)) {
    if (!is.logical(Extra_Arguments$include.lowest)) {
      stop ("The 'include.lowest' argument must be of class 'logical'.")
    }
    if (length(Extra_Arguments$include.lowest) > 1) {
      stop ("The 'include.lowest' argument must be of length 1.")
    }
    if (is.na(Extra_Arguments$include.lowest)) {
      stop ("The 'include.lowest' argument, if specified, must not be missing.")
    }
  }
  if ("right" %in% names(Extra_Arguments)) {
    if (!is.logical(Extra_Arguments$right)) {
      stop ("The 'right' argument must be of class 'logical'.")
    }
    if (length(Extra_Arguments$right) > 1) {
      stop ("The 'right' argument must be of length 1.")
    }
    if (is.na(Extra_Arguments$right)) {
      stop ("The 'right' argument, if specified, must not be missing.")
    }
  }
  if ("fuzz" %in% names(Extra_Arguments)) {
    if (!is.numeric(Extra_Arguments$fuzz)) {
      stop ("The 'fuzz' argument must be of class 'numeric'.")
    }
    if (length(Extra_Arguments$fuzz) > 1) {
      stop ("The 'fuzz' argument must be of length 1.")
    }
    if (is.na(Extra_Arguments$fuzz)) {
      stop ("The 'fuzz' argument, if specified, must not be missing.")
    }
    if (Extra_Arguments$fuzz < 0) {
      stop ("The 'fuzz' argument must not be negative.")
    }
  }
  if ("density" %in% names(Extra_Arguments)) {
    if (!is.numeric(Extra_Arguments$density)) {
      stop ("The 'density' argument must be of class 'numeric'.")
    }
  }
  if ("angle" %in% names(Extra_Arguments)) {
    if (!is.numeric(Extra_Arguments$angle)) {
      stop ("The 'angle' argument must be of class 'numeric'.")
    }
  }
  if ("col" %in% names(Extra_Arguments)) {
    if (tryCatch ({
      col2rgb(Extra_Arguments$col)
      TRUE
    }, error = function(e) {
      FALSE
    })) {
      stop ("The 'col' argument must be something R interprets as a color.")
    }
  }
  if ("border" %in% names(Extra_Arguments)) {
    if (tryCatch ({
      col2rgb(Extra_Arguments$border)
      TRUE
    }, error = function(e) {
      FALSE
    })) {
      stop ("The 'border' argument must be something R interprets as a color.")
    }
  }
  if ("main" %in% names(Extra_Arguments)) {
    if (!all((is.character(Extra_Arguments$main)) | (is.language(Extra_Arguments$main)) | (is.expression(Extra_Arguments$main)) | (is.call(Extra_Arguments$main)))) {
      stop ("The 'main' argument must contain something of class 'character', 'language', 'expression', or 'call'.")
    }
  }
  if ("xlim" %in% names(Extra_Arguments)) {
    if (!is.numeric(Extra_Arguments$xlim)) {
      stop ("The 'xlim' argument must be of class 'numeric'.")
    }
    if (length(Extra_Arguments$xlim) != 2) {
      stop ("The 'xlim' argument must contain exactly 2 numbers.")
    }
    if (any(is.na(Extra_Arguments$xlim))) {
      stop ("The 'xlim' argument, if specified, must not contain any missing values.")
    }
  }
  if ("ylim" %in% names(Extra_Arguments)) {
    if (!is.numeric(Extra_Arguments$ylim)) {
      stop ("The 'ylim' argument must be of class 'numeric'.")
    }
    if (length(Extra_Arguments$ylim) != 2) {
      stop ("The 'ylim' argument must contain exactly 2 numbers.")
    }
    if (any(is.na(Extra_Arguments$ylim))) {
      stop ("The 'ylim' argument, if specified, must not contain any missing values.")
    }
  }
  if ("xlab" %in% names(Extra_Arguments)) {
    if (!all((is.character(Extra_Arguments$xlab)) | (is.language(Extra_Arguments$xlab)) | (is.expression(Extra_Arguments$xlab)) | (is.call(Extra_Arguments$xlab)))) {
      stop ("The 'xlab' argument must contain something of class 'character', 'language', 'expression', or 'call'.")
    }
  }
  if ("ylab" %in% names(Extra_Arguments)) {
    if (!all((is.character(Extra_Arguments$ylab)) | (is.language(Extra_Arguments$ylab)) | (is.expression(Extra_Arguments$ylab)) | (is.call(Extra_Arguments$ylab)))) {
      stop ("The 'ylab' argument must contain something of class 'character', 'language', 'expression', or 'call'.")
    }
  }
  if ("axes" %in% names(Extra_Arguments)) {
    if (!is.logical(Extra_Arguments$axes)) {
      stop ("The 'axes' argument must be of class 'logical'.")
    }
    if (length(Extra_Arguments$axes) > 1) {
      stop ("The 'axes' argument must be of length 1.")
    }
    if (is.na(Extra_Arguments$axes)) {
      stop ("The 'axes' argument, if specified, must not be missing.")
    }
  }
  if ("plot" %in% names(Extra_Arguments)) {
    if (!is.logical(Extra_Arguments$plot)) {
      stop ("The 'plot' argument must be of class 'logical'.")
    }
    if (length(Extra_Arguments$plot) > 1) {
      stop ("The 'plot' argument must be of length 1.")
    }
    if (is.na(Extra_Arguments$plot)) {
      stop ("The 'plot' argument, if specified, must not be missing.")
    }
  }
  if ("labels" %in% names(Extra_Arguments)) {
    if (is.logical(Extra_Arguments$labels)) {
      if (length(Extra_Arguments$labels) > 1) {
        stop ("If the 'labels' argument is of class 'logical', it must be of length 1.")
      }
      if (is.na(Extra_Arguments$labels)) {
        stop ("If the 'labels' argument if specified and is of class 'logical', it must not be missing.")
      }
    } else if ((!is.logical(Extra_Arguments$labels)) & (!is.character(Extra_Arguments$labels))) {
      stop ("The 'labels' argument must be either of class 'logical' or 'character'.")
    }
  }
  if ("nclass" %in% names(Extra_Arguments)) {
    if (!is.numeric(Extra_Arguments$nclass)) {
      stop ("The 'nclass' argument must be numeric.")
    }
    if (length(Extra_Arguments$nclass) != 1) {
      stop ("The 'nclass' argument must be of length 1.")
    }
    if (Extra_Arguments$nclass < 1) {
      stop ("The 'nclass' argument must be positive.")
    }
    if ((Extra_Arguments$nclass %% 1) != 0) {
      stop ("The 'nclass' argument must be an integer.")
    }
  }
  if ("warn.unused" %in% names(Extra_Arguments)) {
    if (!is.logical(Extra_Arguments$warn.unused)) {
      stop ("The 'warn.unused' argument must be of class 'logical'.")
    }
    if (length(Extra_Arguments$warn.unused) > 1) {
      stop ("The 'warn.unused' argument must be of length 1.")
    }
    if (is.na(Extra_Arguments$warn.unused)) {
      stop ("The 'warn.unused' argument, if specified, must not be missing.")
    }
  }
  if ("ylim" %in% names(Extra_Arguments)) {
    stop ("You may not specify the 'ylim' argument while using this function because this function defines the vertical-axis limits internally.")
  }
  Recursive_Helper_Function <- function (New_Vertical_Axis_Limits) {
    hist(Values, ylim = New_Vertical_Axis_Limits, ...)
    Vertical_Axis_Limits <- New_Vertical_Axis_Limits
    Text_Height <- strheight(Text) * Text_Size
    Total_Vertical_Span <- (max(Vertical_Axis_Coordinates) + Text_Height) / (1 - (3 * Proportion_of_Space_Around_the_Histogram_Top_and_Bottom))
    Extra_Vertical_Axis_Space <- Proportion_of_Space_Around_the_Histogram_Top_and_Bottom * Total_Vertical_Span
    New_Vertical_Axis_Limits <- c(0, max(Vertical_Axis_Coordinates) + Extra_Vertical_Axis_Space + Text_Height)
    if (as.character(all.equal(Vertical_Axis_Limits, New_Vertical_Axis_Limits, tolerance = Tolerance)) == "TRUE") {
      list(Vertical_Axis_Limits = New_Vertical_Axis_Limits, Text_Vertical_Coordinate = New_Vertical_Axis_Limits[2] - (Text_Height / 2))
    } else if (!(as.character(all.equal(Vertical_Axis_Limits, New_Vertical_Axis_Limits, tolerance = Tolerance)) == "TRUE")) {
      Recursive_Helper_Function(New_Vertical_Axis_Limits)
    }
  }
  par(mar = Plot_Margins)
  Histogram_Information <- hist(Values, ...)
  Initial_Horizontal_Axis_Limits <- par("usr")[1:2]
  Initial_Vertical_Axis_Limits <- par("usr")[3:4]
  if (!("freq" %in% names(Extra_Arguments))) {
    if ("probability" %in% names(Extra_Arguments)) {
      if (Extra_Arguments$probability == T) {
        Vertical_Axis_Coordinates <- Histogram_Information$density
      } else if (Extra_Arguments$probability == F) {
        Vertical_Axis_Coordinates <- Histogram_Information$counts
      }
    } else if (!("probability" %in% names(Extra_Arguments))) {
      Vertical_Axis_Coordinates <- Histogram_Information$counts
    }
  } else if ("freq" %in% names(Extra_Arguments)) {
    if (Extra_Arguments$freq == F) {
      Vertical_Axis_Coordinates <- Histogram_Information$density
    } else if (Extra_Arguments$freq == T) {
      Vertical_Axis_Coordinates <- Histogram_Information$counts
    }
  }
  Proportion_of_Space_Around_the_Histogram_Top_and_Bottom <- (((diff(Initial_Vertical_Axis_Limits) - max(Vertical_Axis_Coordinates)) / diff(Initial_Vertical_Axis_Limits))) / 2
  Text_Width <- strwidth(Text) * Text_Size
  if (Text_Width > diff(range(Histogram_Information$breaks))) {
    stop ("The plotting window is too narrow and this algorithm will not converge. Please make the plotting window wider or reduce the size of the text and try again.")
  }
  Text_Height <- strheight(Text) * Text_Size
  Total_Vertical_Span <- (max(Vertical_Axis_Coordinates) + Text_Height) / (1 - (3 * Proportion_of_Space_Around_the_Histogram_Top_and_Bottom))
  Extra_Vertical_Axis_Space <- Proportion_of_Space_Around_the_Histogram_Top_and_Bottom * Total_Vertical_Span
  if ((Text_Height + Extra_Vertical_Axis_Space) >= diff(range(Vertical_Axis_Coordinates))) {
    stop ("The plotting window is too short and this algorithm will not converge. Please make the plotting window taller or reduce the size of the text and try again.")
  }
  New_Vertical_Axis_Limits <- c(0, max(Vertical_Axis_Coordinates) + Extra_Vertical_Axis_Space + Text_Height)
  hist(Values, ylim = New_Vertical_Axis_Limits, ...)
  Vertical_Axis_Limits <- New_Vertical_Axis_Limits
  Text_Height <- strheight(Text) * Text_Size
  Total_Vertical_Span <- (max(Vertical_Axis_Coordinates) + Text_Height) / (1 - (3 * Proportion_of_Space_Around_the_Histogram_Top_and_Bottom))
  Extra_Vertical_Axis_Space <- Proportion_of_Space_Around_the_Histogram_Top_and_Bottom * Total_Vertical_Span
  New_Vertical_Axis_Limits <- c(0, max(Vertical_Axis_Coordinates) + Extra_Vertical_Axis_Space + Text_Height)
  if (as.character(all.equal(Vertical_Axis_Limits, New_Vertical_Axis_Limits, tolerance = Tolerance)) == "TRUE") {
    list(Vertical_Axis_Limits = New_Vertical_Axis_Limits, Text_Vertical_Coordinate = New_Vertical_Axis_Limits[2] - (Text_Height / 2))
  } else if (!(as.character(all.equal(Vertical_Axis_Limits, New_Vertical_Axis_Limits, tolerance = Tolerance)) == "TRUE")) {
    Recursive_Helper_Function(New_Vertical_Axis_Limits)
  }
}
