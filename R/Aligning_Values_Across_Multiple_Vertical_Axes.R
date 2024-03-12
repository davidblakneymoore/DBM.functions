#' Aligning Values Across Multiple Vertical Axes
#'
#' This function may be used to align values across multiple vertical axes on plots.
#'
#' `Aligning_Values_Across_Multiple_Vertical_Axes` generates new axis limits for multiple vertical axes such that specified values on the various vertical axes are aligned. The user defines the values to align. Though the example uses `base` R plotting functions, these new axis limits can be used with plotting functions from any package.
#' @param ... the numeric variables comprising the vertical axes.
#' @param Data_Frame an optional argument to provide if all the `...` arguments come from the same data frame and to prevent typing the data frame name multiple times.
#' @param Values_to_Align the values you wish to align across the vertical axes. The default for this argument is a vector of `0`s. This argument must be a numeric vector and it must contain the same number of elements that there are variables being aligned.
#' @param Variable_Weights the weights assigned to each variable. To prevent certain variables from being crowded near the top or the bottom of the plot, a greater weight can be assigned to these variables, which ensures that these variables will take up more of the plotting region (at the expense of other variables, of course). The default for this argument is to assign all the variables the same weight. This argument must be a numeric vector, all the entries must be finite and nonnegative, and it must contain the same number of elements as there are variables being aligned.
#' @param Upper_Axis_Buffers the minimum fractions of blank space you wish to leave around the top of the graph for each variable (above each variable's plotted points). The default for each variable is `0.05` - in other words, at least 5 % of the space on the top of the graph will be empty above each variable's plotted points. This argument must be a numeric vector, all the entries must be between `0` and `1` (inclusive), and it must contain the same number of elements as there are variables being aligned.
#' @param Lower_Axis_Buffers the minimum fractions of blank space you wish to leave around the bottom of the graph for each variable (below each variable's plotted points). The default for each variable is `0.05` - in other words, at least 5 % of the space on the bottom of the graph will be empty below each variable's plotted points. This argument must be a numeric vector, all the entries must be between `0` and `1` (inclusive), and it must contain the same number of elements as there are variables being aligned.
#'
#' @return This function returns the new axis limits for all the vertical axes being aligned.
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' # Example 1: Three Vertical Axes on One Plot With Made-up Data
#'
#' # Generate some made-up data.
#' set.seed(100)
#' Number_of_Rows <- 100
#' Index <- seq_len(Number_of_Rows)
#' Variable_1 <- rnorm(Number_of_Rows, -10, 1)
#' Variable_2 <- rnorm(Number_of_Rows, 0, 1)
#' Variable_3 <- rnorm(Number_of_Rows, 10, 1)
#' Made_Up_Data_for_the_Aligning_Values_Across_Multiple_Vertical_Axes_Function <- data.frame(Index = Index, Variable_1 = Variable_1, Variable_2 = Variable_2, Variable_3 = Variable_3)
#'
#' # These are the values that will align on our graph.
#' Values_to_Align = c(-2, 0, 0)
#'
#' # Weigh the first variable more heavily than the other two.
#' Variable_Weights <- c(0.75, 0.125, 0.125)
#'
#' # Generate the new axis limits.
#' (Final_Vertical_Axis_Limits <- Aligning_Values_Across_Multiple_Vertical_Axes(Variable_1, Variable_2, Variable_3, Data_Frame = Made_Up_Data_for_the_Aligning_Values_Across_Multiple_Vertical_Axes_Function, Values_to_Align = Values_to_Align, Variable_Weights = Variable_Weights))
#'
#' # Make some plots.
#' tryCatch (dev.off(), error = function (e) {
#'
#' })
#' layout(matrix(c(1, 1, 2, 2, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6), ncol = 6, byrow = TRUE), heights = c((3 / 9), (5 / 9), (1 / 9)))
#' par(mar = c(6, 5, 5, 3))
#' plot(Variable_1 ~ Index, data = Made_Up_Data_for_the_Aligning_Values_Across_Multiple_Vertical_Axes_Function, xlab = "", ylab = "", main = "Variable 1", pch = 20)
#' mtext("Index", 1, line = 2.5)
#' mtext("Variable 1", 2, line = 2.5)
#' plot(Variable_2 ~ Index, data = Made_Up_Data_for_the_Aligning_Values_Across_Multiple_Vertical_Axes_Function, xlab = "", ylab = "", main = "Variable 2", pch = 20, col = 2)
#' mtext("Index", 1, line = 2.5)
#' mtext("Variable 2", 2, line = 2.5)
#' plot(Variable_3 ~ Index, data = Made_Up_Data_for_the_Aligning_Values_Across_Multiple_Vertical_Axes_Function, xlab = "", ylab = "", main = "Variable 3", pch = 20, col = 3)
#' mtext("Index", 1, line = 2.5)
#' mtext("Variable 3", 2, line = 2.5)
#' par(mar = c(6, 6, 6, 12))
#' plot(Variable_1 ~ Index, data = Made_Up_Data_for_the_Aligning_Values_Across_Multiple_Vertical_Axes_Function, xlab = "", ylab = "", yaxt = "n", pch = 20, main = "All Variables on One Plot (Values not Aligned)")
#' axis(2, at = pretty(Made_Up_Data_for_the_Aligning_Values_Across_Multiple_Vertical_Axes_Function$Variable_1))
#' mtext("Index", 1, line = 2.5)
#' mtext("Variable 1", 2, line = 2.5)
#' par(new = T)
#' plot(Variable_2 ~ Index, data = Made_Up_Data_for_the_Aligning_Values_Across_Multiple_Vertical_Axes_Function, xlab = "", ylab = "", yaxt = "n", col = 2, pch = 20)
#' axis(4, at = pretty(Made_Up_Data_for_the_Aligning_Values_Across_Multiple_Vertical_Axes_Function$Variable_2))
#' mtext("Variable 2", 4, line = 2.5)
#' par(new = T)
#' plot(Variable_3 ~ Index, data = Made_Up_Data_for_the_Aligning_Values_Across_Multiple_Vertical_Axes_Function, xlab = "", ylab = "", yaxt = "n", col = 3, pch = 20)
#' axis(4, at = pretty(Made_Up_Data_for_the_Aligning_Values_Across_Multiple_Vertical_Axes_Function$Variable_3), line = 5)
#' mtext("Variable 3", 4, line = 7.5)
#' plot(Variable_1 ~ Index, data = Made_Up_Data_for_the_Aligning_Values_Across_Multiple_Vertical_Axes_Function, xlab = "", ylab = "", yaxt = "n", ylim = Final_Vertical_Axis_Limits[[1]], pch = 20, main = "All Variables on One Plot (Values Aligned)")
#' axis(2, at = pretty(range(Final_Vertical_Axis_Limits[[1]])))
#' mtext("Index", 1, line = 2.5)
#' mtext("Variable 1", 2, line = 2.5)
#' abline(h = Values_to_Align[1], lty = 2)
#' par(new = T)
#' plot(Variable_2 ~ Index, data = Made_Up_Data_for_the_Aligning_Values_Across_Multiple_Vertical_Axes_Function, xlab = "", ylab = "", yaxt = "n", ylim = Final_Vertical_Axis_Limits[[2]], col = 2, pch = 20)
#' axis(4, at = pretty(range(Final_Vertical_Axis_Limits[[2]])))
#' mtext("Variable 2", 4, line = 2.5)
#' abline(h = Values_to_Align[2], lty = 2)
#' par(new = T)
#' plot(Variable_3 ~ Index, data = Made_Up_Data_for_the_Aligning_Values_Across_Multiple_Vertical_Axes_Function, xlab = "", ylab = "", yaxt = "n", ylim = Final_Vertical_Axis_Limits[[3]], col = 3, pch = 20)
#' axis(4, at = pretty(range(Final_Vertical_Axis_Limits[[3]])), line = 5)
#' mtext("Variable 3", 4, line = 7.5)
#' abline(h = Values_to_Align[3], lty = 2)
#' par(mar = c(0, 0, 0, 0))
#' plot(0, type = "n", axes = FALSE, xlab = "", ylab = "")
#' legend("center", xpd = TRUE, inset = c(0, -0.3), title = expression(paste(bold("Variable"))), legend = 1:3, col = 1:3, pch = 20, horiz = T)
#'
#' # Example 2: Actual Sugar Maple Sap Flow and Wood Temperature Time-Series Data With Sap Flow on the Primary Vertical Axis and Wood Temperature on the Secondary Vertical Axis
#'
#' ?DBM.functions::Sugar_Maple_Data
#'
#' # Generate the axis limits.
#' (Axis_Limits <- DBM.functions::Aligning_Values_Across_Multiple_Vertical_Axes(Sap_Flow, Wood_Temperature, Data_Frame = DBM.functions::Sugar_Maple_Data))
#'
#' # Make the plot.
#' tryCatch (dev.off(), error = function (e) {
#'
#' })
#' par(mar = c(12, 4, 4, 4))
#' plot(Sap_Flow ~ Time, DBM.functions::Sugar_Maple_Data, ylim = Axis_Limits$Sap_Flow, xlab = "", ylab = "", pch = 20, col = 4, main = "Sugar Maple Sap Flow and Wood Temperature Time-Series Plot\n(Durham, NH; 2023; 1-cm Sapwood Depth)")
#' mtext(expression(paste(bold("Sap Flow (cm * hr" ^ "-1" * ")"))), 2, line = 2.5)
#' mtext(expression(paste(bold("Time"))), 1, line = 2.5)
#' par(new = T)
#' plot(Wood_Temperature ~ Time, DBM.functions::Sugar_Maple_Data, xaxt = "n", yaxt = "n", xlab = "", ylab = "", ylim = Axis_Limits$`Sugar_Maple_Data$Wood_Temperature`, pch = 20, col = 2)
#' axis(4, at = pretty(range(Axis_Limits$Wood_Temperature)))
#' mtext(expression(paste(bold("Wood Temperature (˚ C)"))), 4, line = 2.5)
#' abline(h = 0, lty = 2, lwd = 3)
#' legend("bottom", xpd = TRUE, inset = c(0, -0.325), title = expression(paste(bold("Variable"))), legend = c("Sap Flow", "Wood Temperature"), pch = 19, col = c(4, 2), horiz = TRUE)
#' legend("bottom", xpd = TRUE, inset = c(0, -0.5), title = expression(paste(bold("Reference Line"))), legend = c(expression(paste("Sap Flow = 0 cm * hr" ^ "-1" * " and Wood Temperature = 0 ˚ C"))), lty = 2, lwd = 3, horiz = TRUE)
#'
#' @export
Aligning_Values_Across_Multiple_Vertical_Axes <- function (..., Data_Frame, Values_to_Align = rep(0, ifelse(missing(Data_Frame), length(list(...)), ncol(Data_Frame[, c(which(colnames(Data_Frame) %in% sapply(match.call(expand.dots = FALSE)$..., deparse)))]))), Variable_Weights = rep(1, ifelse(missing(Data_Frame), length(list(...)), ncol(Data_Frame[, c(which(colnames(Data_Frame) %in% sapply(match.call(expand.dots = FALSE)$..., deparse)))]))), Upper_Axis_Buffers = rep(0.05, ifelse(missing(Data_Frame), length(list(...)), ncol(Data_Frame[, c(which(colnames(Data_Frame) %in% sapply(match.call(expand.dots = FALSE)$..., deparse)))]))), Lower_Axis_Buffers = rep(0.05, ifelse(missing(Data_Frame), length(list(...)), ncol(Data_Frame[, c(which(colnames(Data_Frame) %in% sapply(match.call(expand.dots = FALSE)$..., deparse)))])))) {
  Variable_Names <- sapply(match.call(expand.dots = FALSE)$..., deparse)
  if (missing(Data_Frame)) {
    Data_Frame <- as.data.frame(list(...))
    colnames(Data_Frame) <- Variable_Names
  } else if (!missing(Data_Frame)) {
    if (class(Data_Frame) != "data.frame") {
      stop ("The 'Data_Frame' argument must be of class 'data.frame'.")
    }
    Data_Frame <- Data_Frame[, which(colnames(Data_Frame) %in% Variable_Names)]
  }
  Data_Frame <- Data_Frame[which(complete.cases(Data_Frame)), ]
  Data_Frame <- Data_Frame[which(sapply(seq_len(nrow(Data_Frame)), function (x) {
    is.finite(unlist(Data_Frame[x, ]))
  })), ]
  if (ncol(Data_Frame) <= 1) {
    stop ("There must be more than one variable to warrant aligning vertical axes.")
  }
  if (nrow(Data_Frame) < 1) {
    stop ("There must be at least one row where all variables (columns) contain finite, nonmissing values to warrant aligning vertical axes.")
  }
  if (!all(sapply(Data_Frame, is.numeric))) {
    stop ("All variables to align must be numeric.")
  }
  if (length(Values_to_Align) != ncol(Data_Frame)) {
    stop ("The 'Values_to_Align' argument must have the same number of elements as there are variables to align across vertical axes.")
  }
  if (!is.numeric(Values_to_Align) | any(!is.finite(Values_to_Align))) {
    stop ("The 'Values_to_Align' argument must be numeric.")
  }
  if (length(Variable_Weights) != ncol(Data_Frame)) {
    stop ("The 'Variable_Weights' argument must have the same number of elements as there are variables to align across vertical axes.")
  }
  if (!is.numeric(Variable_Weights) | any(Variable_Weights < 0) | any(!is.finite(Variable_Weights))) {
    stop ("The 'Variable_Weights' argument must contain numeric, nonnegative values.")
  }
  if (!is.numeric(Upper_Axis_Buffers) | any(Upper_Axis_Buffers < 0) | any(Upper_Axis_Buffers > 1) | any(!is.finite(Upper_Axis_Buffers))) {
    stop ("The 'Upper_Axis_Buffers' argument must contain numeric values that are between 0 and 1 (inclusive).")
  }
  if (length(Upper_Axis_Buffers) != ncol(Data_Frame)) {
    stop ("The 'Upper_Axis_Buffers' argument must contain the same number of elements as there are variables to align across vertical axes.")
  }
  if (!is.numeric(Lower_Axis_Buffers) | any(Lower_Axis_Buffers < 0) | any(Lower_Axis_Buffers > 1) | any(!is.finite(Lower_Axis_Buffers))) {
    stop ("The 'Lower_Axis_Buffers' argument must contain numeric values that are between 0 and 1 (inclusive).")
  }
  if (length(Lower_Axis_Buffers) != ncol(Data_Frame)) {
    stop ("The 'Lower_Axis_Buffers' argument must contain the same number of elements as there are variables to align across vertical axes.")
  }
  Values_to_Align <- as.list(Values_to_Align)
  Variable_Weights <- Variable_Weights / sum(Variable_Weights)
  Variable_Weights <- as.list(Variable_Weights)
  Number_of_Variables <- ncol(Data_Frame)
  Ranges <- lapply(Data_Frame, function (x) {
    c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
  })
  Ranges <- mapply(function (x, y) {
    if ((x[1] < y) & (x[2] < y)) {
      x[2] <- y
    } else if ((x[1] > y) & (x[2] > y)) {
      x[1] <- y
    }
    x
  }, x = Ranges, y = Values_to_Align, SIMPLIFY = FALSE)
  Ratios <- mapply(function (v, w) {
    (w - v[1]) / (v[2] - v[1])
  }, v = Ranges, w = Values_to_Align, SIMPLIFY = FALSE)
  Final_Ratio <- sum(mapply(function (a, b) {
    a * b
  }, a = Ratios, b = Variable_Weights))
  New_Ranges <- mapply(function (u, v, w, x) {
    if (x > Final_Ratio) {
      c(v[1], (v[1] + ((w - v[1]) / Final_Ratio)))
    } else if (x == Final_Ratio) {
      c(v[1], v[2])
    } else if (x < Final_Ratio) {
      c((v[2] - ((v[2] - w) / (1 - Final_Ratio))), v[2])
    }
  }, v = Ranges, w = Values_to_Align, x = Ratios, SIMPLIFY = FALSE)
  Final_Ranges <- mapply(function (x, y, z) {
    c((x[1] - (diff(c(x[1], x[2])) * z)), (x[2] + (diff(c(x[1], x[2])) * y)))
  }, x = New_Ranges, y = Upper_Axis_Buffers, z = Lower_Axis_Buffers, SIMPLIFY = FALSE)
  names(Final_Ranges) <- Variable_Names
  Final_Ranges <- lapply(Final_Ranges, function (x) {
    names(x) <- c("Minimum", "Maximum")
    x
  })
  Final_Ranges
}
