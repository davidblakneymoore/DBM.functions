#' Making Reasonable Scale Bars
#'
#' This function generates nice round numbers for scale bars using a slick trick: the base-10 logarithm of a given proportion (such as 20 % of the plotting area, as defined by the `Scale_Bar_Factor` argument) is calculated, and then rounded (using the `floor()` and `ceiling()` functions) to the nearest integer, and then the number 10 is raised to these round numbers. This process produces nice, round numbers for scale bars which are based on powers of 10. Using the `floor()` and `ceiling()` functions, unless the given proportion of the plotting area is exactly a multiple of 10, two scale bar possibilities will be generated - one smaller one and one larger one - and the one that's closer to the originally given proportion of the plotting area (the `Scale_Bar_Factor` argument) will be chosen as the final scale bar.
#'
#' `Making_Reasonable_Scale_Bars` generates nice round numbers for scale bars using a slick trick: the base-10 logarithm of a given proportion (such as 20 % of the plotting area, as defined by the `Scale_Bar_Factor` argument) is calculated, and then rounded (using the `floor()` and `ceiling()` functions) to the nearest integer, and then the number 10 is raised to these round numbers. This process produces nice, round numbers for scale bars which are based on powers of 10. Using the `floor()` and `ceiling()` functions, unless the given proportion of the plotting area is exactly a multiple of 10, two scale bar possibilities will be generated - one smaller one and one larger one - and the one that's closer to the originally given proportion of the plotting area (the `Scale_Bar_Factor` argument) will be chosen as the final scale bar.
#'
#' @param Axis_Limits the axis limits from the plot that will receive the scale bar. It is permissible to use either the horizontal or the vertical axis limits, and in some cases, you may wish to generate scale bars for both axes' variables.
#' @param Scale_Bar_Factor the approximate relative proportion of the plotting area that the scale bar should take up (in the direction of the axis being used). This argument should be a number between 0 and 1 (exclusive). The default value for this argument, `0.2`, makes it so that the scale bar will take up as close to 20 % of the axis' range as possible.
#'
#' @return This function returns the numbers that serve as both the text for the scale bar and the relative locations within the plotting area where the tick marks should occur.
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' tryCatch(dev.off(), error = function (e) {
#'
#' })
#' set.seed(410)
#' Plot_1_Horizontal_Axis_Variable <- rbeta(100, 0.5, 0.5)
#' Plot_1_Vertical_Axis_Variable <- runif(100)
#' plot(Plot_1_Vertical_Axis_Variable ~ Plot_1_Horizontal_Axis_Variable, type = "n", xlab = "", ylab = "", axes = F)
#' Plot_1_Horizontal_Axis_Coordinates <- par("usr")[1:2]
#' Plot_1_New_Horizontal_Axis_Coordinates <- c((Plot_1_Horizontal_Axis_Coordinates[1] - (diff(Plot_1_Horizontal_Axis_Coordinates) * 0.3)), Plot_1_Horizontal_Axis_Coordinates[2])
#' Plot_1_Vertical_Axis_Limits <- par("usr")[3:4]
#' Plot_1_Scale_Bar_Numbers <- Making_Reasonable_Scale_Bars(Plot_1_Vertical_Axis_Limits)
#' Plot_2_Horizontal_Axis_Variable <- rcauchy(100)
#' Plot_2_Vertical_Axis_Variable <- runif(100, 990, 9000)
#' plot(Plot_2_Vertical_Axis_Variable ~ Plot_2_Horizontal_Axis_Variable, type = "n", xlab = "", ylab = "", axes = F)
#' Plot_2_Horizontal_Axis_Coordinates <- par("usr")[1:2]
#' Plot_2_New_Horizontal_Axis_Coordinates <- c((Plot_2_Horizontal_Axis_Coordinates[1] - (diff(Plot_2_Horizontal_Axis_Coordinates) * 0.3)), Plot_2_Horizontal_Axis_Coordinates[2])
#' Plot_2_Vertical_Axis_Limits <- par("usr")[3:4]
#' Plot_2_Scale_Bar_Numbers <- Making_Reasonable_Scale_Bars(Plot_2_Vertical_Axis_Limits)
#' par(mfrow = c(1, 2))
#' plot(Plot_1_Vertical_Axis_Variable ~ Plot_1_Horizontal_Axis_Variable, xlim = Plot_1_New_Horizontal_Axis_Coordinates, main = "Example Plot 1", xlab = "Horizontal Axis Variable", ylab = "Vertical Axis Variable")
#' Plot_1_Horizontal_Scale_Bar_Coordinate <- (diff(par("usr")[1:2]) * 0.2) + par("usr")[1]
#' Plot_1_Vertical_Scale_Bar_Tick_Coordinates <- Plot_1_Scale_Bar_Numbers + mean(Plot_1_Vertical_Axis_Limits) - Plot_1_Scale_Bar_Numbers[3]
#' segments(Plot_1_Horizontal_Scale_Bar_Coordinate, Plot_1_Vertical_Scale_Bar_Tick_Coordinates[1], Plot_1_Horizontal_Scale_Bar_Coordinate, Plot_1_Vertical_Scale_Bar_Tick_Coordinates[length(Plot_1_Vertical_Scale_Bar_Tick_Coordinates)])
#' sapply(seq_len(length(Plot_1_Scale_Bar_Numbers)), function (x) {
#'   segments(Plot_1_Horizontal_Scale_Bar_Coordinate, Plot_1_Vertical_Scale_Bar_Tick_Coordinates[x], Plot_1_Horizontal_Scale_Bar_Coordinate - (diff(Plot_1_New_Horizontal_Axis_Coordinates) * 0.02), Plot_1_Vertical_Scale_Bar_Tick_Coordinates[x])
#' })
#' sapply(seq_len(length(Plot_1_Scale_Bar_Numbers)), function (x) {
#'   text(Plot_1_Horizontal_Scale_Bar_Coordinate - (diff(Plot_1_New_Horizontal_Axis_Coordinates) * 0.08), Plot_1_Vertical_Scale_Bar_Tick_Coordinates[x], Plot_1_Scale_Bar_Numbers[x])
#' })
#' plot(Plot_2_Vertical_Axis_Variable ~ Plot_2_Horizontal_Axis_Variable, xlim = Plot_2_New_Horizontal_Axis_Coordinates, main = "Example Plot 2", xlab = "Horizontal Axis Variable", ylab = "Vertical Axis Variable")
#' Plot_2_Horizontal_Scale_Bar_Coordinate <- (diff(par("usr")[1:2]) * 0.2) + par("usr")[1]
#' Plot_2_Vertical_Scale_Bar_Tick_Coordinates <- Plot_2_Scale_Bar_Numbers + mean(Plot_2_Vertical_Axis_Limits) - Plot_2_Scale_Bar_Numbers[3]
#' segments(Plot_2_Horizontal_Scale_Bar_Coordinate, Plot_2_Vertical_Scale_Bar_Tick_Coordinates[1], Plot_2_Horizontal_Scale_Bar_Coordinate, Plot_2_Vertical_Scale_Bar_Tick_Coordinates[length(Plot_2_Vertical_Scale_Bar_Tick_Coordinates)])
#' sapply(seq_len(length(Plot_2_Scale_Bar_Numbers)), function (x) {
#'   segments(Plot_2_Horizontal_Scale_Bar_Coordinate, Plot_2_Vertical_Scale_Bar_Tick_Coordinates[x], Plot_2_Horizontal_Scale_Bar_Coordinate - (diff(Plot_2_New_Horizontal_Axis_Coordinates) * 0.02), Plot_2_Vertical_Scale_Bar_Tick_Coordinates[x])
#' })
#' sapply(seq_len(length(Plot_2_Scale_Bar_Numbers)), function (x) {
#'   text(Plot_2_Horizontal_Scale_Bar_Coordinate - (diff(Plot_2_New_Horizontal_Axis_Coordinates) * 0.08), Plot_2_Vertical_Scale_Bar_Tick_Coordinates[x], Plot_2_Scale_Bar_Numbers[x])
#' })
#'
#' @export
Making_Reasonable_Scale_Bars <- function (Axis_Limits, Scale_Bar_Factor = 0.2) {
  if (length(Axis_Limits) != 2) {
    stop ("The 'Axis_Limits' argument must be of length 2.")
  }
  if (any(is.na(Axis_Limits))) {
    stop ("Please make sure that there are no missing values in the 'Axis_Limits' argument.")
  }
  if (!is.numeric(Axis_Limits)) {
    stop ("The 'Axis_Limits' argument must be numeric.")
  }
  if (length(Scale_Bar_Factor) != 1) {
    stop ("The 'Scale_Bar_Factor' argument must be of length 1.")
  }
  if (is.na(Scale_Bar_Factor)) {
    stop ("The 'Scale_Bar_Factor' argument cannot be missing.")
  }
  if (!is.numeric(Scale_Bar_Factor)) {
    stop ("The 'Scale_Bar_Factor' argument must be numeric.")
  }
  if ((Scale_Bar_Factor <= 0) | (Scale_Bar_Factor >= 1)) {
    stop ("The 'Scale_Bar_Factor' argument must be a number between 0 and 1 (exclusive).")
  }
  Smaller_Scale_Bar_Numbers <- (10 ^ floor(log10(diff(Axis_Limits) * Scale_Bar_Factor))) * c(0, 0.5, 1, 2)
  Larger_Scale_Bar_Numbers <- (10 ^ ceiling(log10(diff(Axis_Limits) * Scale_Bar_Factor))) * c(0, 0.5, 1, 2)
  if (all(Smaller_Scale_Bar_Numbers == Larger_Scale_Bar_Numbers)) {
    Scale_Bar_Numbers <- (10 ^ round(log10(diff(Axis_Limits) * Scale_Bar_Factor))) * c(0, 0.5, 1, 2)
  } else if (!all(Smaller_Scale_Bar_Numbers == Larger_Scale_Bar_Numbers)) {
    if (diff(c(min(Larger_Scale_Bar_Numbers), max(Larger_Scale_Bar_Numbers))) >= diff(Axis_Limits)) {
      Scale_Bar_Numbers <- Smaller_Scale_Bar_Numbers
    } else if (diff(c(min(Larger_Scale_Bar_Numbers), max(Larger_Scale_Bar_Numbers))) < diff(Axis_Limits)) {
      if (abs(diff(c(min(Larger_Scale_Bar_Numbers), max(Larger_Scale_Bar_Numbers))) - (diff(Axis_Limits) * Scale_Bar_Factor)) > abs((diff(Axis_Limits) * Scale_Bar_Factor) - diff(c(min(Smaller_Scale_Bar_Numbers), max(Smaller_Scale_Bar_Numbers))))) {
        Scale_Bar_Numbers <- Smaller_Scale_Bar_Numbers
      } else if (abs(diff(c(min(Larger_Scale_Bar_Numbers), max(Larger_Scale_Bar_Numbers))) - (diff(Axis_Limits) * Scale_Bar_Factor)) < abs((diff(Axis_Limits) * Scale_Bar_Factor) - diff(c(min(Smaller_Scale_Bar_Numbers), max(Smaller_Scale_Bar_Numbers))))) {
        Scale_Bar_Numbers <- Larger_Scale_Bar_Numbers
      } else if (abs(diff(c(min(Larger_Scale_Bar_Numbers), max(Larger_Scale_Bar_Numbers))) - (diff(Axis_Limits) * Scale_Bar_Factor)) == abs((diff(Axis_Limits) * Scale_Bar_Factor) - diff(c(min(Smaller_Scale_Bar_Numbers), max(Smaller_Scale_Bar_Numbers))))) {
        Random_Number <- sample(1:2, 1)
        if (Random_Number == 1) {
          Scale_Bar_Numbers <- Smaller_Scale_Bar_Numbers
        } else if (Random_Number == 2) {
          Scale_Bar_Numbers <- Larger_Scale_Bar_Numbers
        }
      }
    }
  }
  Scale_Bar_Numbers
}
