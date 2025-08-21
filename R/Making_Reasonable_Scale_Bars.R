#' Making Reasonable Scale Bars
#'
#' This function generates nice round numbers for scale bars using a slick trick: the logarithm of one or more specified bases (2, 5, and 10 are good choices) of a given proportion of the plotting area (such as 20 %, as defined by the `Scale_Bar_Factor` argument) is calculated, and then rounded (using the `floor()` and `ceiling()` functions) to the nearest integer, and then each specified base is raised to these corresponding round numbers. This process produces nice, round numbers for scale bars which are based on powers of the specified bases. The possibility that this algorithm finds that's closest to the originally given proportion of the plotting area (the `Scale_Bar_Factor` argument) will be chosen as the final scale bar.
#'
#' `Making_Reasonable_Scale_Bars` generates nice round numbers for scale bars using a slick trick: the logarithm of one or more specified bases (2, 5, and 10 are good choices) of a given proportion of the plotting area (such as 20 %, as defined by the `Scale_Bar_Factor` argument) is calculated, and then rounded (using the `floor()` and `ceiling()` functions) to the nearest integer, and then each specified base is raised to these corresponding round numbers. This process produces nice, round numbers for scale bars which are based on powers of the specified bases. The possibility that this algorithm finds that's closest to the originally given proportion of the plotting area (the `Scale_Bar_Factor` argument) will be chosen as the final scale bar.
#'
#' @param Axis_Limits the axis limits from the plot that will receive the scale bar. It is permissible to use either the horizontal or the vertical axis limits, and in some cases, you may wish to generate scale bars for both axes' variables.
#' @param Scale_Bar_Bases the logarithm bases used to calculate the scale bars. Choose bases that produce round numbers when multiplied by `0.5`. The default bases are `2`, `5`, and `10`.
#' @param Scale_Bar_Factor the approximate relative proportion of the plotting area that the scale bar should take up (in the direction of the axis being used). This argument should be a number between 0 and 1 (exclusive). The default value for this argument, `0.25`, makes it so that the scale bar will take up as close to 25 % of the axis' range as possible.
#' @param Rounding_Method if two options are equally close to the `Scale_Bar_Factor` argument, how one should be chosen. For example, if the `Scale_Bar_Factor` argument is `0.25` (if it is desired that the scale bar takes up 25 % of the axis) and one scale bar possibility takes up 15 % and another scale bar possibility takes up 35 %, then this argument is used to determine which option to use. The possible values this argument can take are `"Up"` and `"Down"` (with the default value being `"Down"`).
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
#' Plot_1_Scale_Bar_Numbers <- Making_Reasonable_Scale_Bars(Plot_1_Vertical_Axis_Limits, Scale_Bar_Bases = 10)
#' Plot_2_Horizontal_Axis_Variable <- rcauchy(100)
#' Plot_2_Vertical_Axis_Variable <- runif(100, 990, 9000)
#' plot(Plot_2_Vertical_Axis_Variable ~ Plot_2_Horizontal_Axis_Variable, type = "n", xlab = "", ylab = "", axes = F)
#' Plot_2_Horizontal_Axis_Coordinates <- par("usr")[1:2]
#' Plot_2_New_Horizontal_Axis_Coordinates <- c((Plot_2_Horizontal_Axis_Coordinates[1] - (diff(Plot_2_Horizontal_Axis_Coordinates) * 0.3)), Plot_2_Horizontal_Axis_Coordinates[2])
#' Plot_2_Vertical_Axis_Limits <- par("usr")[3:4]
#' Plot_2_Scale_Bar_Numbers <- Making_Reasonable_Scale_Bars(Plot_2_Vertical_Axis_Limits, Scale_Bar_Bases = 10)
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
Making_Reasonable_Scale_Bars <- function (Axis_Limits, Scale_Bar_Bases = c(2, 5, 10), Scale_Bar_Factor = 0.25, Rounding_Method = "Down") {
  if (!is.vector(Axis_Limits) | is.list(Axis_Limits)) {
    stop ("The 'Axis_Limits' argument must be a vector.")
  }
  if (is.null(Axis_Limits)) {
    stop("The 'Axis_Limits' argument must be provided.")
  }
  if (length(Axis_Limits) != 2) {
    stop ("The 'Axis_Limits' argument must be of length 2.")
  }
  if (any(is.na(Axis_Limits))) {
    stop ("Please make sure that there are no missing values in the 'Axis_Limits' argument.")
  }
  if (!is.numeric(Axis_Limits)) {
    stop ("The 'Axis_Limits' argument must be numeric.")
  }
  if (is.null(Scale_Bar_Bases)) {
    stop("The 'Scale_Bar_Bases' argument must be provided.")
  }
  if (!is.vector(Scale_Bar_Bases) | is.list(Scale_Bar_Bases)) {
    stop ("The 'Scale_Bar_Bases' argument must be a vector.")
  }
  if (length(Scale_Bar_Bases) == 0) {
    stop ("The 'Scale_Bar_Bases' argument must contain at least one element.")
  }
  if (!is.numeric(Scale_Bar_Bases)) {
    stop ("The 'Scale_Bar_Bases' argument must be numeric.")
  }
  if (any(is.na(Scale_Bar_Bases))) {
    stop ("The 'Scale_Bar_Bases' argument must not contain any missing values.")
  }
  if (length(Scale_Bar_Factor) != 1) {
    stop ("The 'Scale_Bar_Factor' argument must be of length 1.")
  }
  if (is.null(Scale_Bar_Factor)) {
    stop ("The 'Scale_Bar_Factor' argument must be provided.")
  }
  if (is.na(Scale_Bar_Factor)) {
    stop ("The 'Scale_Bar_Factor' argument cannot be missing.")
  }
  if (!is.numeric(Scale_Bar_Factor)) {
    stop ("The 'Scale_Bar_Factor' argument must be numeric.")
  }
  if (length(Scale_Bar_Factor) != 1) {
    stop ("The 'Scale_Bar_Factor' argument must contain only one element.")
  }
  if ((Scale_Bar_Factor <= 0) | (Scale_Bar_Factor >= 1)) {
    stop ("The 'Scale_Bar_Factor' argument must be a number between 0 and 1 (exclusive).")
  }
  if (is.null(Rounding_Method)) {
    stop ("The 'Rounding_Method' argument must be provided.")
  }
  if (is.na(Rounding_Method)) {
    stop ("The 'Rounding_Method' argument cannot be missing.")
  }
  if (!is.character(Rounding_Method)) {
    stop ("The 'Rounding_Method' argument must be a character string.")
  }
  if (length(Rounding_Method) != 1) {
    stop ("The 'Rounding_Method' argument must contain only one element.")
  }
  if (!(Rounding_Method %in% c("Up", "Down"))) {
    stop ("The 'Rounding_Method' argument must be either 'Up' or 'Down'.")
  }
  Axis_Range <- diff(Axis_Limits)
  Target <- Axis_Range * Scale_Bar_Factor
  Possibilities <- do.call("cbind", lapply(Scale_Bar_Bases, function (x) {
    cbind((x ^ floor(log(Target, x))) * c(0, 0.5, 1, 2), (x ^ ceiling(log(Target, x))) * c(0, 0.5, 1, 2))
  }))
  Possibilities <- Possibilities[, which(!sapply(seq_len(ncol(Possibilities)), function (x) {
    diff(Possibilities[c(1, 4), x]) > Axis_Range
  }))]
  Differences <- sapply(Possibilities[4, ], function (x) {
    abs(x - Target)
  })
  Differences <- Differences[which(Differences < Axis_Range)]
  Positions <- which(Differences == min(Differences))
  if (length(unique(Possibilities[4, Positions])) == 1) {
    Final_Value <- unique(Possibilities[4, Positions])
  } else if (length(unique(Possibilities[4, Positions])) != 1) {
    if (Rounding_Method == "Up") {
      Final_Value <- max(unique(Possibilities[4, Positions]))
    } else if (Rounding_Method == "Down") {
      Final_Value <- min(unique(Possibilities[4, Positions]))
    }
  }
  if (length(which(Possibilities[4, ] == Final_Value)) == 1) {
    Scale_Bar_Numbers <- Possibilities[, which(Possibilities[4, ] == Final_Value)]
  } else if (length(which(Possibilities[4, ] == Final_Value)) > 1) {
    Final_Possibility <- as.data.frame(t(Possibilities[, which(Possibilities[4, ] == Final_Value)]))
    Scale_Bar_Numbers <- setNames(unlist(Final_Possibility[-duplicated(Final_Possibility), ]), NULL)
  }
  Scale_Bar_Numbers
}
