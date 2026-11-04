#' Determining if a Distribution of p Values Is Statistically Significant
#'
#' This function determines if a set of p values indicates statistical significance. It was developed for a method of estimating missing data points explained in [this YouTube video](https://youtu.be/obbKyLOVCSo) (between timestamps 1:35:28 and 1:53:08). Briefly, one may estimate a missing data point by using a probability distribution that represents the associated variable well. By randomly sampling from this distribution many times, many potential estimations of the missing value may be obtained. Then, separate hypothesis tests (such as permutational analysis of variance) may be used, with each missing-value estimation being used in one of the hypothesis tests. A vector of p values will be obtained, with each p value corresponding with one of the missing-value estimations. To determine if this p-value distribution indicates statistical significance, its cumulative distribution function may be compared to an idealized cumulative distribution function for the predetermined alpha level (this idealized cumulative distribution function representing alpha is simply a horizontal line segment between quantiles 0 and alpha and at probability 0, a vertical line segment at alpha between probabilities 0 and 1, and a horizontal line segment between quantiles alpha and 1 at probability 1; this stepwise function is the cumulative distribution of just one number, alpha). Next, the areas between the empirical (p-value) cumulative distribution function and the idealized cumulative distribution function are calculated, with areas being estimated separately for regions on either side of the idealized cumulative distribution function. These two areas are necessarily between 0 and 1 (inclusive) because the entire cumulative distribution function is a unit square (both the horizontal and vertical axes are bounded by 0 and 1). Importantly, these areas represent both the number of p values falling on either side of alpha and the distances between these p values and alpha. To determine statistical significance, the area between the cumulative distribution functions to the right of (above) alpha is divided by the sum of both areas between the cumulative distribution functions (the areas on both the left and the right sides of alpha). If this test statistic is less than alpha, we state that the distribution of p values indicates statistical significance.
#'
#' This algorithm is similar to the [Kolmogorov–Smirnov test](https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test), but instead of only considering the maximum vertical distance between two cumulative distribution functions, it considers the total area between two cumulative distribution functions. The algorithm used by this function is an improvement upon the Kolmogorov–Smirnov test, particularly when a cumulative distribution function of p values (a step function) is being compared to an idealized cumulative distribution function representing a single value (alpha). Because the empirical (p-value) cumulative distribution function is a step function, and because the idealized cumulative distribution function contains just one vertical line that spans the entire space, the horizontal distances between the two cumulative distribution functions must be considered alongside the vertical distances between the two cumulative distribution functions. For example, the vector of p values containing 0.01, 0.01, and 0.01 would have the same Kolmogorov–Smirnov-test test statistic as the vector of p values containing 0.04, 0.04, and 0.04 when compared to an idealized cumulative distribution function for an alpha of 0.05, but it's obvious that the cumulative distribution function corresponding with the vector containing 0.01, 0.01, and 0.01 is farther from the idealized cumulative distribution function representing alpha (0.05) than the cumulative distribution function corresponding with the vector containing 0.04, 0.04, and 0.04 is.
#'
#' `Determining_if_a_Distribution_of_p_Values_Is_Statistically_Significant` determines if a set of p values indicates statistical significance. It was developed for a method of estimating missing data points explained in [this YouTube video](https://youtu.be/obbKyLOVCSo) (between timestamps 1:35:28 and 1:53:08). Briefly, one may estimate a missing data point by using a probability distribution that represents the associated variable well. By randomly sampling from this distribution many times, many potential estimations of the missing value may be obtained. Then, separate hypothesis tests (such as permutational analysis of variance) may be used, with each missing-value estimation being used in one of the hypothesis tests. A vector of p values will be obtained, with each p value corresponding with one of the missing-value estimations. To determine if this p-value distribution indicates statistical significance, its cumulative distribution function may be compared to an idealized cumulative distribution function for the predetermined alpha level (this idealized cumulative distribution function representing alpha is simply a horizontal line segment between quantiles 0 and alpha and at probability 0, a vertical line segment at alpha between probabilities 0 and 1, and a horizontal line segment between quantiles alpha and 1 at probability 1; this stepwise function is the cumulative distribution of just one number, alpha). Next, the areas between the empirical (p-value) cumulative distribution function and the idealized cumulative distribution function are calculated, with areas being estimated separately for regions on either side of the idealized cumulative distribution function. These two areas are necessarily between 0 and 1 (inclusive) because the entire cumulative distribution function is a unit square (both the horizontal and vertical axes are bounded by 0 and 1). Importantly, these areas represent both the number of p values falling on either side of alpha and the distances between these p values and alpha. To determine statistical significance, the area between the cumulative distribution functions to the right of (above) alpha is divided by the sum of both areas between the cumulative distribution functions (the areas on both the left and the right sides of alpha). If this test statistic is less than alpha, we state that the distribution of p values indicates statistical significance.
#' @param p_Values a vector of p values (a numeric vector containing numbers between `0` and `1` (inclusive)).
#' @param Alpha the acceptable Type I error rate. The default value for this argument is `0.05.`
#' @param Plot A logical scalar (a vector containing only 1 element) indicating whether plots should be generated. The default value for this argument is `TRUE`. If this argument is `TRUE`, two plots of the p values are returned. One is a probability density function (a histogram) and the other is a cumulative density function. The cumulative density function plot also includes an idealized cumulative density function for alpha (a horizontal line segment between quantiles 0 and alpha and at probability 0, a vertical line segment at alpha between probabilities 0 and 1, and a horizontal line segment between quantiles alpha and 1 at probability 1). The empirical cumulative density function (the cumulative density function of the p values) is compared to this idealized cumulative density function to determine statistical significance.
#'
#' @return This function returns a data frame (`Areas_Representing_p_Value_Abundances_and_Distances_From_Alpha`) containing the area corresponding with the region between the empirical (p-value) cumulative distribution function and the idealized (alpha) cumulative distribution function to the left of (below) alpha (`Below_Alpha`) and the area corresponding with the region between the empirical (p-value) cumulative distribution function and the idealized (alpha) cumulative distribution function to the right of (above) alpha (`Below_Alpha`). It also returns a proportion (`Proportion`), which is calculated be dividing the area between the empirical and idealized cumulative distribution functions to the right of (above) alpha by the sum of both areas between the empirical and idealized cumulative distribution functions (the areas on both the left and the right sides of alpha). It also returns a `logical` scalar indicating if the vector of p values indicates statistical significance (`Significance`).
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' p_Values <- rbeta(50, 0.25, 12)
#' Determining_if_a_Distribution_of_p_Values_Is_Statistically_Significant(p_Values)
#'
#' @export
Determining_if_a_Distribution_of_p_Values_Is_Statistically_Significant <- function (p_Values, Alpha = 0.05, Plot = T) {
  if (!is.numeric(p_Values)) {
    stop ("The 'p_Values' argument must be numeric.")
  }
  if (length(which(!is.na(p_Values))) == 0) {
    stop ("The 'p_Values' argument must contain at least one p value.")
  }
  if ((any(p_Values[which(is.finite(p_Values))] < 0)) | (any(p_Values[which(is.finite(p_Values))] > 1))) {
    stop ("The 'p_Values' argument must only contain numbers between 0 and 1 (inclusive).")
  }
  if (!is.numeric(Alpha)) {
    stop ("The 'Alpha' argument must be numeric.")
  }
  if (length(Alpha) > 1) {
    stop ("The 'Alpha' argument must be of length 1.")
  }
  if (is.na(Alpha)) {
    stop ("The 'Alpha' argument must not be missing.")
  }
  if (!is.logical(Plot)) {
    stop ("The 'Plot' argument must be logical.")
  }
  if (length(Plot) > 1) {
    stop ("The 'Plot' argument must be of length 1.")
  }
  if (is.na(Plot)) {
    stop ("The 'Plot' argument must not be missing.")
  }
  p_Values <- p_Values[which(!is.na(p_Values))]
  p_Values <- p_Values[order(p_Values)]
  p_Values_Less_Than_Alpha <- p_Values[which(p_Values < Alpha)]
  p_Values_Greater_Than_Alpha <- p_Values[which(p_Values > Alpha)]
  if (Plot == T) {
    par(mfrow = c(1, 2))
    hist(p_Values, freq = F, xlab = "p Value", main = "Probability Density Function", xlim = c(0, 1))
    abline(v = Alpha, lty = 2)
    plot(0, type = "n", xlim = c(0, 1), ylim = c(0, 1), main = "Cumulative Distribution Function", xlab = "p Value", ylab = "Probability")
    if ((length(p_Values_Less_Than_Alpha) > 0) & (length(p_Values_Greater_Than_Alpha) > 0)) {
      polygon(c(0, rep(p_Values_Less_Than_Alpha, each = 2), Alpha, Alpha, 0), c(0, 0, rep((seq_along(p_Values_Less_Than_Alpha) / length(p_Values)), each = 2), 0, 0), col = 3, border = NA)
      polygon(c(Alpha, Alpha, rep(p_Values_Greater_Than_Alpha, each = 2), Alpha), c(1, rep(((seq_along(p_Values_Greater_Than_Alpha) / length(p_Values)) + ((length(p_Values_Less_Than_Alpha) - 1) / length(p_Values))), each = 2), 1, 1), col = 2, border = NA)
    } else if (length(p_Values_Less_Than_Alpha) == 0) {
      polygon(c(Alpha, Alpha, rep(p_Values_Greater_Than_Alpha, each = 2), Alpha), c(1, 0, 0, rep((seq_along(p_Values_Greater_Than_Alpha) / length(p_Values)), each = 2)), col = 2, border = NA)
    } else if (length(p_Values_Greater_Than_Alpha) == 0) {
      polygon(c(0, rep(p_Values_Less_Than_Alpha, each = 2), Alpha, Alpha, 0),
              c(0, 0, rep((seq_along(p_Values_Less_Than_Alpha) / length(p_Values)), each = 2), 0, 0), col = 3, border = NA)
    }
    lapply(seq_along(p_Values), function (i) {
      j <- i / length(p_Values)
      if (i == 1) {
        segments(0, ((i - 1) / length(p_Values)), p_Values[i], ((i - 1) / length(p_Values)))
        segments(p_Values[i], ((i - 1) / length(p_Values)), p_Values[i], (i / length(p_Values)))
      } else if ((i > 1) & (i < length(p_Values))) {
        segments(p_Values[(i - 1)], ((i - 1) / length(p_Values)), p_Values[i], ((i - 1) / length(p_Values)))
        segments(p_Values[i], ((i - 1) / length(p_Values)), p_Values[i], (i / length(p_Values)))
      } else if (i == length(p_Values)) {
        segments(p_Values[(i - 1)], ((i - 1) / length(p_Values)), p_Values[i], ((i - 1) / length(p_Values)))
        segments(p_Values[i], ((i - 1) / length(p_Values)), p_Values[i], (i / length(p_Values)))
        segments(p_Values[i], 1, length(p_Values), 1)
      }
    })
    segments(0, 0, Alpha, 0, lty = 2)
    segments(Alpha, 0, Alpha, 1, lty = 2)
    segments(Alpha, 1, 1, 1, lty = 2)
  }
  Area_That_Represents_the_Abundance_of_p_Values_Less_Than_Alpha_and_Their_Distances_Below_Alpha <- sum(diff(c(p_Values_Less_Than_Alpha, Alpha)) * (seq_along(p_Values_Less_Than_Alpha) / length(p_Values)))
  Area_That_Represents_the_Abundance_of_p_Values_Greater_Than_Alpha_and_Their_Distances_Above_Alpha <- sum((diff(c(Alpha, p_Values_Greater_Than_Alpha)) * (rev(seq_along(p_Values_Greater_Than_Alpha) / length(p_Values)))))
  Areas_Representing_p_Value_Abundances_and_Distances_From_Alpha <- data.frame(Below_Alpha = Area_That_Represents_the_Abundance_of_p_Values_Less_Than_Alpha_and_Their_Distances_Below_Alpha, Above_Alpha = Area_That_Represents_the_Abundance_of_p_Values_Greater_Than_Alpha_and_Their_Distances_Above_Alpha)
  Proportion <- Area_That_Represents_the_Abundance_of_p_Values_Greater_Than_Alpha_and_Their_Distances_Above_Alpha / (Area_That_Represents_the_Abundance_of_p_Values_Less_Than_Alpha_and_Their_Distances_Below_Alpha + Area_That_Represents_the_Abundance_of_p_Values_Greater_Than_Alpha_and_Their_Distances_Above_Alpha)
  Significant <- Proportion < Alpha
  list(Areas_Representing_p_Value_Abundances_and_Distances_From_Alpha = Areas_Representing_p_Value_Abundances_and_Distances_From_Alpha, Proportion = Proportion, Significant = Significant)
}
