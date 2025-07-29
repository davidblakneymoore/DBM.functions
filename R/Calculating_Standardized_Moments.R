#' Calculating_Standardized_Moments
#'
#' This function calculates, for a set of observations, [standardized moments](https://en.wikipedia.org/wiki/Standardized_moment). It assumes that the observations are a population, not a sample, and therefore it does not perform any bias corrections.
#'
#' `Calculating_Standardized_Moments` calculates, for a set of observations, [standardized moments](https://en.wikipedia.org/wiki/Standardized_moment). It assumes that the observations are a population, not a sample, and therefore it does not perform any bias corrections.
#' @param Values an R object containing the observations for which the standardized moments will be calculated.
#' @param Moments The moments (in integer form) desired.
#'
#' @return This function returns a vector of the standardized moments desired.
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' Calculating_Standardized_Moments(rlnorm(10), c(1:2, 4:7))
#'
#' @export
Calculating_Standardized_Moments <- function (Values, Moments) {
  if (!is.numeric(Values)) {
    stop ("The 'Values' argument must be of class 'numeric'.")
  }
  if (any(is.na(Values))) {
    warning ("The 'Values' argument contains missing values; the calculations were performed after omitting the missing values.")
  }
  if (!is.integer(Moments)) {
    stop ("The 'Moments' argument must be of class 'integer'.")
  }
  if (any(is.na(Moments))) {
    stop ("The 'Moments' argument must not contain any missing values.")
  }
  Values <- Values[which(!is.na(Values))]
  Moment_Names <- c("Mean", "Variance", "Skewness", "Kurtosis", "Hyperskewness", "Hypertailedness")
  if (max(Moments) <= 6) {
    Moment_Names <- Moment_Names[seq_len(max(Moments))]
  } else if (max(Moments) > 6) {
    Moment_Names <- c(Moment_Names, paste0(Ordinalizing_Integers(setdiff(seq_len(max(Moments)), seq_along(Moment_Names))), "-Order Moment"))
  }
  Moment_Names <- Moment_Names[Moments]
  setNames(sapply(Moments, function (i) {
    (mean((Values - mean(Values)) ^ i)) / ((mean((Values - mean(Values)) ^ 2)) ^ (i / 2))
  }), Moment_Names)
}