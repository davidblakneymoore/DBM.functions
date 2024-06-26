#' Comparing Multiple Independent Correlation Coefficients
#'
#' This function determines if correlation coefficients from independent correlations are statistically significantly different (Levy, 1977).
#'
#' `Comparing_Multiple_Independent_Correlation_Coefficients` compares multiple correlation coefficients from independent correlations (Levy, 1977). When independence of correlations cannot be assumed, please consider using a different procedure. This function only uses `base` R functions, but it was heavily inspired by the `agricolae` package, particularly the `orderPvalue()` and `lastC()` functions (de Mendiburu, 2017).
#' @param Correlation_Coefficients a numeric vector containing the correlation coefficients to be analyzed.
#' @param Numbers_of_Observations a numeric or an integer vector containing the numbers of observations that went in to each of the corresponding correlation coefficients.
#' @param Identifiers a character or a factor vector containing names to identify each corresponding correlation coefficient.
#' @param Data_Frame an optional data frame to include such that column names can be supplied for the first three arguments (the data frame that these columns are from should be provided for the `Data_Frame` argument).
#' @param Alpha a value of alpha against which significance can be tested (the default is `0.05`).
#' @param Control_for_Experimentwise_Error a logical argument specifying whether or not this function should give conservative estimates (by holding the experimentwise error rate at the given value of `Alpha`) or liberal estimates (by using the given value of `Alpha` for each pairwise comparison). The default, `TRUE`, holds the experimentwise error rate at `Alpha` and calculates the comparisonwise error rate based on the number of pairwise comparisons. Since this procedure assumes independence of correlations, the Šidák correction, which is used when there is independence, is used (Šidák, 1967).
#'
#' @return This function returns a data frame containing p values for each pairwise correlation coefficient comparison, a data frame containing separation lettering, and a data frame containing metadata.
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @references de Mendiburu, F. 2017. agricolae: Statistical Procedures for Agricultural Research. R package version 1.2-8. \url{https://CRAN.R-project.org/package=agricolae}.
#' @references Levy, K.J. 1977. Pairwise comparisons involving unequal sample sizes associated with correlations, proportions or variances. Br. J. Math. Stat. Psychol. 30:137-139.
#' @references Šidák, Z.K. 1967. Rectangular Confidence Regions for the Means of Multivariate Normal Distributions. J. Am. Stat. Assoc. 62: 626–633.
#'
#' @examples
#' # Generate Some Made-up Data
#' Made_Up_Data_for_the_Comparing_Multiple_Independent_Correlation_Coefficients_Function <- structure(list(Name = c("Correlation A", "Correlation B", "Correlation C", "Correlation D", "Correlation E"), Coefficient_of_Correlation = c(-0.339, 0.307, 0.919, -0.679, -0.495), Sample_Size = c(42L, 10L, 46L, 98L, 63L)), class = "data.frame", row.names = c(NA, -5L))
#'
#' # Compare the Correlation Coefficients
#' (DBM.functions::Comparing_Multiple_Independent_Correlation_Coefficients(Coefficient_of_Correlation, Sample_Size, Name, Made_Up_Data_for_the_Comparing_Multiple_Independent_Correlation_Coefficients_Function))
#'
#' @export
Comparing_Multiple_Independent_Correlation_Coefficients <- function (Correlation_Coefficients, Numbers_of_Observations, Identifiers, Data_Frame, Alpha = 0.05, Control_for_Experimentwise_Error = TRUE) {
  if (missing(Correlation_Coefficients)) {
    stop ("The 'Correlation_Coefficients' argument is missing.")
  }
  if (missing(Numbers_of_Observations)) {
    stop ("The 'Numbers_of_Observations' argument is missing.")
  }
  if (missing(Identifiers)) {
    stop ("The 'Identifiers' argument is missing.")
  }
  Correlation_Coefficients_Name <- deparse(substitute(Correlation_Coefficients))
  Numbers_of_Observations_Name <- deparse(substitute(Numbers_of_Observations))
  Identifiers_Name <- deparse(substitute(Identifiers))
  if (!missing(Data_Frame)) {
    if (class(Data_Frame) != 'data.frame') {
      stop ("'Data_Frame' must be of class 'data.frame'.")
    }
    if (length(unique(length(Data_Frame[, which(colnames(Data_Frame) == Correlation_Coefficients_Name)]), length(Data_Frame[, which(colnames(Data_Frame) == Numbers_of_Observations_Name)]), length(Data_Frame[, which(colnames(Data_Frame) == Identifiers_Name)]))) != 1) {
      stop ("'Correlation_Coefficients', 'Numbers_of_Observations', and 'Identifiers' must all contain the same number of elements.")
    }
    Data_Frame <- data.frame(Correlation_Coefficients = Data_Frame[, which(colnames(Data_Frame) == Correlation_Coefficients_Name)], Numbers_of_Observations = Data_Frame[, which(colnames(Data_Frame) == Numbers_of_Observations_Name)], Identifiers = Data_Frame[, which(colnames(Data_Frame) == Identifiers_Name)])
  } else if (missing(Data_Frame)) {
    Data_Frame <- data.frame(Correlation_Coefficients = Correlation_Coefficients, Numbers_of_Observations = Numbers_of_Observations, Identifiers = Identifiers)
  }
  Correlation_Coefficients <- Data_Frame$Correlation_Coefficients
  Numbers_of_Observations <- Data_Frame$Numbers_of_Observations
  Identifiers <- Data_Frame$Identifiers
  if (!is.numeric(Correlation_Coefficients)) {
    stop ("'Correlation_Coefficients' must be numeric.")
  }
  if (!is.numeric(Numbers_of_Observations)) {
    stop ("'Numbers_of_Observations' must be numeric.")
  }
  if (!is.numeric(Alpha) | length(Alpha) != 1) {
    stop ("'Alpha' must be a single numeric value.")
  }
  if (Alpha < 0 | Alpha > 1) {
    stop ("'Alpha' must be a number between 0 and 1 (inclusive).")
  }
  if (!is.logical(Control_for_Experimentwise_Error) | length(Control_for_Experimentwise_Error) != 1) {
    stop ("'Control_for_Experimentwise_Error' must be a single logical value.")
  }
  Unsorted_Data_Frame <- data.frame(Identifiers = Identifiers, Correlation_Coefficients = Correlation_Coefficients, Numbers_of_Observations = Numbers_of_Observations)
  Sorted_Data_Frame <- Unsorted_Data_Frame[order(Unsorted_Data_Frame$Correlation_Coefficients, decreasing = TRUE), ]
  rownames(Sorted_Data_Frame) <- NULL
  Correlation_Coefficients <- Sorted_Data_Frame$Correlation_Coefficients
  Numbers_of_Observations <- Sorted_Data_Frame$Numbers_of_Observations
  Identifiers <- Sorted_Data_Frame$Identifiers
  Number_of_Pairwise_Comparisons <- choose(length(Correlation_Coefficients), 2)
  Function_for_Comparing_Correlation_Coefficients <- function (Correlation_Coefficient_1, Correlation_Coefficient_2, Number_of_Observations_1, Number_of_Observations_2) {
    (2 * (1 - pnorm(abs(((0.5 * log((1 + Correlation_Coefficient_1) / (1 - Correlation_Coefficient_1))) - (0.5 * log((1 + Correlation_Coefficient_2) / (1 - Correlation_Coefficient_2)))) / (((1 / (Number_of_Observations_1 - 3)) + (1 / (Number_of_Observations_2 - 3))) ^ 0.5)))))
  }
  if (Control_for_Experimentwise_Error) {
    Corrected_Alpha <- 1 - ((1 - Alpha) ^ (1 / choose(length(Correlation_Coefficients), 2)))
  } else if (!Control_for_Experimentwise_Error) {
    Corrected_Alpha <- Alpha
  }
  Comparison <- NULL
  Correlation_1 <- NULL
  Correlation_2 <- NULL
  Counter_1 <- 0
  for (i in Identifiers[1:(length(Identifiers) - 1)]) {
    for (j in Identifiers[((which(Identifiers == i)) + 1):length(Identifiers)]) {
      Counter_1 <- Counter_1 + 1
      Comparison[Counter_1] <- paste(i, "vs.", j, sep = " ")
      Correlation_1[Counter_1] <- i
      Correlation_2[Counter_1] <- j
    }
  }
  names(Comparison) <- NULL
  names(Correlation_1) <- NULL
  names(Correlation_2) <- NULL
  Counter_2 <- 0
  Counter_3 <- 0
  Counter_4 <- 0
  p_Value <- NULL
  for (i in Correlation_Coefficients[1:(length(Correlation_Coefficients) - 1)]) {
    Counter_2 <- Counter_2 + 1
    Counter_3 <- which(Correlation_Coefficients == i)
    for (j in Correlation_Coefficients[((which(Correlation_Coefficients == i)) + 1):length(Correlation_Coefficients)]) {
      Counter_3 <- Counter_3 + 1
      Counter_4 <- Counter_4 + 1
      p_Value[Counter_4] <- Function_for_Comparing_Correlation_Coefficients(i, j, Numbers_of_Observations[Counter_2], Numbers_of_Observations[Counter_3])
    }
  }
  Significance <- rep("Unknown", length.out = length(p_Value))
  for (i in 1:length(p_Value)) {
    Significance[i] <- ifelse(p_Value[i] < Corrected_Alpha, "Significantly Different", "Not Significantly Different")
  }
  names(p_Value) <- NULL
  names(Significance) <- NULL
  Pairwise_Comparison_Data_Frame <- data.frame(Correlation_1 = Correlation_1, Correlation_2 = Correlation_2, Comparison = Comparison, p_Value = p_Value, Significance = Significance)
  Matrix_of_p_Values <- matrix(1, nrow = length(Correlation_Coefficients), ncol = length(Correlation_Coefficients))
  Matrix_of_p_Values[lower.tri(Matrix_of_p_Values, diag = FALSE)] <- Pairwise_Comparison_Data_Frame$p_Value
  Matrix_of_p_Values <- t(Matrix_of_p_Values)
  Matrix_of_p_Values[lower.tri(Matrix_of_p_Values, diag = FALSE)] <- Pairwise_Comparison_Data_Frame$p_Value
  rownames(Matrix_of_p_Values) <- colnames(Matrix_of_p_Values) <- Identifiers
  Separation_Letters_and_Symbols <- c(letters[1:26], LETTERS[1:26], 1:9, c(".", "+", "-", "*", "/", "#", "$", "%", "&", "^", "[", "]", ":", "@", ";"))
  Separation_Lettering <- rep("", length(Correlation_Coefficients))
  i <- 1
  j <- 1
  Counter_5 <- 0
  Counter_6 <- 1
  Counter_7 <- length(Correlation_Coefficients)
  Counter_8 <- 0
  Separation_Lettering[1] <- Separation_Letters_and_Symbols[Counter_6]
  Row_Number <- as.numeric(rownames(Sorted_Data_Frame))
  Last_Character_Function <- function (x) {
    Getting_Rid_of_Spaces_at_the_End_of_Character_Strings <- sub(" +$", "", x)
    Number_of_Characters_in_Character_Strings <- nchar(Getting_Rid_of_Spaces_at_the_End_of_Character_Strings)
    Last_Character <- substr(Getting_Rid_of_Spaces_at_the_End_of_Character_Strings, Number_of_Characters_in_Character_Strings, Number_of_Characters_in_Character_Strings)
    return (Last_Character)
  }
  while (i < length(Correlation_Coefficients)) {
    Counter_5 <- Counter_5 + 1
    if (Counter_5 > length(Correlation_Coefficients)) {
      break
    }
    for (j in i:length(Correlation_Coefficients)) {
      Nonsignificance_of_p_Value <- Matrix_of_p_Values[Row_Number[j], Row_Number[i]] > Corrected_Alpha
      if (Nonsignificance_of_p_Value) {
        if (Last_Character_Function(Separation_Lettering[j]) != Separation_Letters_and_Symbols[Counter_6]) {
          Separation_Lettering[j] <- paste(Separation_Lettering[j], Separation_Letters_and_Symbols[Counter_6], sep = "")
        }
      } else if (!Nonsignificance_of_p_Value) {
        Counter_6 <- Counter_6 + 1
        Counter_7 <- j
        Counter_8 <- 0
        Counter_9 <- i
        for (k in Counter_7:length(Correlation_Coefficients)) {
          Separation_Lettering[k] <- paste(Separation_Lettering[k], "", sep = "")
        }
        Separation_Lettering[Counter_7] <- paste(Separation_Lettering[Counter_7], Separation_Letters_and_Symbols[Counter_6], sep = "")
        for (l in Counter_9:Counter_7) {
          if (Matrix_of_p_Values[Row_Number[l], Row_Number[Counter_7]] <= Corrected_Alpha) {
            i <- i + 1
            Counter_8 <- 1
          }
          else if (Matrix_of_p_Values[Row_Number[l], Row_Number[Counter_7]] > Corrected_Alpha) {
            break
          }
        }
      }
    }
    if (Counter_8 == 0) {
      i <- i + 1
    }
  }
  Separation_Lettering_Data_Frame <- data.frame(Identifier = Sorted_Data_Frame$Identifiers, Correlation_Coefficient = Sorted_Data_Frame$Correlation_Coefficients, Number_of_Observations = Sorted_Data_Frame$Numbers_of_Observations, Separation_Lettering = Separation_Lettering)
  colnames(Separation_Lettering_Data_Frame)[which(colnames(Separation_Lettering_Data_Frame) != 'Separation_Lettering')] <- c(Identifiers_Name, Correlation_Coefficients_Name, Numbers_of_Observations_Name)
  Metadata <- data.frame(Number_of_Correlation_Coefficients_to_Compare = length(Correlation_Coefficients), Number_of_Pairwise_Comparisons = Number_of_Pairwise_Comparisons, Experimentwise_Error = Alpha, Comparisonwise_Error = Corrected_Alpha, Conservative_or_Liberal = ifelse(Control_for_Experimentwise_Error == TRUE, "Conservative", "Liberal"), Any_Significantly_Different_Correlation_Coefficients = ifelse(any(unlist(sapply(Pairwise_Comparison_Data_Frame$Significance, grepl, "Significantly Different")) == T), "Yes", "No"))
  return (Correlation_Coefficient_Separation_Test_Results <- list(Metadata = Metadata, Pairwise_Comparison_Data_Frame = Pairwise_Comparison_Data_Frame, Separation_Lettering_Data_Frame = Separation_Lettering_Data_Frame))
}
