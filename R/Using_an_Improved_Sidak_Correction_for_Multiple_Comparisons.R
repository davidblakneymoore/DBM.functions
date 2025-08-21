#' Using an Improved Šidák Correction for Multiple Comparisons
#'
#' This function w
#'
#' `Using_an_Improved_Sidak_Correction_for_Multiple_Comparisons` w
#'
#' @param Comparisons w
#' @param p_Values w
#' @param Data_Frame w
#' @param Experimentwise_Type_I_Error_Rate w
#'
#' @return This function returns a data frame that contains the name of the comparisons or analyses, the original p values, the corrected p values (following this improved Šidák correction method), and a column indicating whether or not the corrected p values are statistically significant.
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' # Generating Made-Up Data
#' Number_of_Comparisons <- 100
#' Data_Frame_1 <- data.frame(Comparison = paste('Comparison', seq_len(Number_of_Comparisons)), p_Value = rbeta(Number_of_Comparisons, 0.25, 1))
#' 
#' # Using the Function
#' Using_an_Improved_Sidak_Correction_for_Multiple_Comparisons(Comparison, p_Value, Data_Frame_1)
#'
#' @export
Using_an_Improved_Sidak_Correction_for_Multiple_Comparisons <- function (Comparisons, p_Values, Data_Frame, Experimentwise_Type_I_Error_Rate = 0.05) {
  if (length(Experimentwise_Type_I_Error_Rate) != 1) {
    stop ("The 'Experimentwise_Type_I_Error_Rate' argument must be of length 1.")
  }
  if (!is.numeric(Experimentwise_Type_I_Error_Rate)) {
    stop ("The 'Experimentwise_Type_I_Error_Rate' argument must be numeric.")
  }
  if (missing(Data_Frame)) {
    if (length(Comparisons) != length(p_Values)) {
      stop ("The 'Comparisons' and 'p_Values' arguments must be of the same length.")
    }
    if (!is.numeric(p_Values)) {
      stop ("The 'p_Values' argument must be numeric.")
    }
    if (!is.vector(Comparisons)) {
      stop ("The 'Comparisons' argument must be a vector.")
    }
    if (!is.vector(p_Values)) {
      stop ("The 'p_Values' argument must be a vector.")
    }
    if (length(unique(Comparisons)) != length(Comparisons)) {
      stop ("Each element of the 'Comparisons' argument must be unique.")
    }
  }
  if (!missing(Data_Frame)) {
    if (class(Data_Frame) != 'data.frame') {
      stop ("The 'Data_Frame' argument must be of class 'data.frame'.")
    }
    if (!is.numeric(Data_Frame[, deparse(substitute(p_Values))])) {
      stop ("The 'p_Values' column must be numeric.")
    }
    if (length(unique(Data_Frame[, deparse(substitute(Comparisons))])) != length(Data_Frame[, deparse(substitute(Comparisons))])) {
      stop ("Each element of the 'Comparisons' column must be unique.")
    }
  }
  if (missing(Data_Frame)) {
    Data_Frame <- data.frame(Comparison = Comparisons, p_Value = p_Values)
  } else if (!missing(Data_Frame)) {
    Comparisons <- Data_Frame[, deparse(substitute(Comparisons))]
    p_Values <- Data_Frame[, deparse(substitute(p_Values))]
    Data_Frame <- data.frame(Comparison = Comparisons, p_Value = p_Values)
  }
  Data_Frame <- Data_Frame[order(Data_Frame$p_Value), ]
  Data_Frame$Improved_Sidak_Correction_Adjusted_p_Value <- sapply(seq_len(nrow(Data_Frame)), function (x) {
    1 - (Reduce(`*`, (1 - Data_Frame$p_Value[seq_len(x)])))
  })
  Data_Frame$Significance_Using_the_Improved_Sidak_Correction <- Data_Frame$Improved_Sidak_Correction_Adjusted_p_Value < Experimentwise_Type_I_Error_Rate
  Data_Frame <- Data_Frame[match(Comparisons, Data_Frame$Comparison), ]
  Data_Frame
}
