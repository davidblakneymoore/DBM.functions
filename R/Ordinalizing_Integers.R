#' Ordinalizing Integers
#'
#' This function takes integers and returns character strings comprised of the integer followed by its appropriate ordinal suffix.
#'
#' `Ordinalizing_Integers` takes integers and returns character strings comprised of the integer followed by its appropriate ordinal suffix.
#' @param Integer_Object an R object containing integers.
#'
#' @return This function returns integers followed by their appropriate ordinal suffix.
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' Ordinalizing_Integers(1:25)
#'
#' @export
Ordinalizing_Integers <- function (Integer_Object) {
  if (!is.integer(Integer_Object)) {
    stop("The 'Integer_Vector' argument must be of class 'integer'.")
  }
  sapply(Integer_Object, function (i) {
    if (((i %% 10) == 1) & (!((i %% 100) == 11))) {
      paste0(i, "st")
    } else if (((i %% 10) == 2) & (!((i %% 100) == 12))) {
      paste0(i, "nd")
    } else if (((i %% 10) == 3) & (!((i %% 100) == 13))) {
      paste0(i, "rd")
    } else if ((!(((i %% 10) == 1) & (!((i %% 100) == 11)))) & (!(((i %% 10) == 2) & (!((i %% 100) == 12)))) & (!(((i %% 10) == 3) & (!((i %% 100) == 13))))) {
      paste0(i, "th")
    }
  })
}
