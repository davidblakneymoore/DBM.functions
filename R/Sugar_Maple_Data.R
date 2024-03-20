#' A Time Series of Sap Flow and Wood Temperature Data From a Sugar Maple
#'
#' This data frame contains time-series sap flow and wood temperature data from a sugar maple in Durham, New Hampshire, from the 2023 winter-dormant period. Sap flow and wood temperature values were measured at the same point (at breast height) on the tree at a sapwood depth of 1 centimeter. Positive values of sap flow correspond with upward sap movement and negative sap flow values correspond with downward sap movement. In sugar maples, during winter dormancy, freeze-thaw cycles drive sap flow and sap pressurization events.
#'
#' @format
#' A data frame with 1501 rows and 3 columns:
#' \describe{
#' \item{Time}{A column of dates and times (in `POSIXct` format)}
#' \item{Sap_Flow}{A column of sap flow values (in centimeters per hour)}
#' \item{Wood_Temperature}{A column of wood temperature values (in degrees Celcius)}
#' }
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
"Sugar_Maple_Data"
