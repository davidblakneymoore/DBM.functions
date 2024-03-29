% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Sugar_Maple_Data.R
\docType{data}
\name{Sugar_Maple_Data}
\alias{Sugar_Maple_Data}
\title{A Time Series of Sap Flow and Wood Temperature Data From a Sugar Maple}
\format{
A data frame with 1501 rows and 3 columns:
\describe{
\item{Time}{A column of dates and times (in \code{POSIXct} format)}
\item{Sap_Flow}{A column of sap flow values (in centimeters per hour)}
\item{Wood_Temperature}{A column of wood temperature values (in degrees Celcius)}
}
}
\usage{
Sugar_Maple_Data
}
\description{
This data frame contains time-series sap flow and wood temperature data from a sugar maple in Durham, New Hampshire, from the 2023 winter-dormant period. Sap flow and wood temperature values were measured at the same point (at breast height) on the tree at a sapwood depth of 1 centimeter. Positive values of sap flow correspond with upward sap movement and negative sap flow values correspond with downward sap movement. In sugar maples, during winter dormancy, freeze-thaw cycles drive sap flow and sap pressurization events.
}
\author{
David B. Moore (\email{davidblakneymoore@gmail.com})
}
\keyword{datasets}
