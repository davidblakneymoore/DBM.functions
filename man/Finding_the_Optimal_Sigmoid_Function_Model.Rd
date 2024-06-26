% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Finding_the_Optimal_Sigmoid_Function_Model.R
\name{Finding_the_Optimal_Sigmoid_Function_Model}
\alias{Finding_the_Optimal_Sigmoid_Function_Model}
\title{Finding the Optimal Sigmoid Function Model}
\usage{
Finding_the_Optimal_Sigmoid_Function_Model(
  Predictor,
  Response,
  Data_Frame,
  Maximum_Number_of_Iterations = 10000
)
}
\arguments{
\item{Predictor}{the predictor variable values. These values must be numeric.}

\item{Response}{the response variable. This numeric vector must be binary (it must contain only \code{1}s and \code{0}s).}

\item{Data_Frame}{an optional argument to provide if both the \code{Predictor} and \code{Response} arguments come from the same data frame and to prevent typing the data frame name multiple times.}

\item{Maximum_Number_of_Iterations}{the maximum number of iterations to perform when the \code{nls()} function is used internally to determine the coefficients for each model. The default value is \code{10000}.}
}
\value{
This function returns model parameters for ten different, fully differentiable sigmoid functions and determines which one best fit a binary response data set (where all the response variable values are \code{1}s and \code{0}s). Specifically, this function returns a data table containing each model and goodness-of-fit statistics (deviances and pseudo r squared values), a list of the names of the models, a list of the model coefficients and associated parameters related to their statistical significance, and a list of functions for each model that can be used to generate predicted values.
}
\description{
This function determines which sigmoid model best fits a binary response data set (a data set where the response variable contains only \code{1}s and \code{0}s).
}
\details{
\code{Finding_the_Optimal_Sigmoid_Function_Model} determines which sigmoid model best fits a binary response data set (a data set where the response variable contains only \code{1}s and \code{0}s). Although the logistic function is almost exclusively used for these applications, a variety of sigmoid functions exist and may be more relevant for certain types of data. Ten different, fully differentiable sigmoid functions are compared in this function. The functions are the logistic function (\code{Response = 1 / (1 + exp(-(Intercept + (Slope * Predictor))))}), the hyperbolic tangent (\code{Response = (0.5 * tanh(Intercept + (Slope * Predictor))) + 0.5}), the arctangent function (\code{Response = (0.5 * ((2 / pi) * atan((pi / 2) * (Intercept + (Slope * Predictor))))) + 0.5}), the Gudermannian function (\code{Response = (2 / pi) * atan(tanh((Intercept + (Slope * Predictor)) * pi / 4)) + 0.5}), the error function (\code{Response = (0.5 * ((2 * pnorm((Intercept + (Slope * Predictor)) * sqrt(2), 0, 1)) - 1)) + 0.5}), a generalised logistic function (\code{Response = (1 + exp(-(Intercept + (Slope * Predictor)))) ^ (-Exponent)}), an algebraic function (\code{Response = (0.5 * ((Intercept + (Slope * Predictor)) / sqrt(1 + ((Intercept + (Slope * Predictor)) ^ 2)))) + 0.5}), a more generalised algebraic function (\code{Response = (0.5 * ((Intercept + (Slope * Predictor)) / ((1 + (abs(Intercept + (Slope * Predictor)) ^ Exponent)) ^ (1 / Exponent)))) + 0.5}), the Gompertz function (\code{Response = exp(-exp(Intercept + (Slope * Predictor)))}), and the Gompertz function that has been rotated by \code{pi} radians (\code{Response = 1 - (exp(-exp(Intercept + (Slope * Predictor))))}) (see \href{https://en.wikipedia.org/wiki/Sigmoid_function}{the Wikipedia webpage for sigmoid functions} for a description of some of these functions). Please note that all of these functions are rotationally symmetric with the exception of the Gompertz function and the Gompertz function that has been rotated by \code{pi} radians - these two functions approach one asymptote more quickly than they approach the other. All ten of these functions have been rescaled so that they are asymptotic at the values of \code{0} and \code{1} on the vertical axis.
}
\examples{
# Generate Some Made-up Data
Number_of_Observations <- 100
Minimum_Predictor_Value <- 0
Maximum_Predictor_Value <- 25
set.seed(25)
Predictor_Variable <- runif(Number_of_Observations, Minimum_Predictor_Value, Maximum_Predictor_Value)
Response_Variable <- rbinom(Number_of_Observations, 1, (Predictor_Variable - Minimum_Predictor_Value) / (Maximum_Predictor_Value - Minimum_Predictor_Value))
Data_Frame <- data.frame(Predictor_Variable = Predictor_Variable, Response_Variable = Response_Variable)

# See How the Different Sigmoid Functions Perform Modeling the Data
Function_Output <- DBM.functions::Finding_the_Optimal_Sigmoid_Function_Model(Predictor_Variable, Response_Variable, Data_Frame)
View(Function_Output$Output_Table)
Function_Output$Model_Parameters
assign(paste("Fitted", gsub(" ", "_", Function_Output$Model_Names[[1]]), "Values", sep = "_"), Function_Output$Model_Functions[[1]](seq(min(Data_Frame$Predictor_Variable), max(Data_Frame$Predictor_Variable), by = 0.01)))

# Make a Plot
Horizontal_Axis_Values <- seq(min(Data_Frame$Predictor_Variable), max(Data_Frame$Predictor_Variable), by = 0.01)
Vertical_Axis_Values <- lapply(Function_Output$Model_Functions, function (x) {
  x(Horizontal_Axis_Values)
})
tryCatch (dev.off(), error = function (e) {

})
par(mar = c(16, 4, 4, 2))
plot(Response_Variable ~ Predictor_Variable, Data_Frame, pch = 19, xlab = "", ylab = "", main = "Comparing Sigmoid Function Models")
mtext(expression(paste(bold("Predictor Variable"))), 1, line = 2.5)
mtext(expression(paste(bold("Response Variable"))), 2, line = 2.5)
lapply(seq_len(length(Function_Output$Model_Names)), function (x) {
  points(Vertical_Axis_Values[[x]] ~ Horizontal_Axis_Values, type = "l", col = (x + 1), lwd = 2.5)
})
legend("bottom", xpd = TRUE, inset = c(0, -0.75), title = expression(paste(bold("Function (Deviance; Pseudo-R-Squared Value)"))), legend = paste0(gsub("_", " ", gsub("_Model$", "", names(Function_Output$Model_Parameters))), " (", round(as.numeric(Function_Output$Output_Table[complete.cases(Function_Output$Output_Table), ]$Deviance), 3), "; ", round(as.numeric(Function_Output$Output_Table[complete.cases(Function_Output$Output_Table), ]$Pseudo_R_Squared_Value), 5), ")"), col = (seq_len(length(Function_Output$Model_Parameters)) + 1), lty = 1, lwd = 2.5)

}
\author{
David B. Moore (\email{davidblakneymoore@gmail.com})
}
