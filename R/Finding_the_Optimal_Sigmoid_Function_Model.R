#' Finding the Optimal Sigmoid Function Model
#'
#' This function determines which sigmoid model best fits a binary response data set (a data set where the response variable contains only `1`s and `0`s).
#'
#' `Finding_the_Optimal_Sigmoid_Function_Model` determines which sigmoid model best fits a binary response data set (a data set where the response variable contains only `1`s and `0`s). Although the logistic function is almost exclusively used for these applications, a variety of sigmoid functions exist and may be more relevant for certain types of data. Ten different, fully differentiable sigmoid functions are compared in this function. The functions are the logistic function (`Response = 1 / (1 + exp(-(Intercept + (Slope * Predictor))))`), the hyperbolic tangent (`Response = (0.5 * tanh(Intercept + (Slope * Predictor))) + 0.5`), the arctangent function (`Response = (0.5 * ((2 / pi) * atan((pi / 2) * (Intercept + (Slope * Predictor))))) + 0.5`), the Gudermannian function (`Response = (2 / pi) * atan(tanh((Intercept + (Slope * Predictor)) * pi / 4)) + 0.5`), the error function (`Response = (0.5 * ((2 * pnorm((Intercept + (Slope * Predictor)) * sqrt(2), 0, 1)) - 1)) + 0.5`), a generalised logistic function (`Response = (1 + exp(-(Intercept + (Slope * Predictor)))) ^ (-Exponent)`), an algebraic function (`Response = (0.5 * ((Intercept + (Slope * Predictor)) / sqrt(1 + ((Intercept + (Slope * Predictor)) ^ 2)))) + 0.5`), a more generalised algebraic function (`Response = (0.5 * ((Intercept + (Slope * Predictor)) / ((1 + (abs(Intercept + (Slope * Predictor)) ^ Exponent)) ^ (1 / Exponent)))) + 0.5`), the Gompertz function (`Response = exp(-exp(Intercept + (Slope * Predictor)))`), and the Gompertz function that has been rotated by `pi` radians (`Response = 1 - (exp(-exp(Intercept + (Slope * Predictor))))`) (see [the Wikipedia webpage for sigmoid functions](https://en.wikipedia.org/wiki/Sigmoid_function) for a description of some of these functions). Please note that all of these functions are rotationally symmetric with the exception of the Gompertz function and the Gompertz function that has been rotated by `pi` radians - these two functions approach one asymptote more quickly than they approach the other. All ten of these functions have been rescaled so that they are asymptotic at the values of `0` and `1` on the vertical axis.
#'
#' @param Predictor the predictor variable values. These values must be numeric.
#' @param Response the response variable. This numeric vector must be binary (it must contain only `1`s and `0`s).
#' @param Data_Frame an optional argument to provide if both the `Predictor` and `Response` arguments come from the same data frame and to prevent typing the data frame name multiple times.
#' @param Maximum_Number_of_Iterations the maximum number of iterations to perform when the `nls()` function is used internally to determine the coefficients for each model. The default value is `10000`.
#'
#' @return This function returns model parameters for ten different, fully differentiable sigmoid functions and determines which one best fit a binary response data set (where all the response variable values are `1`s and `0`s). Specifically, this function returns a data table containing each model and goodness-of-fit statistics (deviances and pseudo r squared values), a list of the names of the models, a list of the model coefficients and associated parameters related to their statistical significance, and a list of functions for each model that can be used to generate predicted values.
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' # Generate Some Made-up Data
#' Number_of_Observations <- 100
#' Minimum_Predictor_Value <- 0
#' Maximum_Predictor_Value <- 25
#' set.seed(25)
#' Predictor_Variable <- runif(Number_of_Observations, Minimum_Predictor_Value, Maximum_Predictor_Value)
#' Response_Variable <- rbinom(Number_of_Observations, 1, (Predictor_Variable - Minimum_Predictor_Value) / (Maximum_Predictor_Value - Minimum_Predictor_Value))
#' Data_Frame <- data.frame(Predictor_Variable = Predictor_Variable, Response_Variable = Response_Variable)
#'
#' # See How the Different Sigmoid Functions Perform Modeling the Data
#' Function_Output <- DBM.functions::Finding_the_Optimal_Sigmoid_Function_Model(Predictor_Variable, Response_Variable, Data_Frame)
#' View(Function_Output$Output_Table)
#' Function_Output$Model_Parameters
#' assign(paste("Fitted", gsub(" ", "_", Function_Output$Model_Names[[1]]), "Values", sep = "_"), Function_Output$Model_Functions[[1]](seq(min(Data_Frame$Predictor_Variable), max(Data_Frame$Predictor_Variable), by = 0.01)))
#'
#' # Make a Plot
#' Horizontal_Axis_Values <- seq(min(Data_Frame$Predictor_Variable), max(Data_Frame$Predictor_Variable), by = 0.01)
#' Vertical_Axis_Values <- lapply(Function_Output$Model_Functions, function (x) {
#'   x(Horizontal_Axis_Values)
#' })
#' tryCatch (dev.off(), error = function (e) {
#'
#' })
#' par(mar = c(16, 4, 4, 2))
#' plot(Response_Variable ~ Predictor_Variable, Data_Frame, pch = 19, xlab = "", ylab = "", main = "Comparing Sigmoid Function Models")
#' mtext(expression(paste(bold("Predictor Variable"))), 1, line = 2.5)
#' mtext(expression(paste(bold("Response Variable"))), 2, line = 2.5)
#' lapply(seq_len(length(Function_Output$Model_Names)), function (x) {
#'   points(Vertical_Axis_Values[[x]] ~ Horizontal_Axis_Values, type = "l", col = (x + 1), lwd = 2.5)
#' })
#' legend("bottom", xpd = TRUE, inset = c(0, -0.75), title = expression(paste(bold("Function (Deviance; Pseudo-R-Squared Value)"))), legend = paste0(gsub("_", " ", gsub("_Model$", "", names(Function_Output$Model_Parameters))), " (", round(as.numeric(Function_Output$Output_Table[complete.cases(Function_Output$Output_Table), ]$Deviance), 3), "; ", round(as.numeric(Function_Output$Output_Table[complete.cases(Function_Output$Output_Table), ]$Pseudo_R_Squared_Value), 5), ")"), col = (seq_len(length(Function_Output$Model_Parameters)) + 1), lty = 1, lwd = 2.5)
#'
#' @export
Finding_the_Optimal_Sigmoid_Function_Model <- function (Predictor, Response, Data_Frame, Maximum_Number_of_Iterations = 10000) {
  Predictor_Name <- deparse(substitute(Predictor))
  Response_Name <- deparse(substitute(Response))
  if (!missing(Data_Frame)) {
    if (class(Data_Frame) != 'data.frame') {
      stop ("'Data_Frame' must be of class 'data.frame'.")
    }
    if (!is.vector(Data_Frame[, Predictor_Name])) {
      stop ("The 'Predictor' argument must be a vector.")
    }
    if (!is.vector(Data_Frame[, Response_Name])) {
      stop ("The 'Response' argument must be a vector.")
    }
    if (!is.numeric(Data_Frame[, Predictor_Name])) {
      stop ("The 'Predictor' argument must be numeric.")
    }
    if (!is.numeric(Data_Frame[, Response_Name])) {
      stop ("The 'Response' argument must be numeric.")
    }
    if (length(Data_Frame[, Predictor_Name]) != length(Data_Frame[, Response_Name])) {
      stop ("The 'Predictor' and 'Response' arguments must contain the same numbers of elements.")
    }
    Data_Frame <- Data_Frame[, c(Predictor_Name, Response_Name)]
    colnames(Data_Frame) <- c("Predictor", "Response")
  } else if (missing(Data_Frame)) {
    if (!is.vector(Predictor)) {
      stop ("The 'Predictor' argument must be a vector.")
    }
    if (!is.vector(Response)) {
      stop ("The 'Response' argument must be a vector.")
    }
    if (!is.numeric(Predictor)) {
      stop ("The 'Predictor' argument must be numeric.")
    }
    if (!is.numeric(Response)) {
      stop ("The 'Response' argument must be numeric.")
    }
    if (length(Predictor) != length(Response)) {
      stop ("The 'Predictor' and 'Response' arguments must contain the same numbers of elements.")
    }
    Data_Frame <- data.frame(Predictor = Predictor, Response = Response)
  }
  Data_Frame <- Data_Frame[complete.cases(Data_Frame), ]
  if (nrow(Data_Frame) < 3) {
    stop ("There are not enough data to perform these operations.")
  }
  Predictor <- Data_Frame$Predictor
  Response <- Data_Frame$Response
  Total_Sum_of_Squares <- sum((Response - mean(Response)) ^ 2)
  Logistic_Function_Model <- tryCatch (nls(Response ~ (1 / (1 + exp(-(Intercept + (Slope * Predictor))))), start = list(Intercept = 0, Slope = 0), control = nls.control(maxiter = Maximum_Number_of_Iterations)), error = function (e) {
    NULL
  })
  if (is.null(Logistic_Function_Model)) {
    Logistic_Function_Model_Information <- list(Name = "Logistic Function", Model = Logistic_Function_Model)
  } else if (!is.null(Logistic_Function_Model)) {
    Logistic_Function_Model_Equation <- paste0(Response_Name, " = (1 / (1 + exp(-(", coef(Logistic_Function_Model)["Intercept"], " + (", coef(Logistic_Function_Model)["Slope"], " * ", Predictor_Name, ")))))")
    Logistic_Function_Model_Function <- function (Predictor) {
      Response <- 1 / (1 + exp(-(coef(Logistic_Function_Model)["Intercept"] + (coef(Logistic_Function_Model)["Slope"] * Predictor))))
      names(Response) <- NULL
      Response
    }
    Logistic_Function_Model_Information <- list(Name = "Logistic Function", Equation = Logistic_Function_Model_Equation, Function = Logistic_Function_Model_Function, Model = Logistic_Function_Model)
  }
  Hyperbolic_Tangent_Model <- tryCatch (nls(Response ~ ((0.5 * tanh(Intercept + (Slope * Predictor))) + 0.5), start = list(Intercept = 0, Slope = 0), control = nls.control(maxiter = Maximum_Number_of_Iterations)), error = function (e) {
    NULL
  })
  if (is.null(Hyperbolic_Tangent_Model)) {
    Hyperbolic_Tangent_Model_Information <- list(Name = "Hyperbolic Tangent", Model = Hyperbolic_Tangent_Model)
  } else if (!is.null(Hyperbolic_Tangent_Model)) {
    Hyperbolic_Tangent_Model_Equation <- paste0(Response_Name, " = ((0.5 * tanh(", coef(Hyperbolic_Tangent_Model)["Intercept"], " + (", coef(Hyperbolic_Tangent_Model)["Slope"], " * ", Predictor_Name, "))) + 0.5)")
    Hyperbolic_Tangent_Model_Function <- function (Predictor) {
      Response <- (0.5 * tanh(coef(Hyperbolic_Tangent_Model)["Intercept"] + (coef(Hyperbolic_Tangent_Model)["Slope"] * Predictor))) + 0.5
      names(Response) <- NULL
      Response
    }
    Hyperbolic_Tangent_Model_Information <- list(Name = "Hyperbolic Tangent", Equation = Hyperbolic_Tangent_Model_Equation, Function = Hyperbolic_Tangent_Model_Function, Model = Hyperbolic_Tangent_Model)
  }
  Arctangent_Function_Model <- tryCatch (nls(Response ~ ((0.5 * ((2 / pi) * atan((pi / 2) * (Intercept + (Slope * Predictor))))) + 0.5), start = list(Intercept = 0, Slope = 0), control = nls.control(maxiter = Maximum_Number_of_Iterations)), error = function (e) {
    NULL
  })
  if (is.null(Arctangent_Function_Model)) {
    Arctangent_Function_Model_Information <- list(Name = "Arctangent Function", Model = Arctangent_Function_Model)
  } else if (!is.null(Arctangent_Function_Model)) {
    Arctangent_Function_Model_Equation <- paste0(Response_Name, " = ((0.5 * ((2 / pi) * atan((pi / 2) * (", coef(Arctangent_Function_Model)["Intercept"], " + (", coef(Arctangent_Function_Model)["Slope"], " * ", Predictor_Name, "))))) + 0.5)")
    Arctangent_Function_Model_Function <- function (Predictor) {
      Response <- (0.5 * ((2 / pi) * atan((pi / 2) * (coef(Arctangent_Function_Model)["Intercept"] + (coef(Arctangent_Function_Model)["Slope"] * Predictor))))) + 0.5
      names(Response) <- NULL
      Response
    }
    Arctangent_Function_Model_Information <- list(Name = "Arctangent Function", Equation = Arctangent_Function_Model_Equation, Function = Arctangent_Function_Model_Function, Model = Arctangent_Function_Model)
  }
  Gudermannian_Function_Model <- tryCatch (nls(Response ~ ((2 / pi) * atan(tanh((Intercept + (Slope * Predictor)) * pi / 4)) + 0.5), start = list(Intercept = 0, Slope = 0), control = nls.control(maxiter = Maximum_Number_of_Iterations)), error = function (e) {
    NULL
  })
  if (is.null(Gudermannian_Function_Model)) {
    Gudermannian_Function_Model_Information <- list(Name = "Gudermannian Function", Model = Gudermannian_Function_Model)
  } else if (!is.null(Gudermannian_Function_Model)) {
    Gudermannian_Function_Model_Equation <- paste0(Response_Name, " = ((2 / pi) * atan(tanh((", coef(Gudermannian_Function_Model)["Intercept"], " + (", coef(Gudermannian_Function_Model)["Slope"], " * ", Predictor_Name, ")) * pi / 4)) + 0.5)")
    Gudermannian_Function_Model_Function <- function (Predictor) {
      Response <- (2 / pi) * atan(tanh((coef(Gudermannian_Function_Model)["Intercept"] + (coef(Gudermannian_Function_Model)["Slope"] * Predictor)) * pi / 4)) + 0.5
      names(Response) <- NULL
      Response
    }
    Gudermannian_Function_Model_Information <- list(Name = "Gudermannian Function", Equation = Gudermannian_Function_Model_Equation, Function = Gudermannian_Function_Model_Function, Model = Gudermannian_Function_Model)
  }
  Error_Function_Model <- tryCatch (nls(Response ~ ((0.5 * ((2 * pnorm((Intercept + (Slope * Predictor)) * sqrt(2), 0, 1)) - 1)) + 0.5), start = list(Intercept = 0, Slope = 0), control = nls.control(maxiter = Maximum_Number_of_Iterations)), error = function (e) {
    NULL
  })
  if (is.null(Error_Function_Model)) {
    Error_Function_Model_Information <- list(Name = "Error Function", Model = Error_Function_Model)
  } else if (!is.null(Error_Function_Model)) {
    Error_Function_Model_Equation <- paste0(Response_Name, " = ((0.5 * ((2 * pnorm((", coef(Error_Function_Model)["Intercept"]," + (", coef(Error_Function_Model)["Slope"], " * ", Predictor_Name, ")) * sqrt(2))) - 1)) + 0.5)")
    Error_Function_Model_Function <- function (Predictor) {
      Response <- (0.5 * ((2 * pnorm((coef(Error_Function_Model)["Intercept"] + (coef(Error_Function_Model)["Slope"] * Predictor)) * sqrt(2), 0, 1)) - 1)) + 0.5
      names(Response) <- NULL
      Response
    }
    Error_Function_Model_Information <- list(Name = "Error Function", Equation = Error_Function_Model_Equation, Function = Error_Function_Model_Function, Model = Error_Function_Model)
  }
  Generalised_Logistic_Function_Model <- tryCatch (nls(Response ~ ((1 + exp(-(Intercept + (Slope * Predictor)))) ^ (-Exponent)), start = list(Intercept = 0, Slope = 0, Exponent = 0), control = nls.control(maxiter = Maximum_Number_of_Iterations)), error = function (e) {
    NULL
  })
  if (is.null(Generalised_Logistic_Function_Model)) {
    Generalised_Logistic_Function_Model_Information <- list(Name = "Generalised Logistic Function", Model = Generalised_Logistic_Function_Model)
  } else if (!is.null(Generalised_Logistic_Function_Model)) {
    Generalised_Logistic_Function_Model_Equation <- paste0(Response_Name, " = ((1 + exp(-(", coef(Generalised_Logistic_Function_Model)["Intercept"], " + (", coef(Generalised_Logistic_Function_Model)["Slope"], " * ", Predictor_Name, ")))) ^ (-", coef(Generalised_Logistic_Function_Model)["Exponent"], "))")
    Generalised_Logistic_Function_Model_Function <- function (Predictor) {
      Response <- (1 + exp(-(coef(Generalised_Logistic_Function_Model)["Intercept"] + (coef(Generalised_Logistic_Function_Model)["Slope"] * Predictor)))) ^ (-coef(Generalised_Logistic_Function_Model)["Exponent"])
      names(Response) <- NULL
      Response
    }
    Generalised_Logistic_Function_Model_Information <- list(Name = "Generalised Logistic Function", Equation = Generalised_Logistic_Function_Model_Equation, Function = Generalised_Logistic_Function_Model_Function, Model = Generalised_Logistic_Function_Model)
  }
  Algebraic_Function_Model <- tryCatch (nls(Response ~ ((0.5 * ((Intercept + (Slope * Predictor)) / sqrt(1 + ((Intercept + (Slope * Predictor)) ^ 2)))) + 0.5), start = list(Intercept = 0, Slope = 0), control = nls.control(maxiter = Maximum_Number_of_Iterations)), error = function (e) {
    NULL
  })
  if (is.null(Algebraic_Function_Model)) {
    Algebraic_Function_Model_Information <- list(Name = "Algebraic Function", Model = Algebraic_Function_Model)
  } else if (!is.null(Algebraic_Function_Model)) {
    Algebraic_Function_Model_Equation <- paste0(Response_Name, " = ((0.5 * ((", coef(Algebraic_Function_Model)["Intercept"], " + (", coef(Algebraic_Function_Model)["Slope"], " * ", Predictor_Name, ")) / sqrt(1 + ((", coef(Algebraic_Function_Model)["Intercept"], " + (", coef(Algebraic_Function_Model)["Slope"], " * ", Predictor_Name, ")) ^ 2)))) + 0.5)")
    Algebraic_Function_Model_Function <- function (Predictor) {
      Response <- (0.5 * ((coef(Algebraic_Function_Model)["Intercept"] + (coef(Algebraic_Function_Model)["Slope"] * Predictor)) / sqrt(1 + ((coef(Algebraic_Function_Model)["Intercept"] + (coef(Algebraic_Function_Model)["Slope"] * Predictor)) ^ 2)))) + 0.5
      names(Response) <- NULL
      Response
    }
    Algebraic_Function_Model_Information <- list(Name = "Algebraic Function", Equation = Algebraic_Function_Model_Equation, Function = Algebraic_Function_Model_Function, Model = Algebraic_Function_Model)
  }
  More_General_Algebraic_Function_Model <- tryCatch (nls(Response ~ ((0.5 * ((Intercept + (Slope * Predictor)) / ((1 + (abs(Intercept + (Slope * Predictor)) ^ Exponent)) ^ (1 / Exponent)))) + 0.5), start = list(Intercept = 0, Slope = 0, Exponent = 0), control = nls.control(maxiter = Maximum_Number_of_Iterations)), error = function (e) {
    NULL
  })
  if (is.null(More_General_Algebraic_Function_Model)) {
    More_General_Algebraic_Function_Model_Information <- list(Name = "More General Algebraic Function", Model = More_General_Algebraic_Function_Model)
  } else if (!is.null(More_General_Algebraic_Function_Model)) {
    More_General_Algebraic_Function_Model_Equation <- paste0(Response_Name, " = ((0.5 * ((", coef(More_General_Algebraic_Function_Model)["Intercept"], " + (", coef(More_General_Algebraic_Function_Model)["Slope"], " * ", Predictor_Name, ")) / ((1 + (abs(", coef(More_General_Algebraic_Function_Model)["Intercept"], " + (", coef(More_General_Algebraic_Function_Model)["Slope"], " * ", Predictor_Name, ")) ^ ", coef(More_General_Algebraic_Function_Model)["Exponent"], ")) ^ (1 / ", coef(More_General_Algebraic_Function_Model)["Exponent"], ")))) + 0.5)")
    More_General_Algebraic_Function_Model_Function <- function (Predictor) {
      Response <- (0.5 * ((coef(More_General_Algebraic_Function_Model)["Intercept"] + (coef(More_General_Algebraic_Function_Model)["Slope"] * Predictor)) / ((1 + (abs(coef(More_General_Algebraic_Function_Model)["Intercept"] + (coef(More_General_Algebraic_Function_Model)["Slope"] * Predictor)) ^ coef(More_General_Algebraic_Function_Model)["Exponent"])) ^ (1 / coef(More_General_Algebraic_Function_Model)["Exponent"])))) + 0.5
      names(Response) <- NULL
      Response
    }
    More_General_Algebraic_Function_Model_Information <- list(Name = "More General Algebraic Function", Equation = More_General_Algebraic_Function_Model_Equation, Function = More_General_Algebraic_Function_Model_Function, Model = More_General_Algebraic_Function_Model)
  }
  Gompertz_Function_Model <- tryCatch (nls(Response ~ (exp(-exp(Intercept + (Slope * Predictor)))), start = list(Intercept = 0, Slope = 0), control = nls.control(maxiter = Maximum_Number_of_Iterations)), error = function (e) {
    NULL
  })
  if (is.null(Gompertz_Function_Model)) {
    Gompertz_Function_Model_Information <- list(Name = "Gompertz Function", Model = Gompertz_Function_Model)
  } else if (!is.null(Gompertz_Function_Model)) {
    Gompertz_Function_Model_Equation <- paste0(Response_Name, " = (exp(-exp(", coef(Gompertz_Function_Model)["Intercept"], " + (", coef(Gompertz_Function_Model)["Slope"], " * ", Predictor_Name, "))))")
    Gompertz_Function_Model_Function <- function (Predictor) {
      Response <- exp(-exp(coef(Gompertz_Function_Model)["Intercept"] + (coef(Gompertz_Function_Model)["Slope"] * Predictor)))
      names(Response) <- NULL
      Response
    }
    Gompertz_Function_Model_Information <- list(Name = "Gompertz Function", Equation = Gompertz_Function_Model_Equation, Function = Gompertz_Function_Model_Function, Model = Gompertz_Function_Model)
  }
  Rotated_Gompertz_Function_Model <- tryCatch (nls(Response ~ (1 - (exp(-exp(Intercept + (Slope * Predictor))))), start = list(Intercept = 0, Slope = 0), control = nls.control(maxiter = Maximum_Number_of_Iterations)), error = function (e) {
    NULL
  })
  if (is.null(Rotated_Gompertz_Function_Model)) {
    Rotated_Gompertz_Function_Model_Information <- list(Name = "Rotated Gompertz Function", Model = Rotated_Gompertz_Function_Model)
  } else if (!is.null(Rotated_Gompertz_Function_Model)) {
    Rotated_Gompertz_Function_Model_Equation <- paste0(Response_Name, " = (1 - (exp(-exp(", coef(Rotated_Gompertz_Function_Model)["Intercept"], " + (", coef(Rotated_Gompertz_Function_Model)["Slope"], " * ", Predictor_Name, ")))))")
    Rotated_Gompertz_Function_Model_Function <- function (Predictor) {
      Response <- 1 - (exp(-exp(coef(Rotated_Gompertz_Function_Model)["Intercept"] + (coef(Rotated_Gompertz_Function_Model)["Slope"] * Predictor))))
      names(Response) <- NULL
      Response
    }
    Rotated_Gompertz_Function_Model_Information <- list(Name = "Rotated Gompertz Function", Equation = Rotated_Gompertz_Function_Model_Equation, Function = Rotated_Gompertz_Function_Model_Function, Model = Rotated_Gompertz_Function_Model)
  }
  Model_List <- list(Logistic_Function_Model = Logistic_Function_Model_Information, Hyperbolic_Tangent_Model = Hyperbolic_Tangent_Model_Information, Arctangent_Function_Model = Arctangent_Function_Model_Information, Gudermannian_Function_Model = Gudermannian_Function_Model_Information, Error_Function_Model = Error_Function_Model_Information, Generalised_Logistic_Function_Model = Generalised_Logistic_Function_Model_Information, Algebraic_Function_Model = Algebraic_Function_Model_Information, More_General_Algebraic_Function_Model = More_General_Algebraic_Function_Model_Information, Gompertz_Function_Model = Gompertz_Function_Model_Information, Rotated_Gompertz_Function_Model = Rotated_Gompertz_Function_Model_Information)
  Indices <- sapply(Model_List, function (x) {
    is.null(x$Model)
  })
  Models_That_Are_not_Possible <- sapply(Model_List[which(Indices)], function (x) {
    x$Name
  })
  names(Models_That_Are_not_Possible) <- NULL
  Models_That_Are_Possible <- Model_List[which(!Indices)]
  Models_That_Are_Possible <- lapply(Models_That_Are_Possible, function (x) {
    x$Pseudo_R_Squared_Value <- 1 - (sum(x$Model$m$resid() ^ 2) / Total_Sum_of_Squares)
    x$Deviance <- x$Model$m$deviance()
    x
  })
  Deviances <- sapply(Models_That_Are_Possible, function (x) {
    x$Deviance
  })
  Pseudo_R_Squared_Values <- sapply(Models_That_Are_Possible, function (x) {
    x$Pseudo_R_Squared_Value
  })
  Ordered_Models_That_Are_Possible <- Models_That_Are_Possible[order(Deviances)]
  Output_Table <- as.data.frame(t(as.matrix(as.data.frame(lapply(Ordered_Models_That_Are_Possible, function (x) {
    c(Name = x$Name, Equation = x$Equation, Deviance = x$Deviance, Pseudo_R_Squared_Value = x$Pseudo_R_Squared_Value)
  })))))
  rownames(Output_Table) <- NULL
  Output_Table$Rank <- seq_len(nrow(Output_Table))
  Output_Table$Possible <- "Yes"
  Output_Table <- Output_Table[, c(which(colnames(Output_Table) == "Rank"), which(colnames(Output_Table) == "Possible"), which(!(colnames(Output_Table) %in% c("Rank", "Possible"))))]
  Additional_Rows <- data.frame(Rank = rep(NA, length(Models_That_Are_not_Possible)), Possible = rep("No", length(Models_That_Are_not_Possible)), Name = Models_That_Are_not_Possible, Equation = rep(NA, length(Models_That_Are_not_Possible)), Deviance = rep(NA, length(Models_That_Are_not_Possible)), Pseudo_R_Squared_Value = rep(NA, length(Models_That_Are_not_Possible)))
  Output_Table <- rbind(Output_Table, Additional_Rows)
  Model_Parameters <- lapply(Ordered_Models_That_Are_Possible, function (x) {
    coef(summary(x$Model))
  })
  Model_Functions <- lapply(Ordered_Models_That_Are_Possible, function (x) {
    x$Function
  })
  Model_Names <- lapply(Ordered_Models_That_Are_Possible, function (x) {
    x$Name
  })
  return (list(Output_Table = Output_Table, Model_Names = Model_Names, Model_Parameters = Model_Parameters, Model_Functions = Model_Functions))
}
