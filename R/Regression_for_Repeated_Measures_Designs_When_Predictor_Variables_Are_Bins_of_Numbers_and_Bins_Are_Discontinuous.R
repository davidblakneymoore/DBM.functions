#' Regression for Repeated-Measures Designs When Predictor Variables Are Bins of Numbers and Bins Are Continuous
#'
#' This function w
#'
#' `Regression_for_Repeated_Measures_Designs_When_Predictor_Variables_Are_Bins_of_Numbers_and_Bins_Are_Discontinuous` w
#' @param Treatment w
#' @param Replicate w
#' @param Predictor_Variable_Bin_Lower_Limits w
#' @param Predictor_Variable_Bin_Upper_Limits w
#' @param Predictor_Variable w
#' @param Response_Variable w
#' @param Beta_Distribution_Alpha_Parameter_Values w
#' @param Beta_Distribution_Beta_Parameter_Values w
#' @param Data_Frame w
#' @param Parameters w
#' @param Formula w
#' @param Color_Data w
#' @param Color_Opacity w
#' @param Number_of_Simulations_per_Experimental_Unit w
#' @param Number_of_Histogram_Bins w
#' @param Moments_to_Report w
#' @param Make_Figure_1 w
#' @param Make_Figure_2 w
#' @param Make_Figure_3 w
#' @param Make_Figure_4 w
#' @param Working_Directory w
#' @param First_Figure_Name w
#' @param Second_Figure_Name w
#' @param Third_Figure_Name w
#' @param Fourth_Figure_Name w
#' @param Show_Individual_Plot_Titles w
#' @param Show_Moments_in_Figure_1 w
#' @param Show_Overlapping_Areas_in_Figure_2 w
#' @param Show_Moments_in_Figure_3 w
#' @param Show_Moments_in_Figure_4 w
#' @param Moment_Rounding_Constant w
#' @param Overlapping_Area_Rounding_Constant w
#' @param Number_of_Bins_in_the_Figure_4_Histograms w
#' @param Maximum_Number_of_Iterations w
#' @param Relative_Figure_1_Parameter_Plot_Height w
#' @param Relative_Figure_1_Treatment_Plot_Height w
#' @param Relative_Figure_1_Replicate_Plot_Height w
#' @param Relative_Figure_1_Individual_Plot_Height w
#' @param Relative_Figure_1_Legend_Plot_Height w
#' @param Relative_Figure_1_Individual_Plot_Width w
#' @param Relative_Figure_1_Gap_Plot_Width w
#' @param Relative_Figure_1_Parameter_Spacing_Line_Plot_Height w
#' @param Figure_1_Figure_Height_Constant w
#' @param Figure_1_Figure_Width_Constant w
#' @param Figure_1_Axis_Labels_and_Axis_Titles_Shifting_Constant w
#' @param Figure_1_Parameter_Title_Text_Size w
#' @param Figure_1_Treatment_Title_Text_Size w
#' @param Figure_1_Replicate_Title_Text_Size w
#' @param Figure_1_Axis_Titles_Text_Size w
#' @param Figure_1_Axis_Labels_Text_Size w
#' @param Figure_1_Individual_Plot_Bottom_Margin w
#' @param Figure_1_Individual_Plot_Left_Margin w
#' @param Figure_1_Individual_Plot_Top_Margin w
#' @param Figure_1_Individual_Plot_Right_Margin w
#' @param Figure_1_Moment_Text_Size w
#' @param Figure_1_Legend_Text_Size w
#' @param Figure_1_Panel_Separation_Line_Width w
#' @param Relative_Figure_2_Parameter_Plot_Height w
#' @param Relative_Figure_2_Treatment_Plot_Height w
#' @param Relative_Figure_2_Replicate_Plot_Height w
#' @param Relative_Figure_2_Individual_Plot_Dimension w
#' @param Relative_Figure_2_Gap_Plot_Dimension w
#' @param Relative_Figure_2_Legend_Plot_Height w
#' @param Relative_Figure_2_Parameter_Spacing_Line_Plot_Height w
#' @param Figure_2_Figure_Height_Constant w
#' @param Figure_2_Figure_Width_Constant w
#' @param Figure_2_Axis_Labels_and_Axis_Titles_Shifting_Constant w
#' @param Figure_2_Parameter_Title_Text_Size w
#' @param Figure_2_Treatment_Title_Text_Size w
#' @param Figure_2_Replicate_Title_Text_Size w
#' @param Figure_2_Axis_Titles_Text_Size w
#' @param Figure_2_Axis_Labels_Text_Size w
#' @param Figure_2_Individual_Plot_Bottom_Margin w
#' @param Figure_2_Individual_Plot_Left_Margin w
#' @param Figure_2_Individual_Plot_Top_Margin w
#' @param Figure_2_Individual_Plot_Right_Margin w
#' @param Figure_2_Overlapping_Area_Text_Size w
#' @param Figure_2_Legend_Text_Size w
#' @param Figure_2_Panel_Separation_Line_Width w
#' @param Relative_Figure_3_Parameter_Plot_Height w
#' @param Relative_Figure_3_Treatment_Plot_Dimension w
#' @param Relative_Figure_3_Replicate_Plot_Dimension w
#' @param Relative_Figure_3_Individual_Plot_Dimension w
#' @param Relative_Figure_3_Gap_Plot_Dimension w
#' @param Relative_Figure_3_Legend_Plot_Height w
#' @param Relative_Figure_3_Parameter_Spacing_Line_Plot_Height w
#' @param Figure_3_Figure_Height_Constant w
#' @param Figure_3_Figure_Width_Constant w
#' @param Figure_3_Axis_Labels_and_Axis_Titles_Shifting_Constant w
#' @param Figure_3_Parameter_Title_Text_Size w
#' @param Figure_3_Treatment_Title_Text_Size w
#' @param Figure_3_Replicate_Title_Text_Size w
#' @param Figure_3_Axis_Titles_Text_Size w
#' @param Figure_3_Axis_Labels_Text_Size w
#' @param Figure_3_Individual_Plot_Bottom_Margin w
#' @param Figure_3_Individual_Plot_Left_Margin w
#' @param Figure_3_Individual_Plot_Top_Margin w
#' @param Figure_3_Individual_Plot_Right_Margin w
#' @param Figure_3_Moment_Text_Size w
#' @param Figure_3_Legend_Text_Size w
#' @param Figure_3_Panel_Separation_Line_Width w
#' @param Relative_Figure_4_Parameter_Plot_Height w
#' @param Relative_Figure_4_Treatment_Plot_Dimension w
#' @param Relative_Figure_4_Individual_Plot_Height w
#' @param Relative_Figure_4_Individual_Plot_Width w
#' @param Relative_Figure_4_Parameter_Spacing_Line_Plot_Height w
#' @param Figure_4_Figure_Height_Constant w
#' @param Figure_4_Figure_Width_Constant w
#' @param Figure_4_Axis_Labels_and_Axis_Titles_Shifting_Constant w
#' @param Figure_4_Parameter_Title_Text_Size w
#' @param Figure_4_Treatment_Title_Text_Size w
#' @param Figure_4_Axis_Titles_Text_Size w
#' @param Figure_4_Axis_Labels_Text_Size w
#' @param Figure_4_Individual_Plot_Bottom_Margin w
#' @param Figure_4_Individual_Plot_Left_Margin w
#' @param Figure_4_Individual_Plot_Top_Margin w
#' @param Figure_4_Individual_Plot_Right_Margin w
#' @param Figure_4_Moment_Text_Size w
#' @param Figure_4_Panel_Separation_Line_Width w
#'
#' @return This function returns w
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' Data_Frame <- data.frame(Treatment = rep(LETTERS[1:3], each = 9), Block = rep(1:3, each = 3, length.out = 27), Response_Variable = rnorm(27), Position = rep(c("Low", "Medium", "High"), length.out = 27), Bin_Lower_Limits = rep(c("c(0, 3)", "c(5)", "c(11, 15, 18)"), length.out = 27), Bin_Upper_Limits = rep(c("c(2, 5)", "c(10)", "c(13, 17, 22)"), length.out = 27))
#' Color_Data <- data.frame(Treatment = LETTERS[1:3], Color = c("black", "red", "green"))
#' Linear_Regression_Parameters <- c("Slope", "Intercept")
#' Linear_Regression_Formula <- Response_Variable ~ (Slope * Position) + Intercept
#' (Linear_Regression_Example <- Regression_for_Repeated_Measures_Designs_When_Predictor_Variables_Are_Bins_of_Numbers_and_Bins_Are_Discontinuous(Treatment = "Treatment", Replicate = "Block", Predictor_Variable = "Position", Predictor_Variable_Bin_Lower_Limits = "Bin_Lower_Limits", Predictor_Variable_Bin_Upper_Limits = "Bin_Upper_Limits", Response_Variable = "Response_Variable", Data_Frame = Data_Frame, Parameters = Linear_Regression_Parameters, Formula = Linear_Regression_Formula, Color_Data = Color_Data))
#'
#' @export
Regression_for_Repeated_Measures_Designs_When_Predictor_Variables_Are_Bins_of_Numbers_and_Bins_Are_Discontinuous <- function (Treatment, Replicate, Predictor_Variable, Predictor_Variable_Bin_Lower_Limits, Predictor_Variable_Bin_Upper_Limits, Response_Variable, Beta_Distribution_Alpha_Parameter_Values, Beta_Distribution_Beta_Parameter_Values, Data_Frame, Parameters, Formula, Color_Data, Color_Opacity = 0.5, Number_of_Simulations_per_Experimental_Unit = 1000, Number_of_Histogram_Bins = 100, Moments_to_Report = 4, Make_Figure_1 = T, Make_Figure_2 = T, Make_Figure_3 = T, Make_Figure_4 = T, Working_Directory = getwd(), First_Figure_Name = "Figure 1", Second_Figure_Name = "Figure 2", Third_Figure_Name = "Figure 3", Fourth_Figure_Name = "Figure 4", Show_Individual_Plot_Titles = F, Show_Moments_in_Figure_1 = T, Show_Overlapping_Areas_in_Figure_2 = T, Show_Moments_in_Figure_3 = T, Show_Moments_in_Figure_4 = T, Moment_Rounding_Constant = 3, Overlapping_Area_Rounding_Constant = 3, Number_of_Bins_in_the_Figure_4_Histograms = 10, Maximum_Number_of_Iterations = 1000, Relative_Figure_1_Parameter_Plot_Height = 4, Relative_Figure_1_Treatment_Plot_Height = 3, Relative_Figure_1_Replicate_Plot_Height = 2, Relative_Figure_1_Individual_Plot_Height = 10, Relative_Figure_1_Legend_Plot_Height = 4, Relative_Figure_1_Individual_Plot_Width = 5, Relative_Figure_1_Gap_Plot_Width = 1, Relative_Figure_1_Parameter_Spacing_Line_Plot_Height = 1, Figure_1_Figure_Height_Constant = 30, Figure_1_Figure_Width_Constant = 45, Figure_1_Axis_Labels_and_Axis_Titles_Shifting_Constant = 0, Figure_1_Parameter_Title_Text_Size = 4, Figure_1_Treatment_Title_Text_Size = 3, Figure_1_Replicate_Title_Text_Size = 2, Figure_1_Axis_Titles_Text_Size = 2, Figure_1_Axis_Labels_Text_Size = 2, Figure_1_Individual_Plot_Bottom_Margin = 5, Figure_1_Individual_Plot_Left_Margin = 5, Figure_1_Individual_Plot_Top_Margin = 1, Figure_1_Individual_Plot_Right_Margin = 1, Figure_1_Moment_Text_Size = 2, Figure_1_Legend_Text_Size = 3, Figure_1_Panel_Separation_Line_Width = 2, Relative_Figure_2_Parameter_Plot_Height = 4, Relative_Figure_2_Treatment_Plot_Height = 3, Relative_Figure_2_Replicate_Plot_Height = 2, Relative_Figure_2_Individual_Plot_Dimension = 10, Relative_Figure_2_Gap_Plot_Dimension = 2, Relative_Figure_2_Legend_Plot_Height = 8, Relative_Figure_2_Parameter_Spacing_Line_Plot_Height = 1, Figure_2_Figure_Height_Constant = 75, Figure_2_Figure_Width_Constant = 100, Figure_2_Axis_Labels_and_Axis_Titles_Shifting_Constant = 1, Figure_2_Parameter_Title_Text_Size = 10, Figure_2_Treatment_Title_Text_Size = 8, Figure_2_Replicate_Title_Text_Size = 6, Figure_2_Axis_Titles_Text_Size = 2.5, Figure_2_Axis_Labels_Text_Size = 2.5, Figure_2_Individual_Plot_Bottom_Margin = 10, Figure_2_Individual_Plot_Left_Margin = 10, Figure_2_Individual_Plot_Top_Margin = 1, Figure_2_Individual_Plot_Right_Margin = 1, Figure_2_Overlapping_Area_Text_Size = 4, Figure_2_Legend_Text_Size = 3.5, Figure_2_Panel_Separation_Line_Width = 2, Relative_Figure_3_Parameter_Plot_Height = 4, Relative_Figure_3_Treatment_Plot_Dimension = 3, Relative_Figure_3_Replicate_Plot_Dimension = 2, Relative_Figure_3_Individual_Plot_Dimension = 10, Relative_Figure_3_Gap_Plot_Dimension = 2, Relative_Figure_3_Legend_Plot_Height = 4, Relative_Figure_3_Parameter_Spacing_Line_Plot_Height = 1, Figure_3_Figure_Height_Constant = 60, Figure_3_Figure_Width_Constant = 75, Figure_3_Axis_Labels_and_Axis_Titles_Shifting_Constant = 1, Figure_3_Parameter_Title_Text_Size = 10, Figure_3_Treatment_Title_Text_Size = 8, Figure_3_Replicate_Title_Text_Size = 6, Figure_3_Axis_Titles_Text_Size = 4, Figure_3_Axis_Labels_Text_Size = 4, Figure_3_Individual_Plot_Bottom_Margin = 10, Figure_3_Individual_Plot_Left_Margin = 10, Figure_3_Individual_Plot_Top_Margin = 1, Figure_3_Individual_Plot_Right_Margin = 1, Figure_3_Moment_Text_Size = 4, Figure_3_Legend_Text_Size = 3.5, Figure_3_Panel_Separation_Line_Width = 2, 
                                                                                                                              Relative_Figure_4_Parameter_Plot_Height = 2, Relative_Figure_4_Treatment_Plot_Dimension = 1, Relative_Figure_4_Individual_Plot_Height = 5, Relative_Figure_4_Individual_Plot_Width = 5, Relative_Figure_4_Parameter_Spacing_Line_Plot_Height = 1, Figure_4_Figure_Height_Constant = 75, Figure_4_Figure_Width_Constant = 100, Figure_4_Axis_Labels_and_Axis_Titles_Shifting_Constant = 1.375, Figure_4_Parameter_Title_Text_Size = 4, Figure_4_Treatment_Title_Text_Size = 3, Figure_4_Axis_Titles_Text_Size = 2.5, Figure_4_Axis_Labels_Text_Size = 2.5, Figure_4_Individual_Plot_Bottom_Margin = 10, Figure_4_Individual_Plot_Left_Margin = 10, Figure_4_Individual_Plot_Top_Margin = 1, Figure_4_Individual_Plot_Right_Margin = 1, Figure_4_Moment_Text_Size = 1.75, Figure_4_Panel_Separation_Line_Width = 2, Tolerance = sqrt(.Machine$double.eps)) {
  if (missing(Data_Frame)) {
    stop ("The 'Data_Frame' argument must be provided.")
  }
  if (!is.data.frame(Data_Frame)) {
    stop ("The 'Data_Frame' object must be of class 'data.frame'.")
  }
  if (missing(Treatment)) {
    stop ("The 'Treatment' argument must be provided.")
  }
  Treatment_Name <- substitute(Treatment)
  if (!is.name(Treatment_Name) & !is.character(Treatment_Name)) {
    stop ("The 'Treatment' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
  } else if (is.character(Treatment_Name)) {
    if (!(Treatment_Name %in% colnames(Data_Frame))) {
      stop ("The 'Treatment' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
    }
    Treatment <- Data_Frame[[Treatment_Name]]
  } else if (is.name(Treatment_Name)) {
    if (!(deparse(Treatment_Name) %in% colnames(Data_Frame))) {
      stop ("The 'Treatment' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
    }
    Treatment <- eval(Treatment_Name, Data_Frame)
    Treatment_Name <- deparse(Treatment_Name)
  }
  if (missing(Replicate)) {
    stop ("The 'Replicate' argument must be provided.")
  }
  Replicate_Name <- substitute(Replicate)
  if (!is.name(Replicate_Name) & !is.character(Replicate_Name)) {
    stop ("The 'Replicate' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
  } else if (is.character(Replicate_Name)) {
    if (!(Replicate_Name %in% colnames(Data_Frame))) {
      stop ("The 'Replicate' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
    }
    Replicate <- Data_Frame[[Replicate_Name]]
  } else if (is.name(Replicate_Name)) {
    if (!(deparse(Replicate_Name) %in% colnames(Data_Frame))) {
      stop ("The 'Replicate' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
    }
    Replicate <- eval(Replicate_Name, Data_Frame)
    Replicate_Name <- deparse(Replicate_Name)
  }
  if (missing(Predictor_Variable)) {
    stop ("The 'Predictor_Variable' argument must be provided.")
  }
  Predictor_Variable_Name <- substitute(Predictor_Variable)
  if (!is.name(Predictor_Variable_Name) & !is.character(Predictor_Variable_Name)) {
    stop ("The 'Predictor_Variable' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
  } else if (is.character(Predictor_Variable_Name)) {
    if (!(Predictor_Variable_Name %in% colnames(Data_Frame))) {
      stop ("The 'Predictor_Variable' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
    }
    Predictor_Variable <- Data_Frame[[Predictor_Variable_Name]]
  } else if (is.name(Predictor_Variable_Name)) {
    if (!(deparse(Predictor_Variable_Name) %in% colnames(Data_Frame))) {
      stop ("The 'Predictor_Variable' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
    }
    Predictor_Variable <- eval(Predictor_Variable_Name, Data_Frame)
    Predictor_Variable_Name <- deparse(Predictor_Variable_Name)
  }
  if (missing(Predictor_Variable_Bin_Lower_Limits)) {
    stop ("The 'Predictor_Variable_Bin_Lower_Limits' argument must be provided.")
  }
  Predictor_Variable_Bin_Lower_Limits_Name <- substitute(Predictor_Variable_Bin_Lower_Limits)
  if (!is.name(Predictor_Variable_Bin_Lower_Limits_Name) & !is.character(Predictor_Variable_Bin_Lower_Limits_Name)) {
    stop ("The 'Predictor_Variable_Bin_Lower_Limits' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
  } else if (is.character(Predictor_Variable_Bin_Lower_Limits_Name)) {
    if (!(Predictor_Variable_Bin_Lower_Limits_Name %in% colnames(Data_Frame))) {
      stop ("The 'Predictor_Variable_Bin_Lower_Limits' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
    }
    Predictor_Variable_Bin_Lower_Limits <- Data_Frame[[Predictor_Variable_Bin_Lower_Limits_Name]]
  } else if (is.name(Predictor_Variable_Bin_Lower_Limits_Name)) {
    if (!(deparse(Predictor_Variable_Bin_Lower_Limits_Name) %in% colnames(Data_Frame))) {
      stop ("The 'Predictor_Variable_Bin_Lower_Limits' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
    }
    Predictor_Variable_Bin_Lower_Limits <- eval(Predictor_Variable_Bin_Lower_Limits_Name, Data_Frame)
    Predictor_Variable_Bin_Lower_Limits_Name <- deparse(Predictor_Variable_Bin_Lower_Limits_Name)
  }
  if (!is.character(Predictor_Variable_Bin_Lower_Limits)) {
    stop ("The 'Predictor_Variable_Bin_Lower_Limits' argument must be of class 'character'.")
  }
  if (!all(sapply(Predictor_Variable_Bin_Lower_Limits, function (x) {
    grepl("^c\\(\\s*(\\d+|NA)(\\s*,\\s*(\\d+|NA))*\\s*\\)$", x)
  }))) {
    stop ("The 'Predictor_Variable_Bin_Lower_Limits' argument must contain, inside quotation marks, 'c()' functions which contain one number or multiple numbers separated by commas; these numbers specify the lower limits of the bins.")
  }
  if (missing(Predictor_Variable_Bin_Upper_Limits)) {
    stop ("The 'Predictor_Variable_Bin_Upper_Limits' argument must be provided.")
  }
  Predictor_Variable_Bin_Upper_Limits_Name <- substitute(Predictor_Variable_Bin_Upper_Limits)
  if (!is.name(Predictor_Variable_Bin_Upper_Limits_Name) & !is.character(Predictor_Variable_Bin_Upper_Limits_Name)) {
    stop ("The 'Predictor_Variable_Bin_Upper_Limits' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
  } else if (is.character(Predictor_Variable_Bin_Upper_Limits_Name)) {
    if (!(Predictor_Variable_Bin_Upper_Limits_Name %in% colnames(Data_Frame))) {
      stop ("The 'Predictor_Variable_Bin_Upper_Limits' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
    }
    Predictor_Variable_Bin_Upper_Limits <- Data_Frame[[Predictor_Variable_Bin_Upper_Limits_Name]]
  } else if (is.name(Predictor_Variable_Bin_Upper_Limits_Name)) {
    if (!(deparse(Predictor_Variable_Bin_Upper_Limits_Name) %in% colnames(Data_Frame))) {
      stop ("The 'Predictor_Variable_Bin_Upper_Limits' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
    }
    Predictor_Variable_Bin_Upper_Limits <- eval(Predictor_Variable_Bin_Upper_Limits_Name, Data_Frame)
    Predictor_Variable_Bin_Upper_Limits_Name <- deparse(Predictor_Variable_Bin_Upper_Limits_Name)
  }
  if (!is.character(Predictor_Variable_Bin_Upper_Limits)) {
    stop ("The 'Predictor_Variable_Bin_Upper_Limits' argument must be of class 'character'.")
  }
  if (!all(sapply(Predictor_Variable_Bin_Upper_Limits, function (x) {
    grepl("^c\\(\\s*(\\d+|NA)(\\s*,\\s*(\\d+|NA))*\\s*\\)$", x)
  }))) {
    stop ("The 'Predictor_Variable_Bin_Upper_Limits' argument must contain, inside quotation marks, 'c()' functions which contain one number or multiple numbers separated by commas; these numbers specify the upper limits of the bins.")
  }
  if (missing(Response_Variable)) {
    stop ("The 'Response_Variable' argument must be provided.")
  }
  Response_Variable_Name <- substitute(Response_Variable)
  if (!is.name(Response_Variable_Name) & !is.character(Response_Variable_Name)) {
    stop ("The 'Response_Variable' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
  } else if (is.character(Response_Variable_Name)) {
    if (!(Response_Variable_Name %in% colnames(Data_Frame))) {
      stop ("The 'Response_Variable' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
    }
    Response_Variable <- Data_Frame[[Response_Variable_Name]]
  } else if (is.name(Response_Variable_Name)) {
    if (!(deparse(Response_Variable_Name) %in% colnames(Data_Frame))) {
      stop ("The 'Response_Variable' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
    }
    Response_Variable <- eval(Response_Variable_Name, Data_Frame)
    Response_Variable_Name <- deparse(Response_Variable_Name)
  }
  if (!is.numeric(Response_Variable)) {
    stop ("The 'Response_Variable' argument must be numeric.")
  }
  if (missing(Beta_Distribution_Alpha_Parameter_Values) & missing(Beta_Distribution_Beta_Parameter_Values)) {
    Beta_Distribution_Alpha_Parameter_Values_Name <- "Beta_Distribution_Alpha_Parameter_Values"
    Beta_Distribution_Beta_Parameter_Values_Name <- "Beta_Distribution_Beta_Parameter_Values"
  }
  if (missing(Beta_Distribution_Alpha_Parameter_Values) & !(missing(Beta_Distribution_Beta_Parameter_Values))) {
    stop ("To specify beta distributions for the sampling, the 'Beta_Distribution_Alpha_Parameter_Values' argument must be provided in addition to the 'Beta_Distribution_Beta_Parameter_Values' argument..")
  }
  if (!(missing(Beta_Distribution_Alpha_Parameter_Values)) & missing(Beta_Distribution_Beta_Parameter_Values)) {
    stop ("To specify beta distributions for the sampling, the 'Beta_Distribution_Beta_Parameter_Values' argument must be provided in addition to the 'Beta_Distribution_Alpha_Parameter_Values' argument..")
  }
  if (!(missing(Beta_Distribution_Alpha_Parameter_Values)) & !(missing(Beta_Distribution_Beta_Parameter_Values))) {
    Beta_Distribution_Alpha_Parameter_Values_Name <- substitute(Beta_Distribution_Alpha_Parameter_Values)
    if (!is.name(Beta_Distribution_Alpha_Parameter_Values_Name) & !is.character(Beta_Distribution_Alpha_Parameter_Values_Name)) {
      stop ("The 'Beta_Distribution_Alpha_Parameter_Values' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
    } else if (is.character(Beta_Distribution_Alpha_Parameter_Values_Name)) {
      if (!(Beta_Distribution_Alpha_Parameter_Values_Name %in% colnames(Data_Frame))) {
        stop ("The 'Beta_Distribution_Alpha_Parameter_Values' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
      }
      Beta_Distribution_Alpha_Parameter_Values <- Data_Frame[[Beta_Distribution_Alpha_Parameter_Values_Name]]
    } else if (is.name(Beta_Distribution_Alpha_Parameter_Values_Name)) {
      if (!(deparse(Beta_Distribution_Alpha_Parameter_Values_Name) %in% colnames(Data_Frame))) {
        stop ("The 'Beta_Distribution_Alpha_Parameter_Values' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
      }
      Beta_Distribution_Alpha_Parameter_Values <- eval(Beta_Distribution_Alpha_Parameter_Values_Name, Data_Frame)
      Beta_Distribution_Alpha_Parameter_Values_Name <- deparse(Beta_Distribution_Alpha_Parameter_Values_Name)
    }
    Beta_Distribution_Beta_Parameter_Values_Name <- substitute(Beta_Distribution_Beta_Parameter_Values)
    if (!is.name(Beta_Distribution_Beta_Parameter_Values_Name) & !is.character(Beta_Distribution_Beta_Parameter_Values_Name)) {
      stop ("The 'Beta_Distribution_Beta_Parameter_Values' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
    } else if (is.character(Beta_Distribution_Beta_Parameter_Values_Name)) {
      if (!(Beta_Distribution_Beta_Parameter_Values_Name %in% colnames(Data_Frame))) {
        stop ("The 'Beta_Distribution_Beta_Parameter_Values' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
      }
      Beta_Distribution_Beta_Parameter_Values <- Data_Frame[[Beta_Distribution_Beta_Parameter_Values_Name]]
    } else if (is.name(Beta_Distribution_Beta_Parameter_Values_Name)) {
      if (!(deparse(Beta_Distribution_Beta_Parameter_Values_Name) %in% colnames(Data_Frame))) {
        stop ("The 'Beta_Distribution_Beta_Parameter_Values' argument must be a quoted or unquoted column name from the 'Data_Frame' data frame.")
      }
      Beta_Distribution_Beta_Parameter_Values <- eval(Beta_Distribution_Beta_Parameter_Values_Name, Data_Frame)
      Beta_Distribution_Beta_Parameter_Values_Name <- deparse(Beta_Distribution_Beta_Parameter_Values_Name)
    }
    if (!is.numeric(Beta_Distribution_Alpha_Parameter_Values)) {
      stop ("The 'Beta_Distribution_Alpha_Parameter_Values' argument must be numeric.")
    }
    if (!is.numeric(Beta_Distribution_Beta_Parameter_Values)) {
      stop ("The 'Beta_Distribution_Beta_Parameter_Values' argument must be numeric.")
    }
  }
  if (missing(Parameters)) {
    stop ("The 'Parameters' argument must be provided.")
  }
  if (!is.character(Parameters)) {
    stop ("The 'Parameters' argument must be of class 'character'.")
  }
  if (length(Parameters) >= length(unique(Predictor_Variable))) {
    stop ("The number of parameters to estimate must be less than the number of predictor-variable bins to avoid overfitting the model.")
  }
  if (missing(Formula)) {
    stop ("The 'Formula' argument must be provided.")
  }
  if (":" %in% all.names(Formula)) {
    stop ("Interaction terms are not allowed in this model.")
  }
  if (!identical(c(Response_Variable_Name, Predictor_Variable_Name, Parameters)[order(c(Response_Variable_Name, Predictor_Variable_Name, Parameters))], all.vars(Formula)[order(all.vars(Formula))])) {
    stop ("The response variable specified by the 'Response_Variable' argument, the predictor variable specified by the 'Predictor_Variable' argument, and the parameters specified by the 'Parameters' argument must make up all the terms in the 'Formula' formula, and there must be no additional terms present.")
  }
  if (all.vars(Formula[[2]]) != Response_Variable_Name) {
    stop ("The response variable specified by the 'Response_Variable' argument must also be the response variable in the 'Formula' argument.")
  }
  if (setdiff(all.vars(Formula), c(Response_Variable_Name, Parameters)) != Predictor_Variable_Name) {
    stop ("The predictor variable specified by the 'Predictor_Variable' argument must also be the predictor variable in the 'Formula' argument.")
  }
  if (!identical(setdiff(all.vars(Formula), c(Response_Variable_Name, Predictor_Variable_Name))[order(setdiff(all.vars(Formula), c(Response_Variable_Name, Predictor_Variable_Name)))], Parameters[order(Parameters)])) {
    stop ("The parameters specified by the 'Parameters' argument must be present in the formula.")
  }
  for (i in seq_len(length(Formula) - 1)) {
    Formula_Component <- deparse(Formula[[(i + 1)]])
    Indices <- gregexpr("[()]", Formula_Component)[[1]]
    attributes(Indices) <- NULL
    Parentheses_Only <- strsplit(Formula_Component, "")[[1]][Indices]
    k <- 0
    for (j in seq_along(Parentheses_Only)) {
      if (Parentheses_Only[j] == "(") {
        k <- k + 1
      } else if (Parentheses_Only[j] == ")") {
        k <- k - 1
      }
      if (k < 0) {
        stop ("A close parenthesis that does not have a matching open parenthesis in the 'Formula' object was detected.")
      }
    }
    if (k > 0) {
      stop ("An open parenthesis that does not have a matching close parenthesis in the 'Formula' object was detected.")
    }
  }
  Functions <- setdiff(all.names(Formula[2:length(Formula)]), all.vars(Formula))
  Functions <- Functions[which((Functions != "(") & (Functions != ")"))]
  Mathematical_Scalar_Functions <- c("abs", "sign", "sqrt", "floor", "ceiling", "trunc", "round", "signif", "exp", "log", "expm1", "log1p", "cos", "sin", "tan", "cospi", "sinpi", "tanpi", "acos", "asin", "atan", "atan2", "cosh", "sinh", "tanh", "acosh", "asinh", "atanh", "lgamma", "gamma", "digamma", "trigamma", "+", "-", "*", "/", "^", "%%", "%/%", "log10", "log2", "factorial", "lfactorial")
  Functions_That_Are_not_Allowed <- setdiff(Functions, Mathematical_Scalar_Functions)
  if (length(Functions_That_Are_not_Allowed) > 0) {
    if (length(Functions_That_Are_not_Allowed) == 1) {
      stop (paste0("The function ", Functions_That_Are_not_Allowed, " that is present in the 'Formula' object is either not an R function or is not allowed in this context."))
    } else if (length(Functions_That_Are_not_Allowed) == 2) {
      stop (paste0("The functions ", Functions_That_Are_not_Allowed[1], " and ", Functions_That_Are_not_Allowed[2], " that are present in the 'Formula' object are either not R functions or are not allowed in this context."))
    } else if (length(Functions_That_Are_not_Allowed) > 2) {
      stop (paste0("The functions ", paste0(Reduce(function (x, y) {
        paste0(x, ", ", y)
      }, Functions_That_Are_not_Allowed[1:(length(Functions_That_Are_not_Allowed) - 1)]), ", and ", Functions_That_Are_not_Allowed[length(Functions_That_Are_not_Allowed)]), " that are present in the 'Formula' object are either not R functions or are not allowed in this context."))
    }
  }
  Split_Data <- lapply(split(Data_Frame, Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)]), function (x) {
    split(x, x[, which(colnames(x) == Replicate_Name)])
  })
  Diagnostic_Data_Frame <- do.call("rbind", lapply(Split_Data, function (x) {
    do.call("rbind", lapply(x, function (y) {
      z <- data.frame(Treatment = unique(y[, which(colnames(y) == Treatment_Name)]), Replicate = unique(y[, which(colnames(y) == Replicate_Name)]), Are_There_Enough_Response_Variable_Entries = !(length(which(is.finite(y[, which(colnames(y) == Response_Variable_Name)]))) <= length(Parameters)), Are_There_Enough_Predictor_Variable_Bin_Lower_Limits_Entries = length(which(!sapply(y[, which(colnames(y) == Predictor_Variable_Bin_Lower_Limits_Name)], function (w) {
        any(is.na(eval(parse(text = w))))
      }))) > length(Parameters), Are_There_Enough_Predictor_Variable_Bin_Upper_Limits_Entries = length(which(!sapply(y[, which(colnames(y) == Predictor_Variable_Bin_Upper_Limits_Name)], function (w) {
        any(is.na(eval(parse(text = w))))
      }))) > length(Parameters))
      colnames(z)[which(colnames(z) %in% c("Treatment", "Replicate"))] <- c(Treatment_Name, Replicate_Name)
      z
    }))
  }))
  rownames(Diagnostic_Data_Frame) <- NULL
  if (any(Diagnostic_Data_Frame$Are_There_Enough_Response_Variable_Entries == F)) {
    Experimental_Units_With_Too_Few_Finite_Response_Variables <- paste0(Replicate_Name, " ", Diagnostic_Data_Frame[, which(colnames(Diagnostic_Data_Frame) == Replicate_Name)][which(Diagnostic_Data_Frame$Are_There_Enough_Response_Variable_Entries == F)], " from ", Treatment_Name, " ", Diagnostic_Data_Frame[, which(colnames(Diagnostic_Data_Frame) == Treatment_Name)][which(Diagnostic_Data_Frame$Are_There_Enough_Response_Variable_Entries == F)])
    if (length(Experimental_Units_With_Too_Few_Finite_Response_Variables) == 1) {
      Additional_Text <- paste0("The questionable experimental unit is ", Experimental_Units_With_Too_Few_Finite_Response_Variables, ".")
      stop (paste0("There is 1 experimental unit that doesn't have enough finite data points to fit the model properly - with ", length(Parameters), " model parameters to fit, ", (length(Parameters) + 1), " data points are needed for each regression model. ", Additional_Text, " Please either omit this experimental unit from this analysis or provide the necessary finite data points for the response variable if they are available."))
    } else if (length(Experimental_Units_With_Too_Few_Finite_Response_Variables) == 2) {
      Additional_Text <- paste0("The questionable experimental units are ", Experimental_Units_With_Too_Few_Finite_Response_Variables[1], " and ", Experimental_Units_With_Too_Few_Finite_Response_Variables[2], ".")
      stop (paste0("There are 2 experimental units that don't have enough finite data points to fit the model properly - with ", length(Parameters), " model parameters to fit, ", (length(Parameters) + 1), " data points are needed for each regression model. ", Additional_Text, " Please either omit these experimental units from this analysis or provide the necessary finite data points for the response variable if they are available."))
    } else if (length(Experimental_Units_With_Too_Few_Finite_Response_Variables) > 2) {
      Additional_Text <- paste0("The questionable experimental units are ", Reduce(function (x, y) {
        paste0(x, ", ", y)
      }, Experimental_Units_With_Too_Few_Finite_Response_Variables[1:(length(Experimental_Units_With_Too_Few_Finite_Response_Variables) - 1)]), ", and ", Experimental_Units_With_Too_Few_Finite_Response_Variables[length(Experimental_Units_With_Too_Few_Finite_Response_Variables)], ".")
      stop (paste0("There are ", length(which(Diagnostic_Data_Frame$Are_There_Enough_Response_Variable_Entries == F)), " experimental units that don't have enough finite data points to fit the model properly - with ", length(Parameters), " model parameters to fit, ", (length(Parameters) + 1), " data points are needed for each regression model. ", Additional_Text, " Please either omit these experimental units from this analysis or provide the necessary finite data points for the response variable if they are available."))
    }
  }
  if (any(Diagnostic_Data_Frame$Are_There_Enough_Predictor_Variable_Bin_Lower_Limits_Entries == F)) {
    Experimental_Units_With_Too_Few_Finite_Predictor_Variable_Bin_Lower_Limits <- paste0(Replicate_Name, " ", Diagnostic_Data_Frame[, which(colnames(Diagnostic_Data_Frame) == Replicate_Name)][which(Diagnostic_Data_Frame$Are_There_Enough_Predictor_Variable_Bin_Lower_Limits_Entries == F)], " from ", Treatment_Name, " ", Diagnostic_Data_Frame[, which(colnames(Diagnostic_Data_Frame) == Treatment_Name)][which(Diagnostic_Data_Frame$Are_There_Enough_Predictor_Variable_Bin_Lower_Limits_Entries == F)])
    if (length(Experimental_Units_With_Too_Few_Finite_Predictor_Variable_Bin_Lower_Limits) == 1) {
      Additional_Text <- paste0("The questionable experimental unit is ", Experimental_Units_With_Too_Few_Finite_Predictor_Variable_Bin_Lower_Limits, ".")
      stop (paste0("There is 1 experimental unit that doesn't have enough finite data points to fit the model properly - with ", length(Parameters), " model parameters to fit, ", (length(Parameters) + 1), " data points are needed for each regression model. ", Additional_Text, " Please either omit this experimental unit from this analysis or provide the necessary finite data points for the predictor variable bin lower limits if they are available."))
    } else if (length(Experimental_Units_With_Too_Few_Finite_Predictor_Variable_Bin_Lower_Limits) == 2) {
      Additional_Text <- paste0("The questionable experimental units are ", Experimental_Units_With_Too_Few_Finite_Predictor_Variable_Bin_Lower_Limits[1], " and ", Experimental_Units_With_Too_Few_Finite_Predictor_Variable_Bin_Lower_Limits[2], ".")
      stop (paste0("There are 2 experimental units that don't have enough finite data points to fit the model properly - with ", length(Parameters), " model parameters to fit, ", (length(Parameters) + 1), " data points are needed for each regression model. ", Additional_Text, " Please either omit these experimental units from this analysis or provide the necessary finite data points for the predictor variable bin lower limits if they are available."))
    } else if (length(Experimental_Units_With_Too_Few_Finite_Predictor_Variable_Bin_Lower_Limits) > 2) {
      Additional_Text <- paste0("The questionable experimental units are ", Reduce(function (x, y) {
        paste0(x, ", ", y)
      }, Experimental_Units_With_Too_Few_Finite_Predictor_Variable_Bin_Lower_Limits[1:(length(Experimental_Units_With_Too_Few_Finite_Predictor_Variable_Bin_Lower_Limits) - 1)]), ", and ", Experimental_Units_With_Too_Few_Finite_Predictor_Variable_Bin_Lower_Limits[length(Experimental_Units_With_Too_Few_Finite_Predictor_Variable_Bin_Lower_Limits)], ".")
      stop (paste0("There are ", length(which(Diagnostic_Data_Frame$Are_There_Enough_Predictor_Variable_Bin_Lower_Limits_Entries == F)), " experimental units that don't have enough finite data points to fit the model properly - with ", length(Parameters), " model parameters to fit, ", (length(Parameters) + 1), " data points are needed for each regression model. ", Additional_Text, " Please either omit these experimental units from this analysis or provide the necessary finite data points for the predictor variable bin lower limits if they are available."))
    }
  }
  if (any(Diagnostic_Data_Frame$Are_There_Enough_Predictor_Variable_Bin_Upper_Limits_Entries == F)) {
    Experimental_Units_With_Too_Few_Finite_Predictor_Variable_Bin_Upper_Limits <- paste0(Replicate_Name, " ", Diagnostic_Data_Frame[, which(colnames(Diagnostic_Data_Frame) == Replicate_Name)][which(Diagnostic_Data_Frame$Are_There_Enough_Predictor_Variable_Bin_Upper_Limits_Entries == F)], " from ", Treatment_Name, " ", Diagnostic_Data_Frame[, which(colnames(Diagnostic_Data_Frame) == Treatment_Name)][which(Diagnostic_Data_Frame$Are_There_Enough_Predictor_Variable_Bin_Upper_Limits_Entries == F)])
    if (length(Experimental_Units_With_Too_Few_Finite_Predictor_Variable_Bin_Upper_Limits) == 1) {
      Additional_Text <- paste0("The questionable experimental unit is ", Experimental_Units_With_Too_Few_Finite_Predictor_Variable_Bin_Upper_Limits, ".")
      stop (paste0("There is 1 experimental unit that doesn't have enough finite data points to fit the model properly - with ", length(Parameters), " model parameters to fit, ", (length(Parameters) + 1), " data points are needed for each regression model. ", Additional_Text, " Please either omit this experimental unit from this analysis or provide the necessary finite data points for the predictor variable bin lower limits if they are available."))
    } else if (length(Experimental_Units_With_Too_Few_Finite_Predictor_Variable_Bin_Upper_Limits) == 2) {
      Additional_Text <- paste0("The questionable experimental units are ", Experimental_Units_With_Too_Few_Finite_Predictor_Variable_Bin_Upper_Limits[1], " and ", Experimental_Units_With_Too_Few_Finite_Predictor_Variable_Bin_Upper_Limits[2], ".")
      stop (paste0("There are 2 experimental units that don't have enough finite data points to fit the model properly - with ", length(Parameters), " model parameters to fit, ", (length(Parameters) + 1), " data points are needed for each regression model. ", Additional_Text, " Please either omit these experimental units from this analysis or provide the necessary finite data points for the predictor variable bin lower limits if they are available."))
    } else if (length(Experimental_Units_With_Too_Few_Finite_Predictor_Variable_Bin_Upper_Limits) > 2) {
      Additional_Text <- paste0("The questionable experimental units are ", Reduce(function (x, y) {
        paste0(x, ", ", y)
      }, Experimental_Units_With_Too_Few_Finite_Predictor_Variable_Bin_Upper_Limits[1:(length(Experimental_Units_With_Too_Few_Finite_Predictor_Variable_Bin_Upper_Limits) - 1)]), ", and ", Experimental_Units_With_Too_Few_Finite_Predictor_Variable_Bin_Upper_Limits[length(Experimental_Units_With_Too_Few_Finite_Predictor_Variable_Bin_Upper_Limits)], ".")
      stop (paste0("There are ", length(which(Diagnostic_Data_Frame$Are_There_Enough_Predictor_Variable_Bin_Upper_Limits_Entries == F)), " experimental units that don't have enough finite data points to fit the model properly - with ", length(Parameters), " model parameters to fit, ", (length(Parameters) + 1), " data points are needed for each regression model. ", Additional_Text, " Please either omit these experimental units from this analysis or provide the necessary finite data points for the predictor variable bin lower limits if they are available."))
    }
  }
  if (length(Color_Opacity) != 1) {
    stop ("The 'Color_Opacity' argument must be of length 1.")
  }
  if (is.na(Color_Opacity)) {
    stop ("The 'Color_Opacity' argument must not be missing.")
  }
  if (!is.numeric(Color_Opacity)) {
    stop ("The 'Color_Opacity' argument must be numeric.")
  }
  if ((Color_Opacity <= 0) | (Color_Opacity >= 1)) {
    stop ("The 'Color_Opacity' argument must be between 0 and 1 (exclusive).")
  }
  if (length(Number_of_Simulations_per_Experimental_Unit) != 1) {
    stop ("The 'Number_of_Simulations_per_Experimental_Unit' argument must be of length 1.")
  }
  if (is.na(Number_of_Simulations_per_Experimental_Unit)) {
    stop ("The 'Number_of_Simulations_per_Experimental_Unit' argument must not be missing.")
  }
  if (!is.numeric(Number_of_Simulations_per_Experimental_Unit)) {
    stop ("The 'Number_of_Simulations_per_Experimental_Unit' argument must be numeric.")
  }
  if ((Number_of_Simulations_per_Experimental_Unit %% 1) != 0) {
    stop ("The 'Number_of_Simulations_per_Experimental_Unit' argument must be a whole number.")
  }
  if (Number_of_Simulations_per_Experimental_Unit <= 0) {
    stop ("The 'Number_of_Simulations_per_Experimental_Unit' argument must be a positive number.")
  }
  if (length(Number_of_Histogram_Bins) != 1) {
    stop ("The 'Number_of_Histogram_Bins' argument must be of length 1.")
  }
  if (is.na(Number_of_Histogram_Bins)) {
    stop ("The 'Number_of_Histogram_Bins' argument must not be missing.")
  }
  if (!is.numeric(Number_of_Histogram_Bins)) {
    stop ("The 'Number_of_Histogram_Bins' argument must be numeric.")
  }
  if ((Number_of_Histogram_Bins %% 1) != 0) {
    stop ("The 'Number_of_Histogram_Bins' argument must be a whole number.")
  }
  if (Number_of_Histogram_Bins <= 0) {
    stop ("The 'Number_of_Histogram_Bins' argument must be a positive number.")
  }
  if (!is.numeric(Moments_to_Report)) {
    stop ("The 'Moments_to_Report' argument must be of class 'numeric'.")
  }
  if (any(is.na(Moments_to_Report))) {
    stop ("The 'Moments_to_Report' argument must not contain any missing values.")
  }
  if (!is.numeric(Number_of_Histogram_Bins)) {
    stop ("The 'Number_of_Histogram_Bins' argument must be numeric.")
  }
  if (any((Moments_to_Report %% 1) != 0)) {
    stop ("The 'Moments_to_Report' argument must contain only whole numbers.")
  }
  if (any(Moments_to_Report <= 0)) {
    stop ("The 'Moments_to_Report' argument must contain only positive numbers.")
  }
  if (Working_Directory != getwd()) {
    if (length(Working_Directory) != 1) {
      stop ("The 'Working_Directory' argument must be of length 1.")
    }
    if (!is.character(Working_Directory)) {
      stop ("The 'Working_Directory' argument must be of class 'character'.")
    }
    if (is.na(Working_Directory)) {
      stop ("The 'Working_Directory' argument must not be missing.")
    }
    if (!dir.exists(Working_Directory)) {
      stop ("The '' argument must be a folder that exists on the hard drive of this computer.")
    }
  }
  if (length(Make_Figure_1) != 1) {
    stop ("The 'Make_Figure_1' argument must be of length 1.")
  }
  if (!is.logical(Make_Figure_1)) {
    stop ("The 'Make_Figure_1' argument must be of class 'logical'.")
  }
  if (is.na(Make_Figure_1)) {
    stop ("The 'Make_Figure_1' argument must not be missing.")
  }
  if (length(Make_Figure_2) != 1) {
    stop ("The 'Make_Figure_2' argument must be of length 1.")
  }
  if (!is.logical(Make_Figure_2)) {
    stop ("The 'Make_Figure_2' argument must be of class 'logical'.")
  }
  if (is.na(Make_Figure_2)) {
    stop ("The 'Make_Figure_2' argument must not be missing.")
  }
  if (length(Make_Figure_3) != 1) {
    stop ("The 'Make_Figure_3' argument must be of length 1.")
  }
  if (!is.logical(Make_Figure_3)) {
    stop ("The 'Make_Figure_3' argument must be of class 'logical'.")
  }
  if (is.na(Make_Figure_3)) {
    stop ("The 'Make_Figure_3' argument must not be missing.")
  }
  if (length(Make_Figure_4) != 1) {
    stop ("The 'Make_Figure_4' argument must be of length 1.")
  }
  if (!is.logical(Make_Figure_4)) {
    stop ("The 'Make_Figure_4' argument must be of class 'logical'.")
  }
  if (is.na(Make_Figure_4)) {
    stop ("The 'Make_Figure_4' argument must not be missing.")
  }
  if (length(Show_Individual_Plot_Titles) != 1) {
    stop ("The 'Show_Individual_Plot_Titles' argument must be of length 1.")
  }
  if (!is.logical(Show_Individual_Plot_Titles)) {
    stop ("The 'Show_Individual_Plot_Titles' argument must be of class 'logical'.")
  }
  if (is.na(Show_Individual_Plot_Titles)) {
    stop ("The 'Show_Individual_Plot_Titles' argument must not be missing.")
  }
  if (Make_Figure_1 == T) {
    if (length(Show_Moments_in_Figure_1) != 1) {
      stop ("The 'Show_Moments_in_Figure_1' argument must be of length 1.")
    }
    if (!is.logical(Show_Moments_in_Figure_1)) {
      stop ("The 'Show_Moments_in_Figure_1' argument must be of class 'logical'.")
    }
    if (is.na(Show_Moments_in_Figure_1)) {
      stop ("The 'Show_Moments_in_Figure_1' argument must not be missing.")
    }
    if (length(First_Figure_Name) != 1) {
      stop ("The 'First_Figure_Name' argument must be of length 1.")
    }
    if (!is.character(First_Figure_Name)) {
      stop ("The 'First_Figure_Name' argument must be of class 'character'.")
    }
    if (is.na(First_Figure_Name)) {
      stop ("The 'First_Figure_Name' argument must not be missing.")
    }
    if (Show_Moments_in_Figure_1 == T) {
      if (length(Moment_Rounding_Constant) != 1) {
        stop ("The 'Moment_Rounding_Constant' argument must be of length 1.")
      }
      if (!is.numeric(Moment_Rounding_Constant)) {
        stop ("The 'Moment_Rounding_Constant' argument must be of class 'numeric'.")
      }
      if (is.na(Moment_Rounding_Constant)) {
        stop ("The 'Moment_Rounding_Constant' argument must not be missing.")
      }
      if (Moment_Rounding_Constant < 0) {
        stop ("The 'Moment_Rounding_Constant' argument must not be negative.")
      }
      if ((Moment_Rounding_Constant %% 1) != 0) {
        stop ("The 'Moment_Rounding_Constant' argument must not be negative.")
      }
      if (length(Figure_1_Moment_Text_Size) != 1) {
        stop ("The 'Figure_1_Moment_Text_Size' argument must be of length 1.")
      }
      if (!is.numeric(Figure_1_Moment_Text_Size)) {
        stop ("The 'Figure_1_Moment_Text_Size' argument must be of class 'numeric'.")
      }
      if (is.na(Figure_1_Moment_Text_Size)) {
        stop ("The 'Figure_1_Moment_Text_Size' argument must not be missing.")
      }
      if (Figure_1_Moment_Text_Size <= 0) {
        stop ("The 'Figure_1_Moment_Text_Size' argument must be positive.")
      }
    }
    if (length(Relative_Figure_1_Parameter_Plot_Height) != 1) {
      stop ("The 'Relative_Figure_1_Parameter_Plot_Height' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_1_Parameter_Plot_Height)) {
      stop ("The 'Relative_Figure_1_Parameter_Plot_Height' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_1_Parameter_Plot_Height)) {
      stop ("The 'Relative_Figure_1_Parameter_Plot_Height' argument must not be missing.")
    }
    if (Relative_Figure_1_Parameter_Plot_Height <= 0) {
      stop ("The 'Relative_Figure_1_Parameter_Plot_Height' argument must be positive.")
    }
    if (length(Relative_Figure_1_Treatment_Plot_Height) != 1) {
      stop ("The 'Relative_Figure_1_Treatment_Plot_Height' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_1_Treatment_Plot_Height)) {
      stop ("The 'Relative_Figure_1_Treatment_Plot_Height' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_1_Treatment_Plot_Height)) {
      stop ("The 'Relative_Figure_1_Treatment_Plot_Height' argument must not be missing.")
    }
    if (Relative_Figure_1_Treatment_Plot_Height <= 0) {
      stop ("The 'Relative_Figure_1_Treatment_Plot_Height' argument must be positive.")
    }
    if (length(Relative_Figure_1_Replicate_Plot_Height) != 1) {
      stop ("The 'Relative_Figure_1_Replicate_Plot_Height' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_1_Replicate_Plot_Height)) {
      stop ("The 'Relative_Figure_1_Replicate_Plot_Height' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_1_Replicate_Plot_Height)) {
      stop ("The 'Relative_Figure_1_Replicate_Plot_Height' argument must not be missing.")
    }
    if (Relative_Figure_1_Replicate_Plot_Height <= 0) {
      stop ("The 'Relative_Figure_1_Replicate_Plot_Height' argument must be positive.")
    }
    if (length(Relative_Figure_1_Individual_Plot_Height) != 1) {
      stop ("The 'Relative_Figure_1_Individual_Plot_Height' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_1_Individual_Plot_Height)) {
      stop ("The 'Relative_Figure_1_Individual_Plot_Height' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_1_Individual_Plot_Height)) {
      stop ("The 'Relative_Figure_1_Individual_Plot_Height' argument must not be missing.")
    }
    if (Relative_Figure_1_Individual_Plot_Height <= 0) {
      stop ("The 'Relative_Figure_1_Individual_Plot_Height' argument must be positive.")
    }
    if (length(Relative_Figure_1_Legend_Plot_Height) != 1) {
      stop ("The 'Relative_Figure_1_Legend_Plot_Height' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_1_Legend_Plot_Height)) {
      stop ("The 'Relative_Figure_1_Legend_Plot_Height' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_1_Legend_Plot_Height)) {
      stop ("The 'Relative_Figure_1_Legend_Plot_Height' argument must not be missing.")
    }
    if (Relative_Figure_1_Legend_Plot_Height <= 0) {
      stop ("The 'Relative_Figure_1_Legend_Plot_Height' argument must be positive.")
    }
    if (length(Relative_Figure_1_Individual_Plot_Width) != 1) {
      stop ("The 'Relative_Figure_1_Individual_Plot_Width' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_1_Individual_Plot_Width)) {
      stop ("The 'Relative_Figure_1_Individual_Plot_Width' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_1_Individual_Plot_Width)) {
      stop ("The 'Relative_Figure_1_Individual_Plot_Width' argument must not be missing.")
    }
    if (Relative_Figure_1_Individual_Plot_Width <= 0) {
      stop ("The 'Relative_Figure_1_Individual_Plot_Width' argument must be positive.")
    }
    if (length(Relative_Figure_1_Gap_Plot_Width) != 1) {
      stop ("The 'Relative_Figure_1_Gap_Plot_Width' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_1_Gap_Plot_Width)) {
      stop ("The 'Relative_Figure_1_Gap_Plot_Width' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_1_Gap_Plot_Width)) {
      stop ("The 'Relative_Figure_1_Gap_Plot_Width' argument must not be missing.")
    }
    if (Relative_Figure_1_Gap_Plot_Width <= 0) {
      stop ("The 'Relative_Figure_1_Gap_Plot_Width' argument must be positive.")
    }
    if (length(Relative_Figure_1_Parameter_Spacing_Line_Plot_Height) != 1) {
      stop ("The 'Relative_Figure_1_Parameter_Spacing_Line_Plot_Height' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_1_Parameter_Spacing_Line_Plot_Height)) {
      stop ("The 'Relative_Figure_1_Parameter_Spacing_Line_Plot_Height' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_1_Parameter_Spacing_Line_Plot_Height)) {
      stop ("The 'Relative_Figure_1_Parameter_Spacing_Line_Plot_Height' argument must not be missing.")
    }
    if (Relative_Figure_1_Parameter_Spacing_Line_Plot_Height <= 0) {
      stop ("The 'Relative_Figure_1_Parameter_Spacing_Line_Plot_Height' argument must be positive.")
    }
    if (length(Figure_1_Figure_Height_Constant) != 1) {
      stop ("The 'Figure_1_Figure_Height_Constant' argument must be of length 1.")
    }
    if (!is.numeric(Figure_1_Figure_Height_Constant)) {
      stop ("The 'Figure_1_Figure_Height_Constant' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_1_Figure_Height_Constant)) {
      stop ("The 'Figure_1_Figure_Height_Constant' argument must not be missing.")
    }
    if (Figure_1_Figure_Height_Constant <= 0) {
      stop ("The 'Figure_1_Figure_Height_Constant' argument must be positive.")
    }
    if (length(Figure_1_Figure_Width_Constant) != 1) {
      stop ("The 'Figure_1_Figure_Width_Constant' argument must be of length 1.")
    }
    if (!is.numeric(Figure_1_Figure_Width_Constant)) {
      stop ("The 'Figure_1_Figure_Width_Constant' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_1_Figure_Width_Constant)) {
      stop ("The 'Figure_1_Figure_Width_Constant' argument must not be missing.")
    }
    if (Figure_1_Figure_Width_Constant <= 0) {
      stop ("The 'Figure_1_Figure_Width_Constant' argument must be positive.")
    }
    if (length(Figure_1_Axis_Labels_and_Axis_Titles_Shifting_Constant) != 1) {
      stop ("The 'Figure_1_Axis_Labels_and_Axis_Titles_Shifting_Constant' argument must be of length 1.")
    }
    if (!is.numeric(Figure_1_Axis_Labels_and_Axis_Titles_Shifting_Constant)) {
      stop ("The 'Figure_1_Axis_Labels_and_Axis_Titles_Shifting_Constant' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_1_Axis_Labels_and_Axis_Titles_Shifting_Constant)) {
      stop ("The 'Figure_1_Axis_Labels_and_Axis_Titles_Shifting_Constant' argument must not be missing.")
    }
    if (Figure_1_Axis_Labels_and_Axis_Titles_Shifting_Constant < 0) {
      warning ("The 'Figure_1_Axis_Labels_and_Axis_Titles_Shifting_Constant' argument should not be negative.")
    }
    if (length(Figure_1_Parameter_Title_Text_Size) != 1) {
      stop ("The 'Figure_1_Parameter_Title_Text_Size' argument must be of length 1.")
    }
    if (!is.numeric(Figure_1_Parameter_Title_Text_Size)) {
      stop ("The 'Figure_1_Parameter_Title_Text_Size' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_1_Parameter_Title_Text_Size)) {
      stop ("The 'Figure_1_Parameter_Title_Text_Size' argument must not be missing.")
    }
    if (Figure_1_Parameter_Title_Text_Size <= 0) {
      stop ("The 'Figure_1_Parameter_Title_Text_Size' argument must be positive.")
    }
    if (length(Figure_1_Treatment_Title_Text_Size) != 1) {
      stop ("The 'Figure_1_Treatment_Title_Text_Size' argument must be of length 1.")
    }
    if (!is.numeric(Figure_1_Treatment_Title_Text_Size)) {
      stop ("The 'Figure_1_Treatment_Title_Text_Size' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_1_Treatment_Title_Text_Size)) {
      stop ("The 'Figure_1_Treatment_Title_Text_Size' argument must not be missing.")
    }
    if (Figure_1_Treatment_Title_Text_Size <= 0) {
      stop ("The 'Figure_1_Treatment_Title_Text_Size' argument must be positive.")
    }
    if (length(Figure_1_Replicate_Title_Text_Size) != 1) {
      stop ("The 'Figure_1_Replicate_Title_Text_Size' argument must be of length 1.")
    }
    if (!is.numeric(Figure_1_Replicate_Title_Text_Size)) {
      stop ("The 'Figure_1_Replicate_Title_Text_Size' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_1_Replicate_Title_Text_Size)) {
      stop ("The 'Figure_1_Replicate_Title_Text_Size' argument must not be missing.")
    }
    if (Figure_1_Replicate_Title_Text_Size <= 0) {
      stop ("The 'Figure_1_Replicate_Title_Text_Size' argument must be positive.")
    }
    if (length(Figure_1_Axis_Titles_Text_Size) != 1) {
      stop ("The 'Figure_1_Axis_Titles_Text_Size' argument must be of length 1.")
    }
    if (!is.numeric(Figure_1_Axis_Titles_Text_Size)) {
      stop ("The 'Figure_1_Axis_Titles_Text_Size' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_1_Axis_Titles_Text_Size)) {
      stop ("The 'Figure_1_Axis_Titles_Text_Size' argument must not be missing.")
    }
    if (Figure_1_Axis_Titles_Text_Size <= 0) {
      stop ("The 'Figure_1_Axis_Titles_Text_Size' argument must be positive.")
    }
    if (length(Figure_1_Axis_Labels_Text_Size) != 1) {
      stop ("The 'Figure_1_Axis_Labels_Text_Size' argument must be of length 1.")
    }
    if (!is.numeric(Figure_1_Axis_Labels_Text_Size)) {
      stop ("The 'Figure_1_Axis_Labels_Text_Size' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_1_Axis_Labels_Text_Size)) {
      stop ("The 'Figure_1_Axis_Labels_Text_Size' argument must not be missing.")
    }
    if (Figure_1_Axis_Labels_Text_Size <= 0) {
      stop ("The 'Figure_1_Axis_Labels_Text_Size' argument must be positive.")
    }
    if (length(Figure_1_Individual_Plot_Bottom_Margin) != 1) {
      stop ("The 'Figure_1_Individual_Plot_Bottom_Margin' argument must be of length 1.")
    }
    if (!is.numeric(Figure_1_Individual_Plot_Bottom_Margin)) {
      stop ("The 'Figure_1_Individual_Plot_Bottom_Margin' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_1_Individual_Plot_Bottom_Margin)) {
      stop ("The 'Figure_1_Individual_Plot_Bottom_Margin' argument must not be missing.")
    }
    if (Figure_1_Individual_Plot_Bottom_Margin < 0) {
      stop ("The 'Figure_1_Individual_Plot_Bottom_Margin' argument must not be negative.")
    }
    if (length(Figure_1_Individual_Plot_Left_Margin) != 1) {
      stop ("The 'Figure_1_Individual_Plot_Left_Margin' argument must be of length 1.")
    }
    if (!is.numeric(Figure_1_Individual_Plot_Left_Margin)) {
      stop ("The 'Figure_1_Individual_Plot_Left_Margin' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_1_Individual_Plot_Left_Margin)) {
      stop ("The 'Figure_1_Individual_Plot_Left_Margin' argument must not be missing.")
    }
    if (Figure_1_Individual_Plot_Left_Margin < 0) {
      stop ("The 'Figure_1_Individual_Plot_Left_Margin' argument must not be negative.")
    }
    if (length(Figure_1_Individual_Plot_Top_Margin) != 1) {
      stop ("The 'Figure_1_Individual_Plot_Top_Margin' argument must be of length 1.")
    }
    if (!is.numeric(Figure_1_Individual_Plot_Top_Margin)) {
      stop ("The 'Figure_1_Individual_Plot_Top_Margin' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_1_Individual_Plot_Top_Margin)) {
      stop ("The 'Figure_1_Individual_Plot_Top_Margin' argument must not be missing.")
    }
    if (Figure_1_Individual_Plot_Top_Margin < 0) {
      stop ("The 'Figure_1_Individual_Plot_Top_Margin' argument must not be negative.")
    }
    if (length(Figure_1_Individual_Plot_Right_Margin) != 1) {
      stop ("The 'Figure_1_Individual_Plot_Right_Margin' argument must be of length 1.")
    }
    if (!is.numeric(Figure_1_Individual_Plot_Right_Margin)) {
      stop ("The 'Figure_1_Individual_Plot_Right_Margin' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_1_Individual_Plot_Right_Margin)) {
      stop ("The 'Figure_1_Individual_Plot_Right_Margin' argument must not be missing.")
    }
    if (Figure_1_Individual_Plot_Right_Margin < 0) {
      stop ("The 'Figure_1_Individual_Plot_Right_Margin' argument must not be negative.")
    }
    if (length(Figure_1_Legend_Text_Size) != 1) {
      stop ("The 'Figure_1_Legend_Text_Size' argument must be of length 1.")
    }
    if (!is.numeric(Figure_1_Legend_Text_Size)) {
      stop ("The 'Figure_1_Legend_Text_Size' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_1_Legend_Text_Size)) {
      stop ("The 'Figure_1_Legend_Text_Size' argument must not be missing.")
    }
    if (Figure_1_Legend_Text_Size <= 0) {
      stop ("The 'Figure_1_Legend_Text_Size' argument must be positive.")
    }
    if (length(Figure_1_Panel_Separation_Line_Width) != 1) {
      stop ("The 'Figure_1_Panel_Separation_Line_Width' argument must be of length 1.")
    }
    if (!is.numeric(Figure_1_Panel_Separation_Line_Width)) {
      stop ("The 'Figure_1_Panel_Separation_Line_Width' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_1_Panel_Separation_Line_Width)) {
      stop ("The 'Figure_1_Panel_Separation_Line_Width' argument must not be missing.")
    }
    if (Figure_1_Panel_Separation_Line_Width < 0) {
      stop ("The 'Figure_1_Panel_Separation_Line_Width' argument must not be negative.")
    }
  }
  if (Make_Figure_2 == T) {
    if (length(Show_Overlapping_Areas_in_Figure_2) != 1) {
      stop ("The 'Show_Overlapping_Areas_in_Figure_2' argument must be of length 1.")
    }
    if (!is.logical(Show_Overlapping_Areas_in_Figure_2)) {
      stop ("The 'Show_Overlapping_Areas_in_Figure_2' argument must be of class 'logical'.")
    }
    if (is.na(Show_Overlapping_Areas_in_Figure_2)) {
      stop ("The 'Show_Overlapping_Areas_in_Figure_2' argument must not be missing.")
    }
    if (length(Second_Figure_Name) != 1) {
      stop ("The 'Second_Figure_Name' argument must be of length 1.")
    }
    if (!is.character(Second_Figure_Name)) {
      stop ("The 'Second_Figure_Name' argument must be of class 'character'.")
    }
    if (is.na(Second_Figure_Name)) {
      stop ("The 'Second_Figure_Name' argument must not be missing.")
    }
    if (Show_Overlapping_Areas_in_Figure_2 == T) {
      if (length(Overlapping_Area_Rounding_Constant) != 1) {
        stop ("The 'Overlapping_Area_Rounding_Constant' argument must be of length 1.")
      }
      if (!is.numeric(Overlapping_Area_Rounding_Constant)) {
        stop ("The 'Overlapping_Area_Rounding_Constant' argument must be of class 'numeric'.")
      }
      if (is.na(Overlapping_Area_Rounding_Constant)) {
        stop ("The 'Overlapping_Area_Rounding_Constant' argument must not be missing.")
      }
      if (Overlapping_Area_Rounding_Constant < 0) {
        stop ("The 'Overlapping_Area_Rounding_Constant' argument must not be negative.")
      }
      if ((Overlapping_Area_Rounding_Constant %% 1) != 0) {
        stop ("The 'Overlapping_Area_Rounding_Constant' argument must not be negative.")
      }
      if (length(Figure_2_Overlapping_Area_Text_Size) != 1) {
        stop ("The 'Figure_2_Overlapping_Area_Text_Size' argument must be of length 1.")
      }
      if (!is.numeric(Figure_2_Overlapping_Area_Text_Size)) {
        stop ("The 'Figure_2_Overlapping_Area_Text_Size' argument must be of class 'numeric'.")
      }
      if (is.na(Figure_2_Overlapping_Area_Text_Size)) {
        stop ("The 'Figure_2_Overlapping_Area_Text_Size' argument must not be missing.")
      }
      if (Figure_2_Overlapping_Area_Text_Size <= 0) {
        stop ("The 'Figure_2_Overlapping_Area_Text_Size' argument must be positive.")
      }
    }
    if (length(Relative_Figure_2_Parameter_Plot_Height) != 1) {
      stop ("The 'Relative_Figure_2_Parameter_Plot_Height' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_2_Parameter_Plot_Height)) {
      stop ("The 'Relative_Figure_2_Parameter_Plot_Height' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_2_Parameter_Plot_Height)) {
      stop ("The 'Relative_Figure_2_Parameter_Plot_Height' argument must not be missing.")
    }
    if (Relative_Figure_2_Parameter_Plot_Height <= 0) {
      stop ("The 'Relative_Figure_2_Parameter_Plot_Height' argument must be positive.")
    }
    if (length(Relative_Figure_2_Treatment_Plot_Height) != 1) {
      stop ("The 'Relative_Figure_2_Treatment_Plot_Height' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_2_Treatment_Plot_Height)) {
      stop ("The 'Relative_Figure_2_Treatment_Plot_Height' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_2_Treatment_Plot_Height)) {
      stop ("The 'Relative_Figure_2_Treatment_Plot_Height' argument must not be missing.")
    }
    if (Relative_Figure_2_Treatment_Plot_Height <= 0) {
      stop ("The 'Relative_Figure_2_Treatment_Plot_Height' argument must be positive.")
    }
    if (length(Relative_Figure_2_Replicate_Plot_Height) != 1) {
      stop ("The 'Relative_Figure_2_Replicate_Plot_Height' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_2_Replicate_Plot_Height)) {
      stop ("The 'Relative_Figure_2_Replicate_Plot_Height' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_2_Replicate_Plot_Height)) {
      stop ("The 'Relative_Figure_2_Replicate_Plot_Height' argument must not be missing.")
    }
    if (Relative_Figure_2_Replicate_Plot_Height <= 0) {
      stop ("The 'Relative_Figure_2_Replicate_Plot_Height' argument must be positive.")
    }
    if (length(Relative_Figure_2_Individual_Plot_Dimension) != 1) {
      stop ("The 'Relative_Figure_2_Individual_Plot_Dimension' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_2_Individual_Plot_Dimension)) {
      stop ("The 'Relative_Figure_2_Individual_Plot_Dimension' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_2_Individual_Plot_Dimension)) {
      stop ("The 'Relative_Figure_2_Individual_Plot_Dimension' argument must not be missing.")
    }
    if (Relative_Figure_2_Individual_Plot_Dimension <= 0) {
      stop ("The 'Relative_Figure_2_Individual_Plot_Dimension' argument must be positive.")
    }
    if (length(Relative_Figure_2_Gap_Plot_Dimension) != 1) {
      stop ("The 'Relative_Figure_2_Gap_Plot_Dimension' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_2_Gap_Plot_Dimension)) {
      stop ("The 'Relative_Figure_2_Gap_Plot_Dimension' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_2_Gap_Plot_Dimension)) {
      stop ("The 'Relative_Figure_2_Gap_Plot_Dimension' argument must not be missing.")
    }
    if (Relative_Figure_2_Gap_Plot_Dimension <= 0) {
      stop ("The 'Relative_Figure_2_Gap_Plot_Dimension' argument must be positive.")
    }
    if (length(Relative_Figure_2_Legend_Plot_Height) != 1) {
      stop ("The 'Relative_Figure_2_Legend_Plot_Height' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_2_Legend_Plot_Height)) {
      stop ("The 'Relative_Figure_2_Legend_Plot_Height' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_2_Legend_Plot_Height)) {
      stop ("The 'Relative_Figure_2_Legend_Plot_Height' argument must not be missing.")
    }
    if (Relative_Figure_2_Legend_Plot_Height <= 0) {
      stop ("The 'Relative_Figure_2_Legend_Plot_Height' argument must be positive.")
    }
    if (length(Relative_Figure_2_Parameter_Spacing_Line_Plot_Height) != 1) {
      stop ("The 'Relative_Figure_2_Parameter_Spacing_Line_Plot_Height' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_2_Parameter_Spacing_Line_Plot_Height)) {
      stop ("The 'Relative_Figure_2_Parameter_Spacing_Line_Plot_Height' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_2_Parameter_Spacing_Line_Plot_Height)) {
      stop ("The 'Relative_Figure_2_Parameter_Spacing_Line_Plot_Height' argument must not be missing.")
    }
    if (Relative_Figure_2_Parameter_Spacing_Line_Plot_Height <= 0) {
      stop ("The 'Relative_Figure_2_Parameter_Spacing_Line_Plot_Height' argument must be positive.")
    }
    if (length(Figure_2_Figure_Height_Constant) != 1) {
      stop ("The 'Figure_2_Figure_Height_Constant' argument must be of length 1.")
    }
    if (!is.numeric(Figure_2_Figure_Height_Constant)) {
      stop ("The 'Figure_2_Figure_Height_Constant' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_2_Figure_Height_Constant)) {
      stop ("The 'Figure_2_Figure_Height_Constant' argument must not be missing.")
    }
    if (Figure_2_Figure_Height_Constant <= 0) {
      stop ("The 'Figure_2_Figure_Height_Constant' argument must be positive.")
    }
    if (length(Figure_2_Figure_Width_Constant) != 1) {
      stop ("The 'Figure_2_Figure_Width_Constant' argument must be of length 1.")
    }
    if (!is.numeric(Figure_2_Figure_Width_Constant)) {
      stop ("The 'Figure_2_Figure_Width_Constant' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_2_Figure_Width_Constant)) {
      stop ("The 'Figure_2_Figure_Width_Constant' argument must not be missing.")
    }
    if (Figure_2_Figure_Width_Constant <= 0) {
      stop ("The 'Figure_2_Figure_Width_Constant' argument must be positive.")
    }
    if (length(Figure_2_Axis_Labels_and_Axis_Titles_Shifting_Constant) != 1) {
      stop ("The 'Figure_2_Axis_Labels_and_Axis_Titles_Shifting_Constant' argument must be of length 1.")
    }
    if (!is.numeric(Figure_2_Axis_Labels_and_Axis_Titles_Shifting_Constant)) {
      stop ("The 'Figure_2_Axis_Labels_and_Axis_Titles_Shifting_Constant' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_2_Axis_Labels_and_Axis_Titles_Shifting_Constant)) {
      stop ("The 'Figure_2_Axis_Labels_and_Axis_Titles_Shifting_Constant' argument must not be missing.")
    }
    if (Figure_2_Axis_Labels_and_Axis_Titles_Shifting_Constant < 0) {
      warning ("The 'Figure_2_Axis_Labels_and_Axis_Titles_Shifting_Constant' argument should not be negative.")
    }
    if (length(Figure_2_Parameter_Title_Text_Size) != 1) {
      stop ("The 'Figure_2_Parameter_Title_Text_Size' argument must be of length 1.")
    }
    if (!is.numeric(Figure_2_Parameter_Title_Text_Size)) {
      stop ("The 'Figure_2_Parameter_Title_Text_Size' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_2_Parameter_Title_Text_Size)) {
      stop ("The 'Figure_2_Parameter_Title_Text_Size' argument must not be missing.")
    }
    if (Figure_2_Parameter_Title_Text_Size <= 0) {
      stop ("The 'Figure_2_Parameter_Title_Text_Size' argument must be positive.")
    }
    if (length(Figure_2_Treatment_Title_Text_Size) != 1) {
      stop ("The 'Figure_2_Treatment_Title_Text_Size' argument must be of length 1.")
    }
    if (!is.numeric(Figure_2_Treatment_Title_Text_Size)) {
      stop ("The 'Figure_2_Treatment_Title_Text_Size' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_2_Treatment_Title_Text_Size)) {
      stop ("The 'Figure_2_Treatment_Title_Text_Size' argument must not be missing.")
    }
    if (Figure_2_Treatment_Title_Text_Size <= 0) {
      stop ("The 'Figure_2_Treatment_Title_Text_Size' argument must be positive.")
    }
    if (length(Figure_2_Replicate_Title_Text_Size) != 1) {
      stop ("The 'Figure_2_Replicate_Title_Text_Size' argument must be of length 1.")
    }
    if (!is.numeric(Figure_2_Replicate_Title_Text_Size)) {
      stop ("The 'Figure_2_Replicate_Title_Text_Size' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_2_Replicate_Title_Text_Size)) {
      stop ("The 'Figure_2_Replicate_Title_Text_Size' argument must not be missing.")
    }
    if (Figure_2_Replicate_Title_Text_Size <= 0) {
      stop ("The 'Figure_2_Replicate_Title_Text_Size' argument must be positive.")
    }
    if (length(Figure_2_Axis_Titles_Text_Size) != 1) {
      stop ("The 'Figure_2_Axis_Titles_Text_Size' argument must be of length 1.")
    }
    if (!is.numeric(Figure_2_Axis_Titles_Text_Size)) {
      stop ("The 'Figure_2_Axis_Titles_Text_Size' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_2_Axis_Titles_Text_Size)) {
      stop ("The 'Figure_2_Axis_Titles_Text_Size' argument must not be missing.")
    }
    if (Figure_2_Axis_Titles_Text_Size <= 0) {
      stop ("The 'Figure_2_Axis_Titles_Text_Size' argument must be positive.")
    }
    if (length(Figure_2_Axis_Labels_Text_Size) != 1) {
      stop ("The 'Figure_2_Axis_Labels_Text_Size' argument must be of length 1.")
    }
    if (!is.numeric(Figure_2_Axis_Labels_Text_Size)) {
      stop ("The 'Figure_2_Axis_Labels_Text_Size' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_2_Axis_Labels_Text_Size)) {
      stop ("The 'Figure_2_Axis_Labels_Text_Size' argument must not be missing.")
    }
    if (Figure_2_Axis_Labels_Text_Size <= 0) {
      stop ("The 'Figure_2_Axis_Labels_Text_Size' argument must be positive.")
    }
    if (length(Figure_2_Individual_Plot_Bottom_Margin) != 1) {
      stop ("The 'Figure_2_Individual_Plot_Bottom_Margin' argument must be of length 1.")
    }
    if (!is.numeric(Figure_2_Individual_Plot_Bottom_Margin)) {
      stop ("The 'Figure_2_Individual_Plot_Bottom_Margin' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_2_Individual_Plot_Bottom_Margin)) {
      stop ("The 'Figure_2_Individual_Plot_Bottom_Margin' argument must not be missing.")
    }
    if (Figure_2_Individual_Plot_Bottom_Margin < 0) {
      stop ("The 'Figure_2_Individual_Plot_Bottom_Margin' argument must not be negative.")
    }
    if (length(Figure_2_Individual_Plot_Left_Margin) != 1) {
      stop ("The 'Figure_2_Individual_Plot_Left_Margin' argument must be of length 1.")
    }
    if (!is.numeric(Figure_2_Individual_Plot_Left_Margin)) {
      stop ("The 'Figure_2_Individual_Plot_Left_Margin' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_2_Individual_Plot_Left_Margin)) {
      stop ("The 'Figure_2_Individual_Plot_Left_Margin' argument must not be missing.")
    }
    if (Figure_2_Individual_Plot_Left_Margin < 0) {
      stop ("The 'Figure_2_Individual_Plot_Left_Margin' argument must not be negative.")
    }
    if (length(Figure_2_Individual_Plot_Top_Margin) != 1) {
      stop ("The 'Figure_2_Individual_Plot_Top_Margin' argument must be of length 1.")
    }
    if (!is.numeric(Figure_2_Individual_Plot_Top_Margin)) {
      stop ("The 'Figure_2_Individual_Plot_Top_Margin' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_2_Individual_Plot_Top_Margin)) {
      stop ("The 'Figure_2_Individual_Plot_Top_Margin' argument must not be missing.")
    }
    if (Figure_2_Individual_Plot_Top_Margin < 0) {
      stop ("The 'Figure_2_Individual_Plot_Top_Margin' argument must not be negative.")
    }
    if (length(Figure_2_Individual_Plot_Right_Margin) != 1) {
      stop ("The 'Figure_2_Individual_Plot_Right_Margin' argument must be of length 1.")
    }
    if (!is.numeric(Figure_2_Individual_Plot_Right_Margin)) {
      stop ("The 'Figure_2_Individual_Plot_Right_Margin' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_2_Individual_Plot_Right_Margin)) {
      stop ("The 'Figure_2_Individual_Plot_Right_Margin' argument must not be missing.")
    }
    if (Figure_2_Individual_Plot_Right_Margin < 0) {
      stop ("The 'Figure_2_Individual_Plot_Right_Margin' argument must not be negative.")
    }
    if (length(Figure_2_Legend_Text_Size) != 1) {
      stop ("The 'Figure_2_Legend_Text_Size' argument must be of length 1.")
    }
    if (!is.numeric(Figure_2_Legend_Text_Size)) {
      stop ("The 'Figure_2_Legend_Text_Size' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_2_Legend_Text_Size)) {
      stop ("The 'Figure_2_Legend_Text_Size' argument must not be missing.")
    }
    if (Figure_2_Legend_Text_Size <= 0) {
      stop ("The 'Figure_2_Legend_Text_Size' argument must be positive.")
    }
    if (length(Figure_2_Panel_Separation_Line_Width) != 1) {
      stop ("The 'Figure_2_Panel_Separation_Line_Width' argument must be of length 1.")
    }
    if (!is.numeric(Figure_2_Panel_Separation_Line_Width)) {
      stop ("The 'Figure_2_Panel_Separation_Line_Width' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_2_Panel_Separation_Line_Width)) {
      stop ("The 'Figure_2_Panel_Separation_Line_Width' argument must not be missing.")
    }
    if (Figure_2_Panel_Separation_Line_Width < 0) {
      stop ("The 'Figure_2_Panel_Separation_Line_Width' argument must not be negative.")
    }
  }
  if (Make_Figure_3 == T) {
    if (length(Show_Moments_in_Figure_3) != 1) {
      stop ("The 'Show_Moments_in_Figure_3' argument must be of length 1.")
    }
    if (!is.logical(Show_Moments_in_Figure_3)) {
      stop ("The 'Show_Moments_in_Figure_3' argument must be of class 'logical'.")
    }
    if (is.na(Show_Moments_in_Figure_3)) {
      stop ("The 'Show_Moments_in_Figure_3' argument must not be missing.")
    }
    if (length(Third_Figure_Name) != 1) {
      stop ("The 'Third_Figure_Name' argument must be of length 1.")
    }
    if (!is.character(Third_Figure_Name)) {
      stop ("The 'Third_Figure_Name' argument must be of class 'character'.")
    }
    if (is.na(Third_Figure_Name)) {
      stop ("The 'Third_Figure_Name' argument must not be missing.")
    }
    if (Show_Moments_in_Figure_3 == T) {
      if (length(Moment_Rounding_Constant) != 1) {
        stop ("The 'Moment_Rounding_Constant' argument must be of length 1.")
      }
      if (!is.numeric(Moment_Rounding_Constant)) {
        stop ("The 'Moment_Rounding_Constant' argument must be of class 'numeric'.")
      }
      if (is.na(Moment_Rounding_Constant)) {
        stop ("The 'Moment_Rounding_Constant' argument must not be missing.")
      }
      if (Moment_Rounding_Constant < 0) {
        stop ("The 'Moment_Rounding_Constant' argument must not be negative.")
      }
      if ((Moment_Rounding_Constant %% 1) != 0) {
        stop ("The 'Moment_Rounding_Constant' argument must not be negative.")
      }
      if (length(Figure_3_Moment_Text_Size) != 1) {
        stop ("The 'Figure_3_Moment_Text_Size' argument must be of length 1.")
      }
      if (!is.numeric(Figure_3_Moment_Text_Size)) {
        stop ("The 'Figure_3_Moment_Text_Size' argument must be of class 'numeric'.")
      }
      if (is.na(Figure_3_Moment_Text_Size)) {
        stop ("The 'Figure_3_Moment_Text_Size' argument must not be missing.")
      }
      if (Figure_3_Moment_Text_Size <= 0) {
        stop ("The 'Figure_3_Moment_Text_Size' argument must be positive.")
      }
    }
    if (length(Relative_Figure_3_Parameter_Plot_Height) != 1) {
      stop ("The 'Relative_Figure_3_Parameter_Plot_Height' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_3_Parameter_Plot_Height)) {
      stop ("The 'Relative_Figure_3_Parameter_Plot_Height' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_3_Parameter_Plot_Height)) {
      stop ("The 'Relative_Figure_3_Parameter_Plot_Height' argument must not be missing.")
    }
    if (Relative_Figure_3_Parameter_Plot_Height <= 0) {
      stop ("The 'Relative_Figure_3_Parameter_Plot_Height' argument must be positive.")
    }
    if (length(Relative_Figure_3_Treatment_Plot_Dimension) != 1) {
      stop ("The 'Relative_Figure_3_Treatment_Plot_Dimension' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_3_Treatment_Plot_Dimension)) {
      stop ("The 'Relative_Figure_3_Treatment_Plot_Dimension' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_3_Treatment_Plot_Dimension)) {
      stop ("The 'Relative_Figure_3_Treatment_Plot_Dimension' argument must not be missing.")
    }
    if (Relative_Figure_3_Treatment_Plot_Dimension <= 0) {
      stop ("The 'Relative_Figure_3_Treatment_Plot_Dimension' argument must be positive.")
    }
    if (length(Relative_Figure_3_Replicate_Plot_Dimension) != 1) {
      stop ("The 'Relative_Figure_3_Replicate_Plot_Dimension' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_3_Replicate_Plot_Dimension)) {
      stop ("The 'Relative_Figure_3_Replicate_Plot_Dimension' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_3_Replicate_Plot_Dimension)) {
      stop ("The 'Relative_Figure_3_Replicate_Plot_Dimension' argument must not be missing.")
    }
    if (Relative_Figure_3_Replicate_Plot_Dimension <= 0) {
      stop ("The 'Relative_Figure_3_Replicate_Plot_Dimension' argument must be positive.")
    }
    if (length(Relative_Figure_3_Individual_Plot_Dimension) != 1) {
      stop ("The 'Relative_Figure_3_Individual_Plot_Dimension' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_3_Individual_Plot_Dimension)) {
      stop ("The 'Relative_Figure_3_Individual_Plot_Dimension' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_3_Individual_Plot_Dimension)) {
      stop ("The 'Relative_Figure_3_Individual_Plot_Dimension' argument must not be missing.")
    }
    if (Relative_Figure_3_Individual_Plot_Dimension <= 0) {
      stop ("The 'Relative_Figure_3_Individual_Plot_Dimension' argument must be positive.")
    }
    if (length(Relative_Figure_3_Gap_Plot_Dimension) != 1) {
      stop ("The 'Relative_Figure_3_Gap_Plot_Dimension' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_3_Gap_Plot_Dimension)) {
      stop ("The 'Relative_Figure_3_Gap_Plot_Dimension' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_3_Gap_Plot_Dimension)) {
      stop ("The 'Relative_Figure_3_Gap_Plot_Dimension' argument must not be missing.")
    }
    if (Relative_Figure_3_Gap_Plot_Dimension <= 0) {
      stop ("The 'Relative_Figure_3_Gap_Plot_Dimension' argument must be positive.")
    }
    if (length(Relative_Figure_3_Legend_Plot_Height) != 1) {
      stop ("The 'Relative_Figure_3_Legend_Plot_Height' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_3_Legend_Plot_Height)) {
      stop ("The 'Relative_Figure_3_Legend_Plot_Height' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_3_Legend_Plot_Height)) {
      stop ("The 'Relative_Figure_3_Legend_Plot_Height' argument must not be missing.")
    }
    if (Relative_Figure_3_Legend_Plot_Height <= 0) {
      stop ("The 'Relative_Figure_3_Legend_Plot_Height' argument must be positive.")
    }
    if (length(Relative_Figure_3_Parameter_Spacing_Line_Plot_Height) != 1) {
      stop ("The 'Relative_Figure_3_Parameter_Spacing_Line_Plot_Height' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_3_Parameter_Spacing_Line_Plot_Height)) {
      stop ("The 'Relative_Figure_3_Parameter_Spacing_Line_Plot_Height' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_3_Parameter_Spacing_Line_Plot_Height)) {
      stop ("The 'Relative_Figure_3_Parameter_Spacing_Line_Plot_Height' argument must not be missing.")
    }
    if (Relative_Figure_3_Parameter_Spacing_Line_Plot_Height <= 0) {
      stop ("The 'Relative_Figure_3_Parameter_Spacing_Line_Plot_Height' argument must be positive.")
    }
    if (length(Figure_3_Figure_Height_Constant) != 1) {
      stop ("The 'Figure_3_Figure_Height_Constant' argument must be of length 1.")
    }
    if (!is.numeric(Figure_3_Figure_Height_Constant)) {
      stop ("The 'Figure_3_Figure_Height_Constant' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_3_Figure_Height_Constant)) {
      stop ("The 'Figure_3_Figure_Height_Constant' argument must not be missing.")
    }
    if (Figure_3_Figure_Height_Constant <= 0) {
      stop ("The 'Figure_3_Figure_Height_Constant' argument must be positive.")
    }
    if (length(Figure_3_Figure_Width_Constant) != 1) {
      stop ("The 'Figure_3_Figure_Width_Constant' argument must be of length 1.")
    }
    if (!is.numeric(Figure_3_Figure_Width_Constant)) {
      stop ("The 'Figure_3_Figure_Width_Constant' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_3_Figure_Width_Constant)) {
      stop ("The 'Figure_3_Figure_Width_Constant' argument must not be missing.")
    }
    if (Figure_3_Figure_Width_Constant <= 0) {
      stop ("The 'Figure_3_Figure_Width_Constant' argument must be positive.")
    }
    if (length(Figure_3_Axis_Labels_and_Axis_Titles_Shifting_Constant) != 1) {
      stop ("The 'Figure_3_Axis_Labels_and_Axis_Titles_Shifting_Constant' argument must be of length 1.")
    }
    if (!is.numeric(Figure_3_Axis_Labels_and_Axis_Titles_Shifting_Constant)) {
      stop ("The 'Figure_3_Axis_Labels_and_Axis_Titles_Shifting_Constant' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_3_Axis_Labels_and_Axis_Titles_Shifting_Constant)) {
      stop ("The 'Figure_3_Axis_Labels_and_Axis_Titles_Shifting_Constant' argument must not be missing.")
    }
    if (Figure_3_Axis_Labels_and_Axis_Titles_Shifting_Constant < 0) {
      warning ("The 'Figure_3_Axis_Labels_and_Axis_Titles_Shifting_Constant' argument should not be negative.")
    }
    if (length(Figure_3_Parameter_Title_Text_Size) != 1) {
      stop ("The 'Figure_3_Parameter_Title_Text_Size' argument must be of length 1.")
    }
    if (!is.numeric(Figure_3_Parameter_Title_Text_Size)) {
      stop ("The 'Figure_3_Parameter_Title_Text_Size' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_3_Parameter_Title_Text_Size)) {
      stop ("The 'Figure_3_Parameter_Title_Text_Size' argument must not be missing.")
    }
    if (Figure_3_Parameter_Title_Text_Size <= 0) {
      stop ("The 'Figure_3_Parameter_Title_Text_Size' argument must be positive.")
    }
    if (length(Figure_3_Treatment_Title_Text_Size) != 1) {
      stop ("The 'Figure_3_Treatment_Title_Text_Size' argument must be of length 1.")
    }
    if (!is.numeric(Figure_3_Treatment_Title_Text_Size)) {
      stop ("The 'Figure_3_Treatment_Title_Text_Size' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_3_Treatment_Title_Text_Size)) {
      stop ("The 'Figure_3_Treatment_Title_Text_Size' argument must not be missing.")
    }
    if (Figure_3_Treatment_Title_Text_Size <= 0) {
      stop ("The 'Figure_3_Treatment_Title_Text_Size' argument must be positive.")
    }
    if (length(Figure_3_Replicate_Title_Text_Size) != 1) {
      stop ("The 'Figure_3_Replicate_Title_Text_Size' argument must be of length 1.")
    }
    if (!is.numeric(Figure_3_Replicate_Title_Text_Size)) {
      stop ("The 'Figure_3_Replicate_Title_Text_Size' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_3_Replicate_Title_Text_Size)) {
      stop ("The 'Figure_3_Replicate_Title_Text_Size' argument must not be missing.")
    }
    if (Figure_3_Replicate_Title_Text_Size <= 0) {
      stop ("The 'Figure_3_Replicate_Title_Text_Size' argument must be positive.")
    }
    if (length(Figure_3_Axis_Titles_Text_Size) != 1) {
      stop ("The 'Figure_3_Axis_Titles_Text_Size' argument must be of length 1.")
    }
    if (!is.numeric(Figure_3_Axis_Titles_Text_Size)) {
      stop ("The 'Figure_3_Axis_Titles_Text_Size' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_3_Axis_Titles_Text_Size)) {
      stop ("The 'Figure_3_Axis_Titles_Text_Size' argument must not be missing.")
    }
    if (Figure_3_Axis_Titles_Text_Size <= 0) {
      stop ("The 'Figure_3_Axis_Titles_Text_Size' argument must be positive.")
    }
    if (length(Figure_3_Axis_Labels_Text_Size) != 1) {
      stop ("The 'Figure_3_Axis_Labels_Text_Size' argument must be of length 1.")
    }
    if (!is.numeric(Figure_3_Axis_Labels_Text_Size)) {
      stop ("The 'Figure_3_Axis_Labels_Text_Size' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_3_Axis_Labels_Text_Size)) {
      stop ("The 'Figure_3_Axis_Labels_Text_Size' argument must not be missing.")
    }
    if (Figure_3_Axis_Labels_Text_Size <= 0) {
      stop ("The 'Figure_3_Axis_Labels_Text_Size' argument must be positive.")
    }
    if (length(Figure_3_Individual_Plot_Bottom_Margin) != 1) {
      stop ("The 'Figure_3_Individual_Plot_Bottom_Margin' argument must be of length 1.")
    }
    if (!is.numeric(Figure_3_Individual_Plot_Bottom_Margin)) {
      stop ("The 'Figure_3_Individual_Plot_Bottom_Margin' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_3_Individual_Plot_Bottom_Margin)) {
      stop ("The 'Figure_3_Individual_Plot_Bottom_Margin' argument must not be missing.")
    }
    if (Figure_3_Individual_Plot_Bottom_Margin < 0) {
      stop ("The 'Figure_3_Individual_Plot_Bottom_Margin' argument must not be negative.")
    }
    if (length(Figure_3_Individual_Plot_Left_Margin) != 1) {
      stop ("The 'Figure_3_Individual_Plot_Left_Margin' argument must be of length 1.")
    }
    if (!is.numeric(Figure_3_Individual_Plot_Left_Margin)) {
      stop ("The 'Figure_3_Individual_Plot_Left_Margin' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_3_Individual_Plot_Left_Margin)) {
      stop ("The 'Figure_3_Individual_Plot_Left_Margin' argument must not be missing.")
    }
    if (Figure_3_Individual_Plot_Left_Margin < 0) {
      stop ("The 'Figure_3_Individual_Plot_Left_Margin' argument must not be negative.")
    }
    if (length(Figure_3_Individual_Plot_Top_Margin) != 1) {
      stop ("The 'Figure_3_Individual_Plot_Top_Margin' argument must be of length 1.")
    }
    if (!is.numeric(Figure_3_Individual_Plot_Top_Margin)) {
      stop ("The 'Figure_3_Individual_Plot_Top_Margin' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_3_Individual_Plot_Top_Margin)) {
      stop ("The 'Figure_3_Individual_Plot_Top_Margin' argument must not be missing.")
    }
    if (Figure_3_Individual_Plot_Top_Margin < 0) {
      stop ("The 'Figure_3_Individual_Plot_Top_Margin' argument must not be negative.")
    }
    if (length(Figure_3_Individual_Plot_Right_Margin) != 1) {
      stop ("The 'Figure_3_Individual_Plot_Right_Margin' argument must be of length 1.")
    }
    if (!is.numeric(Figure_3_Individual_Plot_Right_Margin)) {
      stop ("The 'Figure_3_Individual_Plot_Right_Margin' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_3_Individual_Plot_Right_Margin)) {
      stop ("The 'Figure_3_Individual_Plot_Right_Margin' argument must not be missing.")
    }
    if (Figure_3_Individual_Plot_Right_Margin < 0) {
      stop ("The 'Figure_3_Individual_Plot_Right_Margin' argument must not be negative.")
    }
    if (length(Figure_3_Legend_Text_Size) != 1) {
      stop ("The 'Figure_3_Legend_Text_Size' argument must be of length 1.")
    }
    if (!is.numeric(Figure_3_Legend_Text_Size)) {
      stop ("The 'Figure_3_Legend_Text_Size' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_3_Legend_Text_Size)) {
      stop ("The 'Figure_3_Legend_Text_Size' argument must not be missing.")
    }
    if (Figure_3_Legend_Text_Size <= 0) {
      stop ("The 'Figure_3_Legend_Text_Size' argument must be positive.")
    }
    if (length(Figure_3_Panel_Separation_Line_Width) != 1) {
      stop ("The 'Figure_3_Panel_Separation_Line_Width' argument must be of length 1.")
    }
    if (!is.numeric(Figure_3_Panel_Separation_Line_Width)) {
      stop ("The 'Figure_3_Panel_Separation_Line_Width' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_3_Panel_Separation_Line_Width)) {
      stop ("The 'Figure_3_Panel_Separation_Line_Width' argument must not be missing.")
    }
    if (Figure_3_Panel_Separation_Line_Width < 0) {
      stop ("The 'Figure_3_Panel_Separation_Line_Width' argument must not be negative.")
    }
  }
  if (Make_Figure_4 == T) {
    if (length(Show_Moments_in_Figure_4) != 1) {
      stop ("The 'Show_Moments_in_Figure_4' argument must be of length 1.")
    }
    if (!is.logical(Show_Moments_in_Figure_4)) {
      stop ("The 'Show_Moments_in_Figure_4' argument must be of class 'logical'.")
    }
    if (is.na(Show_Moments_in_Figure_4)) {
      stop ("The 'Show_Moments_in_Figure_4' argument must not be missing.")
    }
    if (length(Fourth_Figure_Name) != 1) {
      stop ("The 'Fourth_Figure_Name' argument must be of length 1.")
    }
    if (!is.character(Fourth_Figure_Name)) {
      stop ("The 'Fourth_Figure_Name' argument must be of class 'character'.")
    }
    if (is.na(Fourth_Figure_Name)) {
      stop ("The 'Fourth_Figure_Name' argument must not be missing.")
    }
    if (Show_Moments_in_Figure_4 == T) {
      if (length(Moment_Rounding_Constant) != 1) {
        stop ("The 'Moment_Rounding_Constant' argument must be of length 1.")
      }
      if (!is.numeric(Moment_Rounding_Constant)) {
        stop ("The 'Moment_Rounding_Constant' argument must be of class 'numeric'.")
      }
      if (is.na(Moment_Rounding_Constant)) {
        stop ("The 'Moment_Rounding_Constant' argument must not be missing.")
      }
      if (Moment_Rounding_Constant < 0) {
        stop ("The 'Moment_Rounding_Constant' argument must not be negative.")
      }
      if ((Moment_Rounding_Constant %% 1) != 0) {
        stop ("The 'Moment_Rounding_Constant' argument must not be negative.")
      }
      if (length(Figure_4_Moment_Text_Size) != 1) {
        stop ("The 'Figure_4_Moment_Text_Size' argument must be of length 1.")
      }
      if (!is.numeric(Figure_4_Moment_Text_Size)) {
        stop ("The 'Figure_4_Moment_Text_Size' argument must be of class 'numeric'.")
      }
      if (is.na(Figure_4_Moment_Text_Size)) {
        stop ("The 'Figure_4_Moment_Text_Size' argument must not be missing.")
      }
      if (Figure_4_Moment_Text_Size <= 0) {
        stop ("The 'Figure_4_Moment_Text_Size' argument must be positive.")
      }
    }
    if (length(Number_of_Bins_in_the_Figure_4_Histograms) != 1) {
      stop ("The 'Number_of_Bins_in_the_Figure_4_Histograms' argument must be of length 1.")
    }
    if (is.na(Number_of_Bins_in_the_Figure_4_Histograms)) {
      stop ("The 'Number_of_Bins_in_the_Figure_4_Histograms' argument must not be missing.")
    }
    if (!is.numeric(Number_of_Bins_in_the_Figure_4_Histograms)) {
      stop ("The 'Number_of_Bins_in_the_Figure_4_Histograms' argument must be numeric.")
    }
    if ((Number_of_Bins_in_the_Figure_4_Histograms %% 1) != 0) {
      stop ("The 'Number_of_Bins_in_the_Figure_4_Histograms' argument must be a whole number.")
    }
    if (Number_of_Bins_in_the_Figure_4_Histograms <= 0) {
      stop ("The 'Number_of_Bins_in_the_Figure_4_Histograms' argument must be a positive number.")
    }
    if (length(Relative_Figure_4_Parameter_Plot_Height) != 1) {
      stop ("The 'Relative_Figure_4_Parameter_Plot_Height' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_4_Parameter_Plot_Height)) {
      stop ("The 'Relative_Figure_4_Parameter_Plot_Height' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_4_Parameter_Plot_Height)) {
      stop ("The 'Relative_Figure_4_Parameter_Plot_Height' argument must not be missing.")
    }
    if (Relative_Figure_4_Parameter_Plot_Height <= 0) {
      stop ("The 'Relative_Figure_4_Parameter_Plot_Height' argument must be positive.")
    }
    if (length(Relative_Figure_4_Treatment_Plot_Dimension) != 1) {
      stop ("The 'Relative_Figure_4_Treatment_Plot_Dimension' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_4_Treatment_Plot_Dimension)) {
      stop ("The 'Relative_Figure_4_Treatment_Plot_Dimension' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_4_Treatment_Plot_Dimension)) {
      stop ("The 'Relative_Figure_4_Treatment_Plot_Dimension' argument must not be missing.")
    }
    if (Relative_Figure_4_Treatment_Plot_Dimension <= 0) {
      stop ("The 'Relative_Figure_4_Treatment_Plot_Dimension' argument must be positive.")
    }
    if (length(Relative_Figure_4_Individual_Plot_Height) != 1) {
      stop ("The 'Relative_Figure_4_Individual_Plot_Height' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_4_Individual_Plot_Height)) {
      stop ("The 'Relative_Figure_4_Individual_Plot_Height' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_4_Individual_Plot_Height)) {
      stop ("The 'Relative_Figure_4_Individual_Plot_Height' argument must not be missing.")
    }
    if (Relative_Figure_4_Individual_Plot_Height <= 0) {
      stop ("The 'Relative_Figure_4_Individual_Plot_Height' argument must be positive.")
    }
    if (length(Relative_Figure_4_Individual_Plot_Width) != 1) {
      stop ("The 'Relative_Figure_4_Individual_Plot_Width' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_4_Individual_Plot_Width)) {
      stop ("The 'Relative_Figure_4_Individual_Plot_Width' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_4_Individual_Plot_Width)) {
      stop ("The 'Relative_Figure_4_Individual_Plot_Width' argument must not be missing.")
    }
    if (Relative_Figure_4_Individual_Plot_Width <= 0) {
      stop ("The 'Relative_Figure_4_Individual_Plot_Width' argument must be positive.")
    }
    if (length(Relative_Figure_4_Parameter_Spacing_Line_Plot_Height) != 1) {
      stop ("The 'Relative_Figure_4_Parameter_Spacing_Line_Plot_Height' argument must be of length 1.")
    }
    if (!is.numeric(Relative_Figure_4_Parameter_Spacing_Line_Plot_Height)) {
      stop ("The 'Relative_Figure_4_Parameter_Spacing_Line_Plot_Height' argument must be of class 'numeric'.")
    }
    if (is.na(Relative_Figure_4_Parameter_Spacing_Line_Plot_Height)) {
      stop ("The 'Relative_Figure_4_Parameter_Spacing_Line_Plot_Height' argument must not be missing.")
    }
    if (Relative_Figure_4_Parameter_Spacing_Line_Plot_Height <= 0) {
      stop ("The 'Relative_Figure_4_Parameter_Spacing_Line_Plot_Height' argument must be positive.")
    }
    if (length(Figure_4_Figure_Height_Constant) != 1) {
      stop ("The 'Figure_4_Figure_Height_Constant' argument must be of length 1.")
    }
    if (!is.numeric(Figure_4_Figure_Height_Constant)) {
      stop ("The 'Figure_4_Figure_Height_Constant' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_4_Figure_Height_Constant)) {
      stop ("The 'Figure_4_Figure_Height_Constant' argument must not be missing.")
    }
    if (Figure_4_Figure_Height_Constant <= 0) {
      stop ("The 'Figure_4_Figure_Height_Constant' argument must be positive.")
    }
    if (length(Figure_4_Figure_Width_Constant) != 1) {
      stop ("The 'Figure_4_Figure_Width_Constant' argument must be of length 1.")
    }
    if (!is.numeric(Figure_4_Figure_Width_Constant)) {
      stop ("The 'Figure_4_Figure_Width_Constant' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_4_Figure_Width_Constant)) {
      stop ("The 'Figure_4_Figure_Width_Constant' argument must not be missing.")
    }
    if (Figure_4_Figure_Width_Constant <= 0) {
      stop ("The 'Figure_4_Figure_Width_Constant' argument must be positive.")
    }
    if (length(Figure_4_Axis_Labels_and_Axis_Titles_Shifting_Constant) != 1) {
      stop ("The 'Figure_4_Axis_Labels_and_Axis_Titles_Shifting_Constant' argument must be of length 1.")
    }
    if (!is.numeric(Figure_4_Axis_Labels_and_Axis_Titles_Shifting_Constant)) {
      stop ("The 'Figure_4_Axis_Labels_and_Axis_Titles_Shifting_Constant' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_4_Axis_Labels_and_Axis_Titles_Shifting_Constant)) {
      stop ("The 'Figure_4_Axis_Labels_and_Axis_Titles_Shifting_Constant' argument must not be missing.")
    }
    if (Figure_4_Axis_Labels_and_Axis_Titles_Shifting_Constant < 0) {
      warning ("The 'Figure_4_Axis_Labels_and_Axis_Titles_Shifting_Constant' argument should not be negative.")
    }
    if (length(Figure_4_Parameter_Title_Text_Size) != 1) {
      stop ("The 'Figure_4_Parameter_Title_Text_Size' argument must be of length 1.")
    }
    if (!is.numeric(Figure_4_Parameter_Title_Text_Size)) {
      stop ("The 'Figure_4_Parameter_Title_Text_Size' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_4_Parameter_Title_Text_Size)) {
      stop ("The 'Figure_4_Parameter_Title_Text_Size' argument must not be missing.")
    }
    if (Figure_4_Parameter_Title_Text_Size <= 0) {
      stop ("The 'Figure_4_Parameter_Title_Text_Size' argument must be positive.")
    }
    if (length(Figure_4_Treatment_Title_Text_Size) != 1) {
      stop ("The 'Figure_4_Treatment_Title_Text_Size' argument must be of length 1.")
    }
    if (!is.numeric(Figure_4_Treatment_Title_Text_Size)) {
      stop ("The 'Figure_4_Treatment_Title_Text_Size' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_4_Treatment_Title_Text_Size)) {
      stop ("The 'Figure_4_Treatment_Title_Text_Size' argument must not be missing.")
    }
    if (Figure_4_Treatment_Title_Text_Size <= 0) {
      stop ("The 'Figure_4_Treatment_Title_Text_Size' argument must be positive.")
    }
    if (length(Figure_4_Axis_Titles_Text_Size) != 1) {
      stop ("The 'Figure_4_Axis_Titles_Text_Size' argument must be of length 1.")
    }
    if (!is.numeric(Figure_4_Axis_Titles_Text_Size)) {
      stop ("The 'Figure_4_Axis_Titles_Text_Size' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_4_Axis_Titles_Text_Size)) {
      stop ("The 'Figure_4_Axis_Titles_Text_Size' argument must not be missing.")
    }
    if (Figure_4_Axis_Titles_Text_Size <= 0) {
      stop ("The 'Figure_4_Axis_Titles_Text_Size' argument must be positive.")
    }
    if (length(Figure_4_Axis_Labels_Text_Size) != 1) {
      stop ("The 'Figure_4_Axis_Labels_Text_Size' argument must be of length 1.")
    }
    if (!is.numeric(Figure_4_Axis_Labels_Text_Size)) {
      stop ("The 'Figure_4_Axis_Labels_Text_Size' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_4_Axis_Labels_Text_Size)) {
      stop ("The 'Figure_4_Axis_Labels_Text_Size' argument must not be missing.")
    }
    if (Figure_4_Axis_Labels_Text_Size <= 0) {
      stop ("The 'Figure_4_Axis_Labels_Text_Size' argument must be positive.")
    }
    if (length(Figure_4_Individual_Plot_Bottom_Margin) != 1) {
      stop ("The 'Figure_4_Individual_Plot_Bottom_Margin' argument must be of length 1.")
    }
    if (!is.numeric(Figure_4_Individual_Plot_Bottom_Margin)) {
      stop ("The 'Figure_4_Individual_Plot_Bottom_Margin' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_4_Individual_Plot_Bottom_Margin)) {
      stop ("The 'Figure_4_Individual_Plot_Bottom_Margin' argument must not be missing.")
    }
    if (Figure_4_Individual_Plot_Bottom_Margin < 0) {
      stop ("The 'Figure_4_Individual_Plot_Bottom_Margin' argument must not be negative.")
    }
    if (length(Figure_4_Individual_Plot_Left_Margin) != 1) {
      stop ("The 'Figure_4_Individual_Plot_Left_Margin' argument must be of length 1.")
    }
    if (!is.numeric(Figure_4_Individual_Plot_Left_Margin)) {
      stop ("The 'Figure_4_Individual_Plot_Left_Margin' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_4_Individual_Plot_Left_Margin)) {
      stop ("The 'Figure_4_Individual_Plot_Left_Margin' argument must not be missing.")
    }
    if (Figure_4_Individual_Plot_Left_Margin < 0) {
      stop ("The 'Figure_4_Individual_Plot_Left_Margin' argument must not be negative.")
    }
    if (length(Figure_4_Individual_Plot_Top_Margin) != 1) {
      stop ("The 'Figure_4_Individual_Plot_Top_Margin' argument must be of length 1.")
    }
    if (!is.numeric(Figure_4_Individual_Plot_Top_Margin)) {
      stop ("The 'Figure_4_Individual_Plot_Top_Margin' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_4_Individual_Plot_Top_Margin)) {
      stop ("The 'Figure_4_Individual_Plot_Top_Margin' argument must not be missing.")
    }
    if (Figure_4_Individual_Plot_Top_Margin < 0) {
      stop ("The 'Figure_4_Individual_Plot_Top_Margin' argument must not be negative.")
    }
    if (length(Figure_4_Individual_Plot_Right_Margin) != 1) {
      stop ("The 'Figure_4_Individual_Plot_Right_Margin' argument must be of length 1.")
    }
    if (!is.numeric(Figure_4_Individual_Plot_Right_Margin)) {
      stop ("The 'Figure_4_Individual_Plot_Right_Margin' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_4_Individual_Plot_Right_Margin)) {
      stop ("The 'Figure_4_Individual_Plot_Right_Margin' argument must not be missing.")
    }
    if (Figure_4_Individual_Plot_Right_Margin < 0) {
      stop ("The 'Figure_4_Individual_Plot_Right_Margin' argument must not be negative.")
    }
    if (length(Figure_4_Panel_Separation_Line_Width) != 1) {
      stop ("The 'Figure_4_Panel_Separation_Line_Width' argument must be of length 1.")
    }
    if (!is.numeric(Figure_4_Panel_Separation_Line_Width)) {
      stop ("The 'Figure_4_Panel_Separation_Line_Width' argument must be of class 'numeric'.")
    }
    if (is.na(Figure_4_Panel_Separation_Line_Width)) {
      stop ("The 'Figure_4_Panel_Separation_Line_Width' argument must not be missing.")
    }
    if (Figure_4_Panel_Separation_Line_Width < 0) {
      stop ("The 'Figure_4_Panel_Separation_Line_Width' argument must not be negative.")
    }
  }
  if (length(Maximum_Number_of_Iterations) != 1) {
    stop ("The 'Maximum_Number_of_Iterations' argument must be of length 1.")
  }
  if (is.na(Maximum_Number_of_Iterations)) {
    stop ("The 'Maximum_Number_of_Iterations' argument must not be missing.")
  }
  if (!is.numeric(Maximum_Number_of_Iterations)) {
    stop ("The 'Maximum_Number_of_Iterations' argument must be numeric.")
  }
  if ((Maximum_Number_of_Iterations %% 1) != 0) {
    stop ("The 'Maximum_Number_of_Iterations' argument must be a whole number.")
  }
  if (Maximum_Number_of_Iterations <= 0) {
    stop ("The 'Maximum_Number_of_Iterations' argument must be a positive number.")
  }
  if (length(Tolerance) != 1) {
    stop ("The 'Tolerance' argument must be of length 1.")
  }
  if (is.na(Tolerance)) {
    stop ("The 'Tolerance' argument must not be missing.")
  }
  if (!is.numeric(Tolerance)) {
    stop ("The 'Tolerance' argument must be numeric.")
  }
  if (Tolerance <= 0) {
    stop ("The 'Tolerance' argument must be a positive number.")
  }
  if (missing(Color_Data)) {
    Color_Data <- data.frame(unique(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)]), rainbow(length(unique(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)]))))
    colnames(Color_Data) <- c(Treatment, "Color")
  }
  if (missing(Beta_Distribution_Alpha_Parameter_Values)) {
    Data_Frame$Beta_Distribution_Alpha_Parameter_Values <- Beta_Distribution_Alpha_Parameter_Values <- rep(1, nrow(Data_Frame))
  }
  if (missing(Beta_Distribution_Beta_Parameter_Values)) {
    Data_Frame$Beta_Distribution_Beta_Parameter_Values <- Beta_Distribution_Beta_Parameter_Values <- rep(1, nrow(Data_Frame))
  }
  Color_Data_List <- split(Color_Data, Color_Data[, which(colnames(Color_Data) == Treatment_Name)])
  Color_Data_List <- lapply(Color_Data_List, as.list)
  Color_Data_List <- lapply(Color_Data_List, function (x) {
    if (sum(col2rgb(x$Color)) != 0) {
      Intermediate_Color <- col2rgb(x$Color) / sum(col2rgb(x$Color))
    } else if (sum(col2rgb(x$Color)) == 0) {
      Intermediate_Color <- col2rgb(x$Color)
    }
    c(x, Transparent_Color = rgb(Intermediate_Color[1, ], Intermediate_Color[2, ], Intermediate_Color[3, ], Color_Opacity))
  })
  Unique_Treatments <- unique(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)])
  Number_of_Treatments <- length(Unique_Treatments)
  Moment_Names <- c("Mean", "Variance", "Skewness", "Kurtosis", "Hyperskewness", "Hypertailedness")
  if (Moments_to_Report <= 6) {
    Moment_Names <- Moment_Names[seq_len(Moments_to_Report)]
  } else if (Moments_to_Report > 6) {
    Moment_Names <- c(Moment_Names, paste0(DBM.functions::Ordinalizing_Integers(setdiff(seq_len(Moments_to_Report), seq_along(Moment_Names))), "-Order Moment"))
  }
  Split_Data <- lapply(lapply(split(Data_Frame, Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)]), function (x) {
    split(x, x[, which(colnames(x) == Replicate_Name)])
  }), function (x) {
    lapply(x, function (y) {
      split(y, y[, which(colnames(y) == Predictor_Variable_Name)])
    })
  })
  Quantiles_for_Simulations <- lapply(Split_Data, function (w) {
    lapply(w, function (x) {
      as.data.frame(lapply(x, function (y) {
        Bin_Limits_Data_Frame <- data.frame(Bin_Lower_Limits = eval(parse(text = y[, which(colnames(y) == Predictor_Variable_Bin_Lower_Limits_Name)])), Bin_Upper_Limits = eval(parse(text = y[, which(colnames(y) == Predictor_Variable_Bin_Upper_Limits_Name)])))
        Bin_Limits_Data_Frame$Bin_Segment_Lengths <- sapply(seq_len(nrow(Bin_Limits_Data_Frame)), function (i) {
          Bin_Limits_Data_Frame$Bin_Upper_Limits[i] - Bin_Limits_Data_Frame$Bin_Lower_Limits[i]
        })
        Bin_Limits_Data_Frame$Bin_Segment_Proportions <- Bin_Limits_Data_Frame$Bin_Segment_Lengths / sum(Bin_Limits_Data_Frame$Bin_Segment_Lengths)
        u <- sample(seq_len(nrow(Bin_Limits_Data_Frame)), Number_of_Simulations_per_Experimental_Unit, replace = T, prob = Bin_Limits_Data_Frame$Bin_Segment_Proportions)
        v <- rbeta(Number_of_Simulations_per_Experimental_Unit, 1, 1)
        sapply(seq_len(Number_of_Simulations_per_Experimental_Unit), function (i) {
          (v[i] * Bin_Limits_Data_Frame$Bin_Segment_Lengths[u[i]]) + Bin_Limits_Data_Frame$Bin_Lower_Limits[u[i]]
        })
      }))
    })
  })
  Data_Frame_of_Starting_Values <- expand.grid(setNames(lapply(Parameters, function (k) {
    c(-1, 0, 1)
  }), Parameters))
  Coefficients <- mapply(function (x, y) {
    mapply(function (u, v) {
      g <- as.data.frame(t(sapply(seq_len(nrow(v)), function (z) {
        Temporary_Data_Frame <- data.frame(setNames(unlist(as.vector(v[z, ])), NULL), setNames(unlist(sapply(u, `[`, Response_Variable_Name)), NULL))
        colnames(Temporary_Data_Frame) <- c(Predictor_Variable_Name, Response_Variable_Name)
        h <- lapply(seq_len(nrow(Data_Frame_of_Starting_Values)), function (l) {
          Model <- tryCatch (nls(Formula, data = Temporary_Data_Frame, start = unlist(Data_Frame_of_Starting_Values[l, ]), control = nls.control(maxiter = Maximum_Number_of_Iterations)), error = function (e) {
            NULL
          })
          if (!is.null(Model)) {
            Residual_Sum_of_Squares <- summary(Model)$sigma
            Coefficients <- coef(Model)
          } else if (is.null(Model)) {
            Residual_Sum_of_Squares <- NA
            Coefficients <- setNames(rep(NA, length(Parameters)), Parameters)
          }
          list(Residual_Sum_of_Squares = Residual_Sum_of_Squares, Coefficients = Coefficients)
        })
        if (!all(is.na(unlist(sapply(h, `[`, "Coefficients"))))) {
          h[[which.min(sapply(h, `[`, "Residual_Sum_of_Squares"))]]$Coefficients
        } else if (all(is.na(unlist(sapply(h, `[`, "Coefficients"))))) {
          setNames(rep(NA, length(Parameters)), Parameters)
        }
      })))
      colnames(g) <- Parameters
      g
    }, u = x, v = y, SIMPLIFY = F)
  }, x = Split_Data, y = Quantiles_for_Simulations, SIMPLIFY = F)
  Coefficients <- setNames(lapply(Parameters, function (x) {
    lapply(Coefficients, function (y) {
      lapply(y, function (z) {
        z[, which(colnames(z) == x)]
      })
    })
  }), Parameters)
  Coefficient_Moments <- lapply(Coefficients, function (x) {
    lapply(x, function (y) {
      lapply(y, function (z) {
        z <- z[which(!is.na(z))]
        Moments <- sapply(seq_len(Moments_to_Report), function (i) {
          if (i == 1) {
            mean(z)
          } else if (i == 2) {
            var(z)
          } else if (i > 2) {
            (mean((z - mean(z)) ^ i)) / ((mean((z - mean(z)) ^ 2)) ^ (i / 2))
          }
        })
        names(Moments) <- Moment_Names
        Moments
      })
    })
  })
  tryCatch (dev.off(), error = function (e) {
    NULL
  })
  Figures_1_and_2_Plotting_Information <- lapply(Coefficients, function (x) {
    v <- sapply(x, function (y) {
      u <- sapply(y, function (z) {
        hist(z, freq = F)
        par("usr")[1:2]
      })
      c(min(u[1, ]), max(u[2, ]))
    })
    c(min(v[1, ]), max(v[2, ]))
    Horizontal_Axis_Limits <- c(min(v[1, ]), max(v[2, ]))
    Breaks <- seq(Horizontal_Axis_Limits[1], Horizontal_Axis_Limits[2], length.out = (Number_of_Histogram_Bins + 1))
    v <- sapply(x, function (y) {
      u <- sapply(y, function (z) {
        hist(z, xlim = Horizontal_Axis_Limits, breaks = Breaks, freq = F)
        par("usr")[3:4]
      })
      c(min(u[1, ]), max(u[2, ]))
    })
    Vertical_Axis_Limits <- c(min(v[1, ]), max(v[2, ]))
    list(Horizontal_Axis_Limits = Horizontal_Axis_Limits, Vertical_Axis_Limits = Vertical_Axis_Limits, Breaks = Breaks)
  })
  Overlapping_Areas_List <- setNames(lapply(seq_along(Parameters), function (b) {
    Tables_of_Similarities <- list(NULL)
    for (i in seq_along(Unique_Treatments)) {
      Tables_of_Similarities[[i]] <- list(NULL)
      u <- 1
      Name_Vector <- NULL
      for (j in seq_along(Unique_Treatments)) {
        First_Treatment <- Unique_Treatments[i]
        Second_Treatment <- Unique_Treatments[j]
        Name_Vector[u] <- paste(Treatment_Name, First_Treatment, "and", Treatment_Name, Second_Treatment)
        u <- u + 1
        First_Data_Subset <- Data_Frame[which(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)] == First_Treatment), ]
        Second_Data_Subset <- Data_Frame[which(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)] == Second_Treatment), ]
        Tables_of_Similarities[[i]][[j]] <- matrix(rep(NA, length(unique(Second_Data_Subset[, which(colnames(Second_Data_Subset) == Replicate_Name)])) * length(unique(First_Data_Subset[, which(colnames(First_Data_Subset) == Replicate_Name)]))), nrow = length(unique(First_Data_Subset[, which(colnames(First_Data_Subset) == Replicate_Name)])), ncol = length(unique(Second_Data_Subset[, which(colnames(Second_Data_Subset) == Replicate_Name)])))
        rownames(Tables_of_Similarities[[i]][[j]]) <- paste0(Treatment_Name, " ", First_Treatment, ", ", Replicate_Name, " ", unique(First_Data_Subset[, which(colnames(First_Data_Subset) == Replicate_Name)]))
        colnames(Tables_of_Similarities[[i]][[j]]) <- paste0(Treatment_Name, " ", Second_Treatment, ", ", Replicate_Name, " ", unique(Second_Data_Subset[, which(colnames(Second_Data_Subset) == Replicate_Name)]))
        for (k in seq_along(unique(First_Data_Subset[, which(colnames(First_Data_Subset) == Replicate_Name)]))) {
          for (l in seq_along(unique(Second_Data_Subset[, which(colnames(Second_Data_Subset) == Replicate_Name)]))) {
            First_Replicate_Name <- unique(First_Data_Subset[, which(colnames(First_Data_Subset) == Replicate_Name)])[k]
            Second_Replicate_Name <- unique(Second_Data_Subset[, which(colnames(Second_Data_Subset) == Replicate_Name)])[l]
            First_Histogram_Information <- hist(Coefficients[[b]][[i]][[k]], breaks = Figures_1_and_2_Plotting_Information[[b]]$Breaks, xlim = Figures_1_and_2_Plotting_Information[[b]]$Horizontal_Axis_Limits, ylim = Figures_1_and_2_Plotting_Information[[b]]$Vertical_Axis_Limits, freq = F)
            Second_Histogram_Information <- hist(Coefficients[[b]][[j]][[l]], breaks = Figures_1_and_2_Plotting_Information[[b]]$Breaks, add = T, freq = F)
            Tables_of_Similarities[[i]][[j]][k, l] <- sum(sapply(seq_len(unique(length(First_Histogram_Information$density), length(Second_Histogram_Information$density))), function (x) {
              min(First_Histogram_Information$density[x], Second_Histogram_Information$density[x])
            })) * mean(mean(diff(First_Histogram_Information$breaks)), mean(diff(Second_Histogram_Information$breaks)))
          }
        }
      }
      Tables_of_Similarities[[i]] <- setNames(Tables_of_Similarities[[i]], Name_Vector)
    }
    unlist(Tables_of_Similarities, recursive = F)
  }), Parameters)
  Overlapping_Area_Data_List <- setNames(lapply(seq_along(Parameters), function (b) {
    Overlapping_Area_Data_List <- list(NULL)
    for (i in seq_along(Unique_Treatments)) {
      Overlapping_Area_Data_List[[i]] <- list(NULL)
      u <- 1
      Name_Vector_1 <- NULL
      for (j in seq_along(Unique_Treatments)) {
        First_Treatment <- Unique_Treatments[i]
        Second_Treatment <- Unique_Treatments[j]
        Name_Vector_1[u] <- paste(Treatment_Name, First_Treatment, "and", Treatment_Name, Second_Treatment)
        u <- u + 1
        First_Data_Subset <- Data_Frame[which(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)] == First_Treatment), ]
        Second_Data_Subset <- Data_Frame[which(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)] == Second_Treatment), ]
        Overlapping_Area_Data_List[[i]][[j]] <- list(NULL)
        for (k in seq_along(unique(First_Data_Subset[, which(colnames(First_Data_Subset) == Replicate_Name)]))) {
          v <- 1
          Name_Vector_2 <- NULL
          Overlapping_Area_Data_List[[i]][[j]][[k]] <- list(NULL)
          for (l in seq_along(unique(Second_Data_Subset[, which(colnames(Second_Data_Subset) == Replicate_Name)]))) {
            First_Replicate_Name <- unique(First_Data_Subset[, which(colnames(First_Data_Subset) == Replicate_Name)])[k]
            Second_Replicate_Name <- unique(Second_Data_Subset[, which(colnames(Second_Data_Subset) == Replicate_Name)])[l]
            Name_Vector_2[v] <- paste(Treatment_Name, First_Treatment, Replicate_Name, First_Replicate_Name, "and", Treatment_Name, Second_Treatment, Replicate_Name, Second_Replicate_Name)
            v <- v + 1
            First_Histogram_Information <- hist(Coefficients[[b]][[i]][[k]], breaks = Figures_1_and_2_Plotting_Information[[b]]$Breaks, xlim = Figures_1_and_2_Plotting_Information[[b]]$Horizontal_Axis_Limits, ylim = Figures_1_and_2_Plotting_Information[[b]]$Vertical_Axis_Limits, freq = F)
            Second_Histogram_Information <- hist(Coefficients[[b]][[j]][[l]], breaks = Figures_1_and_2_Plotting_Information[[b]]$Breaks, add = T, freq = F)
            Overlapping_Area_Data_List[[i]][[j]][[k]][[l]] <- list(NULL)
            Overlap_Densities <- pmin(First_Histogram_Information$density, Second_Histogram_Information$density)
            if (any(Overlap_Densities > 0)) {
              Bar_Midpoints <- unique(First_Histogram_Information$mids, Second_Histogram_Information$mids)
              Run_Length_Encoding <- rle(Overlap_Densities)
              if (Run_Length_Encoding$values[1] == 0) {
                Breaks <- Figures_1_and_2_Plotting_Information[[b]]$Breaks[(Run_Length_Encoding$lengths[1] + 1):length(Figures_1_and_2_Plotting_Information[[b]]$Breaks)]
                Bar_Midpoints <- Bar_Midpoints[(Run_Length_Encoding$lengths[1] + 1):length(Bar_Midpoints)]
                Overlap_Densities <- Overlap_Densities[(Run_Length_Encoding$lengths[1] + 1):length(Overlap_Densities)]
              } else if (Run_Length_Encoding$values[1] != 0) {
                Breaks <- Figures_1_and_2_Plotting_Information[[b]]$Breaks
                Bar_Midpoints <- Bar_Midpoints
                Overlap_Densities <- Overlap_Densities
              }
              if (Run_Length_Encoding$values[length(Run_Length_Encoding$values)] == 0) {
                Breaks <- Breaks[1:(length(Breaks) - Run_Length_Encoding$lengths[length(Run_Length_Encoding$lengths)])]
                Bar_Midpoints <- Bar_Midpoints[1:(length(Bar_Midpoints) - Run_Length_Encoding$lengths[length(Run_Length_Encoding$lengths)])]
                Overlap_Densities <- Overlap_Densities[1:(length(Overlap_Densities) - Run_Length_Encoding$lengths[length(Run_Length_Encoding$lengths)])]
              } else if (Run_Length_Encoding$values[length(Run_Length_Encoding$values)] != 0) {
                Breaks <- Breaks
                Bar_Midpoints <- Bar_Midpoints
                Overlap_Densities <- Overlap_Densities
              }
              Overlap_Densities <- Overlap_Densities / sum(Overlap_Densities * mean(diff(Breaks)))
              plot(c(Breaks[1], Bar_Midpoints[which.max(Overlap_Densities)], Breaks[length(Breaks)]), c(0, max(Overlap_Densities), 0), type = "n", axes = F, xlab = "", ylab = "")
              Axis_Limits <- par("usr")
              Mean <- sum(Overlap_Densities * mean(diff(Breaks)) * Bar_Midpoints)
              Variance <- sum(Overlap_Densities * mean(diff(Breaks)) * ((Bar_Midpoints - Mean) ^ 2))
              Moments <- setNames(sapply(seq_len(Moments_to_Report), function (i) {
                if (i == 1) {
                  Mean
                } else if (i == 2) {
                  Variance
                } else if (i > 2) {
                  sum(Overlap_Densities * mean(diff(Breaks)) * ((Bar_Midpoints - Mean) ^ i)) / (Variance ^ (i / 2))
                }
              }), Moment_Names)
              Overlapping_Area_Data_List[[i]][[j]][[k]][[l]] <- list(Make_a_Plot = T, First_Treatment = First_Treatment, First_Replicate_Name = First_Replicate_Name, Second_Treatment = Second_Treatment, Second_Replicate_Name = Second_Replicate_Name, Plot_Title = paste0(Treatment_Name, " ", Second_Treatment, ", ", Replicate_Name, " ", Second_Replicate_Name, "; ", Treatment_Name, " ", First_Treatment, ", ", Replicate_Name, " ", First_Replicate_Name), Parameter = Parameters[b], Overlap_Densities = Overlap_Densities, Bar_Midpoints = Bar_Midpoints, Breaks = Breaks, Color_1 = Color_Data_List[[which(names(Color_Data_List) == First_Treatment)]]$Transparent_Color, Color_2 = Color_Data_List[[which(names(Color_Data_List) == Second_Treatment)]]$Transparent_Color, Axis_Limits = Axis_Limits, Moments = Moments)
            } else if (all(Overlap_Densities == 0)) {
              Overlapping_Area_Data_List[[i]][[j]][[k]][[l]] <- list(Make_a_Plot = F, First_Treatment = First_Treatment, First_Replicate_Name = First_Replicate_Name, Second_Treatment = Second_Treatment, Second_Replicate_Name = Second_Replicate_Name, Plot_Title = paste0(Treatment_Name, " ", First_Treatment, ", ", Replicate_Name, " ", First_Replicate_Name, "; ", Treatment_Name, " ", Second_Treatment, ", ", Replicate_Name, " ", Second_Replicate_Name), Parameter = Parameters[b], Color_1 = Color_Data_List[[which(names(Color_Data_List) == First_Treatment)]]$Transparent_Color, Color_2 = Color_Data_List[[which(names(Color_Data_List) == Second_Treatment)]]$Transparent_Color)
            }
          }
          Overlapping_Area_Data_List[[i]][[j]][[k]] <- setNames(Overlapping_Area_Data_List[[i]][[j]][[k]], Name_Vector_2)
        }
        Overlapping_Area_Data_List[[i]][[j]] <- unlist(Overlapping_Area_Data_List[[i]][[j]], recursive = F)
      }
      Overlapping_Area_Data_List[[i]] <- setNames(Overlapping_Area_Data_List[[i]], Name_Vector_1)
    }
    unlist(Overlapping_Area_Data_List, recursive = F)
  }), Parameters)
  Overlapping_Area_Moments <- lapply(Overlapping_Areas_List, function (x) {
    lapply(x, function (y) {
      setNames(lapply(seq_len(Moments_to_Report), function (i) {
        if (i == 1) {
          mean(as.vector(y))
        } else if (i == 2) {
          var(as.vector(y))
        } else if (i > 2) {
          setNames(DBM.functions::Calculating_Standardized_Moments(as.vector(y), i), NULL)
        }
      }), Moment_Names)
    })
  })
  if (Make_Figure_1 == T) {
    Figure_1_Second_Layout_Matrix_Row <- unlist(mapply(rep, seq_along(Unique_Treatments), sapply(Split_Data, length), SIMPLIFY = F)) + 1
    Figure_1_Third_Layout_Matrix_Row <- seq_len(sum(sapply(Split_Data, length))) + max(Figure_1_Second_Layout_Matrix_Row)
    Figure_1_Layout_Matrix <- rbind(Figure_1_Second_Layout_Matrix_Row, Figure_1_Third_Layout_Matrix_Row, (Figure_1_Third_Layout_Matrix_Row - min(Figure_1_Third_Layout_Matrix_Row) + 1) + max(Figure_1_Third_Layout_Matrix_Row))
    rownames(Figure_1_Layout_Matrix) <- NULL
    Columns_to_Add <- rep(0, nrow(Figure_1_Layout_Matrix))
    Figure_1_Layout_Matrix <- lapply(lapply(seq_along(unique(Figure_1_Second_Layout_Matrix_Row)), function (x) {
      which(Figure_1_Layout_Matrix[1, ] == unique(Figure_1_Second_Layout_Matrix_Row)[x])
    }), function (x) {
      Figure_1_Layout_Matrix[, x]
    })
    if (Number_of_Treatments > 1) {
      for (i in seq_along(Figure_1_Layout_Matrix)) {
        if (i == 1) {
          New_Figure_1_Layout_Matrix <- cbind(Figure_1_Layout_Matrix[[i]], Columns_to_Add)
        } else if ((i > 1) & (i < length(Figure_1_Layout_Matrix))) {
          New_Figure_1_Layout_Matrix <- cbind(New_Figure_1_Layout_Matrix, Figure_1_Layout_Matrix[[i]], Columns_to_Add)
        } else if (i == length(Figure_1_Layout_Matrix)) {
          New_Figure_1_Layout_Matrix <- cbind(New_Figure_1_Layout_Matrix, Figure_1_Layout_Matrix[[i]])
        }
      }
      colnames(New_Figure_1_Layout_Matrix) <- NULL
    }
    New_Figure_1_Layout_Matrix <- rbind(rep(1, ncol(New_Figure_1_Layout_Matrix)), New_Figure_1_Layout_Matrix, rep(max(New_Figure_1_Layout_Matrix) + 1, ncol(New_Figure_1_Layout_Matrix)))
    if (length(Parameters) > 1) {
      for (i in seq_along(Parameters)) {
        if (i == 1) {
          Original_Layout_Matrix <- New_Figure_1_Layout_Matrix
          New_Figure_1_Layout_Matrix <- rbind(New_Figure_1_Layout_Matrix, rep(max(New_Figure_1_Layout_Matrix) + 1, ncol(New_Figure_1_Layout_Matrix)))
        } else if ((i > 1) & (i < length(Parameters))) {
          Addition <- Original_Layout_Matrix
          Addition[which(Addition != 0)] <- Addition[which(Addition != 0)] + max(New_Figure_1_Layout_Matrix)
          New_Figure_1_Layout_Matrix <- rbind(New_Figure_1_Layout_Matrix, Addition, rep(max(Addition) + 1, ncol(New_Figure_1_Layout_Matrix)))
        } else if (i == length(Parameters)) {
          Addition <- Original_Layout_Matrix
          Addition[which(Addition != 0)] <- Addition[which(Addition != 0)] + max(New_Figure_1_Layout_Matrix)
          New_Figure_1_Layout_Matrix <- rbind(New_Figure_1_Layout_Matrix, Addition)
        }
      }
    }
    Figure_1_Layout_Matrix <- New_Figure_1_Layout_Matrix
    Figure_1_Heights <- c(Relative_Figure_1_Parameter_Plot_Height, Relative_Figure_1_Treatment_Plot_Height, Relative_Figure_1_Replicate_Plot_Height, Relative_Figure_1_Individual_Plot_Height, Relative_Figure_1_Legend_Plot_Height)
    if (length(Parameters) > 1) {
      for (i in seq_along(Parameters)) {
        if (i == 1) {
          Original_Figure_1_Heights <- Figure_1_Heights
          Figure_1_Heights <- c(Figure_1_Heights, Relative_Figure_1_Parameter_Spacing_Line_Plot_Height)
        } else if ((i > 1) & (i < length(Parameters))) {
          Addition <- Original_Figure_1_Heights
          Figure_1_Heights <- c(Figure_1_Heights, Addition, Relative_Figure_1_Parameter_Spacing_Line_Plot_Height)
        } else if (i == length(Parameters)) {
          Addition <- Original_Figure_1_Heights
          Figure_1_Heights <- c(Figure_1_Heights, Addition)
        }
      }
    }
    Figure_1_Widths <- rep(NA, ncol(Figure_1_Layout_Matrix))
    Figure_1_Widths[which(apply(Figure_1_Layout_Matrix, 2, function (x) {
      all(x != 0)
    }))] <- Relative_Figure_1_Individual_Plot_Width
    Figure_1_Widths[which(apply(Figure_1_Layout_Matrix, 2, function (x) {
      any(x == 0)
    }))] <- Relative_Figure_1_Gap_Plot_Width
    jpeg(paste0(Working_Directory, "/", First_Figure_Name, ".jpeg"), height = Relative_Figure_1_Individual_Plot_Height * Figure_1_Figure_Height_Constant, width = Relative_Figure_1_Individual_Plot_Width * Figure_1_Figure_Width_Constant)
    layout(matrix(1), heights = Relative_Figure_1_Individual_Plot_Height, widths = Relative_Figure_1_Individual_Plot_Width)
    Figure_1_Vertical_Axis_Stuff <- setNames(lapply(seq_along(Parameters), function (b) {
      lapply(seq_along(Unique_Treatments), function (x) {
        Data_Subset <- Data_Frame[which(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)] == unique(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)])[x]), ]
        lapply(seq_along(unique(Data_Subset[, which(colnames(Data_Subset) == Replicate_Name)])), function (y) {
          if (Show_Moments_in_Figure_1 == T) {
            Output <- DBM.functions::Adding_a_Text_Box_at_the_Top_of_a_Histogram(Coefficients[[b]][[x]][[y]], Text = paste(rep("\n", Moments_to_Report - 1), collapse = ""), Text_Size = Figure_1_Moment_Text_Size, Plot_Margins = c(Figure_1_Individual_Plot_Bottom_Margin, Figure_1_Individual_Plot_Left_Margin, ifelse(Show_Individual_Plot_Titles == T, 4, Figure_1_Individual_Plot_Top_Margin), Figure_1_Individual_Plot_Right_Margin), breaks = Figures_1_and_2_Plotting_Information[[b]]$Breaks, xlim = Figures_1_and_2_Plotting_Information[[b]]$Horizontal_Axis_Limits, freq = F, Tolerance = Tolerance)
          } else if (Show_Moments_in_Figure_1 == F) {
            Histogram_Information <- hist(Coefficients[[b]][[x]][[y]], breaks = Figures_1_and_2_Plotting_Information[[b]]$Breaks, xlim = Figures_1_and_2_Plotting_Information[[b]]$Horizontal_Axis_Limits, freq = F)
            Output <- list(Vertical_Axis_Limits = c(0, max(Histogram_Information$density)), Text_Vertical_Coordinate = NA)
          }
          Output
        })
      })
    }), Parameters)
    dev.off()
    file.remove(paste0(Working_Directory, "/", First_Figure_Name, ".jpeg"))
    Figure_1_Vertical_Axis_Stuff <- lapply(Figure_1_Vertical_Axis_Stuff, function (x) {
      unlist(x, recursive = F)
    })
    Figure_1_Vertical_Axis_Stuff <- lapply(Figure_1_Vertical_Axis_Stuff, function (x) {
      y <- lapply(x, `[`, "Vertical_Axis_Limits")
      g <- sapply(y, function (z) {
        z[[1]][2]
      })
      x[[which.max(g)]]
    })
    jpeg(paste0(Working_Directory, "/", First_Figure_Name, ".jpeg"), height = sum(Figure_1_Heights) * Figure_1_Figure_Height_Constant, width = sum(Figure_1_Widths) * Figure_1_Figure_Width_Constant)
    layout(Figure_1_Layout_Matrix, heights = Figure_1_Heights, widths = Figure_1_Widths)
    lapply(seq_along(Parameters), function (b) {
      par(mar = c(1, 1, 1, 1))
      plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
      text(0, 0, paste0("Parameter: ", Parameters[b]), cex = Figure_1_Parameter_Title_Text_Size)
      lapply(names(Split_Data), function (x) {
        plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
        text(0, 0, paste0(Treatment_Name, ": ", x), cex = Figure_1_Treatment_Title_Text_Size)
      })
      lapply(Split_Data, function (x) {
        lapply(names(x), function (y) {
          plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
          text(0, 0, paste0(Replicate_Name, ": ", y), cex = Figure_1_Replicate_Title_Text_Size)
        })
      })
      par(mar = c(Figure_1_Individual_Plot_Bottom_Margin, Figure_1_Individual_Plot_Left_Margin, ifelse(Show_Individual_Plot_Titles == T, 4, Figure_1_Individual_Plot_Top_Margin), Figure_1_Individual_Plot_Right_Margin), mgp = c(3 + (Figure_1_Axis_Labels_and_Axis_Titles_Shifting_Constant * 3), 1 + Figure_1_Axis_Labels_and_Axis_Titles_Shifting_Constant, 0))
      lapply(seq_along(Coefficients[[b]]), function (x) {
        lapply(seq_along(Coefficients[[b]][[x]]), function (y) {
          Data_of_Interest <- Coefficients[[b]][[x]][[y]]
          Data_of_Interest <- Data_of_Interest[which(!is.na(Data_of_Interest))]
          hist(Data_of_Interest, breaks = Figures_1_and_2_Plotting_Information[[b]]$Breaks, xlab = Parameters[b], xlim = Figures_1_and_2_Plotting_Information[[b]]$Horizontal_Axis_Limits, ylim = Figure_1_Vertical_Axis_Stuff[[b]]$Vertical_Axis_Limits, col = Color_Data_List[[x]]$Color, main = ifelse(Show_Individual_Plot_Titles == T, paste0(Treatment, " ", names(Coefficients[[b]])[x], ", ", Replicate, " ", names(Coefficients[[b]][[x]])[y]), ""), freq = F, cex.lab = Figure_1_Axis_Titles_Text_Size, cex.axis = Figure_1_Axis_Labels_Text_Size)
          if (Show_Moments_in_Figure_1 == T) {
            Moments <- Coefficient_Moments[[b]][[x]][[y]]
            Moments <- ifelse(is.finite(Moments), as.character(round(Moments, Moment_Rounding_Constant)), "Undefined")
            text(mean(Figures_1_and_2_Plotting_Information[[b]]$Horizontal_Axis_Limits), Figure_1_Vertical_Axis_Stuff[[b]]$Text_Vertical_Coordinate, labels = paste(paste0(names(Moments), ": ", Moments), collapse = "\n"), cex = Figure_1_Moment_Text_Size)
          }
        })
      })
      par(mar = c(1, 1, 1, 1), mgp = c(3, 1, 0))
      plot(0, type = "n", xlim = c(-1, 1), ylim = c(-1, 1), xlab = "", ylab = "", axes = F)
      Actual_Plot_Dimensions <- par("pin")
      Text_Height <- (strheight("") * Figure_1_Legend_Text_Size)
      Gap_Height <- (strheight("\n") * Figure_1_Legend_Text_Size) - (2 * Text_Height)
      Gap_Width <- strwidth("\t") * Figure_1_Legend_Text_Size
      Space_Width <- strwidth(" ")
      Text_Width <- sapply(unique(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)]), function (i) {
        strwidth(i) * Figure_1_Legend_Text_Size
      })
      Maximum_Text_Width <- max(sapply(unique(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)]), function (i) {
        strwidth(i) * Figure_1_Legend_Text_Size
      }))
      Color_Box_Width <- ((Text_Height * Actual_Plot_Dimensions[2]) / Actual_Plot_Dimensions[1])
      Total_Width <- (Color_Box_Width * length(unique(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)]))) + (Gap_Width * ((length(unique(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)])) * 2) - 1)) + (Maximum_Text_Width * length(unique(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)])))
      Horizontal_Coordinates <- list(NULL)
      for (i in seq_along(unique(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)]))) {
        Horizontal_Coordinates[[i]] <- list(Color_Box_Coordinate = NULL, Text_Coordinate = NULL)
        Horizontal_Coordinates[[i]]$Color_Box_Coordinate <- (-Total_Width / 2) + (Color_Box_Width * (((2 * i) - 1) / 2)) + (2 * (Gap_Width * (i - 1))) + (Maximum_Text_Width * (i - 1))
        Horizontal_Coordinates[[i]]$Text_Coordinate <- setNames((-Total_Width / 2) + (Color_Box_Width * i) + (Gap_Width * ((i * 2) - 1)) + (Maximum_Text_Width * (i - 1)) + (Text_Width[i] / 2), NULL)
      }
      Horizontal_Coordinates <- setNames(Horizontal_Coordinates, unique(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)]))
      Title_Text_Height <- strheight(expression(paste(underline("X")))) * Figure_1_Legend_Text_Size
      Overall_Legend_Height <- Title_Text_Height + Gap_Height + Text_Height
      Vertical_Coordinates <- c((0.5 * Overall_Legend_Height) - (0.5 * Title_Text_Height), (0.5 * Overall_Legend_Height) - Title_Text_Height - (0.5 * Text_Height) - Gap_Height)
      text(0, Vertical_Coordinates[1], bquote(underline(.(Treatment))), cex = Figure_1_Legend_Text_Size)
      lapply(seq_along(unique(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)])), function (x) {
        rect(Horizontal_Coordinates[[x]]$Color_Box_Coordinate - (Color_Box_Width / 2), Vertical_Coordinates[2] - (Text_Height / 2), Horizontal_Coordinates[[x]]$Color_Box_Coordinate + (Color_Box_Width / 2), Vertical_Coordinates[2] + (Text_Height / 2), col = Color_Data_List[[x]]$Color)
        text(Horizontal_Coordinates[[x]]$Text_Coordinate, Vertical_Coordinates[2], unique(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)])[x], cex = Figure_1_Legend_Text_Size)
      })
      if (Parameters[b] != Parameters[length(Parameters)]) {
        par(mar = c(1, 1, 1, 1))
        plot(0, type = "n", xlab = "", ylab = "", axes = F, xlim = c(-1, 1), ylim = c(-1, 1))
        abline(h = 0, lwd = Figure_1_Panel_Separation_Line_Width)
      }
    })
    dev.off()
  }
  if (Make_Figure_2 == T) {
    Figure_2_Preliminary_Layout_Matrix <- sapply(Split_Data, length) %o% sapply(Split_Data, length)
    rownames(Figure_2_Preliminary_Layout_Matrix) <- colnames(Figure_2_Preliminary_Layout_Matrix) <- sapply(Split_Data, length)
    List_of_Matrices <- list(NULL)
    k <- 1
    for (j in seq_len(nrow(Figure_2_Preliminary_Layout_Matrix))) {
      for (i in seq_len(ncol(Figure_2_Preliminary_Layout_Matrix))) {
        List_of_Matrices[[k]] <- matrix(seq_len(Figure_2_Preliminary_Layout_Matrix[i, j]), nrow = as.numeric(rownames(Figure_2_Preliminary_Layout_Matrix)[i]), ncol = as.numeric(colnames(Figure_2_Preliminary_Layout_Matrix)[j]), byrow = T)
        k <- k + 1
      }
    }
    for (i in seq_len(length(List_of_Matrices))) {
      if (i == 1) {
        Previous_Maximum <- max(List_of_Matrices[[i]])
      } else if (i > 1) {
        List_of_Matrices[[i]] <- List_of_Matrices[[i]] + Previous_Maximum
        Previous_Maximum <- max(List_of_Matrices[[i]])
      }
    }
    Vector_1 <- seq_len(length(List_of_Matrices)) %/% ncol(Figure_2_Preliminary_Layout_Matrix)
    Vector_1 <- c(0, Vector_1[-length(Vector_1)])
    Vector_2 <- seq_len(length(List_of_Matrices)) %% ncol(Figure_2_Preliminary_Layout_Matrix)
    Vector_2 <- c(0, Vector_2[-length(Vector_2)])
    Intermediate_List_of_Matrices_1 <- mapply(function (i, j) {
      if (j != max(Vector_2)) {
        rbind(i, rep(0, ncol(i)))
      } else if (j == max(Vector_2)) {
        i
      }
    }, i = List_of_Matrices, j = Vector_2, SIMPLIFY = F)
    New_List_of_Matrices <- list(NULL)
    for (i in seq_along(unique(Vector_1))) {
      New_List_of_Matrices[[i]] <- do.call("rbind", Intermediate_List_of_Matrices_1[which(Vector_1 == unique(Vector_1)[i])])
    }
    Vector_3 <- seq_len(length(New_List_of_Matrices)) %% nrow(Figure_2_Preliminary_Layout_Matrix)
    Vector_3 <- c(0, Vector_3[-length(Vector_3)])
    Intermediate_List_of_Matrices_2 <- mapply(function (i, j) {
      if (j != max(Vector_3)) {
        cbind(i, rep(0, nrow(i)))
      } else if (j == max(Vector_3)) {
        i
      }
    }, i = New_List_of_Matrices, j = Vector_3, SIMPLIFY = F)
    Figure_2_Layout_Matrix <- do.call("cbind", Intermediate_List_of_Matrices_2)
    Figure_2_Layout_Matrix[which(Figure_2_Layout_Matrix != 0)] <- Figure_2_Layout_Matrix[which(Figure_2_Layout_Matrix != 0)] + 1 + (2 * length(Split_Data)) + (4 * sum(sapply(Split_Data, length)))
    Figure_2_Layout_Matrix_Top <- rbind((2 * unlist(mapply(rep, seq_along(Split_Data), sapply(Split_Data, length), SIMPLIFY = F))), ((2 * rep(seq_len(sum(sapply(Split_Data, length))))) + (2 * length(Split_Data))), ((2 * rep(seq_len(sum(sapply(Split_Data, length))))) + ((2 * length(Split_Data)) + (2 * sum(sapply(Split_Data, length))))), rep(0, sum(sapply(Split_Data, length))))
    Vector_4 <- sapply(Split_Data, length)[1]
    for (i in seq_len(length(sapply(Split_Data, length)) - 1)) {
      Vector_4[i + 1] <- Vector_4[i] + sapply(Split_Data, length)[i + 1]
    }
    Vector_4 <- setNames(Vector_4, NULL)
    Vector_5 <- 1
    for (i in seq_len(length(Vector_4) - 1)) {
      Vector_5[i + 1] <- Vector_4[i] + 1
    }
    Indices <- mapply(seq, Vector_5, Vector_4, SIMPLIFY = F)
    Split_Final_Layout_Matrix_Top <- lapply(Indices, function (x) {
      Figure_2_Layout_Matrix_Top[, x]
    })
    Vector_2 <- seq_len(length(Split_Final_Layout_Matrix_Top)) %% ncol(Figure_2_Layout_Matrix_Top)
    Vector_2 <- c(0, Vector_2[-length(Vector_2)])
    Intermediate_List_of_Matrices_1 <- mapply(function (i, j) {
      if (j != max(Vector_2)) {
        cbind(i, rep(0, nrow(i)))
      } else if (j == max(Vector_2)) {
        i
      }
    }, i = Split_Final_Layout_Matrix_Top, j = Vector_2, SIMPLIFY = F)
    Figure_2_Layout_Matrix_Top <- do.call("cbind", Intermediate_List_of_Matrices_1)
    Figure_2_Layout_Matrix_Left_Side <- cbind(((2 * unlist(mapply(rep, seq_along(Split_Data), each = sapply(Split_Data, length), SIMPLIFY = F))) + 1), ((2 * rep(seq_len(sum(sapply(Split_Data, length))))) + (2 * length(Split_Data)) + 1), ((2 * rep(seq_len(sum(sapply(Split_Data, length))))) + ((2 * length(Split_Data)) + (2 * sum(sapply(Split_Data, length)))) + 1), rep(0, (sum(sapply(Split_Data, length)))))
    Split_Final_Layout_Matrix_Left_Side <- lapply(Indices, function (x) {
      Figure_2_Layout_Matrix_Left_Side[x, ]
    })
    Intermediate_List_of_Matrices_1 <- mapply(function (i, j) {
      if (j != max(Vector_2)) {
        rbind(i, rep(0, ncol(i)))
      } else if (j == max(Vector_2)) {
        i
      }
    }, i = Split_Final_Layout_Matrix_Left_Side, j = Vector_2, SIMPLIFY = F)
    Figure_2_Layout_Matrix_Left_Side <- do.call("rbind", Intermediate_List_of_Matrices_1)
    Figure_2_Layout_Matrix_Top_Left_Corner <- matrix(rep(0, (4 * 4)), nrow = 4, ncol = 4)
    Figure_2_Layout_Matrix <- cbind(rbind(Figure_2_Layout_Matrix_Top_Left_Corner, Figure_2_Layout_Matrix_Left_Side), rbind(Figure_2_Layout_Matrix_Top, Figure_2_Layout_Matrix))
    Figure_2_Layout_Matrix <- rbind(rep(1, ncol(Figure_2_Layout_Matrix)), Figure_2_Layout_Matrix, rep(0, ncol(Figure_2_Layout_Matrix)), rep((max(Figure_2_Layout_Matrix) + 1), ncol(Figure_2_Layout_Matrix)))
    if (length(Parameters) > 1) {
      for (i in seq_along(Parameters)) {
        if (i == 1) {
          Original_Final_Layout_Matrix <- Figure_2_Layout_Matrix
          Figure_2_Layout_Matrix <- rbind(Figure_2_Layout_Matrix, rep(max(Figure_2_Layout_Matrix) + 1, ncol(Figure_2_Layout_Matrix)))
        } else if ((i > 1) & (i < length(Parameters))) {
          Addition <- Original_Final_Layout_Matrix
          Addition[which(Addition != 0)] <- Addition[which(Addition != 0)] + max(Figure_2_Layout_Matrix)
          Figure_2_Layout_Matrix <- rbind(Figure_2_Layout_Matrix, Addition, rep(max(Addition) + 1, ncol(Figure_2_Layout_Matrix)))
        } else if (i == length(Parameters)) {
          Addition <- Original_Final_Layout_Matrix
          Addition[which(Addition != 0)] <- Addition[which(Addition != 0)] + max(Figure_2_Layout_Matrix)
          Figure_2_Layout_Matrix <- rbind(Figure_2_Layout_Matrix, Addition)
        }
      }
    }
    Figure_2_Heights <- rep(Relative_Figure_2_Individual_Plot_Dimension, sum(sapply(Split_Data, length)))
    List <- lapply(Indices, function (i) {
      Figure_2_Heights[i]
    })
    Figure_2_Heights <- do.call("c", lapply(seq_along(List), function (x) {
      if (x < length(List)) {
        c(List[[x]], 2)
      } else if (x == length(List)) {
        List[[x]]
      }
    }))
    Figure_2_Heights <- c(Relative_Figure_2_Parameter_Plot_Height, Relative_Figure_2_Treatment_Plot_Height, Relative_Figure_2_Replicate_Plot_Height, Relative_Figure_2_Individual_Plot_Dimension, Relative_Figure_2_Gap_Plot_Dimension, Figure_2_Heights, Relative_Figure_2_Gap_Plot_Dimension, Relative_Figure_2_Legend_Plot_Height)
    if (length(Parameters) > 1) {
      for (i in seq_along(Parameters)) {
        if (i == 1) {
          Original_Heights <- Figure_2_Heights
          Figure_2_Heights <- c(Figure_2_Heights, Relative_Figure_2_Gap_Plot_Dimension)
        } else if ((i > 1) & (i < length(Parameters))) {
          Addition <- Original_Heights
          Figure_2_Heights <- c(Figure_2_Heights, Addition, Relative_Figure_2_Gap_Plot_Dimension)
        } else if (i == length(Parameters)) {
          Addition <- Original_Heights
          Figure_2_Heights <- c(Figure_2_Heights, Addition)
        }
      }
    }
    Figure_2_Widths <- rep(Relative_Figure_2_Individual_Plot_Dimension, sum(sapply(Split_Data, length)))
    List <- lapply(Indices, function (i) {
      Figure_2_Widths[i]
    })
    Figure_2_Widths <- do.call("c", lapply(seq_along(List), function (x) {
      if (x < length(List)) {
        c(List[[x]], Relative_Figure_2_Gap_Plot_Dimension)
      } else if (x == length(List)) {
        List[[x]]
      }
    }))
    Figure_2_Widths <- c(3, 2, 10, 3, Figure_2_Widths)
    jpeg(paste0(Working_Directory, "/", Second_Figure_Name, ".jpeg"), height = Relative_Figure_2_Individual_Plot_Dimension * Figure_2_Figure_Height_Constant, width = Relative_Figure_2_Individual_Plot_Dimension * Figure_2_Figure_Width_Constant)
    layout(matrix(1), heights = Relative_Figure_2_Individual_Plot_Dimension, widths = Relative_Figure_2_Individual_Plot_Dimension)
    Figure_2_Vertical_Axis_Stuff <- setNames(lapply(seq_along(Parameters), function (b) {
      lapply(seq_along(Unique_Treatments), function (x) {
        Data_Subset <- Data_Frame[which(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)] == unique(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)])[x]), ]
        lapply(seq_along(unique(Data_Subset[, which(colnames(Data_Subset) == Replicate_Name)])), function (y) {
          if (Show_Overlapping_Areas_in_Figure_2 == T) {
            DBM.functions::Adding_a_Text_Box_at_the_Top_of_a_Histogram(Coefficients[[b]][[x]][[y]], Text = "", Text_Size = Figure_2_Overlapping_Area_Text_Size, Plot_Margins = c(Figure_2_Individual_Plot_Bottom_Margin, Figure_2_Individual_Plot_Left_Margin, ifelse(Show_Individual_Plot_Titles == T, 4, Figure_2_Individual_Plot_Top_Margin), Figure_2_Individual_Plot_Right_Margin), breaks = Figures_1_and_2_Plotting_Information[[b]]$Breaks, xlim = Figures_1_and_2_Plotting_Information[[b]]$Horizontal_Axis_Limits, freq = F, Tolerance = Tolerance)
          } else if (Show_Overlapping_Areas_in_Figure_2 == F) {
            Histogram_Information <- hist(Coefficients[[b]][[x]][[y]], breaks = Figures_1_and_2_Plotting_Information[[b]]$Breaks, xlim = Figures_1_and_2_Plotting_Information[[b]]$Horizontal_Axis_Limits, freq = F)
            list(Vertical_Axis_Limits = c(0, max(Histogram_Information$density)), Vertical_Text_Coordinate = NA)
          }
        })
      })
    }), Parameters)
    dev.off()
    file.remove(paste0(Working_Directory, "/", Second_Figure_Name, ".jpeg"))
    Figure_2_Vertical_Axis_Stuff <- lapply(Figure_2_Vertical_Axis_Stuff, function (x) {
      unlist(x, recursive = F)
    })
    Figure_2_Vertical_Axis_Stuff <- lapply(Figure_2_Vertical_Axis_Stuff, function (x) {
      y <- lapply(x, `[`, "Vertical_Axis_Limits")
      g <- sapply(y, function (z) {
        z[[1]][2]
      })
      x[[which.max(g)]]
    })
    jpeg(paste0(Working_Directory, "/", Second_Figure_Name, ".jpeg"), height = sum(Figure_2_Heights) * Figure_2_Figure_Height_Constant, width = sum(Figure_2_Widths) * Figure_2_Figure_Width_Constant)
    layout(Figure_2_Layout_Matrix, heights = Figure_2_Heights, widths = Figure_2_Widths)
    lapply(seq_along(Parameters), function (b) {
      par(mar = c(1, 1, 1, 1))
      plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
      text(0, 0, paste0("Parameter: ", Parameters[b]), cex = Figure_2_Parameter_Title_Text_Size)
      lapply(seq_along(Unique_Treatments), function (x) {
        lapply(1:2, function (y) {
          plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
          text(0, 0, paste0(Treatment_Name, ": ", Unique_Treatments[x]), cex = Figure_2_Treatment_Title_Text_Size, srt = ifelse(y == 1, 0, 90))
        })
      })
      lapply(seq_along(Unique_Treatments), function (x) {
        Data_Subset <- Data_Frame[which(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)] == Unique_Treatments[x]), ]
        lapply(seq_along(unique(Data_Subset[, which(colnames(Data_Subset) == Replicate_Name)])), function (y) {
          lapply(1:2, function (z) {
            plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
            text(0, 0, paste0(Replicate_Name, ": ", unique(Data_Subset[, which(colnames(Data_Subset) == Replicate_Name)])[y]), cex = Figure_2_Replicate_Title_Text_Size, srt = ifelse(z == 1, 0, 90))
          })
        })
      })
      if (Show_Overlapping_Areas_in_Figure_2 == T) {
        Figures_1_and_2_Plotting_Information[[b]]$Horizontal_Text_Coordinate <- mean(c(Figures_1_and_2_Plotting_Information[[b]]$Horizontal_Axis_Limits))
      }
      par(mar = c(Figure_2_Individual_Plot_Bottom_Margin, Figure_2_Individual_Plot_Left_Margin, ifelse(Show_Individual_Plot_Titles == T, 4, Figure_2_Individual_Plot_Top_Margin), Figure_2_Individual_Plot_Right_Margin), mgp = c(3 + (Figure_2_Axis_Labels_and_Axis_Titles_Shifting_Constant * 3), 1 + Figure_2_Axis_Labels_and_Axis_Titles_Shifting_Constant, 0))
      lapply(seq_along(Unique_Treatments), function (x) {
        Data_Subset <- Data_Frame[which(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)] == unique(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)])[x]), ]
        lapply(seq_along(unique(Data_Subset[, which(colnames(Data_Subset) == Replicate_Name)])), function (y) {
          lapply(seq_len(2), function (z) {
            hist(Coefficients[[b]][[x]][[y]], breaks = Figures_1_and_2_Plotting_Information[[b]]$Breaks, xlab = Parameters[b], xlim = Figures_1_and_2_Plotting_Information[[b]]$Horizontal_Axis_Limits, ylim = Figure_2_Vertical_Axis_Stuff[[b]]$Vertical_Axis_Limits, col = Color_Data_List[[x]]$Color, main = ifelse(Show_Individual_Plot_Titles == T, paste0(Treatment, ": ", names(Coefficients[[b]])[x], ", ", Replicate, ": ", names(Coefficients[[b]][[x]])[y]), ""), freq = F, cex.lab = Figure_2_Axis_Titles_Text_Size, cex.axis = Figure_2_Axis_Labels_Text_Size, tcl = -0.5 - (0.5 * Figure_2_Axis_Labels_and_Axis_Titles_Shifting_Constant))
          })
        })
      })
      lapply(seq_along(Unique_Treatments), function (i) {
        lapply(seq_along(Unique_Treatments), function (j) {
          First_Treatment <- Unique_Treatments[j]
          Second_Treatment <- Unique_Treatments[i]
          First_Treatment_Coefficients <- Coefficients[[b]][[j]]
          Second_Treatment_Coefficients <- Coefficients[[b]][[i]]
          lapply(seq_along(First_Treatment_Coefficients), function (k) {
            lapply(seq_along(Second_Treatment_Coefficients), function (l) {
              First_Color <- col2rgb(Color_Data_List[[j]]$Color)
              if (sum(First_Color) != 0) {
                First_Color <- First_Color / sum(First_Color)
              }
              Second_Color <- col2rgb(Color_Data_List[[i]]$Color)
              if (sum(Second_Color) != 0) {
                Second_Color <- Second_Color / sum(Second_Color)
              }
              First_Replicate_Name <- names(First_Treatment_Coefficients)[k]
              Second_Replicate_Name <- names(Second_Treatment_Coefficients)[l]
              First_Histogram_Information <- hist(First_Treatment_Coefficients[[k]], breaks = Figures_1_and_2_Plotting_Information[[b]]$Breaks, xlim = Figures_1_and_2_Plotting_Information[[b]]$Horizontal_Axis_Limits, ylim = Figure_2_Vertical_Axis_Stuff[[b]]$Vertical_Axis_Limits, col = rgb(First_Color[1, ], First_Color[2, ], First_Color[3, ], 0.5), xlab = Parameters[b], main = ifelse(Show_Individual_Plot_Titles == T, paste0(Treatment, " ", First_Treatment, ", ", Replicate, " ", First_Replicate_Name, "; ", Treatment, " ", Second_Treatment, ", ", Replicate, " ", Second_Replicate_Name), ""), freq = F, cex.lab = Figure_2_Axis_Titles_Text_Size, cex.axis = Figure_2_Axis_Labels_Text_Size, tcl = -0.5 - (0.5 * Figure_2_Axis_Labels_and_Axis_Titles_Shifting_Constant))
              Second_Histogram_Information <- hist(Second_Treatment_Coefficients[[l]], breaks = Figures_1_and_2_Plotting_Information[[b]]$Breaks, col = rgb(Second_Color[1, ], Second_Color[2, ], Second_Color[3, ], 0.5), add = T, freq = F, cex.lab = Figure_2_Axis_Titles_Text_Size, cex.axis = Figure_2_Axis_Labels_Text_Size, tcl = 0)
              Overlapping_Area <- sum(sapply(seq_len(unique(length(First_Histogram_Information$density), length(Second_Histogram_Information$density))), function (x) {
                min(First_Histogram_Information$density[x], Second_Histogram_Information$density[x])
              })) * mean(mean(diff(First_Histogram_Information$breaks)), mean(diff(Second_Histogram_Information$breaks)))
              if (Show_Overlapping_Areas_in_Figure_2 == T) {
                text(mean(par("usr")[1:2]), Figure_2_Vertical_Axis_Stuff[[b]]$Text_Vertical_Coordinate, paste("Overlapping Area:", round(Overlapping_Area, Overlapping_Area_Rounding_Constant)), cex = Figure_2_Overlapping_Area_Text_Size)
              }
            })
          })
        })
      })
      par(mar = c(1, 1, 1, 1), mgp = c(3, 1, 0))
      Treatment_Group_Combinations <- combn(Number_of_Treatments, 2)
      Treatment_Group_Combinations <- lapply(seq_len(ncol(Treatment_Group_Combinations)), function (g) {
        Treatments <- setNames(unlist(sapply(Color_Data_List[Treatment_Group_Combinations[, g]], `[`, "Treatment")), NULL)
        Text <- paste0(Treatments[1], " and ", Treatments[2])
        Colors <- sapply(Color_Data_List[Treatment_Group_Combinations[, g]], function (k) {
          k$Transparent_Color
        })
        list(Text = Text, Colors = Colors)
      })
      plot(0, type = "n", xlim = c(-1, 1), ylim = c(-1, 1), xlab = "", ylab = "", axes = F)
      Actual_Plot_Dimensions <- par("pin")
      Text_Height <- (strheight("") * Figure_2_Legend_Text_Size)
      Gap_Height <- (strheight("\n") * Figure_2_Legend_Text_Size) - (2 * Text_Height)
      Gap_Width <- strwidth("\t") * Figure_2_Legend_Text_Size
      Space_Width <- strwidth(" ")
      Text_Width <- sapply(Unique_Treatments, function (i) {
        strwidth(i) * Figure_2_Legend_Text_Size
      })
      Maximum_Text_Width <- max(Text_Width)
      Color_Box_Width <- ((Text_Height * Actual_Plot_Dimensions[2]) / Actual_Plot_Dimensions[1])
      Total_Width <- (Color_Box_Width * Number_of_Treatments) + (Gap_Width * ((Number_of_Treatments * 2) - 1)) + (Maximum_Text_Width * Number_of_Treatments)
      Horizontal_Coordinates <- list(NULL)
      for (i in seq_along(Unique_Treatments)) {
        Horizontal_Coordinates[[i]] <- list(Color_Box_Coordinate = NULL, Text_Coordinate = NULL)
        Horizontal_Coordinates[[i]]$Color_Box_Coordinate <- (-Total_Width / 2) + (Color_Box_Width * (((2 * i) - 1) / 2)) + (2 * (Gap_Width * (i - 1))) + (Maximum_Text_Width * (i - 1))
        Horizontal_Coordinates[[i]]$Text_Coordinate <- setNames((-Total_Width / 2) + (Color_Box_Width * i) + (Gap_Width * ((i * 2) - 1)) + (Maximum_Text_Width * (i - 1)) + (Text_Width[i] / 2), NULL)
      }
      Horizontal_Coordinates <- setNames(Horizontal_Coordinates, Unique_Treatments)
      Title_Text_Height <- strheight(expression(paste(bold("X")))) * Figure_2_Legend_Text_Size
      Subtitle_Text_Height <- strheight(expression(paste(underline("X")))) * Figure_2_Legend_Text_Size
      Overall_Legend_Height <- (2 * Title_Text_Height) + (3 * Subtitle_Text_Height) + (6.5 * Text_Height) + (12 * Gap_Height)
      Vertical_Coordinates <- c((0.5 * Overall_Legend_Height) - (0.5 * Title_Text_Height), (0.5 * Overall_Legend_Height) - Title_Text_Height - (0.5 * Subtitle_Text_Height) - (0.5 * Text_Height) - (2 * Gap_Height), (0.5 * Overall_Legend_Height) - Title_Text_Height - Subtitle_Text_Height - Text_Height - (3 * Gap_Height), (0.5 * Overall_Legend_Height) - (1.5 * Title_Text_Height) - Subtitle_Text_Height - (2.5 * Text_Height) - (5 * Gap_Height), (0.5 * Overall_Legend_Height) - (2 * Title_Text_Height) - (1.5 * Subtitle_Text_Height) - (3 * Text_Height) - (7 * Gap_Height), (0.5 * Overall_Legend_Height) - (2 * Title_Text_Height) - (2 * Subtitle_Text_Height) - (3.5 * Text_Height) - (8 * Gap_Height), (0.5 * Overall_Legend_Height) - (2 * Title_Text_Height) - (2.5 * Subtitle_Text_Height) - (10 * Gap_Height) - (4.5 * Text_Height), (0.5 * Overall_Legend_Height) - (2 * Title_Text_Height) - (3 * Subtitle_Text_Height) - (5 * Text_Height) - (11 * Gap_Height), (0.5 * Overall_Legend_Height) - (2 * Title_Text_Height) - (3 * Subtitle_Text_Height) - (6 * Text_Height) - (12 * Gap_Height))
      text(0, Vertical_Coordinates[1], expression(paste(bold("Marginal Plots"))), cex = Figure_2_Legend_Text_Size)
      text(0, Vertical_Coordinates[2], bquote(underline(.(Treatment))), cex = Figure_2_Legend_Text_Size)
      lapply(seq_along(Unique_Treatments), function (x) {
        rect(Horizontal_Coordinates[[x]]$Color_Box_Coordinate - (Color_Box_Width / 2), Vertical_Coordinates[3] - (Text_Height / 2), Horizontal_Coordinates[[x]]$Color_Box_Coordinate + (Color_Box_Width / 2), Vertical_Coordinates[3] + (Text_Height / 2), col = Color_Data_List[[x]]$Color)
        text(Horizontal_Coordinates[[x]]$Text_Coordinate, Vertical_Coordinates[3], Unique_Treatments[x], cex = Figure_2_Legend_Text_Size)
      })
      text(0, Vertical_Coordinates[4], expression(paste(bold("Joint Plots"))), cex = Figure_2_Legend_Text_Size)
      text(0, Vertical_Coordinates[5], bquote(underline(.(Treatment))), cex = Figure_2_Legend_Text_Size)
      lapply(seq_along(Unique_Treatments), function (x) {
        rect(Horizontal_Coordinates[[x]]$Color_Box_Coordinate - (Color_Box_Width / 2), Vertical_Coordinates[6] - (Text_Height / 2), Horizontal_Coordinates[[x]]$Color_Box_Coordinate + (Color_Box_Width / 2), Vertical_Coordinates[6] + (Text_Height / 2), col = Color_Data_List[[x]]$Transparent_Color)
        text(Horizontal_Coordinates[[x]]$Text_Coordinate, Vertical_Coordinates[6], Unique_Treatments[x], cex = Figure_2_Legend_Text_Size)
      })
      text(0, Vertical_Coordinates[7], bquote(underline(.(Treatment ~ "Combination"))), cex = Figure_2_Legend_Text_Size)
      Text_Width <- sapply(Treatment_Group_Combinations, function (i) {
        strwidth(i$Text) * Figure_2_Legend_Text_Size
      })
      Maximum_Text_Width <- max(Text_Width)
      Total_Width <- (Color_Box_Width * length(Treatment_Group_Combinations)) + (Gap_Width * ((length(Treatment_Group_Combinations) * 2) - 1)) + (Maximum_Text_Width * length(Treatment_Group_Combinations))
      Horizontal_Coordinates <- list(NULL)
      for (i in seq_along(Treatment_Group_Combinations)) {
        Horizontal_Coordinates[[i]] <- list(Color_Box_Coordinate = NULL, Text_Coordinate = NULL)
        Horizontal_Coordinates[[i]]$Color_Box_Coordinate <- (-Total_Width / 2) + (Color_Box_Width * (((2 * i) - 1) / 2)) + (2 * (Gap_Width * (i - 1))) + (Maximum_Text_Width * (i - 1))
        Horizontal_Coordinates[[i]]$Text_Coordinate <- setNames((-Total_Width / 2) + (Color_Box_Width * i) + (Gap_Width * ((i * 2) - 1)) + (Maximum_Text_Width * (i - 1)) + (Text_Width[i] / 2), NULL)
      }
      Horizontal_Coordinates <- setNames(Horizontal_Coordinates, NULL)
      lapply(seq_along(Treatment_Group_Combinations), function (x) {
        text(Horizontal_Coordinates[[x]]$Text_Coordinate, Vertical_Coordinates[8], Treatment_Group_Combinations[[x]]$Text, cex = Figure_2_Legend_Text_Size)
        rect(Horizontal_Coordinates[[x]]$Color_Box_Coordinate - (((Text_Height * Actual_Plot_Dimensions[2]) / Actual_Plot_Dimensions[1]) / 2), Vertical_Coordinates[8] - (Text_Height / 2), Horizontal_Coordinates[[x]]$Color_Box_Coordinate + (((Text_Height * Actual_Plot_Dimensions[2]) / Actual_Plot_Dimensions[1]) / 2), Vertical_Coordinates[8] + (Text_Height / 2), col = Treatment_Group_Combinations[[x]]$Colors[1])
        rect(Horizontal_Coordinates[[x]]$Color_Box_Coordinate - (((Text_Height * Actual_Plot_Dimensions[2]) / Actual_Plot_Dimensions[1]) / 2), Vertical_Coordinates[8] - (Text_Height / 2), Horizontal_Coordinates[[x]]$Color_Box_Coordinate + (((Text_Height * Actual_Plot_Dimensions[2]) / Actual_Plot_Dimensions[1]) / 2), Vertical_Coordinates[8] + (Text_Height / 2), col = Treatment_Group_Combinations[[x]]$Colors[2])
      })
      New_Text <- sapply(Unique_Treatments, function (x) {
        paste0(x, " and ", x)
      })
      Text_Width <- sapply(New_Text, function (i) {
        strwidth(i) * Figure_2_Legend_Text_Size
      })
      Maximum_Text_Width <- max(Text_Width)
      Total_Width <- (Color_Box_Width * length(New_Text)) + (Gap_Width * ((length(New_Text) * 2) - 1)) + (Maximum_Text_Width * length(New_Text))
      Horizontal_Coordinates <- list(NULL)
      for (i in seq_along(New_Text)) {
        Horizontal_Coordinates[[i]] <- list(Color_Box_Coordinate = NULL, Text_Coordinate = NULL)
        Horizontal_Coordinates[[i]]$Color_Box_Coordinate <- (-Total_Width / 2) + (Color_Box_Width * (((2 * i) - 1) / 2)) + (2 * (Gap_Width * (i - 1))) + (Maximum_Text_Width * (i - 1))
        Horizontal_Coordinates[[i]]$Text_Coordinate <- setNames((-Total_Width / 2) + (Color_Box_Width * i) + (Gap_Width * ((i * 2) - 1)) + (Maximum_Text_Width * (i - 1)) + (Text_Width[i] / 2), NULL)
      }
      Horizontal_Coordinates <- setNames(Horizontal_Coordinates, Unique_Treatments)
      lapply(seq_along(New_Text), function (x) {
        rect(Horizontal_Coordinates[[x]]$Color_Box_Coordinate - (Color_Box_Width / 2), Vertical_Coordinates[9] - (Text_Height / 2), Horizontal_Coordinates[[x]]$Color_Box_Coordinate + (Color_Box_Width / 2), Vertical_Coordinates[9] + (Text_Height / 2), col = Color_Data_List[[x]]$Transparent_Color)
        rect(Horizontal_Coordinates[[x]]$Color_Box_Coordinate - (Color_Box_Width / 2), Vertical_Coordinates[9] - (Text_Height / 2), Horizontal_Coordinates[[x]]$Color_Box_Coordinate + (Color_Box_Width / 2), Vertical_Coordinates[9] + (Text_Height / 2), col = Color_Data_List[[x]]$Transparent_Color)
        text(Horizontal_Coordinates[[x]]$Text_Coordinate, Vertical_Coordinates[9], New_Text[x], cex = Figure_2_Legend_Text_Size)
      })
      if (Parameters[b] != Parameters[length(Parameters)]) {
        par(mar = c(1, 1, 1, 1))
        plot(0, type = "n", xlab = "", ylab = "", axes = F, xlim = c(-1, 1), ylim = c(-1, 1))
        abline(h = 0, lwd = Figure_2_Panel_Separation_Line_Width)
      }
    })
    dev.off()
  }
  if (Make_Figure_3 == T) {
    Preliminary_Figure_3_Layout_Matrix <- sapply(Split_Data, length) %o% sapply(Split_Data, length)
    rownames(Preliminary_Figure_3_Layout_Matrix) <- colnames(Preliminary_Figure_3_Layout_Matrix) <- sapply(Split_Data, length)
    List_of_Matrices <- list(NULL)
    k <- 1
    for (j in seq_len(nrow(Preliminary_Figure_3_Layout_Matrix))) {
      for (i in seq_len(ncol(Preliminary_Figure_3_Layout_Matrix))) {
        List_of_Matrices[[k]] <- matrix(seq_len(Preliminary_Figure_3_Layout_Matrix[i, j]), nrow = as.numeric(rownames(Preliminary_Figure_3_Layout_Matrix)[i]), ncol = as.numeric(colnames(Preliminary_Figure_3_Layout_Matrix)[j]))
        k <- k + 1
      }
    }
    for (i in seq_len(length(List_of_Matrices))) {
      if (i == 1) {
        Previous_Maximum <- max(List_of_Matrices[[i]])
      } else if (i > 1) {
        List_of_Matrices[[i]] <- List_of_Matrices[[i]] + Previous_Maximum
        Previous_Maximum <- max(List_of_Matrices[[i]])
      }
    }
    Vector_1 <- seq_len(length(List_of_Matrices)) %/% ncol(Preliminary_Figure_3_Layout_Matrix)
    Vector_1 <- c(0, Vector_1[-length(Vector_1)])
    Vector_2 <- seq_len(length(List_of_Matrices)) %% ncol(Preliminary_Figure_3_Layout_Matrix)
    Vector_2 <- c(0, Vector_2[-length(Vector_2)])
    Intermediate_List_of_Matrices_1 <- mapply(function (i, j) {
      if (j != max(Vector_2)) {
        rbind(i, rep(0, ncol(i)))
      } else if (j == max(Vector_2)) {
        i
      }
    }, i = List_of_Matrices, j = Vector_2, SIMPLIFY = F)
    New_List_of_Matrices <- list(NULL)
    for (i in seq_along(unique(Vector_1))) {
      New_List_of_Matrices[[i]] <- do.call("rbind", Intermediate_List_of_Matrices_1[which(Vector_1 == unique(Vector_1)[i])])
    }
    Vector_3 <- seq_len(length(New_List_of_Matrices)) %% nrow(Preliminary_Figure_3_Layout_Matrix)
    Vector_3 <- c(0, Vector_3[-length(Vector_3)])
    Intermediate_List_of_Matrices_2 <- mapply(function (i, j) {
      if (j != max(Vector_3)) {
        cbind(i, rep(0, nrow(i)))
      } else if (j == max(Vector_3)) {
        i
      }
    }, i = New_List_of_Matrices, j = Vector_3, SIMPLIFY = F)
    Figure_3_Layout_Matrix <- do.call("cbind", Intermediate_List_of_Matrices_2)
    Figure_3_Layout_Matrix[which(Figure_3_Layout_Matrix != 0)] <- Figure_3_Layout_Matrix[which(Figure_3_Layout_Matrix != 0)] + 1 + (2 * length(Split_Data)) + (2 * sum(sapply(Split_Data, length)))
    Figure_3_Layout_Matrix_Top <- rbind((2 * unlist(mapply(rep, seq_along(Split_Data), sapply(Split_Data, length), SIMPLIFY = F))), ((2 * rep(seq_len(sum(sapply(Split_Data, length))))) + (2 * length(Split_Data))), rep(0, sum(sapply(Split_Data, length))))
    Vector_4 <- sapply(Split_Data, length)[1]
    for (i in seq_len(length(sapply(Split_Data, length)) - 1)) {
      Vector_4[i + 1] <- Vector_4[i] + sapply(Split_Data, length)[i + 1]
    }
    Vector_4 <- setNames(Vector_4, NULL)
    Vector_5 <- 1
    for (i in seq_len(length(Vector_4) - 1)) {
      Vector_5[i + 1] <- Vector_4[i] + 1
    }
    Indices <- mapply(seq, Vector_5, Vector_4, SIMPLIFY = F)
    Split_Figure_3_Layout_Matrix_Top <- lapply(Indices, function (x) {
      Figure_3_Layout_Matrix_Top[, x]
    })
    Vector_2 <- seq_len(length(Split_Figure_3_Layout_Matrix_Top)) %% ncol(Figure_3_Layout_Matrix_Top)
    Vector_2 <- c(0, Vector_2[-length(Vector_2)])
    Intermediate_List_of_Matrices_1 <- mapply(function (i, j) {
      if (j != max(Vector_2)) {
        cbind(i, rep(0, nrow(i)))
      } else if (j == max(Vector_2)) {
        i
      }
    }, i = Split_Figure_3_Layout_Matrix_Top, j = Vector_2, SIMPLIFY = F)
    Figure_3_Layout_Matrix_Top <- do.call("cbind", Intermediate_List_of_Matrices_1)
    Figure_3_Layout_Matrix_Left_Side <- cbind(((2 * unlist(mapply(rep, seq_along(Split_Data), each = sapply(Split_Data, length), SIMPLIFY = F))) + 1), ((2 * rep(seq_len(sum(sapply(Split_Data, length))))) + (2 * length(Split_Data)) + 1), rep(0, (sum(sapply(Split_Data, length)))))
    Split_Figure_3_Layout_Matrix_Left_Side <- lapply(Indices, function (x) {
      Figure_3_Layout_Matrix_Left_Side[x, ]
    })
    Intermediate_List_of_Matrices_1 <- mapply(function (i, j) {
      if (j != max(Vector_2)) {
        rbind(i, rep(0, ncol(i)))
      } else if (j == max(Vector_2)) {
        i
      }
    }, i = Split_Figure_3_Layout_Matrix_Left_Side, j = Vector_2, SIMPLIFY = F)
    Figure_3_Layout_Matrix_Left_Side <- do.call("rbind", Intermediate_List_of_Matrices_1)
    Figure_3_Layout_Matrix_Top_Left_Corner <- matrix(rep(0, (3 ^ 2)), nrow = 3, ncol = 3)
    Figure_3_Layout_Matrix <- cbind(rbind(Figure_3_Layout_Matrix_Top_Left_Corner, Figure_3_Layout_Matrix_Left_Side), rbind(Figure_3_Layout_Matrix_Top, Figure_3_Layout_Matrix))
    Figure_3_Layout_Matrix <- rbind(rep(1, ncol(Figure_3_Layout_Matrix)), Figure_3_Layout_Matrix, rep(0, ncol(Figure_3_Layout_Matrix)), rep((max(Figure_3_Layout_Matrix) + 1), ncol(Figure_3_Layout_Matrix)))
    if (length(Parameters) > 1) {
      for (i in seq_along(Parameters)) {
        if (i == 1) {
          Original_Final_Layout_Matrix <- Figure_3_Layout_Matrix
          Figure_3_Layout_Matrix <- rbind(Figure_3_Layout_Matrix, rep(max(Figure_3_Layout_Matrix) + 1, ncol(Figure_3_Layout_Matrix)))
        } else if ((i > 1) & (i < length(Parameters))) {
          Addition <- Original_Final_Layout_Matrix
          Addition[which(Addition != 0)] <- Addition[which(Addition != 0)] + max(Figure_3_Layout_Matrix)
          Figure_3_Layout_Matrix <- rbind(Figure_3_Layout_Matrix, Addition, rep(max(Addition) + 1, ncol(Figure_3_Layout_Matrix)))
        } else if (i == length(Parameters)) {
          Addition <- Original_Final_Layout_Matrix
          Addition[which(Addition != 0)] <- Addition[which(Addition != 0)] + max(Figure_3_Layout_Matrix)
          Figure_3_Layout_Matrix <- rbind(Figure_3_Layout_Matrix, Addition)
        }
      }
    }
    Heights <- rep(Relative_Figure_3_Individual_Plot_Dimension, sum(sapply(Split_Data, length)))
    List <- lapply(Indices, function (i) {
      Heights[i]
    })
    Heights <- do.call("c", lapply(seq_along(List), function (x) {
      if (x < length(List)) {
        c(List[[x]], Relative_Figure_3_Gap_Plot_Dimension)
      } else if (x == length(List)) {
        List[[x]]
      }
    }))
    Heights <- c(Relative_Figure_3_Parameter_Plot_Height, Relative_Figure_3_Treatment_Plot_Dimension, Relative_Figure_3_Replicate_Plot_Dimension, Relative_Figure_3_Gap_Plot_Dimension, Heights, Relative_Figure_3_Gap_Plot_Dimension, Relative_Figure_3_Legend_Plot_Height)
    if (length(Parameters) > 1) {
      for (i in seq_along(Parameters)) {
        if (i == 1) {
          Original_Heights <- Heights
          Heights <- c(Heights, Relative_Figure_3_Parameter_Spacing_Line_Plot_Height)
        } else if ((i > 1) & (i < length(Parameters))) {
          Addition <- Original_Heights
          Heights <- c(Heights, Addition, Relative_Figure_3_Parameter_Spacing_Line_Plot_Height)
        } else if (i == length(Parameters)) {
          Addition <- Original_Heights
          Heights <- c(Heights, Addition)
        }
      }
    }
    Widths <- rep(Relative_Figure_3_Individual_Plot_Dimension, sum(sapply(Split_Data, length)))
    List <- lapply(Indices, function (i) {
      Widths[i]
    })
    Widths <- do.call("c", lapply(seq_along(List), function (x) {
      if (x < length(List)) {
        c(List[[x]], Relative_Figure_3_Gap_Plot_Dimension)
      } else if (x == length(List)) {
        List[[x]]
      }
    }))
    Widths <- c(Relative_Figure_3_Treatment_Plot_Dimension, Relative_Figure_3_Replicate_Plot_Dimension, Relative_Figure_3_Gap_Plot_Dimension, Widths)
    jpeg(paste0(Working_Directory, "/", Third_Figure_Name, ".jpeg"), height = Relative_Figure_3_Individual_Plot_Dimension * Figure_3_Figure_Height_Constant, width = Relative_Figure_3_Individual_Plot_Dimension * Figure_3_Figure_Width_Constant)
    layout(matrix(1), heights = Relative_Figure_3_Individual_Plot_Dimension, widths = Relative_Figure_3_Individual_Plot_Dimension)
    Figure_3_Vertical_Axis_Stuff <- setNames(lapply(seq_along(Parameters), function (b) {
      lapply(seq_along(Overlapping_Area_Data_List[[b]]), function (i) {
        lapply(seq_along(Overlapping_Area_Data_List[[b]][[i]]), function (j) {
          l <- Overlapping_Area_Data_List[[b]][[i]][[j]]
          if (l$Make_a_Plot == T) {
            g <- lapply(seq_along(l$Overlap_Densities), function (h) {
              list(Horizontal_Axis_Coordinates = c(l$Breaks[h], l$Breaks[h], l$Breaks[h + 1], l$Breaks[h + 1], l$Breaks[h]), Vertical_Axis_Coordinates = c(0, l$Overlap_Densities[h], l$Overlap_Densities[h], 0, 0))
            })
            Horizontal_Axis_Coordinates <- setNames(unlist(lapply(g, `[`, "Horizontal_Axis_Coordinates")), NULL)
            Vertical_Axis_Coordinates <- setNames(unlist(lapply(g, `[`, "Vertical_Axis_Coordinates")), NULL)
            if (Show_Moments_in_Figure_3 == T) {
              DBM.functions::Adding_a_Text_Box_to_the_Top_or_Bottom_of_a_Plot(Horizontal_Axis_Coordinates, Vertical_Axis_Coordinates, Text = paste(rep("\n", Moments_to_Report - 1), collapse = ""), Text_Size = Figure_3_Moment_Text_Size, Plot_Margins = c(Figure_3_Individual_Plot_Bottom_Margin, Figure_3_Individual_Plot_Left_Margin, ifelse(Show_Individual_Plot_Titles == T, 4, Figure_3_Individual_Plot_Top_Margin), Figure_3_Individual_Plot_Right_Margin), Tolerance = Tolerance)
            } else if (Show_Moments_in_Figure_3 == F) {
              list(Vertical_Axis_Limits = c(0, max(Vertical_Axis_Coordinates)), Text_Vertical_Coordinate = NA)
            }
          } else if (l$Make_a_Plot == F) {
            NULL
          }
        })
      })
    }), Parameters)
    dev.off()
    file.remove(paste0(Working_Directory, "/", Third_Figure_Name, ".jpeg"))
    Figure_3_Vertical_Axis_Stuff <- lapply(Figure_3_Vertical_Axis_Stuff, function (i) {
      unlist(i, recursive = F)
    })
    Figure_3_Vertical_Axis_Stuff <- lapply(Figure_3_Vertical_Axis_Stuff, function (x) {
      x <- x[which(!sapply(x, is.null))]
      x[[which.max(sapply(lapply(x, `[`, "Vertical_Axis_Limits"), function (y) {
        y[[1]][2]
      }))]]
    })
    Figure_3_Horizontal_Axis_Stuff <- setNames(lapply(seq_along(Parameters), function (b) {
      lapply(seq_along(Overlapping_Area_Data_List[[b]]), function (i) {
        lapply(seq_along(Overlapping_Area_Data_List[[b]][[i]]), function (j) {
          l <- Overlapping_Area_Data_List[[b]][[i]][[j]]
          if (l$Make_a_Plot == T) {
            g <- lapply(seq_along(l$Overlap_Densities), function (h) {
              list(Horizontal_Axis_Coordinates = c(l$Breaks[h], l$Breaks[h], l$Breaks[h + 1], l$Breaks[h + 1], l$Breaks[h]), Vertical_Axis_Coordinates = c(0, l$Overlap_Densities[h], l$Overlap_Densities[h], 0, 0))
            })
            Horizontal_Axis_Coordinates <- setNames(unlist(lapply(g, `[`, "Horizontal_Axis_Coordinates")), NULL)
            Vertical_Axis_Coordinates <- setNames(unlist(lapply(g, `[`, "Vertical_Axis_Coordinates")), NULL)
            plot(Horizontal_Axis_Coordinates, Vertical_Axis_Coordinates, type = "n", xlab = "", ylab = "", axes = F)
            Horizontal_Axis_Limits <- par("usr")[1:2]
          } else if (l$Make_a_Plot == F) {
            Horizontal_Axis_Limits <- NULL
          }
          Horizontal_Axis_Limits
        })
      })
    }), Parameters)
    Figure_3_Horizontal_Axis_Stuff <- lapply(Figure_3_Horizontal_Axis_Stuff, function (i) {
      unlist(i, recursive = F)
    })
    Figure_3_Horizontal_Axis_Stuff <- lapply(Figure_3_Horizontal_Axis_Stuff, function (x) {
      x <- x[which(!sapply(x, is.null))]
      Minima <- sapply(x, function (y) {
        y[1]
      })
      Maxima <- sapply(x, function (y) {
        y[2]
      })
      c(min(Minima), max(Maxima))
    })
    jpeg(paste0(Working_Directory, "/", Third_Figure_Name, ".jpeg"), height = sum(Heights) * Figure_3_Figure_Height_Constant, width = sum(Widths) * Figure_3_Figure_Width_Constant)
    layout(Figure_3_Layout_Matrix, heights = Heights, widths = Widths)
    lapply(seq_along(Parameters), function (b) {
      par(mar = c(1, 1, 1, 1))
      plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
      text(0, 0, paste0("Parameter: ", Parameters[b]), cex = Figure_3_Parameter_Title_Text_Size)
      lapply(seq_along(Unique_Treatments), function (x) {
        lapply(1:2, function (y) {
          plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
          text(0, 0, paste0(Treatment_Name, ": ", Unique_Treatments[x]), cex = Figure_3_Treatment_Title_Text_Size, srt = ifelse(y == 1, 0, 90))
        })
      })
      lapply(seq_along(Unique_Treatments), function (x) {
        Data_Subset <- Data_Frame[which(Data_Frame[, which(colnames(Data_Frame) == Treatment_Name)] == Unique_Treatments[x]), ]
        lapply(seq_along(unique(Data_Subset[, which(colnames(Data_Subset) == Replicate_Name)])), function (y) {
          lapply(1:2, function (z) {
            plot(0, type = "n", axes = F, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
            text(0, 0, paste0(Replicate_Name, ": ", unique(Data_Subset[, which(colnames(Data_Subset) == Replicate_Name)])[y]), cex = Figure_3_Replicate_Title_Text_Size, srt = ifelse(z == 1, 0, 90))
          })
        })
      })
      par(mar = c(Figure_3_Individual_Plot_Bottom_Margin, Figure_3_Individual_Plot_Left_Margin, ifelse(Show_Individual_Plot_Titles == T, 4, Figure_3_Individual_Plot_Top_Margin), Figure_3_Individual_Plot_Right_Margin), mgp = c(3 + (Figure_3_Axis_Labels_and_Axis_Titles_Shifting_Constant * 3), 1 + Figure_3_Axis_Labels_and_Axis_Titles_Shifting_Constant, 0))
      lapply(seq_along(Overlapping_Area_Data_List[[b]]), function (i) {
        lapply(seq_along(Overlapping_Area_Data_List[[b]][[i]]), function (j) {
          l <- Overlapping_Area_Data_List[[b]][[i]][[j]]
          if (l$Make_a_Plot == T) {
            plot(0, type = "n", xlab = Parameters[b], ylab = "Density", main = ifelse(Show_Individual_Plot_Titles == T, l$Plot_Title, ""), axes = F, xlim = Figure_3_Horizontal_Axis_Stuff[[b]], ylim = Figure_3_Vertical_Axis_Stuff[[b]]$Vertical_Axis_Limits, cex.lab = Figure_3_Axis_Titles_Text_Size, tcl = -0.5 - (0.5 * Figure_3_Axis_Labels_and_Axis_Titles_Shifting_Constant))
            axis(1, at = pretty(Figure_3_Horizontal_Axis_Stuff[[b]]), cex.axis = Figure_3_Axis_Labels_Text_Size)
            axis(2, at = pretty(Figure_3_Vertical_Axis_Stuff[[b]]$Vertical_Axis_Limits), cex.axis = Figure_3_Axis_Labels_Text_Size)
            for (h in seq_along(l$Overlap_Densities)) {
              polygon(c(l$Breaks[h], l$Breaks[h], l$Breaks[h + 1], l$Breaks[h + 1], l$Breaks[h]), c(0, l$Overlap_Densities[h], l$Overlap_Densities[h], 0, 0), col = l$Color_1)
              polygon(c(l$Breaks[h], l$Breaks[h], l$Breaks[h + 1], l$Breaks[h + 1], l$Breaks[h]), c(0, l$Overlap_Densities[h], l$Overlap_Densities[h], 0, 0), col = l$Color_2)
            }
            if (Show_Moments_in_Figure_3 == T) {
              Moments <- Overlapping_Area_Data_List[[b]][[i]][[j]]$Moments
              Moments <- ifelse(is.finite(Moments), as.character(round(Moments, Moment_Rounding_Constant)), "Undefined")
              text(mean(Figure_3_Horizontal_Axis_Stuff[[b]]), Figure_3_Vertical_Axis_Stuff[[b]]$Text_Vertical_Coordinate, labels = paste(paste0(names(Moments), ": ", Moments), collapse = "\n"), cex = Figure_3_Moment_Text_Size)
            }
          } else if (l$Make_a_Plot == F) {
            plot(0, type = "n", xlab = Parameters[b], ylab = "Density", main = ifelse(Show_Individual_Plot_Titles == T, l$Plot_Title, ""), axes = F, xlim = Figure_3_Horizontal_Axis_Stuff[[b]], ylim = Figure_3_Vertical_Axis_Stuff[[b]]$Vertical_Axis_Limits, cex.lab = Figure_3_Axis_Titles_Text_Size, tcl = -0.5 - (0.5 * Figure_3_Axis_Labels_and_Axis_Titles_Shifting_Constant))
            axis(1, at = pretty(Figure_3_Horizontal_Axis_Stuff[[b]]), cex.axis = Figure_3_Axis_Labels_Text_Size)
            axis(2, at = pretty(Figure_3_Vertical_Axis_Stuff[[b]]$Vertical_Axis_Limits), cex.axis = Figure_3_Axis_Labels_Text_Size)
          }
        })
      })
      par(mar = c(1, 1, 1, 1), mgp = c(3, 1, 0))
      Treatment_Group_Combinations <- combn(Number_of_Treatments, 2)
      Treatment_Group_Combinations <- lapply(seq_len(ncol(Treatment_Group_Combinations)), function (g) {
        Treatments <- setNames(unlist(sapply(Color_Data_List[Treatment_Group_Combinations[, g]], `[`, "Treatment")), NULL)
        Text <- paste0(Treatments[1], " and ", Treatments[2])
        Colors <- sapply(Color_Data_List[Treatment_Group_Combinations[, g]], function (k) {
          k$Transparent_Color
        })
        list(Text = Text, Colors = Colors)
      })
      plot(0, type = "n", xlim = c(-1, 1), ylim = c(-1, 1), xlab = "", ylab = "", axes = F)
      Actual_Plot_Dimensions <- par("pin")
      Text_Height <- (strheight("") * Figure_3_Legend_Text_Size)
      Gap_Height <- (strheight("\n") * Figure_3_Legend_Text_Size) - (2 * Text_Height)
      Gap_Width <- strwidth("\t") * Figure_3_Legend_Text_Size
      Space_Width <- strwidth(" ")
      Color_Box_Width <- ((Text_Height * Actual_Plot_Dimensions[2]) / Actual_Plot_Dimensions[1])
      Title_Text_Height <- strheight(expression(paste(underline("X")))) * Figure_3_Legend_Text_Size
      Overall_Legend_Height <- Title_Text_Height + (2 * Text_Height) + (2 * Gap_Height)
      Vertical_Coordinates <- c((0.5 * Overall_Legend_Height) - (0.5 * Title_Text_Height), (0.5 * Overall_Legend_Height) - Title_Text_Height - (0.5 * Text_Height) - Gap_Height, (0.5 * Overall_Legend_Height) - Title_Text_Height - (1.5 * Text_Height) - (2 * Gap_Height))
      text(0, Vertical_Coordinates[1], bquote(underline(.(Treatment ~ "Combination"))), cex = Figure_3_Legend_Text_Size)
      Text_Width <- sapply(Treatment_Group_Combinations, function (i) {
        strwidth(i$Text) * Figure_3_Legend_Text_Size
      })
      Maximum_Text_Width <- max(Text_Width)
      Total_Width <- (Color_Box_Width * length(Treatment_Group_Combinations)) + (Gap_Width * ((length(Treatment_Group_Combinations) * 2) - 1)) + (Maximum_Text_Width * length(Treatment_Group_Combinations))
      Horizontal_Coordinates <- list(NULL)
      for (i in seq_along(Treatment_Group_Combinations)) {
        Horizontal_Coordinates[[i]] <- list(Color_Box_Coordinate = NULL, Text_Coordinate = NULL)
        Horizontal_Coordinates[[i]]$Color_Box_Coordinate <- (-Total_Width / 2) + (Color_Box_Width * (((2 * i) - 1) / 2)) + (2 * (Gap_Width * (i - 1))) + (Maximum_Text_Width * (i - 1))
        Horizontal_Coordinates[[i]]$Text_Coordinate <- setNames((-Total_Width / 2) + (Color_Box_Width * i) + (Gap_Width * ((i * 2) - 1)) + (Maximum_Text_Width * (i - 1)) + (Text_Width[i] / 2), NULL)
      }
      Horizontal_Coordinates <- setNames(Horizontal_Coordinates, NULL)
      lapply(seq_along(Treatment_Group_Combinations), function (x) {
        text(Horizontal_Coordinates[[x]]$Text_Coordinate, Vertical_Coordinates[2], Treatment_Group_Combinations[[x]]$Text, cex = Figure_3_Legend_Text_Size)
        rect(Horizontal_Coordinates[[x]]$Color_Box_Coordinate - (((Text_Height * Actual_Plot_Dimensions[2]) / Actual_Plot_Dimensions[1]) / 2), Vertical_Coordinates[2] - (Text_Height / 2), Horizontal_Coordinates[[x]]$Color_Box_Coordinate + (((Text_Height * Actual_Plot_Dimensions[2]) / Actual_Plot_Dimensions[1]) / 2), Vertical_Coordinates[2] + (Text_Height / 2), col = Treatment_Group_Combinations[[x]]$Colors[1])
        rect(Horizontal_Coordinates[[x]]$Color_Box_Coordinate - (((Text_Height * Actual_Plot_Dimensions[2]) / Actual_Plot_Dimensions[1]) / 2), Vertical_Coordinates[2] - (Text_Height / 2), Horizontal_Coordinates[[x]]$Color_Box_Coordinate + (((Text_Height * Actual_Plot_Dimensions[2]) / Actual_Plot_Dimensions[1]) / 2), Vertical_Coordinates[2] + (Text_Height / 2), col = Treatment_Group_Combinations[[x]]$Colors[2])
      })
      New_Text <- sapply(Unique_Treatments, function (x) {
        paste0(x, " and ", x)
      })
      Text_Width <- sapply(New_Text, function (i) {
        strwidth(i) * Figure_3_Legend_Text_Size
      })
      Maximum_Text_Width <- max(Text_Width)
      Total_Width <- (Color_Box_Width * length(New_Text)) + (Gap_Width * ((length(New_Text) * 2) - 1)) + (Maximum_Text_Width * length(New_Text))
      Horizontal_Coordinates <- list(NULL)
      for (i in seq_along(New_Text)) {
        Horizontal_Coordinates[[i]] <- list(Color_Box_Coordinate = NULL, Text_Coordinate = NULL)
        Horizontal_Coordinates[[i]]$Color_Box_Coordinate <- (-Total_Width / 2) + (Color_Box_Width * (((2 * i) - 1) / 2)) + (2 * (Gap_Width * (i - 1))) + (Maximum_Text_Width * (i - 1))
        Horizontal_Coordinates[[i]]$Text_Coordinate <- setNames((-Total_Width / 2) + (Color_Box_Width * i) + (Gap_Width * ((i * 2) - 1)) + (Maximum_Text_Width * (i - 1)) + (Text_Width[i] / 2), NULL)
      }
      Horizontal_Coordinates <- setNames(Horizontal_Coordinates, Unique_Treatments)
      lapply(seq_along(New_Text), function (x) {
        rect(Horizontal_Coordinates[[x]]$Color_Box_Coordinate - (Color_Box_Width / 2), Vertical_Coordinates[3] - (Text_Height / 2), Horizontal_Coordinates[[x]]$Color_Box_Coordinate + (Color_Box_Width / 2), Vertical_Coordinates[3] + (Text_Height / 2), col = Color_Data_List[[x]]$Transparent_Color)
        rect(Horizontal_Coordinates[[x]]$Color_Box_Coordinate - (Color_Box_Width / 2), Vertical_Coordinates[3] - (Text_Height / 2), Horizontal_Coordinates[[x]]$Color_Box_Coordinate + (Color_Box_Width / 2), Vertical_Coordinates[3] + (Text_Height / 2), col = Color_Data_List[[x]]$Transparent_Color)
        text(Horizontal_Coordinates[[x]]$Text_Coordinate, Vertical_Coordinates[3], New_Text[x], cex = Figure_3_Legend_Text_Size)
      })
      if (Parameters[b] != Parameters[length(Parameters)]) {
        par(mar = c(1, 1, 1, 1))
        plot(0, type = "n", xlab = "", ylab = "", axes = F, xlim = c(-1, 1), ylim = c(-1, 1))
        abline(h = 0, lwd = Figure_3_Panel_Separation_Line_Width)
      }
    })
    dev.off()
  }
  if (Make_Figure_4 == T) {
    Figure_4_Layout_Matrix <- matrix(seq_len(Number_of_Treatments ^ 2) + (2 * Number_of_Treatments) + 1, nrow = Number_of_Treatments, ncol = Number_of_Treatments)
    Figure_4_Layout_Matrix <- rbind(seq_along(Unique_Treatments) * 2, Figure_4_Layout_Matrix)
    Figure_4_Layout_Matrix <- cbind(c(0, (seq_along(Unique_Treatments) * 2) + 1), Figure_4_Layout_Matrix)
    Figure_4_Layout_Matrix <- rbind(rep(1, ncol(Figure_4_Layout_Matrix)), Figure_4_Layout_Matrix)
    if (length(Parameters) > 1) {
      for (i in seq_along(Parameters)) {
        if (i == 1) {
          Original_Layout_Matrix <- Figure_4_Layout_Matrix
          Figure_4_Layout_Matrix <- rbind(Figure_4_Layout_Matrix, rep(max(Figure_4_Layout_Matrix) + 1, ncol(Figure_4_Layout_Matrix)))
        } else if ((i > 1) & (i < length(Parameters))) {
          Addition <- Original_Layout_Matrix
          Addition[which(Addition != 0)] <- Addition[which(Addition != 0)] + max(Figure_4_Layout_Matrix)
          Figure_4_Layout_Matrix <- rbind(Figure_4_Layout_Matrix, Addition, rep(max(Addition) + 1, ncol(Figure_4_Layout_Matrix)))
        } else if (i == length(Parameters)) {
          Addition <- Original_Layout_Matrix
          Addition[which(Addition != 0)] <- Addition[which(Addition != 0)] + max(Figure_4_Layout_Matrix)
          Figure_4_Layout_Matrix <- rbind(Figure_4_Layout_Matrix, Addition)
        }
      }
    }
    Figure_4_Heights <- c(Relative_Figure_4_Parameter_Plot_Height, Relative_Figure_4_Treatment_Plot_Dimension, rep(Relative_Figure_4_Individual_Plot_Height, Number_of_Treatments))
    if (length(Parameters) > 1) {
      for (i in seq_along(Parameters)) {
        if (i == 1) {
          Original_Figure_4_Heights <- Figure_4_Heights
          Figure_4_Heights <- c(Figure_4_Heights, Relative_Figure_4_Parameter_Spacing_Line_Plot_Height)
        } else if ((i > 1) & (i < length(Parameters))) {
          Addition <- Original_Figure_4_Heights
          Figure_4_Heights <- c(Figure_4_Heights, Addition, Relative_Figure_4_Parameter_Spacing_Line_Plot_Height)
        } else if (i == length(Parameters)) {
          Addition <- Original_Figure_4_Heights
          Figure_4_Heights <- c(Figure_4_Heights, Addition)
        }
      }
    }
    Figure_4_Widths <- c(Relative_Figure_4_Treatment_Plot_Dimension, rep(Relative_Figure_4_Individual_Plot_Width, Number_of_Treatments))
    tryCatch (dev.off(), error = function (e) {
      NULL
    })
    Figure_4_Plotting_Information <- lapply(Overlapping_Areas_List, function (x) {
      Horizontal_Axis_Limits <- sapply(x, function (y) {
        hist(y, freq = F)
        par("usr")[1:2]
      })
      Horizontal_Axis_Limits <- c(min(Horizontal_Axis_Limits[1, ]), max(Horizontal_Axis_Limits[2, ]))
      Breaks <- seq(Horizontal_Axis_Limits[1], Horizontal_Axis_Limits[2], length.out = (Number_of_Bins_in_the_Figure_4_Histograms + 1))
      list(Horizontal_Axis_Limits = Horizontal_Axis_Limits, Breaks = Breaks)
    })
    jpeg(paste0(Working_Directory, "/", Fourth_Figure_Name, ".jpeg"), height = Relative_Figure_4_Individual_Plot_Height * Figure_4_Figure_Height_Constant, width = Relative_Figure_4_Individual_Plot_Width * Figure_4_Figure_Width_Constant)
    layout(matrix(1), heights = Relative_Figure_4_Individual_Plot_Height, widths = Relative_Figure_4_Individual_Plot_Width)
    Figure_4_Vertical_Axis_Stuff <- setNames(lapply(seq_along(Parameters), function (b) {
      lapply(seq_along(Overlapping_Areas_List[[b]]), function (i) {
        if (Show_Moments_in_Figure_4 == T) {
          DBM.functions::Adding_a_Text_Box_at_the_Top_of_a_Histogram(Overlapping_Areas_List[[b]][[i]], Text = paste(rep("\n", (Moments_to_Report - 1)), collapse = ""), Text_Size = Figure_4_Moment_Text_Size, Plot_Margins = c(Figure_4_Individual_Plot_Bottom_Margin, Figure_4_Individual_Plot_Left_Margin, Figure_4_Individual_Plot_Top_Margin, Figure_4_Individual_Plot_Right_Margin), xlim = Figure_4_Plotting_Information[[b]]$Horizontal_Axis_Limits, breaks = Figure_4_Plotting_Information[[b]]$Breaks, freq = F, Tolerance = Tolerance)
        } else if (Show_Moments_in_Figure_4 == F) {
          Histogram_Information <- hist(Overlapping_Areas_List[[b]][[i]], xlim = Figure_4_Plotting_Information[[b]]$Horizontal_Axis_Limits, breaks = Figure_4_Plotting_Information[[b]]$Breaks, freq = F)
          list(Vertical_Axis_Limits = c(0, max(Histogram_Information$density)), Text_Vertical_Coordinate = NA)
        }
      })
    }), Parameters)
    dev.off()
    file.remove(paste0(Working_Directory, "/", Fourth_Figure_Name, ".jpeg"))
    Figure_4_Vertical_Axis_Stuff <- lapply(Figure_4_Vertical_Axis_Stuff, function (x) {
      x[[which.max(sapply(x, function (z) {
        z$Vertical_Axis_Limits[2]
      }))]]
    })
    jpeg(paste0(Working_Directory, "/", Fourth_Figure_Name, ".jpeg"), height = sum(Figure_4_Heights) * Figure_4_Figure_Height_Constant, width = sum(Figure_4_Widths) * Figure_4_Figure_Width_Constant)
    layout(Figure_4_Layout_Matrix, heights = Figure_4_Heights, widths = Figure_4_Widths)
    lapply(seq_along(Parameters), function (b) {
      par(mar = c(1, 1, 1, 1))
      plot(0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n", axes = F, xlab = "", ylab = "")
      text(0, 0, bquote(bold(.(paste0("Parameter: ", Parameters[b])))), cex = Figure_4_Parameter_Title_Text_Size)
      lapply(seq_along(Unique_Treatments), function (x) {
        lapply(1:2, function (y) {
          plot(0, xlim = c(-1, 1), ylim = c(-1, 1), type = "n", axes = F, xlab = "", ylab = "")
          text(0, 0, bquote(bold(.(paste0(Treatment_Name, ": ", Unique_Treatments[x])))), cex = Figure_4_Treatment_Title_Text_Size, srt = ifelse(y == 1, 0, 90))
        })
      })
      par(mar = c(Figure_4_Individual_Plot_Bottom_Margin, Figure_4_Individual_Plot_Left_Margin, Figure_4_Individual_Plot_Top_Margin, Figure_4_Individual_Plot_Right_Margin), mgp = c(3 + (Figure_4_Axis_Labels_and_Axis_Titles_Shifting_Constant * 3), 1 + Figure_4_Axis_Labels_and_Axis_Titles_Shifting_Constant, 0))
      lapply(seq_along(Overlapping_Areas_List[[b]]), function (i) {
        hist(Overlapping_Areas_List[[b]][[i]], xlim = Figure_4_Plotting_Information[[b]]$Horizontal_Axis_Limits, breaks = Figure_4_Plotting_Information[[b]]$Breaks, ylim = Figure_4_Vertical_Axis_Stuff[[b]]$Vertical_Axis_Limits, freq = F, xlab = "Overlapping Area", main = "", cex.axis = Figure_4_Axis_Labels_Text_Size, cex.lab = Figure_4_Axis_Titles_Text_Size)
        Moments <- unlist(Overlapping_Area_Moments[[b]][[i]])
        if (Show_Moments_in_Figure_4 == T) {
          text(0.5, Figure_4_Vertical_Axis_Stuff[[b]]$Text_Vertical_Coordinate, labels = paste(paste0(names(Moments), ": ", ifelse(is.finite(Moments), as.character(round(Moments, Moment_Rounding_Constant)), "Undefined")), collapse = "\n"), cex = Figure_4_Moment_Text_Size)
        }
      })
      par(mar = c(1, 1, 1, 1), mgp = c(3, 1, 0))
      if (Parameters[b] != Parameters[length(Parameters)]) {
        plot(0, type = "n", xlab = "", ylab = "", axes = F, xlim = c(-1, 1), ylim = c(-1, 1))
        abline(h = 0, lwd = Figure_4_Panel_Separation_Line_Width)
      }
    })
    dev.off()
  }
  Coefficients_Moments_Data_Frame <- do.call("rbind", mapply(function (x, y) {
    do.call("rbind", mapply(function (a, b) {
      do.call("rbind", mapply(function (u, v) {
        Moment_Data <- as.data.frame(t(u))
        Moment_Data <- cbind(b, v, y, Moment_Data)
        colnames(Moment_Data)[1:3] <- c(Treatment_Name, Replicate_Name, "Parameter")
        Moment_Data
      }, u = a, v = names(a), SIMPLIFY = F))
    }, a = x, b = names(x), SIMPLIFY = F))
  }, x = Coefficient_Moments, y = names(Coefficient_Moments), SIMPLIFY = F))
  rownames(Coefficients_Moments_Data_Frame) <- NULL
  Overlapping_Area_Data_Frame_1 <- do.call("rbind", lapply(seq_along(Parameters), function (i) {
    do.call("rbind", lapply(seq_along(Overlapping_Area_Data_List[[i]]), function (j) {
      do.call("rbind", lapply(seq_along(Overlapping_Area_Data_List[[i]][[j]]), function (k) {
        l <- Overlapping_Area_Data_List[[i]][[j]][[k]]
        x <- Overlapping_Areas_List[[i]][[j]]
        Experimental_Units_in_the_Comparison <- strsplit(l$Plot_Title, "; ")[[1]]
        Overlapping_Area <- unique(c(x[which(rownames(x) == Experimental_Units_in_the_Comparison[2]), which(colnames(x) == Experimental_Units_in_the_Comparison[1])], x[which(rownames(x) == Experimental_Units_in_the_Comparison[1]), which(colnames(x) == Experimental_Units_in_the_Comparison[2])]))
        if (l$Make_a_Plot == T) {
          g <- cbind(data.frame(Coefficient = l$Parameter, First_Experimental_Unit_Treatment = l$First_Treatment, First_Experimental_Unit_Replicate = l$First_Replicate_Name, Second_Experimental_Unit_Treatment = l$Second_Treatment, Second_Experimental_Unit_Replicate = l$Second_Replicate_Name, Comparison = gsub("; ", " and ", l$Plot_Title), Overlapping_Area = Overlapping_Area), t(l$Moments))
        } else if (l$Make_a_Plot == F) {
          Moments <- setNames(rep(NA, Moments_to_Report), Moment_Names)
          g <- cbind(data.frame(Coefficient = l$Parameter, First_Experimental_Unit_Treatment = l$First_Treatment, First_Experimental_Unit_Replicate = l$First_Replicate_Name, Second_Experimental_Unit_Treatment = l$Second_Treatment, Second_Experimental_Unit_Replicate = l$Second_Replicate_Name, Comparison = gsub("; ", " and ", l$Plot_Title), Overlapping_Area = Overlapping_Area), t(Moments))
        }
        colnames(g)[which(colnames(g) == "First_Experimental_Unit_Treatment")] <- paste("First Experimental Unit", Treatment_Name)
        colnames(g)[which(colnames(g) == "First_Experimental_Unit_Replicate")] <- paste("First Experimental Unit", Replicate_Name)
        colnames(g)[which(colnames(g) == "Second_Experimental_Unit_Treatment")] <- paste("Second Experimental Unit", Treatment_Name)
        colnames(g)[which(colnames(g) == "Second_Experimental_Unit_Replicate")] <- paste("Second Experimental Unit", Replicate_Name)
        g
      }))
    }))
  }))
  rownames(Overlapping_Area_Data_Frame_1) <- NULL
  Overlapping_Area_Data_Frame_2 <- do.call("rbind", lapply(seq_along(Parameters), function (i) {
    do.call("rbind", lapply(seq_along(Overlapping_Areas_List[[i]]), function (j) {
      Treatments <- gsub(paste0(Treatment_Name, " "), "", strsplit(names(Overlapping_Areas_List[[i]])[j], " and ")[[1]])
      g <- cbind(Coefficient = Parameters[i], First_Treatment = Treatments[1], Second_Treatment = Treatments[2], Comparison = names(Overlapping_Areas_List[[i]])[j], as.data.frame(t(unlist(Overlapping_Area_Moments[[i]][[j]]))))
      colnames(g)[which(colnames(g) == "First_Treatment")] <- paste("First", Treatment_Name)
      colnames(g)[which(colnames(g) == "Second_Treatment")] <- paste("Second", Treatment_Name)
      g
    }))
  }))
  rownames(Overlapping_Area_Data_Frame_2) <- NULL
  list(Coefficients_Moments_Data_Frame = Coefficients_Moments_Data_Frame, Overlapping_Area_Data_Frame_1 = Overlapping_Area_Data_Frame_1, Overlapping_Area_Data_Frame_2 = Overlapping_Area_Data_Frame_2)
}