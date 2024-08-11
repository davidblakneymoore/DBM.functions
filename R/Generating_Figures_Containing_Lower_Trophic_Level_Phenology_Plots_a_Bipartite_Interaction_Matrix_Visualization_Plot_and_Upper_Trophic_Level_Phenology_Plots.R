#' Generating Figures Containing Lower-Trophic-Level Phenology Plots, a Bipartite Interaction Matrix Visualization Plot, and Upper-Trophic-Level Phenology Plots
#'
#' This function generates violin-like phenology plots that depict the relative abundances of lower-trophic-level and higher-trophic-level species over time. It also generates bipartite interaction matrix visualization plots which are positioned centrally between the two sets of phenology plots.
#'
#' `Generating_Figures_Containing_Lower_Trophic_Level_Phenology_Plots_a_Bipartite_Interaction_Matrix_Visualization_Plot_and_Upper_Trophic_Level_Phenology_Plots` generates violin-like phenology plots that depict the relative abundances of lower-trophic-level and higher-trophic-level species over time. It also generates bipartite interaction matrix visualization plots which are positioned centrally between the two sets of phenology plots.
#'
#' In bipartite parlance, 'nodes' refer to species (from either trophic level) and 'edges' refer to interactions between these species. Bipartite networks are special because species interact only across, and not within, trophic levels.
#'
#' This function was heavily inspired by a manuscript that contained a figure very similar to the one this function returns (Russo et al., 2013). It was also heavily inspired by my colleague Isaac Ativor's work in plant-pollinator interactions.
#'
#' This function relies on the \link[DBM.functions]{Attempting_to_Diagonalize_a_Matrix_Based_on_Its_Full_Diagonal} function and the \link[DBM.functions]{Making_Reasonable_Scale_Bars} function, which are both also found in the `DBM.functions` package.
#'
#' @param List_of_Bipartite_Interaction_Matrices_by_Date a list of matrices. Each matrix contains quantitative information about the interactions between lower-trophic-level species and higher-trophic-level species. Each row must represent a lower-trophic-level species and each column must represent a higher-trophic-level species; similarly, the row names must be the names of the lower-trophic-level species and the column names must be the names of the higher-trophic-level species. Finally, the names of each matrix must be the date the observations were made, with the dates following the typical `"Date"` format that R uses (`"%Y-%m-%d"`). No missing values are allowed in the matrices.
#' @param Overall_Plot_Title the overall figure title. The default value, `NULL`, means that an overall figure title will be omitted.
#' @param Lower_Trophic_Level_Plot_Title a name for the lower trophic level, such as `"Plants"` or `"Prey"`. The default value, `"Lower Trophic Level"`, is a generic title that can be used in any figure.
#' @param Higher_Trophic_Level_Plot_Title a name for the higher trophic level, such as `"Pollinators"` or `"Predators"`. The default value, `"Higher Trophic Level"`, is a generic title that can be used in any figure.
#' @param Prioritize_Interaction_Strength_Over_Alphabetization_in_Species_Layout whether species should be arranged such that species from different trophic levels that interact more frequently are closer together and species that interact less frequently are farther apart on the bipartite interaction visualization plot. If putting frequently interacting species closer together and infrequently interacting species farther apart is prioritized, the species will probably not be arranged alphabetically in the resulting figure. The default value, `FALSE`, causes the species from both trophic levels to be arranged alphabetically.
#' @param Include_Scale_Bars whether vertical scale bars should be included on the left and right sides of the figure. The default value, `TRUE`, causes scale bars to be included.
#' @param Scale_Bar_Factor a number between 0 and 1 (exclusive) that specifies approximately how tall the scale bar will be relative to the vertical axis range. The default value for this argument, `0.25`, makes it so that the scale bar will take up as close to 25 % of the vertical axis' range as possible.
#' @param Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool the approximate proportion of lower-trophic-level species to pool into a 'other species' group. First, lower-trophic-level species are ordered by their total numbers of occurrences, and then this argument is used to determine the threshold for inclusion in the figure. Species that do not meet this threshold (that have low numbers of occurrences) are pooled into an 'other species' group. The default value, `0`, means that no species will be pooled.
#' @param Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool  the approximate proportion of higher-trophic-level species to pool into a 'other species' group. First, higher-trophic-level species are ordered by their total numbers of occurrences, and then this argument is used to determine the threshold for inclusion in the figure. Species that do not meet this threshold (that have low numbers of occurrences) are pooled into an 'other species' group. The default value, `0`, means that no species will be pooled.
#' @param Lower_Trophic_Level_Color the color of the violin-like phenology plots and the bars (from the bipartite interaction visualization plot) for the lower-trophic-level species. The default value to this argument is `"darkgreen"`.
#' @param Higher_Trophic_Level_Color the color of the violin-like phenology plots and the bars (from the bipartite interaction visualization plot) for the higher-trophic-level species. The default value to this argument is `"darkred"`.
#' @param Interaction_Color the color of the edges (from the bipartite interaction visualization plot) for the interactions between the lower-trophic-level species and the higher-trophic-level species. The default value to this argument is `"black"`.
#' @param How_to_Treat_Missing_Values a character string that specifies how to handle missing values in the matrices. The three options for this argument are one of `"Remove the Rows"`, `"Remove the Columns"`, and `"Make Missing Values Zeros"`. The `"Make Missing Values Zeros"` option is not recommended because missing values do not necessarily mean that the species (the node) was not present. The default value, `"Remove the Columns"`, means that columns, not rows, which contain missing values will be removed (the lower-trophic-level species are preserved at the expense of the higher-trophic-level species).
#' @param Relative_Plot_Widths the relative widths of the lower-trophic-level violin-like phenology plots, the plot containing lower-trophic-level species names, the bipartite interaction visualization plot, the plot containing the higher-trophic-level species names, and the higher-trophic-level violin-like phenology plots. This argument must be a numeric vector containing 5 positive numbers, and it must be the same forward and backward to ensure that the resulting figure is symmetric. The default value to this argument is `c(11, 4, 10, 4, 11)`.
#' @param Relative_Plot_Heights the relative heights of the plots containing the titles and all the other plots. This argument must be a numeric vector contain two (if an overall figure title is not to be included) or three (if an overall figure title is to be included) positive numbers. The default value to this argument, `c(rep(1, ifelse(is.null(Overall_Plot_Title), 1, 2)), ifelse(is.null(Overall_Plot_Title), 9, 8))`, means that when there is no overall figure title, the heights will be `c(1, 9)`, and when there is an overall figure title, the heights will be `c(1, 8, 8)`.
#' @param Row_of_Already_Pooled_Species_Name the row name representing already-pooled species (such as miscellaneous species or other species), if there is a row of this nature present. The default value, `NULL`, assumes that there are no already-pooled rows. This pooled group will be placed at the bottom of the figure, or it will be pooled with other rows if an appropriate 'Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool' argument is provided.
#' @param Column_of_Already_Pooled_Species_Name the column name representing already-pooled species (such as miscellaneous species or other species), if there is a column of this nature present. The default value, `NULL`, assumes that there are no already-pooled columns. This pooled group will be placed at the bottom of the figure, or it will be pooled with other columns if an appropriate 'Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool' argument is provided.
#' @param Gap_Size_Constant a nonnegative number that specifies how much space is allocated between bars vertically (within trophic levels) in the bipartite interaction visualization plot, where each bar represents a species. Using higher numbers for this argument will lead to more space between species' bars. The default value, `0.2`, specifies that 20 % of the cumulative bar heights (without any space) will be divided up equally between the bars in this plot.
#' @param Relative_Bar_Width a number between `0` and `1` (inclusive) that specifies how wide the bars representing species are in the bipartite interaction visualization plot. The default value, `0.2`, specifies that 20 % of the plotting areas' widths are to be taken up by the bars for both trophic levels combined. The remaining 80 % of the space will be taken up by the interactions (the edges).
#' @param Horizontal_Scale_Text_Shift_Constant a positive number specifying the relative horizontal distance (relative to the overall horizontal axis range) the scale bar text will be from the scale bar. The default value to this argument is `0.1`.
#' @param Vertical_Scale_Title_Shift_Constant a positive number specifying the relative vertical distance (relative to the overall vertical axis range) the scale bar title will be from the top of the scale bar. The default value to this argument is `0.1`.
#' @param Overall_Plot_Title_Font_Size the relative size of the overall figure title. This argument must be a single positive number. The default value to this argument is `3.25`.
#' @param Trophic_Level_Plot_Title_Font_Size the relative size of the titles for the lower-trophic-level and higher-trophic-level title plots. This argument must be a single positive number. The default value to this argument is `2.5`.
#' @param Phenology_and_Bipartite_Plot_Title_Font_Size the relative size of the titles of the lower-trophic-level violin-like phenology plots, the plot containing lower-trophic-level species names, the bipartite interaction visualization plot, the plot containing the higher-trophic-level species names, and the higher-trophic-level violin-like phenology plots. This argument must be a single positive number. The default value to this argument is `1.5`.
#' @param Species_Name_Font_Size the relative size of the species names for the lower-trophic-level violin-like phenology plots and the higher-trophic-level violin-like phenology plots. This argument must be a single positive number. The default value to this argument is `1.5`.
#' @param Horizontal_Axis_Title_Font_Size the relative size of the horizontal axis titles for the lower-trophic-level violin-like phenology plots and the higher-trophic-level violin-like phenology plots. This argument must be a single positive number. The default value to this argument is `1`.
#' @param Horizontal_Axis_Dates_Font_Size the relative size of the dates on the horizontal axes of the lower-trophic-level violin-like phenology plots and higher-trophic-level violin-like phenology plots. This argument must be a single positive number. The default value to this argument is `1`.
#' @param Scale_Bar_Title_Font_Size the relative size of the scale bar titles. This argument must be a single positive number. The default value to this argument is `1`.
#' @param Scale_Bar_Text_Font_Size the relative size of the scale bar text. This argument must be a single positive number. The default value to this argument is `1`.
#' @param Left_and_Right_Figure_Margins the relative distance between the left figure margin and the left edge of the lower-trophic-level violin-like phenology plots, and the relative distance between the right figure margin and the right edge of the higher-trophic-level violin-like phenology plots. This argument must be a single positive number. The default value to this argument is `9`.
#' @param Between_Title_Plots_and_Other_Plots_Margin the relative distance between the bottom of the title plots (which are on the top row of the figure) and the top of the plots on the bottom row of the figure. This argument must be a single positive number. The default value to this argument is `1`.
#' @param Bottom_Margin the relative distance between the edges of the plots on the bottom row of the figure and the lower figure margin. This argument must be a single positive number. The default value to this argument is `8`.
#'
#' @return This function returns a figure with violin-like phenology plots showing the abundances of species from the lower trophic levels over time on the left side of the figure and violin-like phenology plots showing the abundances of species from the higher trophic levels over time on the right side of the figure. In the middle of these two is a bipartite interaction matrix visualization which shows how the two trophic levels interacted.
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @references Russo, L., N. DeBarros, S. Yang, K. Shea, and D. Mortensen. 2013. Supporting crop pollinators with floral resources: network-based phenological matching. Ecol. Evol. 3:3125-3140.
#'
#' @examples
#' # Generate Made-Up Data
#' set.seed(21)
#' List_of_Bipartite_Interaction_Matrices_by_Date_1 <- list("2020-04-05" = matrix(round(rchisq(28, 6)), ncol = 7), "2020-05-10" = matrix(round(rf(28, 5, 2)), ncol = 7), "2020-06-09" = matrix(round(rlnorm(28)), ncol = 7), "2020-07-13" = matrix(round(rexp(28)), ncol = 7))
#' List_of_Bipartite_Interaction_Matrices_by_Date_1 <- lapply(List_of_Bipartite_Interaction_Matrices_by_Date_1, function (x) {
#'   rownames(x) <- paste("Species", letters[seq_len(nrow(x))])
#'   colnames(x) <- paste("Species", letters[seq_len(ncol(x))])
#'   x
#' })
#' List_of_Bipartite_Interaction_Matrices_by_Date_2 <- list("2020-04-05" = matrix(round(rchisq(28, 6)), ncol = 7), "2020-05-10" = matrix(round(rf(28, 5, 2)), ncol = 7), "2020-06-09" = matrix(round(rlnorm(28)), ncol = 7), "2020-07-13" = matrix(round(rexp(28)), ncol = 7))
#' List_of_Bipartite_Interaction_Matrices_by_Date_2 <- lapply(List_of_Bipartite_Interaction_Matrices_by_Date_2, function (x) {
#'   rownames(x) <- paste("Species", letters[seq_len(nrow(x))])
#'   colnames(x) <- c(paste("Species", LETTERS[seq_len(ncol(x) - 1)]), "Miscellaneous Species")
#'   x
#' })
#'
#' # Example Figure 1
#' Generating_Figures_Containing_Lower_Trophic_Level_Phenology_Plots_a_Bipartite_Interaction_Matrix_Visualization_Plot_and_Upper_Trophic_Level_Phenology_Plots(List_of_Bipartite_Interaction_Matrices_by_Date_1, Include_Scale_Bars = FALSE)
#'
#' # Example Figure 2
#' Generating_Figures_Containing_Lower_Trophic_Level_Phenology_Plots_a_Bipartite_Interaction_Matrix_Visualization_Plot_and_Upper_Trophic_Level_Phenology_Plots(List_of_Bipartite_Interaction_Matrices_by_Date_1, Scale_Bar_Factor = (1 / 3), Prioritize_Interaction_Strength_Over_Alphabetization_in_Species_Layout = TRUE, Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool = 0.25, Overall_Plot_Title = "Data From All Treatments Combined", Lower_Trophic_Level_Plot_Title = "Plants", Higher_Trophic_Level_Plot_Title = "Pollinators")
#'
#' # Example Figure 3
#' Generating_Figures_Containing_Lower_Trophic_Level_Phenology_Plots_a_Bipartite_Interaction_Matrix_Visualization_Plot_and_Upper_Trophic_Level_Phenology_Plots(List_of_Bipartite_Interaction_Matrices_by_Date_2, Scale_Bar_Factor = (1 / 3), Column_of_Already_Pooled_Species_Name = "Miscellaneous Species", Species_Name_Font_Size = 1.25)
#'
#' @export
Generating_Figures_Containing_Lower_Trophic_Level_Phenology_Plots_a_Bipartite_Interaction_Matrix_Visualization_Plot_and_Upper_Trophic_Level_Phenology_Plots <- function (List_of_Bipartite_Interaction_Matrices_by_Date, Overall_Plot_Title = NULL, Lower_Trophic_Level_Plot_Title = "Lower Trophic Level", Higher_Trophic_Level_Plot_Title = "Higher Trophic Level", Prioritize_Interaction_Strength_Over_Alphabetization_in_Species_Layout = FALSE, Lower_Trophic_Level_Color = "darkgreen", Higher_Trophic_Level_Color = "darkred", Interaction_Color = "black", How_to_Treat_Missing_Values = "Remove the Columns", Include_Scale_Bars = TRUE, Scale_Bar_Factor = (1 / 3), Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool = 0, Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool = 0, Relative_Plot_Widths = c(11, 4, 10, 4, 11), Relative_Plot_Heights = c(rep(1, ifelse(is.null(Overall_Plot_Title), 1, 2)), ifelse(is.null(Overall_Plot_Title), 9, 8)), Row_of_Already_Pooled_Species_Name = NULL, Column_of_Already_Pooled_Species_Name = NULL, Gap_Size_Constant = 0.2, Relative_Bar_Width = 0.2, Horizontal_Scale_Text_Shift_Constant = 0.1, Vertical_Scale_Title_Shift_Constant = 0.1, Scale_Bar_Title_Font_Size = 1, Scale_Bar_Text_Font_Size = 1, Overall_Plot_Title_Font_Size = 3.25, Trophic_Level_Plot_Title_Font_Size = 2.5, Phenology_and_Bipartite_Plot_Title_Font_Size = 1.5, Species_Name_Font_Size = 1.5, Horizontal_Axis_Title_Font_Size = 1, Horizontal_Axis_Dates_Font_Size = 1, Left_and_Right_Figure_Margins = 9, Between_Title_Plots_and_Other_Plots_Margin = 1, Bottom_Margin = 8) {
  if (!is.list(List_of_Bipartite_Interaction_Matrices_by_Date)) {
    stop("The 'List_of_Bipartite_Interaction_Matrices_by_Date' argument must be a list.")
  }
  if (!all(sapply(List_of_Bipartite_Interaction_Matrices_by_Date, function (x) {
    is.matrix(x)
  }))) {
    stop ("Each element of the 'List_of_Bipartite_Interaction_Matrices_by_Date' argument must be a matrix.")
  }
  if ((!is.vector(apply(sapply(List_of_Bipartite_Interaction_Matrices_by_Date, function (x) {
    dim(x)
  }), 1, unique))) | (length(apply(sapply(List_of_Bipartite_Interaction_Matrices_by_Date, function (x) {
    dim(x)
  }), 1, unique)) != 2)) {
    stop ("The matrix elements of the 'List_of_Bipartite_Interaction_Matrices_by_Date' argument must all have the same dimensions.")
  }
  if (!all(sapply(List_of_Bipartite_Interaction_Matrices_by_Date, is.numeric))) {
    stop ("The elements of the 'List_of_Bipartite_Interaction_Matrices_by_Date' argument, which are all matrices, must be numeric.")
  }
  if (all(is.na(tryCatch (as.Date(names(List_of_Bipartite_Interaction_Matrices_by_Date)), error = function (e) {
    NA
  })))) {
    stop ("The names of the 'List_of_Bipartite_Interaction_Matrices_by_Date' argument must all be in proper 'Date' syntax ('%Y-%m-%d').")
  }
  Row_Names <- lapply(List_of_Bipartite_Interaction_Matrices_by_Date, rownames)
  if (any(sapply(Row_Names, is.null))) {
    stop ("Row names must be provided for all matrices in the 'List_of_Bipartite_Interaction_Matrices_by_Date' argument.")
  }
  if (any(sapply(Row_Names, function (x) {
    any(is.na(x))
  }))) {
    stop ("There may not be any missing row names in any matrices in the 'List_of_Bipartite_Interaction_Matrices_by_Date' argument.")
  }
  Identical_Row_Names <- NULL
  k <- 1
  for (i in 1:(length(Row_Names) - 1)) {
    for (j in (i + 1):length(Row_Names)) {
      Identical_Row_Names[k] <- all(Row_Names[[i]] %in% Row_Names[[j]]) & all(Row_Names[[j]] %in% Row_Names[[i]])
      k <- k + 1
    }
  }
  if (!all(Identical_Row_Names)) {
    stop ("The matrix elements of the 'List_of_Bipartite_Interaction_Matrices_by_Date' argument must all contain the same row names.")
  }
  Column_Names <- lapply(List_of_Bipartite_Interaction_Matrices_by_Date, colnames)
  if (any(sapply(Column_Names, is.null))) {
    stop ("Column names must be provided for all matrices in the 'List_of_Bipartite_Interaction_Matrices_by_Date' argument.")
  }
  if (any(sapply(Column_Names, function (x) {
    any(is.na(x))
  }))) {
    stop ("There may not be any missing column names in any matrices in the 'List_of_Bipartite_Interaction_Matrices_by_Date' argument.")
  }
  Identical_Column_Names <- NULL
  k <- 1
  for (i in 1:(length(Column_Names) - 1)) {
    for (j in (i + 1):length(Column_Names)) {
      Identical_Column_Names[k] <- all(Column_Names[[i]] %in% Column_Names[[j]]) & all(Column_Names[[j]] %in% Column_Names[[i]])
      k <- k + 1
    }
  }
  if (!all(Identical_Column_Names)) {
    stop ("The matrix elements of the 'List_of_Bipartite_Interaction_Matrices_by_Date' argument must all contain the same column names.")
  }
  if (!is.null(Overall_Plot_Title)) {
    if (length(Overall_Plot_Title) != 1) {
      stop ("The 'Overall_Plot_Title' argument must be of length 1.")
    }
    if (is.na(Overall_Plot_Title)) {
      stop ("The 'Overall_Plot_Title' argument must not be missing.")
    }
    if (!is.character(Overall_Plot_Title)) {
      stop ("The 'Overall_Plot_Title' must be a character string.")
    }
  }
  if (length(Lower_Trophic_Level_Plot_Title) != 1) {
    stop ("The 'Lower_Trophic_Level_Plot_Title' argument must be of length 1.")
  }
  if (is.na(Lower_Trophic_Level_Plot_Title)) {
    stop ("The 'Lower_Trophic_Level_Plot_Title' argument must not be missing.")
  }
  if (!is.character(Lower_Trophic_Level_Plot_Title)) {
    stop ("The 'Lower_Trophic_Level_Plot_Title' must be a character string.")
  }
  if (length(Higher_Trophic_Level_Plot_Title) != 1) {
    stop ("The 'Higher_Trophic_Level_Plot_Title' argument must be of length 1.")
  }
  if (is.na(Higher_Trophic_Level_Plot_Title)) {
    stop ("The 'Higher_Trophic_Level_Plot_Title' argument must not be missing.")
  }
  if (!is.character(Higher_Trophic_Level_Plot_Title)) {
    stop ("The 'Higher_Trophic_Level_Plot_Title' must be a character string.")
  }
  if (length(Prioritize_Interaction_Strength_Over_Alphabetization_in_Species_Layout) != 1) {
    stop ("The 'Prioritize_Interaction_Strength_Over_Alphabetization_in_Species_Layout' argument must be of length 1.")
  }
  if (is.na(Prioritize_Interaction_Strength_Over_Alphabetization_in_Species_Layout)) {
    stop ("The 'Prioritize_Interaction_Strength_Over_Alphabetization_in_Species_Layout' argument must not be missing.")
  }
  if (!is.logical(Prioritize_Interaction_Strength_Over_Alphabetization_in_Species_Layout)) {
    stop ("The 'Prioritize_Interaction_Strength_Over_Alphabetization_in_Species_Layout' must be logical (either 'TRUE' or 'FALSE).")
  }
  if (length(Include_Scale_Bars) != 1) {
    stop ("The 'Include_Scale_Bars' argument must be of length 1.")
  }
  if (is.na(Include_Scale_Bars)) {
    stop ("The 'Include_Scale_Bars' argument must not be missing.")
  }
  if (!is.logical(Include_Scale_Bars)) {
    stop ("The 'Include_Scale_Bars' must be logical (either 'TRUE' or 'FALSE).")
  }
  if (length(Scale_Bar_Factor) != 1) {
    stop ("The 'Scale_Bar_Factor' argument must be of length 1.")
  }
  if (is.na(Scale_Bar_Factor)) {
    stop ("The 'Scale_Bar_Factor' argument must must be provided.")
  }
  if (!is.numeric(Scale_Bar_Factor)) {
    stop ("The 'Scale_Bar_Factor' argument must be a number.")
  }
  if ((Scale_Bar_Factor >= 1) | (Scale_Bar_Factor <= 0)) {
    stop ("The 'Scale_Bar_Factor' argument must be between '0' and '1' (exclusive).")
  }
  if (length(Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool) != 1) {
    stop ("The 'Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool' argument must be of length 1.")
  }
  if (is.na(Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool)) {
    stop ("The 'Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool' argument must must be provided.")
  }
  if (!is.numeric(Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool)) {
    stop ("The 'Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool' argument must be a number.")
  }
  if ((Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool >= 1) | (Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool < 0)) {
    stop ("The 'Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool' argument must be between '0' (inclusive) and '1' (exclusive).")
  }
  if (length(Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool) != 1) {
    stop ("The 'Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool' argument must be of length 1.")
  }
  if (is.na(Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool)) {
    stop ("The 'Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool' argument must must be provided.")
  }
  if (!is.numeric(Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool)) {
    stop ("The 'Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool' argument must be a number.")
  }
  if ((Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool >= 1) | (Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool < 0)) {
    stop ("The 'Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool' argument must be between '0' (inclusive) and '1' (exclusive).")
  }
  if (length(Lower_Trophic_Level_Color) != 1) {
    stop ("The 'Lower_Trophic_Level_Color' argument must be of length 1.")
  }
  if (is.na(Lower_Trophic_Level_Color)) {
    stop ("The 'Lower_Trophic_Level_Color' argument must must be provided.")
  }
  if (!setNames(sapply(Lower_Trophic_Level_Color, function (x) {
    tryCatch(is.matrix(col2rgb(x)), error = function (e) {
      FALSE
    })
  }), NULL)) {
    stop ("The 'Lower_Trophic_Level_Color' argument must be an R color.")
  }
  if (length(Higher_Trophic_Level_Color) != 1) {
    stop ("The 'Higher_Trophic_Level_Color' argument must be of length 1.")
  }
  if (is.na(Higher_Trophic_Level_Color)) {
    stop ("The 'Higher_Trophic_Level_Color' argument must must be provided.")
  }
  if (!setNames(sapply(Higher_Trophic_Level_Color, function (x) {
    tryCatch(is.matrix(col2rgb(x)), error = function (e) {
      FALSE
    })
  }), NULL)) {
    stop ("The 'Higher_Trophic_Level_Color' argument must be an R color.")
  }
  if (length(Interaction_Color) != 1) {
    stop ("The 'Interaction_Color' argument must be of length 1.")
  }
  if (is.na(Interaction_Color)) {
    stop ("The 'Interaction_Color' argument must must be provided.")
  }
  if (!setNames(sapply(Interaction_Color, function (x) {
    tryCatch(is.matrix(col2rgb(x)), error = function (e) {
      FALSE
    })
  }), NULL)) {
    stop ("The 'Interaction_Color' argument must be an R color.")
  }
  if (length(How_to_Treat_Missing_Values) != 1) {
    stop ("The 'How_to_Treat_Missing_Values' argument must be of length 1.")
  }
  if (is.na(How_to_Treat_Missing_Values)) {
    stop ("The 'How_to_Treat_Missing_Values' argument must must be provided.")
  }
  if (!is.character(How_to_Treat_Missing_Values)) {
    stop ("The 'How_to_Treat_Missing_Values' argument must be a character string.")
  }
  if (!(How_to_Treat_Missing_Values %in% c("Make Missing Values Zeros", "Remove the Rows", "Remove the Columns"))) {
    stop ("The 'How_to_Treat_Missing_Values' argument must be either 'Make Missing Values Zeros', 'Remove the Rows', and 'Remove the Columns'.")
  }
  if (length(Relative_Plot_Widths) != 5) {
    stop ("The 'Relative_Plot_Widths' argument must be of length 5.")
  }
  if (any(is.na(Relative_Plot_Widths))) {
    stop ("The 'Relative_Plot_Widths' argument must contain no missing values.")
  }
  if (!is.numeric(Relative_Plot_Widths)) {
    stop ("The 'Relative_Plot_Widths' argument must be numeric.")
  }
  if (any(Relative_Plot_Widths <= 0)) {
    stop ("Each element of the 'Relative_Plot_Widths' argument must be positive.")
  }
  if (!all(rev(Relative_Plot_Widths) == Relative_Plot_Widths)) {
    stop ("The 'Relative_Plot_Widths' argument must be a palindrome for the figure to be symmetric.")
  }
  if (is.null(Overall_Plot_Title)) {
    if (length(Relative_Plot_Heights) != 2) {
      stop ("Since there is not an additional plot at the top of the figure for the overall figure title (the 'Overall_Plot_Title' argument), the 'Relative_Plot_Heights' argument must be of length 2.")
    }
  } else if (!is.null(Overall_Plot_Title)) {
    if (length(Relative_Plot_Heights) != 3) {
      stop ("Since there is an additional plot at the top of the figure for the overall figure title (the 'Overall_Plot_Title' argument), the 'Relative_Plot_Heights' argument must be of length 3.")
    }
  }
  if (any(is.na(Relative_Plot_Heights))) {
    stop ("The 'Relative_Plot_Heights' argument must contain no missing values.")
  }
  if (!is.numeric(Relative_Plot_Heights)) {
    stop ("The 'Relative_Plot_Heights' argument must be numeric.")
  }
  if (any(Relative_Plot_Heights <= 0)) {
    stop ("Each element of the 'Relative_Plot_Heights' argument must be positive.")
  }
  if (!is.null(Row_of_Already_Pooled_Species_Name)) {
    if (length(Row_of_Already_Pooled_Species_Name) != 1) {
      stop ("The 'Row_of_Already_Pooled_Species_Name' argument must be of length 1.")
    }
    if (is.na(Row_of_Already_Pooled_Species_Name)) {
      stop ("The 'Row_of_Already_Pooled_Species_Name' argument must either be provided or be 'NULL'.")
    }
    if (!is.character(Row_of_Already_Pooled_Species_Name)) {
      stop ("The 'Row_of_Already_Pooled_Species_Name' argument must be a character string.")
    }
    if (!(Row_of_Already_Pooled_Species_Name %in% apply(as.matrix(as.data.frame(Row_Names)), 1, unique))) {
      stop ("The 'Row_of_Already_Pooled_Species_Name' argument must be a row name in each of the matrices in the 'List_of_Bipartite_Interaction_Matrices_by_Date' object.")
    }
  }
  if (!is.null(Column_of_Already_Pooled_Species_Name)) {
    if (length(Column_of_Already_Pooled_Species_Name) != 1) {
      stop ("The 'Column_of_Already_Pooled_Species_Name' argument must be of length 1.")
    }
    if (is.na(Column_of_Already_Pooled_Species_Name)) {
      stop ("The 'Column_of_Already_Pooled_Species_Name' argument must either be provided or be 'NULL'.")
    }
    if (!is.character(Column_of_Already_Pooled_Species_Name)) {
      stop ("The 'Column_of_Already_Pooled_Species_Name' argument must be a character string.")
    }
    if (!(Column_of_Already_Pooled_Species_Name %in% apply(as.matrix(as.data.frame(Column_Names)), 1, unique))) {
      stop ("The 'Column_of_Already_Pooled_Species_Name' argument must be a column name in each of the matrices in the 'List_of_Bipartite_Interaction_Matrices_by_Date' object.")
    }
  }
  if ((Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool > 0) & (!is.null(Row_of_Already_Pooled_Species_Name))) {
    stop ("If there is already a row representing pooled lower-trophic-level species, additional pooling of lower-trophic-level species cannot be done. If additional pooling is desired, please do it before using this function.")
  }
  if ((Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool > 0) & (!is.null(Column_of_Already_Pooled_Species_Name))) {
    stop ("If there is already a column representing pooled higher-trophic-level species, additional pooling of higher-trophic-level species cannot be done. If additional pooling is desired, please do it before using this function.")
  }
  if (length(Gap_Size_Constant) != 1) {
    stop ("The 'Gap_Size_Constant' argument must be of length 1.")
  }
  if (is.na(Gap_Size_Constant)) {
    stop ("The 'Gap_Size_Constant' argument must be provided.")
  }
  if (!is.numeric(Gap_Size_Constant)) {
    stop ("The 'Gap_Size_Constant' argument must be a number.")
  }
  if (Gap_Size_Constant < 0) {
    stop ("The 'Gap_Size_Constant' argument must be positive.")
  }
  if (length(Relative_Bar_Width) != 1) {
    stop ("The 'Relative_Bar_Width' argument must be of length 1.")
  }
  if (is.na(Relative_Bar_Width)) {
    stop ("The 'Relative_Bar_Width' argument must must be provided.")
  }
  if (!is.numeric(Relative_Bar_Width)) {
    stop ("The 'Relative_Bar_Width' argument must be a number between '0' and '1' (inclusive).")
  }
  if ((Relative_Bar_Width > 1) | (Relative_Bar_Width < 0)) {
    stop ("The 'Relative_Bar_Width' argument must be between '0' and '1' (inclusive).")
  }
  if (length(Horizontal_Scale_Text_Shift_Constant) != 1) {
    stop ("The 'Horizontal_Scale_Text_Shift_Constant' argument must be of length 1.")
  }
  if (is.na(Horizontal_Scale_Text_Shift_Constant)) {
    stop ("The 'Horizontal_Scale_Text_Shift_Constant' argument must must be provided.")
  }
  if (!is.numeric(Horizontal_Scale_Text_Shift_Constant)) {
    stop ("The 'Horizontal_Scale_Text_Shift_Constant' argument must be a number.")
  }
  if (Horizontal_Scale_Text_Shift_Constant <= 0) {
    stop ("The 'Horizontal_Scale_Text_Shift_Constant' argument must be positive.")
  }
  if (length(Vertical_Scale_Title_Shift_Constant) != 1) {
    stop ("The 'Vertical_Scale_Title_Shift_Constant' argument must be of length 1.")
  }
  if (is.na(Vertical_Scale_Title_Shift_Constant)) {
    stop ("The 'Vertical_Scale_Title_Shift_Constant' argument must must be provided.")
  }
  if (!is.numeric(Vertical_Scale_Title_Shift_Constant)) {
    stop ("The 'Vertical_Scale_Title_Shift_Constant' argument must be a number.")
  }
  if (Vertical_Scale_Title_Shift_Constant <= 0) {
    stop ("The 'Vertical_Scale_Title_Shift_Constant' argument must be positive.")
  }
  if (length(Overall_Plot_Title_Font_Size) != 1) {
    stop ("The 'Overall_Plot_Title_Font_Size' argument must be of length 1.")
  }
  if (is.na(Overall_Plot_Title_Font_Size)) {
    stop ("The 'Overall_Plot_Title_Font_Size' argument must must be provided.")
  }
  if (!is.numeric(Overall_Plot_Title_Font_Size)) {
    stop ("The 'Overall_Plot_Title_Font_Size' argument must be a number.")
  }
  if (Overall_Plot_Title_Font_Size <= 0) {
    stop ("The 'Overall_Plot_Title_Font_Size' argument must be positive.")
  }
  if (length(Trophic_Level_Plot_Title_Font_Size) != 1) {
    stop ("The 'Trophic_Level_Plot_Title_Font_Size' argument must be of length 1.")
  }
  if (is.na(Trophic_Level_Plot_Title_Font_Size)) {
    stop ("The 'Trophic_Level_Plot_Title_Font_Size' argument must must be provided.")
  }
  if (!is.numeric(Trophic_Level_Plot_Title_Font_Size)) {
    stop ("The 'Trophic_Level_Plot_Title_Font_Size' argument must be a number.")
  }
  if (Trophic_Level_Plot_Title_Font_Size <= 0) {
    stop ("The 'Trophic_Level_Plot_Title_Font_Size' argument must be positive.")
  }
  if (length(Phenology_and_Bipartite_Plot_Title_Font_Size) != 1) {
    stop ("The 'Phenology_and_Bipartite_Plot_Title_Font_Size' argument must be of length 1.")
  }
  if (is.na(Phenology_and_Bipartite_Plot_Title_Font_Size)) {
    stop ("The 'Phenology_and_Bipartite_Plot_Title_Font_Size' argument must must be provided.")
  }
  if (!is.numeric(Phenology_and_Bipartite_Plot_Title_Font_Size)) {
    stop ("The 'Phenology_and_Bipartite_Plot_Title_Font_Size' argument must be a number.")
  }
  if (Phenology_and_Bipartite_Plot_Title_Font_Size <= 0) {
    stop ("The 'Phenology_and_Bipartite_Plot_Title_Font_Size' argument must be positive.")
  }
  if (length(Species_Name_Font_Size) != 1) {
    stop ("The 'Species_Name_Font_Size' argument must be of length 1.")
  }
  if (is.na(Species_Name_Font_Size)) {
    stop ("The 'Species_Name_Font_Size' argument must must be provided.")
  }
  if (!is.numeric(Species_Name_Font_Size)) {
    stop ("The 'Species_Name_Font_Size' argument must be a number.")
  }
  if (Species_Name_Font_Size <= 0) {
    stop ("The 'Species_Name_Font_Size' argument must be positive.")
  }
  if (length(Horizontal_Axis_Title_Font_Size) != 1) {
    stop ("The 'Horizontal_Axis_Title_Font_Size' argument must be of length 1.")
  }
  if (is.na(Horizontal_Axis_Title_Font_Size)) {
    stop ("The 'Horizontal_Axis_Title_Font_Size' argument must must be provided.")
  }
  if (!is.numeric(Horizontal_Axis_Title_Font_Size)) {
    stop ("The 'Horizontal_Axis_Title_Font_Size' argument must be a number.")
  }
  if (Horizontal_Axis_Title_Font_Size <= 0) {
    stop ("The 'Horizontal_Axis_Title_Font_Size' argument must be positive.")
  }
  if (length(Horizontal_Axis_Dates_Font_Size) != 1) {
    stop ("The 'Horizontal_Axis_Dates_Font_Size' argument must be of length 1.")
  }
  if (is.na(Horizontal_Axis_Dates_Font_Size)) {
    stop ("The 'Horizontal_Axis_Dates_Font_Size' argument must must be provided.")
  }
  if (!is.numeric(Horizontal_Axis_Dates_Font_Size)) {
    stop ("The 'Horizontal_Axis_Dates_Font_Size' argument must be a number.")
  }
  if (Horizontal_Axis_Dates_Font_Size <= 0) {
    stop ("The 'Horizontal_Axis_Dates_Font_Size' argument must be positive.")
  }
  if (length(Scale_Bar_Title_Font_Size) != 1) {
    stop ("The 'Scale_Bar_Title_Font_Size' argument must be of length 1.")
  }
  if (is.na(Scale_Bar_Title_Font_Size)) {
    stop ("The 'Scale_Bar_Title_Font_Size' argument must must be provided.")
  }
  if (!is.numeric(Scale_Bar_Title_Font_Size)) {
    stop ("The 'Scale_Bar_Title_Font_Size' argument must be a number.")
  }
  if (Scale_Bar_Title_Font_Size <= 0) {
    stop ("The 'Scale_Bar_Title_Font_Size' argument must be positive.")
  }
  if (length(Scale_Bar_Text_Font_Size) != 1) {
    stop ("The 'Scale_Bar_Text_Font_Size' argument must be of length 1.")
  }
  if (is.na(Scale_Bar_Text_Font_Size)) {
    stop ("The 'Scale_Bar_Text_Font_Size' argument must must be provided.")
  }
  if (!is.numeric(Scale_Bar_Text_Font_Size)) {
    stop ("The 'Scale_Bar_Text_Font_Size' argument must be a number.")
  }
  if (Scale_Bar_Text_Font_Size <= 0) {
    stop ("The 'Scale_Bar_Text_Font_Size' argument must be positive.")
  }
  if (length(Left_and_Right_Figure_Margins) != 1) {
    stop ("The 'Left_and_Right_Figure_Margins' argument must be of length 1.")
  }
  if (is.na(Left_and_Right_Figure_Margins)) {
    stop ("The 'Left_and_Right_Figure_Margins' argument must must be provided.")
  }
  if (!is.numeric(Left_and_Right_Figure_Margins)) {
    stop ("The 'Left_and_Right_Figure_Margins' argument must be a number.")
  }
  if (Left_and_Right_Figure_Margins <= 0) {
    stop ("The 'Left_and_Right_Figure_Margins' argument must be positive.")
  }
  if (length(Between_Title_Plots_and_Other_Plots_Margin) != 1) {
    stop ("The 'Between_Title_Plots_and_Other_Plots_Margin' argument must be of length 1.")
  }
  if (is.na(Between_Title_Plots_and_Other_Plots_Margin)) {
    stop ("The 'Between_Title_Plots_and_Other_Plots_Margin' argument must must be provided.")
  }
  if (!is.numeric(Between_Title_Plots_and_Other_Plots_Margin)) {
    stop ("The 'Between_Title_Plots_and_Other_Plots_Margin' argument must be a number.")
  }
  if (Between_Title_Plots_and_Other_Plots_Margin <= 0) {
    stop ("The 'Between_Title_Plots_and_Other_Plots_Margin' argument must be positive.")
  }
  if (length(Bottom_Margin) != 1) {
    stop ("The 'Bottom_Margin' argument must be of length 1.")
  }
  if (is.na(Bottom_Margin)) {
    stop ("The 'Bottom_Margin' argument must must be provided.")
  }
  if (!is.numeric(Bottom_Margin)) {
    stop ("The 'Bottom_Margin' argument must be a number.")
  }
  if (Bottom_Margin <= 0) {
    stop ("The 'Bottom_Margin' argument must be positive.")
  }
  if (is.null(Row_of_Already_Pooled_Species_Name)) {
    List_of_Bipartite_Interaction_Matrices_by_Date <- lapply(List_of_Bipartite_Interaction_Matrices_by_Date, function (x) {
      x[order(rownames(x)), ]
    })
  } else if (!is.null(Row_of_Already_Pooled_Species_Name)) {
    List_of_Bipartite_Interaction_Matrices_by_Date <- lapply(List_of_Bipartite_Interaction_Matrices_by_Date, function (x) {
      Already_Pooled_Row <- x[which(rownames(x) == Row_of_Already_Pooled_Species_Name), ]
      Other_Rows <- x[which(rownames(x) != Row_of_Already_Pooled_Species_Name), ]
      Other_Rows <- Other_Rows[order(colnames(Other_Rows)), ]
      y <- rbind(Other_Rows, Already_Pooled_Row)
      rownames(y)[nrow(y)] <- Row_of_Already_Pooled_Species_Name
      y
    })
  }
  if (is.null(Column_of_Already_Pooled_Species_Name)) {
    List_of_Bipartite_Interaction_Matrices_by_Date <- lapply(List_of_Bipartite_Interaction_Matrices_by_Date, function (x) {
      x[, order(colnames(x))]
    })
  } else if (!is.null(Column_of_Already_Pooled_Species_Name)) {
    List_of_Bipartite_Interaction_Matrices_by_Date <- lapply(List_of_Bipartite_Interaction_Matrices_by_Date, function (x) {
      Already_Pooled_Column <- x[, which(colnames(x) == Column_of_Already_Pooled_Species_Name)]
      Other_Columns <- x[, which(colnames(x) != Column_of_Already_Pooled_Species_Name)]
      Other_Columns <- Other_Columns[, order(colnames(Other_Columns))]
      y <- cbind(Other_Columns, Already_Pooled_Column)
      colnames(y)[ncol(y)] <- Column_of_Already_Pooled_Species_Name
      y
    })
  }
  Dates <- as.Date(names(List_of_Bipartite_Interaction_Matrices_by_Date))
  List_of_Bipartite_Interaction_Matrices_by_Date <- List_of_Bipartite_Interaction_Matrices_by_Date[order(Dates)]
  Dates <- Dates[order(Dates)]
  if (How_to_Treat_Missing_Values == "Make Missing Values Zeros") {
    List_of_Bipartite_Interaction_Matrices_by_Date <- lapply(List_of_Bipartite_Interaction_Matrices_by_Date, function (x) {
      x[is.na(x)] <- 0
      x
    })
  } else if (How_to_Treat_Missing_Values == "Remove the Rows") {
    Rows_With_Missing_Values <- apply(sapply(List_of_Bipartite_Interaction_Matrices_by_Date, function (x) {
      apply(x, 2, function (y) {
        any(is.na(y))
      })
    }), 1, function (x) {
      any(x == T)
    })
    Row_Names <- names(Rows_With_Missing_Values)
    Rows_With_Missing_Values <- setNames(as.vector(Rows_With_Missing_Values), Row_Names)
    List_of_Bipartite_Interaction_Matrices_by_Date <- lapply(List_of_Bipartite_Interaction_Matrices_by_Date, function (x) {
      x[, which(!Rows_With_Missing_Values)]
    })
  } else if (How_to_Treat_Missing_Values == "Remove the Columns") {
    Columns_With_Missing_Values <- apply(sapply(List_of_Bipartite_Interaction_Matrices_by_Date, function (x) {
      apply(x, 1, function (y) {
        any(is.na(y))
      })
    }), 1, function (x) {
      any(x == T)
    })
    Column_Names <- names(Columns_With_Missing_Values)
    Columns_With_Missing_Values <- setNames(as.vector(Columns_With_Missing_Values), Column_Names)
    List_of_Bipartite_Interaction_Matrices_by_Date <- lapply(List_of_Bipartite_Interaction_Matrices_by_Date, function (x) {
      x[which(!Columns_With_Missing_Values), ]
    })
  }
  if (Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool > 0) {
    if (round(unique(sapply(List_of_Bipartite_Interaction_Matrices_by_Date, nrow)) * Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool) < 2) {
      stop (paste0("The 'Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool' argument supplied is too small - only one species would be pooled into an 'other species' group if this algorithm proceeds. With this number of lower-trophic-level species, if any pooling is to occur, the 'Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool' argument must be greater than ", (1.5 / unique(sapply(List_of_Bipartite_Interaction_Matrices_by_Date, nrow))), ". The 'Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool' argument may also be set to '0' to prevent any pooling from occurring."))
    }
    if (round(unique(sapply(List_of_Bipartite_Interaction_Matrices_by_Date, nrow)) * Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool) > (unique(sapply(List_of_Bipartite_Interaction_Matrices_by_Date, nrow)) - 2)) {
      stop (paste0("The 'Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool' argument supplied is too large - only one species would not be pooled into an 'other species' group if this algorithm proceeds. With this number of lower-trophic-level species, if pooling occurs and if more than one species is to remain unpooled, the 'Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool' argument must be less than ", ((unique(sapply(List_of_Bipartite_Interaction_Matrices_by_Date, nrow)) - 1.5) / unique(sapply(List_of_Bipartite_Interaction_Matrices_by_Date, nrow))), ". The 'Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool' argument may also be set to '0' to prevent any pooling from occurring."))
    }
    All_Lower_Trophic_Level_Observations <- rowSums(apply(simplify2array(List_of_Bipartite_Interaction_Matrices_by_Date), 1:2, sum))
    Reordered_Lower_Trophic_Level_Column_Sums <- All_Lower_Trophic_Level_Observations[rev(order(All_Lower_Trophic_Level_Observations))]
    Lower_Trophic_Level_Threshold <- setNames(Reordered_Lower_Trophic_Level_Column_Sums[(length(All_Lower_Trophic_Level_Observations) - (round(length(All_Lower_Trophic_Level_Observations) * Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool)))], NULL)
    Lower_Trophic_Level_Indices_to_Keep <- which(All_Lower_Trophic_Level_Observations > Lower_Trophic_Level_Threshold)
    Lower_Trophic_Level_Indices_to_Pool <- which(All_Lower_Trophic_Level_Observations <= Lower_Trophic_Level_Threshold)
    List_of_Bipartite_Interaction_Matrices_by_Date <- lapply(List_of_Bipartite_Interaction_Matrices_by_Date, function (x) {
      Rows_to_Keep <- x[Lower_Trophic_Level_Indices_to_Keep, ]
      Rows_to_Pool <- x[Lower_Trophic_Level_Indices_to_Pool, ]
      New_Matrix <- rbind(Rows_to_Keep, rowSums(Rows_to_Pool))
      rownames(New_Matrix)[nrow(New_Matrix)] <- "Other Species"
      New_Matrix
    })
  }
  if (Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool > 0) {
    if (round(unique(sapply(List_of_Bipartite_Interaction_Matrices_by_Date, ncol)) * Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool) < 2) {
      stop (paste0("The 'Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool' argument supplied is too small - only one species would be pooled into an 'other species' group if this algorithm proceeds. With this number of higher-trophic-level species, if any pooling is to occur, the 'Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool' argument must be greater than ", 1.5 / unique(sapply(List_of_Bipartite_Interaction_Matrices_by_Date, ncol)), ". The 'Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool' argument may also be set to '0' to prevent any pooling from occurring."))
    }
    if (round(unique(sapply(List_of_Bipartite_Interaction_Matrices_by_Date, ncol)) * Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool) > (unique(sapply(List_of_Bipartite_Interaction_Matrices_by_Date, ncol)) - 1)) {
      stop (paste0("The 'Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool' argument supplied is too large - only one species would not be pooled into an 'other species' group if this algorithm proceeds. With this number of higher-trophic-level species, if pooling occurs and if more than one species is to remain unpooled, the 'Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool' argument must be less than ", ((unique(sapply(List_of_Bipartite_Interaction_Matrices_by_Date, ncol)) - 1.5) / unique(sapply(List_of_Bipartite_Interaction_Matrices_by_Date, ncol))), ". The 'Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool' argument may also be set to '0' to prevent any pooling from occurring."))
    }
    All_Higher_Trophic_Level_Observations <- colSums(apply(simplify2array(List_of_Bipartite_Interaction_Matrices_by_Date), 1:2, sum))
    Reordered_Higher_Trophic_Level_Column_Sums <- All_Higher_Trophic_Level_Observations[rev(order(All_Higher_Trophic_Level_Observations))]
    Higher_Trophic_Level_Threshold <- setNames(Reordered_Higher_Trophic_Level_Column_Sums[(length(All_Higher_Trophic_Level_Observations) - (round(length(All_Higher_Trophic_Level_Observations) * Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool)))], NULL)
    Higher_Trophic_Level_Indices_to_Keep <- which(All_Higher_Trophic_Level_Observations > Higher_Trophic_Level_Threshold)
    Higher_Trophic_Level_Indices_to_Pool <- which(All_Higher_Trophic_Level_Observations <= Higher_Trophic_Level_Threshold)
    List_of_Bipartite_Interaction_Matrices_by_Date <- lapply(List_of_Bipartite_Interaction_Matrices_by_Date, function (x) {
      Columns_to_Keep <- x[, Higher_Trophic_Level_Indices_to_Keep]
      Columns_to_Pool <- x[, Higher_Trophic_Level_Indices_to_Pool]
      New_Matrix <- cbind(Columns_to_Keep, rowSums(Columns_to_Pool))
      colnames(New_Matrix)[ncol(New_Matrix)] <- "Other Species"
      New_Matrix
    })
  }
  Bipartite_Interaction_Array <- simplify2array(List_of_Bipartite_Interaction_Matrices_by_Date)
  Aggregated_Matrix <- apply(Bipartite_Interaction_Array, 1:2, sum)
  if (Prioritize_Interaction_Strength_Over_Alphabetization_in_Species_Layout == TRUE) {
    Aggregated_Matrix_to_Reorder <- Aggregated_Matrix
    if (Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool > 0) {
      Aggregated_Matrix_to_Reorder <- Aggregated_Matrix_to_Reorder[which(rownames(Aggregated_Matrix_to_Reorder) != "Other Species"), ]
    } else if (!is.null(Row_of_Already_Pooled_Species_Name)) {
      Aggregated_Matrix_to_Reorder <- Aggregated_Matrix_to_Reorder[which(rownames(Aggregated_Matrix_to_Reorder) != Row_of_Already_Pooled_Species_Name), ]
    }
    if (Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool > 0) {
      Aggregated_Matrix_to_Reorder <- Aggregated_Matrix_to_Reorder[, which(colnames(Aggregated_Matrix_to_Reorder) != "Other Species")]
    } else if (!is.null(Column_of_Already_Pooled_Species_Name)) {
      Aggregated_Matrix_to_Reorder <- Aggregated_Matrix_to_Reorder[, which(colnames(Aggregated_Matrix_to_Reorder) != Column_of_Already_Pooled_Species_Name)]
    }
    Optimally_Reordered_Matrix <- DBM.functions::Attempting_to_Diagonalize_a_Matrix_Based_on_Its_Full_Diagonal(Aggregated_Matrix_to_Reorder)
    Optimally_Reordered_Matrix <- Optimally_Reordered_Matrix$Optimally_Reordered_Matrix
    if (Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool > 0) {
      Optimally_Reordered_Matrix <- rbind(Optimally_Reordered_Matrix, Aggregated_Matrix[which(rownames(Aggregated_Matrix) == "Other Species"), ])
      rownames(Optimally_Reordered_Matrix)[nrow(Optimally_Reordered_Matrix)] <- "Other Species"
    } else if (!is.null(Row_of_Already_Pooled_Species_Name)) {
      Optimally_Reordered_Matrix <- rbind(Optimally_Reordered_Matrix, Aggregated_Matrix[which(rownames(Aggregated_Matrix) == Row_of_Already_Pooled_Species_Name), ])
      rownames(Optimally_Reordered_Matrix)[nrow(Optimally_Reordered_Matrix)] <- Row_of_Already_Pooled_Species_Name
    }
    if (Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool > 0) {
      Optimally_Reordered_Matrix <- cbind(Optimally_Reordered_Matrix, Aggregated_Matrix[, which(colnames(Aggregated_Matrix) == "Other Species")])
      colnames(Optimally_Reordered_Matrix)[ncol(Optimally_Reordered_Matrix)] <- "Other Species"
    } else if (!is.null(Column_of_Already_Pooled_Species_Name)) {
      Optimally_Reordered_Matrix <- cbind(Optimally_Reordered_Matrix, Aggregated_Matrix[, which(colnames(Aggregated_Matrix) == Column_of_Already_Pooled_Species_Name)])
      colnames(Optimally_Reordered_Matrix)[ncol(Optimally_Reordered_Matrix)] <- Column_of_Already_Pooled_Species_Name
    }
    Aggregated_Matrix <- Optimally_Reordered_Matrix
    Bipartite_Interaction_List <- setNames(lapply(seq_len(dim(Bipartite_Interaction_Array)[3]), function (x) {
      Bipartite_Interaction_Array[, , x]
    }), dimnames(Bipartite_Interaction_Array)[[3]])
    Bipartite_Interaction_List <- lapply(Bipartite_Interaction_List, function (x) {
      x[match(rownames(Aggregated_Matrix), rownames(x)), match(colnames(Aggregated_Matrix), colnames(x))]
    })
    Bipartite_Interaction_Array <- simplify2array(Bipartite_Interaction_List)
  }
  Row_Name_Order <- rownames(Aggregated_Matrix)
  Column_Name_Order <- colnames(Aggregated_Matrix)
  Lower_Trophic_Level_Plot_Heights <- rowSums(Aggregated_Matrix)
  Higher_Trophic_Level_Plot_Heights <- colSums(Aggregated_Matrix)
  Bipartite_Interaction_Array <- Bipartite_Interaction_Array[which(!(Lower_Trophic_Level_Plot_Heights == 0)), which(!(Higher_Trophic_Level_Plot_Heights == 0)), ]
  Aggregated_Matrix <- Aggregated_Matrix[which(!(Lower_Trophic_Level_Plot_Heights == 0)), which(!(Higher_Trophic_Level_Plot_Heights == 0))]
  Lower_Trophic_Level_Species_Abundances_by_Date <- apply(Bipartite_Interaction_Array, c(1, 3), sum)
  Higher_Trophic_Level_Species_Abundances_by_Date <- apply(Bipartite_Interaction_Array, c(2, 3), sum)
  Total_Number_of_Occurrences <- sum(Aggregated_Matrix)
  Number_of_Lower_Trophic_Levels <- nrow(Aggregated_Matrix)
  Number_of_Lower_Trophic_Level_Gaps <- Number_of_Lower_Trophic_Levels - 1
  Number_of_Higher_Trophic_Levels <- ncol(Aggregated_Matrix)
  Number_of_Higher_Trophic_Level_Gaps <- Number_of_Higher_Trophic_Levels - 1
  Gap_Size_Sum <- Total_Number_of_Occurrences * Gap_Size_Constant
  Lower_Trophic_Level_Gap_Size <-  Gap_Size_Sum / Number_of_Lower_Trophic_Level_Gaps
  Higher_Trophic_Level_Gap_Size <-  Gap_Size_Sum / Number_of_Higher_Trophic_Level_Gaps
  Lower_Trophic_Level_Heights <- rowSums(Aggregated_Matrix)
  Cumulative_Lower_Trophic_Level_Sums <- cumsum(Lower_Trophic_Level_Heights)
  Cumulative_Lower_Trophic_Level_Heights_Vector <- c(0, as.vector(sapply(seq_len(Number_of_Lower_Trophic_Levels), function (i) {
    c((Cumulative_Lower_Trophic_Level_Sums[i] + (Lower_Trophic_Level_Gap_Size * (i - 1))), (Cumulative_Lower_Trophic_Level_Sums[i] + (Lower_Trophic_Level_Gap_Size * i)))
  })))
  Cumulative_Lower_Trophic_Level_Heights_Vector <- Cumulative_Lower_Trophic_Level_Heights_Vector[-c(length(Cumulative_Lower_Trophic_Level_Heights_Vector))]
  Cumulative_Lower_Trophic_Level_Heights_Data_Frame <- as.data.frame(t(sapply(seq_along(unique(rownames(Aggregated_Matrix))), function (i) {
    Cumulative_Lower_Trophic_Level_Heights_Vector[((2 * i) - 1):(2 * i)]
  })))
  colnames(Cumulative_Lower_Trophic_Level_Heights_Data_Frame) <- c("Upper_Bound", "Lower_Bound")
  Cumulative_Lower_Trophic_Level_Heights_Data_Frame$Lower_Trophic_Level_Species <- unique(rownames(Aggregated_Matrix))
  Cumulative_Lower_Trophic_Level_Heights_Data_Frame <- Cumulative_Lower_Trophic_Level_Heights_Data_Frame[, c(which(colnames(Cumulative_Lower_Trophic_Level_Heights_Data_Frame) == "Lower_Trophic_Level_Species"), which(colnames(Cumulative_Lower_Trophic_Level_Heights_Data_Frame) != "Lower_Trophic_Level_Species"))]
  Cumulative_Lower_Trophic_Level_Heights_Data_Frame$Midpoint <- (Cumulative_Lower_Trophic_Level_Heights_Data_Frame$Upper_Bound + Cumulative_Lower_Trophic_Level_Heights_Data_Frame$Lower_Bound) / 2
  Higher_Trophic_Level_Heights <- colSums(Aggregated_Matrix)
  Cumulative_Higher_Trophic_Level_Sums <- cumsum(Higher_Trophic_Level_Heights)
  Cumulative_Higher_Trophic_Level_Heights_Vector <- c(0, as.vector(sapply(seq_len(Number_of_Higher_Trophic_Levels), function (i) {
    c((Cumulative_Higher_Trophic_Level_Sums[i] + (Higher_Trophic_Level_Gap_Size * (i - 1))), (Cumulative_Higher_Trophic_Level_Sums[i] + (Higher_Trophic_Level_Gap_Size * i)))
  })))
  Cumulative_Higher_Trophic_Level_Heights_Vector <- Cumulative_Higher_Trophic_Level_Heights_Vector[-c(length(Cumulative_Higher_Trophic_Level_Heights_Vector))]
  Cumulative_Higher_Trophic_Level_Heights_Data_Frame <- as.data.frame(t(sapply(seq_along(unique(colnames(Aggregated_Matrix))), function (i) {
    Cumulative_Higher_Trophic_Level_Heights_Vector[((2 * i) - 1):(2 * i)]
  })))
  colnames(Cumulative_Higher_Trophic_Level_Heights_Data_Frame) <- c("Upper_Bound", "Lower_Bound")
  Cumulative_Higher_Trophic_Level_Heights_Data_Frame$Higher_Trophic_Level_Species <- unique(colnames(Aggregated_Matrix))
  Cumulative_Higher_Trophic_Level_Heights_Data_Frame <- Cumulative_Higher_Trophic_Level_Heights_Data_Frame[, c(which(colnames(Cumulative_Higher_Trophic_Level_Heights_Data_Frame) == "Higher_Trophic_Level_Species"), which(colnames(Cumulative_Higher_Trophic_Level_Heights_Data_Frame) != "Higher_Trophic_Level_Species"))]
  Cumulative_Higher_Trophic_Level_Heights_Data_Frame$Midpoint <- (Cumulative_Higher_Trophic_Level_Heights_Data_Frame$Upper_Bound + Cumulative_Higher_Trophic_Level_Heights_Data_Frame$Lower_Bound) / 2
  Data_in_Long_Format <- as.data.frame(as.table(Aggregated_Matrix))
  colnames(Data_in_Long_Format) <- c("Lower_Trophic_Level_Species", "Higher_Trophic_Level_Species", "Number_of_Interactions")
  Data_in_Long_Format_Split_by_Lower_Trophic_Level_Species <- split(Data_in_Long_Format, Data_in_Long_Format$Lower_Trophic_Level_Species)
  Data_in_Long_Format_Split_by_Lower_Trophic_Level_Species <- Data_in_Long_Format_Split_by_Lower_Trophic_Level_Species[match(Row_Name_Order, names(Data_in_Long_Format_Split_by_Lower_Trophic_Level_Species))]
  Cumulative_Lower_Trophic_Level_Heights_Split_by_Lower_Trophic_Level_Species <- split(Cumulative_Lower_Trophic_Level_Heights_Data_Frame, Cumulative_Lower_Trophic_Level_Heights_Data_Frame$Lower_Trophic_Level_Species)
  Cumulative_Lower_Trophic_Level_Heights_Split_by_Lower_Trophic_Level_Species <- Cumulative_Lower_Trophic_Level_Heights_Split_by_Lower_Trophic_Level_Species[match(Row_Name_Order, names(Cumulative_Lower_Trophic_Level_Heights_Split_by_Lower_Trophic_Level_Species))]
  Data_in_Long_Format_Split_by_Lower_Trophic_Level_Species <- mapply(function (x, y) {
    Lower_Trophic_Level_Range <- c(0, cumsum(x$Number_of_Interactions)) + y$Upper_Bound
    x$Lower_Trophic_Level_Upper_Bound <- Lower_Trophic_Level_Range[-c(length(Lower_Trophic_Level_Range))]
    x$Lower_Trophic_Level_Lower_Bound <- Lower_Trophic_Level_Range[-c(1)]
    x
  }, x = Data_in_Long_Format_Split_by_Lower_Trophic_Level_Species, y = Cumulative_Lower_Trophic_Level_Heights_Split_by_Lower_Trophic_Level_Species, SIMPLIFY = F)
  Data_in_Long_Format <- do.call("rbind", Data_in_Long_Format_Split_by_Lower_Trophic_Level_Species)
  rownames(Data_in_Long_Format) <- NULL
  Data_in_Long_Format_Split_by_Higher_Trophic_Level_Species <- split(Data_in_Long_Format, Data_in_Long_Format$Higher_Trophic_Level_Species)
  Data_in_Long_Format_Split_by_Higher_Trophic_Level_Species <- Data_in_Long_Format_Split_by_Higher_Trophic_Level_Species[match(Column_Name_Order, names(Data_in_Long_Format_Split_by_Higher_Trophic_Level_Species))]
  Cumulative_Higher_Trophic_Level_Heights_Split_by_Higher_Trophic_Level_Species <- split(Cumulative_Higher_Trophic_Level_Heights_Data_Frame, Cumulative_Higher_Trophic_Level_Heights_Data_Frame$Higher_Trophic_Level_Species)
  Cumulative_Higher_Trophic_Level_Heights_Split_by_Higher_Trophic_Level_Species <- Cumulative_Higher_Trophic_Level_Heights_Split_by_Higher_Trophic_Level_Species[match(Column_Name_Order, names(Cumulative_Higher_Trophic_Level_Heights_Split_by_Higher_Trophic_Level_Species))]
  Data_in_Long_Format_Split_by_Higher_Trophic_Level_Species <- mapply(function (x, y) {
    Higher_Trophic_Level_Range <- c(0, cumsum(x$Number_of_Interactions)) + y$Upper_Bound
    x$Higher_Trophic_Level_Upper_Bound <- Higher_Trophic_Level_Range[-c(length(Higher_Trophic_Level_Range))]
    x$Higher_Trophic_Level_Lower_Bound <- Higher_Trophic_Level_Range[-c(1)]
    x
  }, x = Data_in_Long_Format_Split_by_Higher_Trophic_Level_Species, y = Cumulative_Higher_Trophic_Level_Heights_Split_by_Higher_Trophic_Level_Species, SIMPLIFY = F)
  Data_in_Long_Format <- do.call("rbind", Data_in_Long_Format_Split_by_Higher_Trophic_Level_Species)
  rownames(Data_in_Long_Format) <- NULL
  Data_in_Long_Format <- Data_in_Long_Format[which(Data_in_Long_Format$Number_of_Interactions != 0), ]
  Data_in_Long_Format <- Data_in_Long_Format[order(Data_in_Long_Format$Number_of_Interactions), ]
  Vertical_Axis_Limits <- c(0, (Total_Number_of_Occurrences + Gap_Size_Sum))
  if (Include_Scale_Bars == TRUE) {
    Scale_Bar_Numbers <- DBM.functions::Making_Reasonable_Scale_Bars(Vertical_Axis_Limits, Scale_Bar_Factor = Scale_Bar_Factor)
    Vertical_Scale_Bar_Tick_Coordinates <- (-1 * (Scale_Bar_Numbers - Scale_Bar_Numbers[3])) + mean(Vertical_Axis_Limits)
  }
  Date_Range <- range(Dates)
  Date_Labels <- pretty(Date_Range)
  Widths <- c(Relative_Plot_Widths[1], Relative_Plot_Widths[2], (Relative_Plot_Widths[3] / 2), (Relative_Plot_Widths[3] / 2), Relative_Plot_Widths[4], Relative_Plot_Widths[5])
  Widths <- Widths / sum(Widths)
  Heights <- Relative_Plot_Heights / sum(Relative_Plot_Heights)
  Layout_Matrix <- matrix(c(1, 1, 1, 2, 2, 2, 3, 4, 5, 5, 6, 7), nrow = 2, byrow = T)
  if (!is.null(Overall_Plot_Title)) {
    Layout_Matrix <- Layout_Matrix + 1
    Layout_Matrix <- rbind(rep(1, ncol(Layout_Matrix)), Layout_Matrix)
  }
  layout(Layout_Matrix, widths = Widths, heights = Heights)
  par(mar = c(1, 1, 1, 1))
  if (!is.null(Overall_Plot_Title)) {
    plot(0, xlab = "", ylab = "", axes = F, type = "n", main = paste0("\n", Overall_Plot_Title), cex.main = Overall_Plot_Title_Font_Size)
  }
  plot(0, xlab = "", ylab = "", axes = F, type = "n", main = paste0("\n", Lower_Trophic_Level_Plot_Title), cex.main = Trophic_Level_Plot_Title_Font_Size)
  plot(0, xlab = "", ylab = "", axes = F, type = "n", main = paste0("\n", Higher_Trophic_Level_Plot_Title), cex.main = Trophic_Level_Plot_Title_Font_Size)
  if (Include_Scale_Bars == TRUE) {
    par(mar = c(Bottom_Margin, Left_and_Right_Figure_Margins, Between_Title_Plots_and_Other_Plots_Margin, 1))
  } else if (Include_Scale_Bars == FALSE) {
    par(mar = c(Bottom_Margin, 1, Between_Title_Plots_and_Other_Plots_Margin, 1))
  }
  plot(0, xlab = "", ylab = "", xlim = Date_Range, ylim = rev(Vertical_Axis_Limits), type = "n", axes = F, main = "Phenology", cex.main = Phenology_and_Bipartite_Plot_Title_Font_Size)
  axis.Date(1, at = Date_Labels, cex.axis = Horizontal_Axis_Dates_Font_Size)
  mtext("Date", 1, line = 4.5, cex = Horizontal_Axis_Title_Font_Size)
  sapply(seq_along(unique(rownames(Aggregated_Matrix))), function (i) {
    Lower_Trophic_Level_Species <- unique(rownames(Aggregated_Matrix))[i]
    Lower_Bounds <- (Cumulative_Lower_Trophic_Level_Heights_Data_Frame[which(Cumulative_Lower_Trophic_Level_Heights_Data_Frame$Lower_Trophic_Level_Species == Lower_Trophic_Level_Species), ]$Midpoint) - (Lower_Trophic_Level_Species_Abundances_by_Date[which(rownames(Lower_Trophic_Level_Species_Abundances_by_Date) == Lower_Trophic_Level_Species), ] / 2)
    Upper_Bounds <- (Cumulative_Lower_Trophic_Level_Heights_Data_Frame[which(Cumulative_Lower_Trophic_Level_Heights_Data_Frame$Lower_Trophic_Level_Species == Lower_Trophic_Level_Species), ]$Midpoint) + (Lower_Trophic_Level_Species_Abundances_by_Date[which(rownames(Lower_Trophic_Level_Species_Abundances_by_Date) == Lower_Trophic_Level_Species), ] / 2)
    polygon(c(Dates, rev(Dates), Dates[1]), c(Upper_Bounds, rev(Lower_Bounds), Upper_Bounds[1]), col = Lower_Trophic_Level_Color)
  })
  if (Include_Scale_Bars == TRUE) {
    Horizontal_Axis_Coordinates <- par("usr")[1:2]
    Horizontal_Scale_Bar_Coordinate <- Horizontal_Axis_Coordinates[1] - (abs(diff(Horizontal_Axis_Coordinates)) * 0.1)
    segments(Horizontal_Scale_Bar_Coordinate, Vertical_Scale_Bar_Tick_Coordinates[1], Horizontal_Scale_Bar_Coordinate, Vertical_Scale_Bar_Tick_Coordinates[length(Vertical_Scale_Bar_Tick_Coordinates)], xpd = T)
    sapply(seq_len(length(Scale_Bar_Numbers)), function (i) {
      segments(Horizontal_Scale_Bar_Coordinate, Vertical_Scale_Bar_Tick_Coordinates[i], (Horizontal_Scale_Bar_Coordinate - (abs(diff(Horizontal_Axis_Coordinates)) * 0.1)), Vertical_Scale_Bar_Tick_Coordinates[i], xpd = T)
    })
    sapply(seq_len(length(Scale_Bar_Numbers)), function (i) {
      text((Horizontal_Scale_Bar_Coordinate - (abs(diff(Horizontal_Axis_Coordinates)) * (0.1 + Horizontal_Scale_Text_Shift_Constant))), Vertical_Scale_Bar_Tick_Coordinates[i], Scale_Bar_Numbers[i], xpd = T, cex = Scale_Bar_Text_Font_Size)
    })
    text((Horizontal_Scale_Bar_Coordinate - (abs(diff(Horizontal_Axis_Coordinates)) * (0.1 + Horizontal_Scale_Text_Shift_Constant))), (Vertical_Scale_Bar_Tick_Coordinates[length(Vertical_Scale_Bar_Tick_Coordinates)] - (abs(diff(Vertical_Axis_Limits)) * Vertical_Scale_Title_Shift_Constant)), "Total\nNumber of\nObserved\nInteractions", xpd = T, cex = Scale_Bar_Title_Font_Size)
  }
  par(mar = c(Bottom_Margin, 1, Between_Title_Plots_and_Other_Plots_Margin, 1))
  plot(0, xlab = "", ylab = "", ylim = rev(Vertical_Axis_Limits), type = "n", axes = F, xpd = T, main = "Species", cex.main = Phenology_and_Bipartite_Plot_Title_Font_Size)
  sapply(seq_along(unique(rownames(Aggregated_Matrix))), function (i) {
    text(mean(c(par("usr")[1:2])), Cumulative_Lower_Trophic_Level_Heights_Data_Frame$Midpoint, rownames(Aggregated_Matrix), xpd = T, cex = Species_Name_Font_Size)
  })
  par(mar = c(Bottom_Margin, 1, Between_Title_Plots_and_Other_Plots_Margin, 1))
  plot(0, xlim = c(0, 10), ylim = rev(Vertical_Axis_Limits), xlab = "", ylab = "", type = "n", axes = F, main = "Bipartite Interaction Matrix Visualization", cex.main = Phenology_and_Bipartite_Plot_Title_Font_Size)
  sapply(seq_len(Number_of_Lower_Trophic_Levels), function (i) {
    polygon(c(0, (10 * Relative_Bar_Width), (10 * Relative_Bar_Width), 0, 0), c(Cumulative_Lower_Trophic_Level_Heights_Data_Frame[i, ]$Upper_Bound, Cumulative_Lower_Trophic_Level_Heights_Data_Frame[i, ]$Upper_Bound, Cumulative_Lower_Trophic_Level_Heights_Data_Frame[i, ]$Lower_Bound, Cumulative_Lower_Trophic_Level_Heights_Data_Frame[i, ]$Lower_Bound, Cumulative_Lower_Trophic_Level_Heights_Data_Frame[i, ]$Upper_Bound), col = Lower_Trophic_Level_Color)
  })
  sapply(seq_len(Number_of_Higher_Trophic_Levels), function (i) {
    polygon(c((10 - (10 * Relative_Bar_Width)), 10, 10, (10 - (10 * Relative_Bar_Width)), (10 - (10 * Relative_Bar_Width))), c(Cumulative_Higher_Trophic_Level_Heights_Data_Frame[i, ]$Upper_Bound, Cumulative_Higher_Trophic_Level_Heights_Data_Frame[i, ]$Upper_Bound, Cumulative_Higher_Trophic_Level_Heights_Data_Frame[i, ]$Lower_Bound, Cumulative_Higher_Trophic_Level_Heights_Data_Frame[i, ]$Lower_Bound, Cumulative_Higher_Trophic_Level_Heights_Data_Frame[i, ]$Upper_Bound), col = Higher_Trophic_Level_Color)
  })
  sapply(seq_len(nrow(Data_in_Long_Format)), function (x) {
    polygon(c((10 * Relative_Bar_Width), (10 - (10 * Relative_Bar_Width)), (10 - (10 * Relative_Bar_Width)), (10 * Relative_Bar_Width), (10 * Relative_Bar_Width)), c(Data_in_Long_Format$Lower_Trophic_Level_Lower_Bound[x], Data_in_Long_Format$Higher_Trophic_Level_Lower_Bound[x], Data_in_Long_Format$Higher_Trophic_Level_Upper_Bound[x], Data_in_Long_Format$Lower_Trophic_Level_Upper_Bound[x], Data_in_Long_Format$Lower_Trophic_Level_Lower_Bound[x]), col = Interaction_Color)
  })
  par(mar = c(Bottom_Margin, 1, Between_Title_Plots_and_Other_Plots_Margin, 1))
  plot(0, xlab = "", ylab = "", ylim = rev(Vertical_Axis_Limits), type = "n", axes = F, xpd = T, main = "Species", cex.main = Phenology_and_Bipartite_Plot_Title_Font_Size)
  sapply(seq_along(unique(colnames(Aggregated_Matrix))), function (i) {
    text(mean(c(par("usr")[1:2])), Cumulative_Higher_Trophic_Level_Heights_Data_Frame$Midpoint, colnames(Aggregated_Matrix), xpd = T, cex = Species_Name_Font_Size)
  })
  if (Include_Scale_Bars == TRUE) {
    par(mar = c(Bottom_Margin, 1, Between_Title_Plots_and_Other_Plots_Margin, Left_and_Right_Figure_Margins))
  } else if (Include_Scale_Bars == FALSE) {
    par(mar = c(Bottom_Margin, 1, Between_Title_Plots_and_Other_Plots_Margin, 1))
  }
  plot(0, xlab = "", ylab = "", xlim = Date_Range, ylim = rev(Vertical_Axis_Limits), type = "n", axes = F, main = "Phenology", cex.main = Phenology_and_Bipartite_Plot_Title_Font_Size)
  axis.Date(1, at = Date_Labels, cex.axis = Horizontal_Axis_Dates_Font_Size)
  mtext("Date", 1, line = 4.5, cex = Horizontal_Axis_Title_Font_Size)
  sapply(seq_along(unique(colnames(Aggregated_Matrix))), function (i) {
    Higher_Trophic_Level_Species <- unique(colnames(Aggregated_Matrix))[i]
    Lower_Bounds <- (Cumulative_Higher_Trophic_Level_Heights_Data_Frame[which(Cumulative_Higher_Trophic_Level_Heights_Data_Frame$Higher_Trophic_Level_Species == Higher_Trophic_Level_Species), ]$Midpoint) - (Higher_Trophic_Level_Species_Abundances_by_Date[which(rownames(Higher_Trophic_Level_Species_Abundances_by_Date) == Higher_Trophic_Level_Species), ] / 2)
    Upper_Bounds <- (Cumulative_Higher_Trophic_Level_Heights_Data_Frame[which(Cumulative_Higher_Trophic_Level_Heights_Data_Frame$Higher_Trophic_Level_Species == Higher_Trophic_Level_Species), ]$Midpoint) + (Higher_Trophic_Level_Species_Abundances_by_Date[which(rownames(Higher_Trophic_Level_Species_Abundances_by_Date) == Higher_Trophic_Level_Species), ] / 2)
    polygon(c(Dates, rev(Dates), Dates[1]), c(Upper_Bounds, rev(Lower_Bounds), Upper_Bounds[1]), col = Higher_Trophic_Level_Color)
  })
  if (Include_Scale_Bars == TRUE) {
    Horizontal_Axis_Coordinates <- par("usr")[1:2]
    Horizontal_Scale_Bar_Coordinate <- Horizontal_Axis_Coordinates[2] + (abs(diff(Horizontal_Axis_Coordinates)) * 0.1)
    segments(Horizontal_Scale_Bar_Coordinate, Vertical_Scale_Bar_Tick_Coordinates[1], Horizontal_Scale_Bar_Coordinate, Vertical_Scale_Bar_Tick_Coordinates[length(Vertical_Scale_Bar_Tick_Coordinates)], xpd = T)
    sapply(seq_len(length(Scale_Bar_Numbers)), function (i) {
      segments(Horizontal_Scale_Bar_Coordinate, Vertical_Scale_Bar_Tick_Coordinates[i], (Horizontal_Scale_Bar_Coordinate + (abs(diff(Horizontal_Axis_Coordinates)) * 0.1)), Vertical_Scale_Bar_Tick_Coordinates[i], xpd = T)
    })
    sapply(seq_len(length(Scale_Bar_Numbers)), function (i) {
      text((Horizontal_Scale_Bar_Coordinate + (abs(diff(Horizontal_Axis_Coordinates)) * (0.1 + Horizontal_Scale_Text_Shift_Constant))), Vertical_Scale_Bar_Tick_Coordinates[i], Scale_Bar_Numbers[i], xpd = T, cex = Scale_Bar_Text_Font_Size)
    })
    text((Horizontal_Scale_Bar_Coordinate + (abs(diff(Horizontal_Axis_Coordinates)) * (0.1 + Horizontal_Scale_Text_Shift_Constant))), (Vertical_Scale_Bar_Tick_Coordinates[length(Vertical_Scale_Bar_Tick_Coordinates)] - (abs(diff(Vertical_Axis_Limits)) * Vertical_Scale_Title_Shift_Constant)), "Total\nNumber of\nObserved\nInteractions", xpd = T, cex = Scale_Bar_Title_Font_Size)
  }
}
