#' Generating Figures Containing a Bipartite Interaction Matrix Visualization Plot and Corresponding Higher and Lower Trophic Level Phenology Plots
#'
#' This function generates violin-like phenology plots that depict the relative abundances of lower-trophic-level and higher-trophic-level species over time. It also generates bipartite interaction matrix visualization plots which are positioned centrally between the two sets of phenology plots.
#'
#' Unfortunately, this function can't handle missing values at this point!
#'
#' `Generating_Figures_Containing_a_Bipartite_Interaction_Matrix_Visualization_Plot_and_Corresponding_Higher_and_Lower_Trophic_Level_Phenology_Plots` generates violin-like phenology plots that depict the relative abundances of lower-trophic-level and higher-trophic-level species over time. It also generates bipartite interaction matrix visualization plots which are positioned centrally between the two sets of phenology plots.
#'
#' In bipartite parlance, 'nodes' refer to species (from either trophic level) and 'edges' refer to interactions between these species. Bipartite networks are special because species interact only across, and not within, trophic levels.
#'
#' This function was heavily inspired by a manuscript that contained a figure very similar to the one this function returns (Russo et al., 2013). It was also heavily inspired by my colleague Isaac Ativor's work in plant-pollinator interactions.
#'
#' This function relies on the \link[DBM.functions]{Attempting_to_Diagonalize_a_Matrix_Based_on_Its_Full_Diagonal} function and the \link[DBM.functions]{Making_Reasonable_Scale_Bars} function, which are both also found in the `DBM.functions` package.
#'
#' @param Data_Frame a data frame containing information about the number of interactions between the higher- and lower-trophic-level species, the higher-trophic-level species, the lower-trophic-level species, the dates, and the factors that will be used to split the data to generate multiple panels for the resulting figure. This data frame must contain a column of dates (in `"Date"` format), a column of lower-trophic-level species (in `"character"` format), and one or more columns of higher-trophic-level species (containing the names of these species as the column names and `"numeric"` entries that represent the numbers of interactions between each particular higher-trophic-level species and the corresponding lower-trophic-level species). This argument must be provided for the function to work.
#' @param Higher_Trophic_Level_Species a `"character"` vector containing the names of each of the higher-trophic-level species in the experiment. These names must also be column names in the `Data_Frame` data frame; the entries of these columns will be `"numeric"` and will represent the numbers of interactions between higher- and lower-trophic-level species. This argument must be provided for the function to work.
#' @param Lower_Trophic_Level_Species a `"character"` vector of length `1` containing the name of the column in the `Data_Frame` data frame where the lower-trophic-level-species names are stored. This argument must be provided for the function to work.
#' @param Date a `"character"` vector of length `1` containing the name of the column in the `Data_Frame` data frame where the dates are stored. This argument must be provided for the function to work.
#' @param Overall_Plot_Title the overall figure title. The default value, `NULL`, means that an overall figure title will be omitted.
#' @param Lower_Trophic_Level_Plot_Title a name for the lower trophic level, such as `"Plants"` or `"Prey"`. The default value, `"Lower Trophic Level"`, is a generic title that can be used in any figure.
#' @param Higher_Trophic_Level_Plot_Title a name for the higher trophic level, such as `"Pollinators"` or `"Predators"`. The default value, `"Higher Trophic Level"`, is a generic title that can be used in any figure.
#' @param Prioritize_Interaction_Strength_Over_Alphabetization_in_Species_Layout whether species should be arranged such that species from different trophic levels that interact more frequently are closer together and species that interact less frequently are farther apart on the bipartite interaction visualization plot. If putting frequently interacting species closer together and infrequently interacting species farther apart is prioritized, the species will probably not be arranged alphabetically in the resulting figure. The default value, `FALSE`, causes the species from both trophic levels to be arranged alphabetically.
#' @param Include_Scale_Bars whether vertical scale bars should be included on the left and right sides of the figure. The default value, `TRUE`, causes scale bars to be included.
#' @param Scale_Bar_Factor a number between 0 and 1 (exclusive) that specifies approximately how tall the scale bar will be relative to the vertical axis range. The default value for this argument, `0.25`, makes it so that the scale bar will take up as close to 25 % of the vertical axis' range as possible.
#' @param Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool the approximate proportion of lower-trophic-level species to pool into a 'other species' group. First, lower-trophic-level species are ordered by their total numbers of occurrences, and then this argument is used to determine the threshold for inclusion in the figure. Species that do not meet this threshold (that have low numbers of occurrences) are pooled into an 'other species' group. The default value, `0`, means that no species will be pooled.
#' @param Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool  the approximate proportion of higher-trophic-level species to pool into a 'other species' group. First, higher-trophic-level species are ordered by their total numbers of occurrences, and then this argument is used to determine the threshold for inclusion in the figure. Species that do not meet this threshold (that have low numbers of occurrences) are pooled into an 'other species' group. The default value, `0`, means that no species will be pooled.
#' @param Lower_Trophic_Level_Species_Names_Data_Frame a data frame (of class `"data.frame"`) that contains two columns - one column, which must be named `"Row_Name"`, must have entries that identically match the names of the rows of the matrices (which represent the lower-trophic-level species), and the other column, which must be named `"Names_for_Plotting"`, will have entries that will appear as the (potentially stylized, using the `expression()` function, for example) lower-trophic-level species names in the resulting figure.
#' @param Higher_Trophic_Level_Species_Names_Data_Frame a data frame (of class `"data.frame"`) that contains two columns - one column, which must be named `"Column_Name"`, must have entries that identically match the names of the columns of the matrices (which represent the higher-trophic-level species), and the other column, which must be named `"Names_for_Plotting"`, will have entries that will appear as the (potentially stylized, using the `expression()` function, for example) higher-trophic-level species names in the resulting figure.
#' @param Lower_Trophic_Level_Color the color of the violin-like phenology plots and the bars (from the bipartite interaction visualization plot) for the lower-trophic-level species. The default value to this argument is `"darkgreen"`.
#' @param Higher_Trophic_Level_Color the color of the violin-like phenology plots and the bars (from the bipartite interaction visualization plot) for the higher-trophic-level species. The default value to this argument is `"darkred"`.
#' @param Interaction_Color the color of the edges (from the bipartite interaction visualization plot) for the interactions between the lower-trophic-level species and the higher-trophic-level species. The default value to this argument is `"black"`.
#' @param Relative_Plot_Widths the relative widths of the lower-trophic-level violin-like phenology plots, the plot containing lower-trophic-level species names, the bipartite interaction visualization plot, the plot containing the higher-trophic-level species names, and the higher-trophic-level violin-like phenology plots. This argument must be a numeric vector containing 5 positive numbers, and it must be the same forward and backward to ensure that the resulting figure is symmetric. The default value to this argument is `c(11, 4, 10, 4, 11)`.
#' @param Relative_Plot_Heights the relative heights of the plots containing the titles and all the other plots. This argument must be a numeric vector and contain two (if an overall figure title is not to be included) or three (if an overall figure title is to be included) positive numbers. The default value to this argument, `c(rep(1, ifelse(is.null(Overall_Plot_Title), 1, 2)), ifelse(is.null(Overall_Plot_Title), 9, 8))`, means that when there is no overall figure title, the heights will be `c(1, 9)`, and when there is an overall figure title, the heights will be `c(1, 1, 8)`.
#' @param Already_Pooled_Lower_Trophic_Level_Species_Name the row name representing already-pooled species (such as miscellaneous species or other species), if there is a row of this nature present. The default value, `NULL`, assumes that there are no already-pooled rows. This pooled group will be placed at the bottom of the figure, or it will be pooled with other rows if an appropriate 'Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool' argument is provided.
#' @param Already_Pooled_Higher_Trophic_Level_Species_Name the column name representing already-pooled species (such as miscellaneous species or other species), if there is a column of this nature present. The default value, `NULL`, assumes that there are no already-pooled columns. This pooled group will be placed at the bottom of the figure, or it will be pooled with other columns if an appropriate 'Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool' argument is provided.
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
#' Data_Frame_1 <- structure(list(Date = structure(c(18357, 18357, 18357, 18357, 18392, 18392, 18392, 18392, 18422, 18422, 18422, 18422, 18456, 18456, 18456, 18456), class = "Date"), Lower_Trophic_Level_Species = c("Species a", "Species b", "Species c", "Species d", "Species a", "Species b", "Species c", "Species d", "Species a", "Species b", "Species c", "Species d", "Species a", "Species b", "Species c", "Species d"), `Species A` = c(8, 7, 12, 2, 8, 1, 3, 0, 2, 1, 4, 0, 1, 0, 3, 1), `Species B` = c(9, 11, 6, 24, 3, 1, 4, 7, 2, 1, 16, 3, 1, 2, 0, 2), `Species C` = c(6, 5, 2, 8, 31, 10, 1, 1, 3, 1, 5, 0, 4, 1, 1, 0), `Species D` = c(9, 6, 8, 9, 2, 10, 0, 4, 2, 1, 0, 0, 1, 3, 0, 0), `Species E` = c(6, 1, 4, 2, 1, 1, 0, 3, 2, 2, 1, 0, 0, 0, 0, 1), `Species F` = c(12, 5, 13, 5, 0, 0, 3, 8, 1, 1, 2, 4, 1, 2, 1, 1), `Species G` = c(10, 11, 5, 1, 1, 0, 1, 1, 1, 4, 1, 0, 0, 2, 0, 0)), row.names = c(NA, -16L), class = "data.frame")
#' Higher_Trophic_Level_Species_1 <- c("Species A", "Species B", "Species C", "Species D", "Species E", "Species F", "Species G")
#' Lower_Trophic_Level_Species_1 <- "Lower_Trophic_Level_Species"
#' Date_1 <- "Date"
#' Lower_Trophic_Level_Species_Names_Data_Frame_1 <- data.frame(Lower_Trophic_Level_Species_Name = paste("Species", letters[seq_len(4)]), Names_for_Plotting = c("expression(paste(italic('Lotus corniculatus')))", "expression(paste(italic('Medicago sativa')))", "expression(paste(italic('Pisum'), ' sp.'))", "expression(paste(italic('Trifolium'), ' spp.'))"))
#' Higher_Trophic_Level_Species_Names_Data_Frame_1 <- data.frame(Column_Name = paste("Species", LETTERS[seq_len(7)]), Names_for_Plotting = c("expression(paste(italic('Apis mellifera')))", "expression(paste(italic('Bombus'), ' spp.'))", "expression(paste('Coleoptera spp.'))", "expression(paste(italic('Dolichiovespula'), ' spp.'))", "expression(paste('Lepidoptera spp.'))", "expression(paste(italic('Vespa'), ' spp.'))", "expression(paste(italic('Xylocopa'), ' spp.'))"))
#' Data_Frame_2 <- structure(list(Date = structure(c(18357, 18357, 18357, 18357, 18392, 18392, 18392, 18392, 18422, 18422, 18422, 18422, 18456, 18456, 18456, 18456), class = "Date"), Lower_Trophic_Level_Species = c("Species a", "Species b", "Species c", "Species d", "Species a", "Species b", "Species c", "Species d", "Species a", "Species b", "Species c", "Species d", "Species a", "Species b", "Species c", "Species d"), `Species A` = c(3, 3, 9, 14, 3, 2, 1, 57, 1, 0, 0, 2, 1, 2, 1, 1), `Species B` = c(6, 6, 8, 5, 3, 1, 1, 0, 1, 4, 2, 1, 1, 0, 0, 3), `Species C` = c(8, 5, 12, 5, 1, 1, 1, 1, 1, 7, 1, 2, 0, 1, 1, 4), `Species D` = c(1, 3, 5, 2, 1, 6, 3, 2, 0, 8, 7, 4, 0, 0, 6, 1), `Species E` = c(2, 7, 7, 7, 1, 1, 0, 2, 1, 1, 3, 1, 0, 1, 0, 1), `Species F` = c(5, 3, 5, 3, 4, 0, 0, 1, 1, 5, 1, 0, 1, 0, 0, 0), `Miscellaneous Species` = c(6, 11, 6, 14, 1, 0, 1, 8, 1, 3, 0, 2, 4, 1, 1, 1)), row.names = c(NA, -16L), class = "data.frame")
#' Higher_Trophic_Level_Species_2 <- c("Species A", "Species B", "Species C", "Species D", "Species E", "Species F", "Miscellaneous Species")
#' Lower_Trophic_Level_Species_2 <- "Lower_Trophic_Level_Species"
#' Date_2 <- "Date"
#' Lower_Trophic_Level_Species_2 <- "Lower_Trophic_Level_Species"
#'
#' # Example Figure 1
#' Generating_Figures_Containing_a_Bipartite_Interaction_Matrix_Visualization_Plot_and_Corresponding_Higher_and_Lower_Trophic_Level_Phenology_Plots(Data_Frame_1, Higher_Trophic_Level_Species_1, Lower_Trophic_Level_Species_1, Date_1, Include_Scale_Bars = FALSE)
#'
#' # Example Figure 2
#' Generating_Figures_Containing_a_Bipartite_Interaction_Matrix_Visualization_Plot_and_Corresponding_Higher_and_Lower_Trophic_Level_Phenology_Plots(Data_Frame_1, Higher_Trophic_Level_Species_1, Lower_Trophic_Level_Species_1, Date_1, Scale_Bar_Factor = (1 / 3), Prioritize_Interaction_Strength_Over_Alphabetization_in_Species_Layout = TRUE, Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool = 0.25)
#'
#' # Example Figure 3
#' Generating_Figures_Containing_a_Bipartite_Interaction_Matrix_Visualization_Plot_and_Corresponding_Higher_and_Lower_Trophic_Level_Phenology_Plots(Data_Frame_2, Higher_Trophic_Level_Species_2, Lower_Trophic_Level_Species_2, Date_2, Scale_Bar_Factor = (1 / 3), Already_Pooled_Higher_Trophic_Level_Species_Name = "Miscellaneous Species", Species_Name_Font_Size = 1.25)
#'
#' # Example Figure 4
#' Generating_Figures_Containing_a_Bipartite_Interaction_Matrix_Visualization_Plot_and_Corresponding_Higher_and_Lower_Trophic_Level_Phenology_Plots(Data_Frame_1, Higher_Trophic_Level_Species_1, Lower_Trophic_Level_Species_1, Date_1, Scale_Bar_Factor = (1 / 3), Lower_Trophic_Level_Species_Names_Data_Frame = Lower_Trophic_Level_Species_Names_Data_Frame_1, Higher_Trophic_Level_Species_Names_Data_Frame = Higher_Trophic_Level_Species_Names_Data_Frame_1, Overall_Plot_Title = "Example Figure 4", Lower_Trophic_Level_Plot_Title = "Plants", Higher_Trophic_Level_Plot_Title = "Pollinators")
#'
#' @export
Generating_Figures_Containing_a_Bipartite_Interaction_Matrix_Visualization_Plot_and_Corresponding_Higher_and_Lower_Trophic_Level_Phenology_Plots <- function (Data_Frame, Higher_Trophic_Level_Species, Lower_Trophic_Level_Species, Date, Overall_Plot_Title = NULL, Lower_Trophic_Level_Plot_Title = "Lower Trophic Level", Higher_Trophic_Level_Plot_Title = "Higher Trophic Level", Prioritize_Interaction_Strength_Over_Alphabetization_in_Species_Layout = FALSE, Lower_Trophic_Level_Color = "darkgreen", Higher_Trophic_Level_Color = "darkred", Interaction_Color = "black", Method_for_Aggregating = "Sum", Include_Scale_Bars = TRUE, Scale_Bar_Factor = (1 / 3), Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool = 0, Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool = 0, Lower_Trophic_Level_Species_Names_Data_Frame = NULL, Higher_Trophic_Level_Species_Names_Data_Frame = NULL, Relative_Plot_Widths = c(11, 4, 10, 4, 11), Relative_Plot_Heights = c(rep(1, ifelse(is.null(Overall_Plot_Title), 1, 2)), ifelse(is.null(Overall_Plot_Title), 9, 8)), Already_Pooled_Lower_Trophic_Level_Species_Name = NULL, Already_Pooled_Higher_Trophic_Level_Species_Name = NULL, Gap_Size_Constant = 0.2, Relative_Bar_Width = 0.2, Horizontal_Scale_Text_Shift_Constant = 0.1, Vertical_Scale_Title_Shift_Constant = 0.1, Scale_Bar_Title_Font_Size = 1, Scale_Bar_Text_Font_Size = 1, Overall_Plot_Title_Font_Size = 3.25, Trophic_Level_Plot_Title_Font_Size = 2.5, Phenology_and_Bipartite_Plot_Title_Font_Size = 1.5, Species_Name_Font_Size = 1.5, Horizontal_Axis_Title_Font_Size = 1, Horizontal_Axis_Dates_Font_Size = 1, Left_and_Right_Figure_Margins = 9, Between_Title_Plots_and_Other_Plots_Margin = 1, Bottom_Margin = 8) {
  if (is.null(Data_Frame)) {
    stop ("The 'Data_Frame' argument must not be missing.")
  }
  if (!is.data.frame(Data_Frame)) {
    stop ("The 'Data_Frame' argument must be of class 'data.frame'.")
  }
  if (!is.character(Higher_Trophic_Level_Species)) {
    stop ("The 'Higher_Trophic_Level_Species' argument must be a vector of character strings.")
  }
  if (any(is.na(Higher_Trophic_Level_Species))) {
    stop ("The 'Higher_Trophic_Level_Species' argument must not contain any missing values.")
  }
  if (!all(Higher_Trophic_Level_Species %in% colnames(Data_Frame))) {
    stop ("Each element of the 'Higher_Trophic_Level_Species' argument must be a column name in the 'Data_Frame' data frame.")
  }
  if (!all(sapply(Higher_Trophic_Level_Species, function (x) {
    is.numeric(Data_Frame[, which(colnames(Data_Frame) == x)])
  }))) {
    stop ("Each of the 'Higher_Trophic_Level_Species' columns in the 'Data_Frame' data frame must be numeric.")
  }
  if (length(Lower_Trophic_Level_Species) != 1) {
    stop ("The 'Lower_Trophic_Level_Species' argument must be of length 1.")
  }
  if (is.na(Lower_Trophic_Level_Species)) {
    stop ("The 'Lower_Trophic_Level_Species' argument must must not be missing.")
  }
  if (!is.character(Lower_Trophic_Level_Species)) {
    stop ("The 'Lower_Trophic_Level_Species' argument must be a character string.")
  }
  if (!(Lower_Trophic_Level_Species %in% colnames(Data_Frame))) {
    stop ("The 'Lower_Trophic_Level_Species' argument must be a column name in the 'Data_Frame' data frame.")
  }
  if (!is.character(Data_Frame[, which(colnames(Data_Frame) == Lower_Trophic_Level_Species)])) {
    stop ("The 'Lower_Trophic_Level_Species' column in the 'Data_Frame' data frame must be of class 'character'.")
  }
  if (length(Date) != 1) {
    stop ("The 'Date' argument must be of length 1.")
  }
  if (is.na(Date)) {
    stop ("The 'Date' argument must not be missing.")
  }
  if (!is.character(Date)) {
    stop ("The 'Date' argument must be a character string.")
  }
  if (!(Date %in% colnames(Data_Frame))) {
    stop ("The 'Date' argument must be a column name in the 'Data_Frame' data frame.")
  }
  if (!inherits(Data_Frame[, which(colnames(Data_Frame) == Date)], 'Date')) {
    stop ("The 'Date' column in the 'Data_Frame' data frame must be of class 'Date'.")
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
  if (!is.null(Lower_Trophic_Level_Species_Names_Data_Frame)) {
    if (class(Lower_Trophic_Level_Species_Names_Data_Frame) != "data.frame") {
      stop ("The 'Lower_Trophic_Level_Species_Names_Data_Frame' argument must be of class 'data.frame'.")
    }
    if (!("Lower_Trophic_Level_Species_Name" %in% colnames(Lower_Trophic_Level_Species_Names_Data_Frame))) {
      stop ("The 'Lower_Trophic_Level_Species_Names_Data_Frame' data frame must contain a column named 'Lower_Trophic_Level_Species_Name'.")
    }
    if (!("Names_for_Plotting" %in% colnames(Lower_Trophic_Level_Species_Names_Data_Frame))) {
      stop ("The 'Lower_Trophic_Level_Species_Names_Data_Frame' data frame must contain a column named 'Names_for_Plotting'.")
    }
    if (!all(Lower_Trophic_Level_Species_Names_Data_Frame$Lower_Trophic_Level_Species_Name %in% Data_Frame[, which(colnames(Data_Frame) == Lower_Trophic_Level_Species)])) {
      stop ("Elements in the 'Lower_Trophic_Level_Species_Name' column from the 'Lower_Trophic_Level_Species_Names_Data_Frame' data frame must also be present in the 'Lower_Trophic_Level_Species' column of the 'Data_Frame' data frame.")
    }
  }
  if (!is.null(Higher_Trophic_Level_Species_Names_Data_Frame)) {
    if (class(Higher_Trophic_Level_Species_Names_Data_Frame) != "data.frame") {
      stop ("The 'Higher_Trophic_Level_Species_Names_Data_Frame' argument must be of class 'data.frame'.")
    }
    if (!("Column_Name" %in% colnames(Higher_Trophic_Level_Species_Names_Data_Frame))) {
      stop ("The 'Higher_Trophic_Level_Species_Names_Data_Frame' data frame must contain a column named 'Column_Name' whose entries must identically match the names of the columns of the matrices (which represent the higher-trophic-level species).")
    }
    if (!("Names_for_Plotting" %in% colnames(Higher_Trophic_Level_Species_Names_Data_Frame))) {
      stop ("The 'Higher_Trophic_Level_Species_Names_Data_Frame' data frame must contain a column named 'Names_for_Plotting' whose entries will appear as the (potentially stylized) higher-trophic-level species names in the resulting figure.")
    }
    if (!all(Higher_Trophic_Level_Species_Names_Data_Frame$Column_Name %in% colnames(Data_Frame))) {
      stop ("Elements in the 'Column_Name' column from the 'Higher_Trophic_Level_Species_Names_Data_Frame' data frame must also be column names of the 'Data_Frame' data frame.")
    }
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
  if (length(Method_for_Aggregating) != 1) {
    stop ("The 'Method_for_Aggregating' argument must be of length 1.")
  }
  if (is.na(Method_for_Aggregating)) {
    stop ("The 'Method_for_Aggregating' argument must must be provided.")
  }
  if (!is.character(Method_for_Aggregating)) {
    stop ("The 'Method_for_Aggregating' argument must be a character string.")
  }
  if (!(Method_for_Aggregating %in% c("Sum", "Average"))) {
    stop ("The 'Method_for_Aggregating' argument must be either 'Sum' or 'Average'.")
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
  if (!is.null(Already_Pooled_Lower_Trophic_Level_Species_Name)) {
    if (length(Already_Pooled_Lower_Trophic_Level_Species_Name) != 1) {
      stop ("The 'Already_Pooled_Lower_Trophic_Level_Species_Name' argument must be of length 1.")
    }
    if (!is.character(Already_Pooled_Lower_Trophic_Level_Species_Name)) {
      stop ("The 'Already_Pooled_Lower_Trophic_Level_Species_Name' argument must be a character string.")
    }
    if (!(Already_Pooled_Lower_Trophic_Level_Species_Name %in% Data_Frame[, which(colnames(Data_Frame) == Lower_Trophic_Level_Species)])) {
      stop ("The 'Already_Pooled_Lower_Trophic_Level_Species_Name' argument must be an element of the 'Lower_Trophic_Level_Species' column in the 'Data_Frame' data frame.")
    }
  }
  if (!is.null(Already_Pooled_Higher_Trophic_Level_Species_Name)) {
    if (length(Already_Pooled_Higher_Trophic_Level_Species_Name) != 1) {
      stop ("The 'Already_Pooled_Higher_Trophic_Level_Species_Name' argument must be of length 1.")
    }
    if (!is.character(Already_Pooled_Higher_Trophic_Level_Species_Name)) {
      stop ("The 'Already_Pooled_Higher_Trophic_Level_Species_Name' argument must be a character string.")
    }
    if (!(Already_Pooled_Higher_Trophic_Level_Species_Name %in% colnames(Data_Frame))) {
      stop ("The 'Already_Pooled_Higher_Trophic_Level_Species_Name' argument must be a column name in the 'Data_Frame' data frame.")
    }
    if (!(Already_Pooled_Higher_Trophic_Level_Species_Name %in% Higher_Trophic_Level_Species)) {
      stop ("The 'Already_Pooled_Higher_Trophic_Level_Species_Name' argument must be an element of the 'Higher_Trophic_Level_Species' argument.")
    }
  }
  if ((Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool > 0) & (!is.null(Already_Pooled_Lower_Trophic_Level_Species_Name))) {
    stop ("If there is already a row representing pooled lower-trophic-level species, additional pooling of lower-trophic-level species cannot be done. If additional pooling is desired, please do it before using this function.")
  }
  if ((Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool > 0) & (!is.null(Already_Pooled_Higher_Trophic_Level_Species_Name))) {
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
  Split_Data <- split(Data_Frame, Data_Frame[, which(colnames(Data_Frame) == Date)])
  Split_Data <- lapply(Split_Data, function (x) {
    split(x, x[, which(colnames(x) == Lower_Trophic_Level_Species)])
  })
  Numbers_of_Rows_of_Each_Combination <- setNames(unlist(mapply(function (u, v) {
    mapply(function (x, y) {
      nrow(x)
    }, x = u, y = names(u), SIMPLIFY = F)
  }, u = Split_Data, v = names(Split_Data), SIMPLIFY = F)), NULL)
  Dates_of_Each_Combination <- setNames(unlist(mapply(function (u, v) {
    mapply(function (x, y) {
      v
    }, x = u, y = names(u), SIMPLIFY = F)
  }, u = Split_Data, v = names(Split_Data), SIMPLIFY = F)), NULL)
  Lower_Trophic_Level_Species_of_Each_Combination <- setNames(unlist(mapply(function (u, v) {
    mapply(function (x, y) {
      y
    }, x = u, y = names(u), SIMPLIFY = F)
  }, u = Split_Data, v = names(Split_Data), SIMPLIFY = F)), NULL)
  Numbers_of_Rows <- data.frame(Date = Dates_of_Each_Combination, Lower_Trophic_Level_Species = Lower_Trophic_Level_Species_of_Each_Combination, Number_of_Rows = Numbers_of_Rows_of_Each_Combination)
  All_Combinations <- expand.grid(unique(Data_Frame[, which(colnames(Data_Frame) == Date)]), unique(Data_Frame[, which(colnames(Data_Frame) == Lower_Trophic_Level_Species)]))
  colnames(All_Combinations) <- c(Date, Lower_Trophic_Level_Species)
  Actual_Combinations <- as.data.frame(t(as.matrix(as.data.frame(strsplit(unique(as.character(interaction(Data_Frame[, which(colnames(Data_Frame) == Date)], Data_Frame[, which(colnames(Data_Frame) == Lower_Trophic_Level_Species)]))), "[.]")))))
  rownames(Actual_Combinations) <- NULL
  colnames(Actual_Combinations) <- c(Date, Lower_Trophic_Level_Species)
  Vector <- sapply(seq_len(nrow(All_Combinations)), function (x) {
    all(!sapply(seq_len(nrow(Actual_Combinations)), function (y) {
      all(All_Combinations[x, ] == Actual_Combinations[y, ])
    }))
  })
  if ((any(Numbers_of_Rows$Number_of_Rows != max(Numbers_of_Rows$Number_of_Rows))) | (any(Vector == T))) {
    Maximum_Value <- max(Numbers_of_Rows$Number_of_Rows)
    Missing_Lower_Trophic_Level_Species_and_Dates <- Numbers_of_Rows[which(Numbers_of_Rows$Number_of_Rows < Maximum_Value), ]
    First_Batch_of_Messages <- lapply(seq_len(nrow(Missing_Lower_Trophic_Level_Species_and_Dates)), function (i) {
      if (Missing_Lower_Trophic_Level_Species_and_Dates$Number_of_Rows[i] == 1) {
        paste0("there is only ", Missing_Lower_Trophic_Level_Species_and_Dates$Number_of_Rows[i], " row for the ", Missing_Lower_Trophic_Level_Species_and_Dates$Lower_Trophic_Level_Species[i], " lower-trophic-level species and the ", Missing_Lower_Trophic_Level_Species_and_Dates$Date[i], " date")
      } else if (Missing_Lower_Trophic_Level_Species_and_Dates$Number_of_Rows[i] > 1) {
        paste0("there are only ", Missing_Lower_Trophic_Level_Species_and_Dates$Number_of_Rows[i], " rows for the ", Missing_Lower_Trophic_Level_Species_and_Dates$Lower_Trophic_Level_Species[i], " lower-trophic-level species and the ", Missing_Lower_Trophic_Level_Species_and_Dates$Date[i], " date")
      }
    })
    Missing_Combinations <- All_Combinations[which(Vector), ]
    Second_Batch_of_Messages <- lapply(seq_len(nrow(Missing_Combinations)), function (i) {
      paste0("there are no rows for the ", Missing_Combinations$Lower_Trophic_Level_Species[i], " lower-trophic-level species and the ", Missing_Combinations$Date[i], " date")
    })
    Messages <- c(Second_Batch_of_Messages, First_Batch_of_Messages)
    stop (paste0("The data set is not balanced: ", Reduce(function (x, y) {
      paste0(x, ", ", y)
    }, Messages[seq_len((length(Messages) - 1))]), ", and ", Messages[[length(Messages)]], ", but there should be ", Maximum_Value, " row in each case."))
  }
  if (Method_for_Aggregating == "Sum") {
    Data_Frame <- Reduce(function (a, b) {
      merge(a, b, by = c("Date", "Lower_Trophic_Level_Species"))
    }, lapply(Higher_Trophic_Level_Species, function (x) {
      y <- aggregate(Data_Frame[, which(colnames(Data_Frame) == x)], list(Data_Frame[, which(colnames(Data_Frame) == Date)], Data_Frame[, which(colnames(Data_Frame) == Lower_Trophic_Level_Species)]), sum)
      colnames(y) <- c("Date", "Lower_Trophic_Level_Species", x)
      y
    }))
  } else if (Method_for_Aggregating == "Average") {
    Data_Frame <- Reduce(function (a, b) {
      merge(a, b, by = c("Date", "Lower_Trophic_Level_Species"))
    }, lapply(Higher_Trophic_Level_Species, function (x) {
      y <- aggregate(Data_Frame[, which(colnames(Data_Frame) == x)], list(Data_Frame[, which(colnames(Data_Frame) == Date)], Data_Frame[, which(colnames(Data_Frame) == Lower_Trophic_Level_Species)]), mean)
      colnames(y) <- c("Date", "Lower_Trophic_Level_Species", x)
      y
    }))
  }
  List_of_Bipartite_Interaction_Matrices_by_Date <- split(Data_Frame, Data_Frame[, which(colnames(Data_Frame) == Date)])
  List_of_Bipartite_Interaction_Matrices_by_Date <- lapply(List_of_Bipartite_Interaction_Matrices_by_Date, function (x) {
    Lower_Trophic_Level_Species <- x[, which(colnames(Data_Frame) == Lower_Trophic_Level_Species)]
    x <- as.matrix(x[, c(which(colnames(x) %in% Higher_Trophic_Level_Species))])
    rownames(x) <- Lower_Trophic_Level_Species
    x
  })
  if (is.null(Already_Pooled_Lower_Trophic_Level_Species_Name)) {
    List_of_Bipartite_Interaction_Matrices_by_Date <- lapply(List_of_Bipartite_Interaction_Matrices_by_Date, function (x) {
      x[order(rownames(x)), ]
    })
  } else if (!is.null(Already_Pooled_Lower_Trophic_Level_Species_Name)) {
    List_of_Bipartite_Interaction_Matrices_by_Date <- lapply(List_of_Bipartite_Interaction_Matrices_by_Date, function (x) {
      Already_Pooled_Row <- x[which(rownames(x) == Already_Pooled_Lower_Trophic_Level_Species_Name), ]
      Other_Rows <- x[which(rownames(x) != Already_Pooled_Lower_Trophic_Level_Species_Name), ]
      Other_Rows <- Other_Rows[order(colnames(Other_Rows)), ]
      y <- rbind(Other_Rows, Already_Pooled_Row)
      rownames(y)[nrow(y)] <- Already_Pooled_Lower_Trophic_Level_Species_Name
      y
    })
  }
  if (is.null(Already_Pooled_Higher_Trophic_Level_Species_Name)) {
    List_of_Bipartite_Interaction_Matrices_by_Date <- lapply(List_of_Bipartite_Interaction_Matrices_by_Date, function (x) {
      x[, order(colnames(x))]
    })
  } else if (!is.null(Already_Pooled_Higher_Trophic_Level_Species_Name)) {
    List_of_Bipartite_Interaction_Matrices_by_Date <- lapply(List_of_Bipartite_Interaction_Matrices_by_Date, function (x) {
      Already_Pooled_Column <- x[, which(colnames(x) == Already_Pooled_Higher_Trophic_Level_Species_Name)]
      Other_Columns <- x[, which(colnames(x) != Already_Pooled_Higher_Trophic_Level_Species_Name)]
      Other_Columns <- Other_Columns[, order(colnames(Other_Columns))]
      y <- cbind(Other_Columns, Already_Pooled_Column)
      colnames(y)[ncol(y)] <- Already_Pooled_Higher_Trophic_Level_Species_Name
      y
    })
  }
  Dates <- as.Date(names(List_of_Bipartite_Interaction_Matrices_by_Date))
  List_of_Bipartite_Interaction_Matrices_by_Date <- List_of_Bipartite_Interaction_Matrices_by_Date[order(Dates)]
  Dates <- Dates[order(Dates)]
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
    } else if (!is.null(Already_Pooled_Lower_Trophic_Level_Species_Name)) {
      Aggregated_Matrix_to_Reorder <- Aggregated_Matrix_to_Reorder[which(rownames(Aggregated_Matrix_to_Reorder) != Already_Pooled_Lower_Trophic_Level_Species_Name), ]
    }
    if (Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool > 0) {
      Aggregated_Matrix_to_Reorder <- Aggregated_Matrix_to_Reorder[, which(colnames(Aggregated_Matrix_to_Reorder) != "Other Species")]
    } else if (!is.null(Already_Pooled_Higher_Trophic_Level_Species_Name)) {
      Aggregated_Matrix_to_Reorder <- Aggregated_Matrix_to_Reorder[, which(colnames(Aggregated_Matrix_to_Reorder) != Already_Pooled_Higher_Trophic_Level_Species_Name)]
    }
    Optimally_Reordered_Matrix <- DBM.functions::Attempting_to_Diagonalize_a_Matrix_Based_on_Its_Full_Diagonal(Aggregated_Matrix_to_Reorder)
    Optimally_Reordered_Matrix <- Optimally_Reordered_Matrix$Optimally_Reordered_Matrix
    if (Approximate_Proportion_of_Lower_Trophic_Level_Species_to_Pool > 0) {
      Optimally_Reordered_Matrix <- rbind(Optimally_Reordered_Matrix, Aggregated_Matrix[which(rownames(Aggregated_Matrix) == "Other Species"), ])
      rownames(Optimally_Reordered_Matrix)[nrow(Optimally_Reordered_Matrix)] <- "Other Species"
    } else if (!is.null(Already_Pooled_Lower_Trophic_Level_Species_Name)) {
      Optimally_Reordered_Matrix <- rbind(Optimally_Reordered_Matrix, Aggregated_Matrix[which(rownames(Aggregated_Matrix) == Already_Pooled_Lower_Trophic_Level_Species_Name), ])
      rownames(Optimally_Reordered_Matrix)[nrow(Optimally_Reordered_Matrix)] <- Already_Pooled_Lower_Trophic_Level_Species_Name
    }
    if (Approximate_Proportion_of_Higher_Trophic_Level_Species_to_Pool > 0) {
      Optimally_Reordered_Matrix <- cbind(Optimally_Reordered_Matrix, Aggregated_Matrix[, which(colnames(Aggregated_Matrix) == "Other Species")])
      colnames(Optimally_Reordered_Matrix)[ncol(Optimally_Reordered_Matrix)] <- "Other Species"
    } else if (!is.null(Already_Pooled_Higher_Trophic_Level_Species_Name)) {
      Optimally_Reordered_Matrix <- cbind(Optimally_Reordered_Matrix, Aggregated_Matrix[, which(colnames(Aggregated_Matrix) == Already_Pooled_Higher_Trophic_Level_Species_Name)])
      colnames(Optimally_Reordered_Matrix)[ncol(Optimally_Reordered_Matrix)] <- Already_Pooled_Higher_Trophic_Level_Species_Name
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
  if (is.null(Lower_Trophic_Level_Species_Names_Data_Frame)) {
    sapply(seq_along(unique(rownames(Aggregated_Matrix))), function (i) {
      text(mean(c(par("usr")[1:2])), Cumulative_Lower_Trophic_Level_Heights_Data_Frame$Midpoint, rownames(Aggregated_Matrix), xpd = T, cex = Species_Name_Font_Size)
    })
  } else if (!is.null(Lower_Trophic_Level_Species_Names_Data_Frame)) {
    sapply(seq_along(rownames(Aggregated_Matrix)), function (i) {
      text(mean(c(par("usr")[1:2])), Cumulative_Lower_Trophic_Level_Heights_Data_Frame$Midpoint[i], ifelse(rownames(Aggregated_Matrix)[i] %in% Lower_Trophic_Level_Species_Names_Data_Frame$Row_Name, tryCatch (eval(parse(text = Lower_Trophic_Level_Species_Names_Data_Frame$Names_for_Plotting[match(rownames(Aggregated_Matrix), Lower_Trophic_Level_Species_Names_Data_Frame$Row_Name)][i])), error = function (e) {
        rownames(Aggregated_Matrix)[i]
      }), rownames(Aggregated_Matrix)[i]), xpd = T, cex = Species_Name_Font_Size)
    })
  }
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

  #####
  if (is.null(Higher_Trophic_Level_Species_Names_Data_Frame)) {
    sapply(seq_along(unique(colnames(Aggregated_Matrix))), function (i) {
      text(mean(c(par("usr")[1:2])), Cumulative_Higher_Trophic_Level_Heights_Data_Frame$Midpoint, colnames(Aggregated_Matrix), xpd = T, cex = Species_Name_Font_Size)
    })
  } else if (!is.null(Higher_Trophic_Level_Species_Names_Data_Frame)) {
    sapply(seq_along(colnames(Aggregated_Matrix)), function (i) {
      text(mean(c(par("usr")[1:2])), Cumulative_Higher_Trophic_Level_Heights_Data_Frame$Midpoint[i], ifelse(colnames(Aggregated_Matrix)[i] %in% Higher_Trophic_Level_Species_Names_Data_Frame$Column_Name, tryCatch (eval(parse(text = Higher_Trophic_Level_Species_Names_Data_Frame$Names_for_Plotting[match(colnames(Aggregated_Matrix), Higher_Trophic_Level_Species_Names_Data_Frame$Column_Name)][i])), error = function (e) {
        colnames(Aggregated_Matrix)[i]
      }), colnames(Aggregated_Matrix)[i]))
    })
  }
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
