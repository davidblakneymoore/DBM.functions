# `DBM.functions`: A Variety of Functions for a Variety of Applications


#### Overview

This R package, which has no dependencies, contains a variety of functions for a variety of applications. Download it by running the line of code `devtools::install_github("davidblakneymoore/DBM.functions")`. For a much more detailed description of some of these functions, please read [the vignette](https://github.com/davidblakneymoore/DBM.functions/blob/main/vignettes/DBM.functions.Rmd).


#### Functions

The function `Aligning_Values_Across_Multiple_Vertical_Axes()` generates axis limits for aligning values across multiple vertical axes on plots. [Here](https://raw.githubusercontent.com/davidblakneymoore/DBM.functions/main/Aligning%20Three%20Variables%20Across%20Three%20Vertical%20Axes%20Figure.jpeg) and [here](https://raw.githubusercontent.com/davidblakneymoore/DBM.functions/main/Sugar%20Maple%20Sap%20Flow%20and%20Wood%20Temperature%20Time-Series%20Plot.jpeg) are example figures that used this function to align values across multiple vertical axes.

The function `Arranging_Plots_Nicely()` generates a plot layout matrix that is as square as possible - in other words, it generates a plot layout matrix whose number of rows and number of columns differ by either `0` (if possible) or `1` (as a last resort). [Here](https://raw.githubusercontent.com/davidblakneymoore/DBM.functions/main/Plotting%20the%20'mpg'%20Column%20Against%20Other%20Columns%20From%20the%20'mtcars'%20Data%20Frame.jpeg) is an example figure that was made using this function.

The function `Attempting_to_Diagonalize_a_Matrix_Based_on_Its_Full_Diagonal()` performs row exchanges and column exchanges on a matrix until the largest entries in the matrix lie as close as possible to the full diagonal and the smallest entries in the matrix lie as far as possible from the full diagonal. This procedure is useful for creating bipartite interaction matrix visualization plots where the species (nodes) from different trophic levels that interact more frequently are closer together and species (nodes) that interact less frequently are farther apart on the bipartite interaction visualization plot.

The function `Comparing_Multiple_Independent_Correlation_Coefficients()` compares multiple correlation coefficients from independent correlations (Levy, 1977). This function generates p values for pairwise correlation coefficient comparisons as well as means separation lettering. It has been cited in several publications (Beghin, 2023; Chue and Yeo, 2022; Findor et al., 2021; Matko and Sedlmeier, 2023).

The function `Finding_the_Full_Diagonal_of_a_Matrix()` finds the full diagonal of a matrix. A full diagonal of a matrix is the diagonal of the matrix that is the line segment that starts at the entry in the first row and the first column of the matrix and that ends at the entry in the last row and the last column of the matrix. If the numbers of rows and columns are coprime, the only two matrix entries that fall perfectly along the full diagonal are its two endpoints; when the numbers of rows and coliumns are not coprime, there will be more than two elements of the full diagonal. For square matrices, the main diagonal and the full diagonal are identical, but for non-square matrices, the main diagonal and the full diagonal are not identical.

The function `Finding_the_Optimal_Sigmoid_Function_Model()` determines which sigmoid function best fits a binary response data set (a data set where the response variable contains only `1`s and `0`s) from ten different, fully differentiable sigmoid functions. [Here](https://raw.githubusercontent.com/davidblakneymoore/DBM.functions/main/Comparing%20Sigmoid%20Function%20Models.jpeg) is a plot that shows what this function can do.

The function `Generating_Figures_Containing_Lower_Trophic_Level_Phenology_Plots_a_Bipartite_Interaction_Matrix_Visualization_Plot_and_Upper_Trophic_Level_Phenology_Plots()` generates violin-like phenology plots that depict the relative abundances of lower-trophic-level and higher-trophic-level species over time, and it also generates bipartite interaction matrix visualization plots which are positioned centrally between the two sets of phenology plots. [Here](https://raw.githubusercontent.com/davidblakneymoore/DBM.functions/main/Bipartite%20Interaction%20Matrix%20Visualization%20Plot%20With%20Phenology%20Plots%20-%20Example%201.jpeg) and [here](https://raw.githubusercontent.com/davidblakneymoore/DBM.functions/main/Bipartite%20Interaction%20Matrix%20Visualization%20Plot%20With%20Phenology%20Plots%20-%20Example%202.jpeg) are figures created using made-up data with this function. This function was heavily inspired by a manuscript that contained a figure very similar to the one this function returns (Russo et al., 2013).

The function `Generating_Permutations()` generates all possible permutations for a given number of items.

The function `Making_Reasonable_Scale_Bars()` generates nice round numbers for scale bars using a slick trick: the base-10 logarithm of a given proportion (such as 25 % of the plotting area, as defined by the `Scale_Bar_Factor` argument) is calculated, and then rounded (using the `floor()` and `ceiling()` functions) to the nearest integer, and then the number 10 is raised to these round numbers. This process produces nice, round numbers for scale bars which are based on powers of 10. Using the `floor()` and `ceiling()` functions, unless the given proportion of the plotting area (in the units of the plotted variable) is exactly a multiple of 10, two scale bar possibilities will be generated - one smaller one and one larger one - and the one that's closer to the originally given proportion of the plotting area (the `Scale_Bar_Factor` argument) will be chosen as the final scale bar.

The function `Optimally_Assigning_Experimental_Units_to_Treatment_Groups_With_a_Blocking_Variable()` assigns experimental units to treatment groups (for cases when there is a blocking variable) in a way that ensures that treatment groups are as balanced as possible for a particular set of experimental units' variables. This function works by calculating means and possibly other higher-order mathematical moments (such as variances, skewnesses, and kurtoses) for a particular set of experimental units' variables that have already been measured and, out of every single possible grouping arrangement, choosing the one that holds these moments as similar as possible across treatment groups.

The function `Optimally_Assigning_Experimental_Units_to_Treatment_Groups_Without_a_Blocking_Variable()` assigns experimental units to treatment groups (for cases when there is no blocking variable) in a way that ensures that treatment groups are as balanced as possible for a particular set of experimental units' variables. This function works by calculating means and possibly other higher-order mathematical moments (such as variances, skewnesses, and kurtoses) for a particular set of experimental units' variables that have already been measured and, out of every single possible grouping arrangement, choosing the one that holds these moments as similar as possible across treatment groups.


#### Data Frames

The data frame `Sugar_Maple_Data` may be used with the `Aligning_Values_Across_Multiple_Vertical_Axes()` function - the `Sap_Flow` and `Wood_Temperature` columns in this data frame can be aligned across primary and secondary vertical axes at the values of `0` as shown [here](https://raw.githubusercontent.com/davidblakneymoore/DBM.functions/main/Sugar%20Maple%20Sap%20Flow%20and%20Wood%20Temperature%20Time-Series%20Plot.jpeg).


#### Works Cited

Beghin, G. 2023. Does the Lay Concept of Mental Disorder Necessitate a Dysfunction? Advances in Experimental Philosophy of Medicine, edited by Kristien Hens and Andreas De Block. Bloomsbury Publishing. Pp. 71-96.

Chue, K.L., and A. Yeo. 2022. Exploring associations of positive relationships and adolescent well-being across cultures. Youth Soc. 00:1-12.

Findor, A., M. Hruska, P. Jankovská, and M. Pobudová. 2021. Re-examining public opinion preferences for migrant categorizations: “Refugees” are evaluated more negatively than “migrants” and “foreigners” related to participants’ direct, extended, and mass-mediated intergroup contact experiences. Int. J. Intercult. Relat. 80:262-273.

Levy, K.J. 1977. Pairwise comparisons involving unequal sample sizes associated with correlations, proportions or variances. Br. J. Math. Stat. Psychol. 30:137-139.

Matko, K., and P. Sedlmeier. 2023. Which meditation technique for whom? An experimental single-case study comparing concentrative, humming, observing-thoughts, and walking meditation.

Moore, D.B. 2024. `DBM.functions`: A Variety of Functions for a Variety of Applications. R package version 0.0.0.9000. https://github.com/davidblakneymoore/DBM.functions.

Russo, L., N. DeBarros, S. Yang, K. Shea, and D. Mortensen. 2013. Supporting crop pollinators with floral resources: network-based phenological matching. Ecol. Evol. 3:3125-3140.
