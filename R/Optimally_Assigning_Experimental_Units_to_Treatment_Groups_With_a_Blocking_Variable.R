#' Optimally Assigning Experimental Units to Treatment Groups With a Blocking Variable
#'
#' This function may be used to assign experimental units to treatment groups (for cases when there is a blocking variable) when you wish to ensure that treatment groups are as balanced as possible for a particular set of experimental units' variables.
#'
#' `Optimally_Assigning_Experimental_Units_to_Treatment_Groups_With_a_Blocking_Variable` may be used when setting out a study or an experiment (for cases when there is a blocking variable) to optimize the assignments of experimental units to treatment groups. Although randomization is typically used to assign treatments to experimental units, sometimes, randomization may unintentionally create treatment groups that are not balanced. For example, perhaps after randomizing, certain treatment groups end up being comprised of experimental units that are bigger or heavier or hotter or shadier or wetter than the experimental units in the other treatment groups are. This undesirable effect may be avoided by using this function, which ensures that means and possibly other higher-order mathematical moments (such as variances, skewnesses, and kurtoses) for a particular set of experimental units' variables you've already measured are as similar as possible across treatment groups.
#'
#' This function was designed for studies or experiments that are blocked - it ensures that balance is maintained across treatment groups within blocks. One of the advantages of blocked studies or experiments is that blocks may account for some of the overall variability in the data (sample processing should be done, and measurements should be taken, by block), and if a problem arises with a block (or multiple blocks), this block (or these blocks) may be dropped from the study without losing the balanced structure of the study or experiment. By ensuring that treatment groups are balanced within blocks, this function ensures that if a block (or multiple blocks) must be dropped from the study or experiment, the remaining blocks will still be as balanced as possible since each block was balanced independently.
#'
#' Please note that randomization is still a very useful technique - if you suspect that you did not measure a suite of experimental unit variables that may have a strong influence on the response variables in your study or experiment, randomization may be a better method than this optimization method.
#'
#' Also, please note that this function may take an extremely long time to run when there are many potential experimental units!
#' @param ... the variables you measured on your experimental units that you wish to keep as consistent as possible across treatment groups.
#' @param Identifiers the names of the experimental units.
#' @param Blocking_Variable the names of the blocks. If there is more than one blocking variable, please use R's `interaction()` function to combine the different blocking variables into one variable before using the function.
#' @param Data_Frame an optional argument to provide if all the `...` arguments, the `Identifiers` argument, and the `Blocking_Variable` argument all come from the same data frame and to prevent typing the data frame name multiple times.
#' @param Number_of_Treatment_Groups the number of treatment groups in the study or experiment.
#' @param Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group the number of experimental units to be assigned to each treatment group in each block in the study or experiment. This function only handles balanced designs where each treatment group contains the same number of experimental units from each block.
#' @param Variable_Weights the relative weights (importances) of each of the `...` arguments (variables) in the calculations. Variables that are weighed more heavily will be held more consistent across treatment groups than variables that are weighed less heavily. By default, each variable is weighed equally.
#' @param Moment_Weighing_Method the method of weighing mathematical moments. Almost all of the built-in methods weigh moments sequentially, with lower-order moments being weighed more heavily than higher-order moments are weighed. This function calculates moments up to the order of the number of experimental units in each treatment group (as specified in the `Number_of_Experimental_Units_in_Each_Treatment_Group` argument). The default method weighs each moment more heavily than the subsequent moment by a factor of the Golden Ratio (`((1 + sqrt(5)) / 2)`), with lower-order moments being weighed the most heavily and higher-order moments being weighed the least heavily.
#' @param Custom_Moment_Weights the weights of each of the mathematical moments (in order from the lowest-order moment to the highest-order moment) if and only if `"Custom Weights"` is specified for the `Moment_Weighing_Method` argument.
#'
#' @return This function returns optimal treatment group assignments (for cases when there is a blocking variable). Since experimental units are assigned to treatment groups separately (in a completely independent fashion) by block, each of the main list elements of the function's output correspond to a different block.
#'
#' @author David B. Moore (\email{davidblakneymoore@@gmail.com})
#'
#' @examples
#' # Generate Some Made-up Data
#' set.seed(2024)
#' Data_Frame <- data.frame(Tree_Identifier = paste("Tree", seq_len(40), sep = "_"), Block = rep(1:4, each = 10, length.out = 40), Diameter_at_Breast_Height = round(runif(40, 25, 50), 1), Height = round(runif(40, 30, 40), 1), Crown_Class = sample(c("Dominant", "Codominant", "Intermediate", "Overtopped"), 40, replace = TRUE), Significant_Defect_Present = sample(c("Yes", "No"), 40, replace = TRUE, prob = c(0.05, 0.95)))
#'
#' # Determine Which Combinations Are Optimal for Assigning Experimental Units to Treatment Groups
#' Optimal_Group_Assignments_by_Block <- DBM.functions::Optimally_Assigning_Experimental_Units_to_Treatment_Groups_With_a_Blocking_Variable(Diameter_at_Breast_Height, Height, Crown_Class, Significant_Defect_Present, Identifiers = Tree_Identifier, Blocking_Variable = Block, Data_Frame = Data_Frame, Number_of_Treatment_Groups = 4, Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group = 2)
#' (The_Best_3_Grouping_Combinations_for_Each_Block <- lapply(Optimal_Group_Assignments_by_Block, `[`, seq_len(3)))
#'
#' @export
Optimally_Assigning_Experimental_Units_to_Treatment_Groups_With_a_Blocking_Variable <- function (..., Identifiers, Blocking_Variable, Data_Frame = NULL, Number_of_Treatment_Groups, Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group, Variable_Weights = rep(1, ifelse(missing(Data_Frame), length(list(...)), ncol(Data_Frame[, c(which(colnames(Data_Frame) %in% sapply(match.call(expand.dots = FALSE)$..., deparse)))]))), Moment_Weighing_Method = c("Exponential (2 Base)", "Exponential (Golden Ratio Base)", "Ordinal", "Exponential (Euler's Number Base)", "None", "Custom Weights"), Custom_Moment_Weights = NULL) {
  Variable_Names <- sapply(match.call(expand.dots = FALSE)$..., deparse)
  Identifiers_Name <- deparse(substitute(Identifiers))
  Blocking_Variable_Name <- deparse(substitute(Blocking_Variable))
  if (missing(Data_Frame)) {
    if (length(unique(sapply(list(...), length))) != 1) {
      stop ("The '...' arguments must all contain the same number of elements.")
    }
    if ((unique(sapply(list(...), length))) != length(Identifiers)) {
      stop ("The 'Identifiers' argument must contain the same number of elements as each of the '...' arguments.")
    }
    if ((unique(sapply(list(...), length))) != length(Blocking_Variable)) {
      stop ("The 'Blocking_Variable' argument must contain the same number of elements as each of the '...' arguments.")
    }
    Data_Frame <- as.data.frame(list(...))
    colnames(Data_Frame) <- Variable_Names
    Data_Frame[, Identifiers_Name] <- Identifiers
    Data_Frame[, Blocking_Variable_Name] <- Blocking_Variable
    Data_Frame <- Data_Frame[, c(which(colnames(Data_Frame) == Identifiers_Name), which(colnames(Data_Frame) == Blocking_Variable_Name), which((colnames(Data_Frame) != Identifiers_Name) & (colnames(Data_Frame) != Blocking_Variable_Name)))]
  } else if (!missing(Data_Frame)) {
    if (!("data.frame" %in% class(Data_Frame))) {
      stop ("The 'Data_Frame' argument must be of class 'data.frame'.")
    }
    Data_Frame <- Data_Frame[, which(colnames(Data_Frame) %in% c(Identifiers_Name, Blocking_Variable_Name, Variable_Names))]
    Data_Frame <- Data_Frame[, c(which(colnames(Data_Frame) == Identifiers_Name), which(colnames(Data_Frame) == Blocking_Variable_Name), which((colnames(Data_Frame) != Identifiers_Name) & (colnames(Data_Frame) != Blocking_Variable_Name)))]
  }
  if (any(is.na(Data_Frame[, Identifiers_Name]))) {
    stop ("Please make sure there are no missing values in the 'Identifiers' argument.")
  }
  if (any(is.na(Data_Frame[, Blocking_Variable_Name]))) {
    stop ("Please make sure there are no missing values in the 'Blocking_Variable' argument.")
  }
  if (length(unique(Data_Frame[, Identifiers_Name])) != length(Data_Frame[, Identifiers_Name])) {
    stop ("Please make sure each of the elements in the 'Identifiers' argument are distinct.")
  }
  if (any(!complete.cases(Data_Frame))) {
    stop ("Please ensure that all '...' arguments contain no missing values.")
  }
  if ((!is.vector(Number_of_Treatment_Groups)) | (length(Number_of_Treatment_Groups) != 1)) {
    stop ("The 'Number_of_Treatment_Groups' argument must be a vector of length 1.")
  }
  if (!is.finite(Number_of_Treatment_Groups)) {
    stop ("The 'Number_of_Treatment_Groups' argument must be finite.")
  }
  if (!is.numeric(Number_of_Treatment_Groups)) {
    stop ("The 'Number_of_Treatment_Groups' argument must be numeric")
  }
  if (Number_of_Treatment_Groups < 1) {
    stop ("The 'Number_of_Treatment_Groups' argument must be a positive number.")
  }
  if ((Number_of_Treatment_Groups %% 1) != 0) {
    stop ("The 'Number_of_Treatment_Groups' argument must be an integer.")
  }
  if ((!is.vector(Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group)) | (length(Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group) != 1)) {
    stop ("The 'Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group' argument must be a vector of length 1.")
  }
  if (!is.finite(Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group)) {
    stop ("The 'Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group' argument must be finite.")
  }
  if (!is.numeric(Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group)) {
    stop ("The 'Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group' argument must be numeric")
  }
  if (Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group < 1) {
    stop ("The 'Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group' argument must be a positive number.")
  }
  if ((Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group %% 1) != 0) {
    stop ("The 'Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group' argument must be an integer.")
  }
  if ((Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group * Number_of_Treatment_Groups) > min(sapply(split(Data_Frame, Data_Frame[, which(colnames(Data_Frame) == Blocking_Variable_Name)]), nrow))) {
    stop (paste("At least one block does not contain enough experimental units to have", Number_of_Treatment_Groups, "treatment groups with", Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group, "experimental units in each group."))
  }
  if (any(!is.finite(Variable_Weights))) {
    stop ("All elements in the 'Variable_Weights' argument must be finite.")
  }
  if (!is.numeric(Variable_Weights)) {
    stop ("The 'Variable_Weights' argument must be numeric.")
  }
  if ((!is.vector(Variable_Weights)) | (length(Variable_Weights) != length(Variable_Names))) {
    stop ("The 'Variable_Weights' argument must be a vector and contain the same number of elements as there are '...' arguments.")
  }
  if (any(Variable_Weights < 0)) {
    stop ("The 'Variable_Weights' argument must contain only nonnegative numbers.")
  }
  if (any(!(Moment_Weighing_Method %in% c("Exponential (Golden Ratio Base)", "Exponential (2 Base)", "Ordinal", "Exponential (Euler's Number Base)", "None", "Custom Weights")))) {
    stop ("The 'Moment_Weighing_Method' argument must be 'Exponential (Golden Ratio Base)', 'Exponential (2 Base)', 'Ordinal', 'Exponential (Euler's Number Base)', 'None', or 'Custom Weights'.")
  }
  if (Moment_Weighing_Method[1] == "Custom Weights") {
    if (is.null(Custom_Moment_Weights)) {
      stop ("Please supply the 'Custom_Moment_Weights' argument if you wish to use custom weights to weigh the moments.")
    }
    if ((!is.vector(Custom_Moment_Weights)) | (length(Custom_Moment_Weights) != Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group)) {
      stop ("The 'Custom_Moment_Weights' argument must be a vector containing 'Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group' elements.")
    }
    if (any(!is.finite(Custom_Moment_Weights))) {
      stop ("All elements in the 'Custom_Moment_Weights' argument must be finite.")
    }
    if (!is.numeric(Custom_Moment_Weights)) {
      stop ("The 'Custom_Moment_Weights' argument must be numeric.")
    }
    if (any(Custom_Moment_Weights < 0)) {
      stop ("The 'Custom_Moment_Weights' argument must contain only nonnegative numbers.")
    }
  }
  if (Moment_Weighing_Method[1] == "Exponential (Golden Ratio Base)") {
    Moment_Weights <- rev(((1 + sqrt(5)) / 2) ^ (seq_len(Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group) - 1))
  } else if (Moment_Weighing_Method[1] == "Exponential (2 Base)") {
    Moment_Weights <- rev(2 ^ (seq_len(Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group) - 1))
  } else if (Moment_Weighing_Method[1] == "Ordinal") {
    Moment_Weights <- rev(seq_len(Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group))
  } else if (Moment_Weighing_Method[1] == "Exponential (Euler's Number Base)") {
    Moment_Weights <- rev(exp(seq_len(Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group) - 1))
  } else if (Moment_Weighing_Method[1] == "None") {
    Moment_Weights <- rep(1, length(Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group))
  } else if (Moment_Weighing_Method[1] == "Custom Weights") {
    Moment_Weights <- Custom_Moment_Weights
  }
  Variables_for_the_Calculations <- Data_Frame[, which(colnames(Data_Frame) %in% Variable_Names)]
  Categorical_Measurements <- as.data.frame(Variables_for_the_Calculations[, sapply(Variables_for_the_Calculations, function (x) {
    is.character(x) | is.factor(x)
  })])
  colnames(Categorical_Measurements) <- colnames(Variables_for_the_Calculations)[sapply(Variables_for_the_Calculations, function (x) {
    is.character(x) | is.factor(x)
  })]
  Numeric_Measurements <- as.data.frame(Variables_for_the_Calculations[, sapply(Variables_for_the_Calculations, function (x) {
    !is.character(x) & !is.factor(x)
  })])
  colnames(Numeric_Measurements) <- colnames(Variables_for_the_Calculations)[sapply(Variables_for_the_Calculations, function (x) {
    !is.character(x) & !is.factor(x)
  })]
  Number_of_Unique_Categories <- NULL
  for (i in seq_len(ncol(Categorical_Measurements))) {
    Number_of_Unique_Categories[i] <- length(unique(Categorical_Measurements[, i]))
  }
  Total_Number_of_Unique_Categories <- sum(Number_of_Unique_Categories)
  Numeric_Variable_Weights <- Variable_Weights[which(sapply(Variables_for_the_Calculations, function (x) {
    !is.character(x) & !is.factor(x)
  }))]
  Categorical_Variable_Weights <- Variable_Weights[which(sapply(Variables_for_the_Calculations, function (x) {
    is.character(x) | is.factor(x)
  }))]
  Categorical_Variable_Weights <- unlist(mapply(rep, (Categorical_Variable_Weights / Number_of_Unique_Categories), Number_of_Unique_Categories))
  Dummy_Variables <- as.data.frame(matrix(NA, ncol = Total_Number_of_Unique_Categories, nrow = nrow(Data_Frame)))
  k <- 1
  for (i in seq_len(ncol(Categorical_Measurements))) {
    for (j in seq_len(length(unique(Categorical_Measurements[, i])))) {
      Dummy_Variables[, k] <- ifelse(Categorical_Measurements[, i] == unique(Categorical_Measurements[, i])[j], 1, 0)
      colnames(Dummy_Variables)[k] <- paste0(colnames(Categorical_Measurements)[i], "_", unique(Categorical_Measurements[, i])[j])
      k <- k + 1
    }
  }
  Variables_for_the_Calculations <- data.frame(Numeric_Measurements, Dummy_Variables)
  Variable_Weights <- c(Numeric_Variable_Weights, Categorical_Variable_Weights)
  Data_Frame <- cbind(Data_Frame[, Identifiers_Name], Data_Frame[, Blocking_Variable_Name], Variables_for_the_Calculations)
  colnames(Data_Frame)[which(colnames(Data_Frame) == "Data_Frame[, Identifiers_Name]")] <- Identifiers_Name
  colnames(Data_Frame)[which(colnames(Data_Frame) == "Data_Frame[, Blocking_Variable_Name]")] <- Blocking_Variable_Name
  colnames(Data_Frame)[which(colnames(Data_Frame) == "Identifiers")] <- Identifiers_Name
  colnames(Data_Frame)[which(colnames(Data_Frame) == "Blocks")] <- Blocking_Variable_Name
  Data_Split_by_Block <- split(Data_Frame, Data_Frame[, which(colnames(Data_Frame) == Blocking_Variable_Name)])
  names(Data_Split_by_Block) <- paste("Block", names(Data_Split_by_Block))
  Rescaled_Data_by_Block <- lapply(Data_Split_by_Block, function (u) {
    Rescaled_Data <- as.data.frame(lapply(u[, which((colnames(u) != Identifiers_Name) & (colnames(u) != Blocking_Variable_Name))], function (x) {
      (x - mean(x)) / sd(x)
    }))
    Rescaled_Data[, Identifiers_Name] <- u[, Identifiers_Name]
    Rescaled_Data <- Rescaled_Data[, c(which(colnames(Rescaled_Data) == Identifiers_Name), which(colnames(Rescaled_Data) == Blocking_Variable_Name), which((colnames(Rescaled_Data) != Identifiers_Name) & (colnames(Rescaled_Data) != Blocking_Variable_Name)))]
    Rescaled_Data
  })
  Output <- vector(mode = 'list', length = Number_of_Treatment_Groups)
  Possible_Groups_Function <- function (x) {
    if (is.list(x)) {
      lapply(x, Possible_Groups_Function)
    } else if (!is.list(x)) {
      as.list(as.data.frame(combn(x, Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group)))
    }
  }
  Remaining_Items_Function <- function (x, y) {
    if (!is.list(y)) {
      lapply(x, function (z) {
        setdiff(y, z)
      })
    } else if (is.list(y)) {
      mapply(Remaining_Items_Function, x = x, y = y, SIMPLIFY = F)
    }
  }
  All_Possible_Groups_Function <- function (x) {
    for (i in seq_len(Number_of_Treatment_Groups - 1)) {
      if (i == 1) {
        Group_Possibilities <- Possible_Groups_Function(x)
      } else if (i > 1) {
        Group_Possibilities <- Possible_Groups_Function(Remaining_Items)
      }
      Output[[i]] <- Group_Possibilities
      if (!all(sapply(Group_Possibilities, is.list))) {
        Remaining_Items <- lapply(Group_Possibilities, function (y) {
          setdiff(x, y)
        })
      } else if (all(sapply(Group_Possibilities, is.list))) {
        Remaining_Items <- Remaining_Items_Function(Group_Possibilities, Remaining_Items)
      }
    }
    if (Number_of_Treatment_Groups == 1) {
      Output[[Number_of_Treatment_Groups]] <- Possible_Groups_Function(x)
    } else if (Number_of_Treatment_Groups > 1) {
      Output[[Number_of_Treatment_Groups]] <- Possible_Groups_Function(Remaining_Items)
    }
    Output
  }
  Ordered_Grouping_Combinations_by_Block <- mapply(function (u, v) {
    All_Possible_Groups <- All_Possible_Groups_Function(u[, which(colnames(u) == Identifiers_Name)])
    Repitition_Times <- choose(length(u[, which(colnames(u) == Identifiers_Name)]) - (Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group * (0:(Number_of_Treatment_Groups - 1))), Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group)
    Repitition_Times <- c(Repitition_Times[2:length(Repitition_Times)], 1)
    Repitition_Times <- lapply((length(Repitition_Times) - seq_len(length(Repitition_Times))) + 1, function (x) {
      rev(Repitition_Times)[1:x]
    })
    Repitition_Times <- lapply(Repitition_Times, function (y) {
      Reduce(`*`, y)
    })
    All_Possible_Groups <- lapply(All_Possible_Groups, function(x) {
      z <- sapply(x, function (y) {
        class(y)[1] == "list"
      })
      w <- c(x[!z], unlist(x[z], recursive = F))
      if (sum(z)){
        Recall(w)
      } else if (!sum(z)) {
        w
      }
    })
    All_Possible_Groups <- mapply(function (x, y) {
      x[rep(seq_len(length(x)), each = y)]
    }, x = All_Possible_Groups, y = Repitition_Times, SIMPLIFY = F)
    All_Possible_Groups <- lapply(seq_len(unique(sapply(All_Possible_Groups, length))), function (x) {
      lapply(All_Possible_Groups,"[[", x)
    })
    List_of_Possible_Groups <- lapply(All_Possible_Groups, function (x) {
      names(x) <- paste0("Treatment_Group_", seq_len(Number_of_Treatment_Groups))
      x
    })
    names(List_of_Possible_Groups) <- NULL
    Ordered_List_of_Possible_Groups_1 <- lapply(List_of_Possible_Groups, function (x) {
      lapply(x, sort)
    })
    Ordered_List_of_Possible_Groups_2 <- lapply(Ordered_List_of_Possible_Groups_1, function (x) {
      order(sapply(x, function (y) {
        y[1]
      }))
    })
    Ordered_List_of_Possible_Groups_1 <- mapply(function (x, y) {
      x[y]
    }, x = Ordered_List_of_Possible_Groups_1, y = Ordered_List_of_Possible_Groups_2, SIMPLIFY = F)
    Ordered_List_of_Possible_Groups_1 <- lapply(Ordered_List_of_Possible_Groups_1, function (x) {
      do.call('c', x)
    })
    Ordered_List_of_Possible_Groups_1 <- lapply(Ordered_List_of_Possible_Groups_1, function (x) {
      names(x) <- NULL
      x
    })
    List_of_Possible_Groups <- List_of_Possible_Groups[-c(which(duplicated(Ordered_List_of_Possible_Groups_1)))]
    Original_List_of_Possible_Groups <- lapply(List_of_Possible_Groups, function (w) {
      lapply(w, function (y) {
        z <- u[u[, Identifiers_Name] %in% y, ]
        z[, Identifiers_Name] <- y
        z[, c(which(colnames(z) == Identifiers_Name), which(colnames(z) == Blocking_Variable_Name), which((colnames(z) != Identifiers_Name) & (colnames(z) != Blocking_Variable_Name)))]
      })
    })
    Rescaled_List_of_Possible_Groups <- lapply(List_of_Possible_Groups, function (w) {
      lapply(w, function (y) {
        z <- v[v[, Identifiers_Name] %in% y, ]
        z[, Identifiers_Name] <- y
        z[, c(which(colnames(z) == Identifiers_Name), which(colnames(z) != Identifiers_Name))]
      })
    })
    Rescaled_Moments <- lapply(Rescaled_List_of_Possible_Groups, function (y) {
      lapply(y, function (z) {
        u <- sapply(z[, which(colnames(z) != Identifiers_Name)], function (a) {
          sapply(seq_len(Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group), function (b) {
            sum(a ^ b)
          })
        })
        rownames(u) <- paste("Moment", seq_len(nrow(u)), sep = "_")
        u
      })
    })
    Pairwise_Combinations <- combn(seq_len(Number_of_Treatment_Groups), 2)
    Rescaled_Moment_Differences <- lapply(Rescaled_Moments, function (x) {
      Group_Pairings <- lapply(seq_len(ncol(Pairwise_Combinations)), function (y) {
        x[Pairwise_Combinations[, y]]
      })
      lapply(Group_Pairings, function (y) {
        abs(y[[1]] - y[[2]])
      })
    })
    Unlisted_Rescaled_Moment_Differences <- unlist(Rescaled_Moment_Differences, recursive = FALSE)
    Rescaled_Moment_Differences_Array <- simplify2array(Unlisted_Rescaled_Moment_Differences)
    Mean_Rescaled_Moment_Differences <- apply(Rescaled_Moment_Differences_Array, c(1, 2), mean)
    Relativized_Rescaled_Moment_Differences <- lapply(Rescaled_Moment_Differences, function (x) {
      lapply(x, function (y) {
        y / Mean_Rescaled_Moment_Differences
      })
    })
    Weighted_Relativized_Rescaled_Moment_Differences <- lapply(Relativized_Rescaled_Moment_Differences, function (x) {
      lapply(x, function (y) {
        a <- t(apply(y, 1, function (z) {
          z * Variable_Weights
        }))
        b <- apply(a, 2, function (z) {
          z * Moment_Weights
        })
        b
      })
    })
    Combination_Scores <- sapply(Weighted_Relativized_Rescaled_Moment_Differences, function (x) {
      sum(sapply(x, sum))
    })
    Ordered_Grouping_Combinations <- Original_List_of_Possible_Groups[order(Combination_Scores)]
    Ordered_Grouping_Combinations_Moments <- lapply(Ordered_Grouping_Combinations, function (x) {
      lapply(x, function (y) {
        if (Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group >= 1) {
          Means <- sapply(y[, which((colnames(y) != Identifiers_Name) & (colnames(y) != Blocking_Variable_Name))], mean)
        }
        if (Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group >= 2) {
          Variances <- sapply(y[, which((colnames(y) != Identifiers_Name) & (colnames(y) != Blocking_Variable_Name))], var)
        }
        if (Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group >= 3) {
          Skewnesses <- sapply(y[, which((colnames(y) != Identifiers_Name) & (colnames(y) != Blocking_Variable_Name))], function (z) {
            ((1 / length(z)) * sum((z - mean(z)) ^ 3)) / (((1 / (length(z) - 1)) * sum((z - mean(z)) ^ 2)) ^ (3 / 2))
          })
        }
        if (Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group >= 4) {
          Kurtoses <- sapply(y[, which((colnames(y) != Identifiers_Name) & (colnames(y) != Blocking_Variable_Name))], function (z) {
            (((1 / length(z)) * sum((z - mean(z)) ^ 4)) / (((1 / length(z)) * sum((z - mean(z)) ^ 2)) ^ 2)) - 3
          })
        }
        if (Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group >= 5) {
          Hyperskewnesses <- sapply(y[, which((colnames(y) != Identifiers_Name) & (colnames(y) != Blocking_Variable_Name))], function (z) {
            (1 / length(z)) * sum(((z - mean(z)) / sd(z)) ^ 5)
          })
        }
        if (Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group >= 6) {
          Hypertailednesses <- sapply(y[, which((colnames(y) != Identifiers_Name) & (colnames(y) != Blocking_Variable_Name))], function (z) {
            (1 / length(z)) * sum(((z - mean(z)) / sd(z)) ^ 6)
          })
        }
        if (Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group >= 7) {
          Higher_Order_Moments <- lapply(7:length(Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group), function (i) {
            sapply(y[, which((colnames(y) != Identifiers_Name) & (colnames(y) != Blocking_Variable_Name))], function (z) {
              (1 / length(z)) * sum(((z - mean(z)) / sd(z)) ^ i)
            })
          })
          names(Higher_Order_Moments) <- paste("Moment", 7:length(Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group), sep = "_")
          Higher_Order_Moments <- as.data.frame(t(as.matrix(as.data.frame(Higher_Order_Moments))))
          Output <- rbind(Means, Variances, Skewnesses, Kurtoses, Hyperskewnesses, Hypertailednesses, Higher_Order_Moments)
        }
        if (Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group == 6) {
          Output <- rbind(Means, Variances, Skewnesses, Kurtoses, Hyperskewnesses, Hypertailednesses)
        }
        if (Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group == 5) {
          Output <- rbind(Means, Variances, Skewnesses, Kurtoses, Hyperskewnesses)
        }
        if (Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group == 4) {
          Output <- rbind(Means, Variances, Skewnesses, Kurtoses)
        }
        if (Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group == 3) {
          Output <- rbind(Means, Variances, Skewnesses)
        }
        if (Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group == 2) {
          Output <- rbind(Means, Variances)
        }
        if (Number_of_Experimental_Units_From_Each_Block_in_Each_Treatment_Group == 1) {
          Output <- rbind(Means)
        }
        Output
      })
    })
    Optimal_Combinations <- lapply(seq_len(length(Ordered_Grouping_Combinations_Moments)), function (x) {
      list(Group_Assignments = Ordered_Grouping_Combinations[[x]], Moment_Information = Ordered_Grouping_Combinations_Moments[[x]])
    })
    names(Optimal_Combinations) <- paste0("The ", ifelse(substr(seq_len(length(Ordered_Grouping_Combinations_Moments)), nchar(seq_len(length(Ordered_Grouping_Combinations_Moments))), nchar(seq_len(length(Ordered_Grouping_Combinations_Moments)))) %in% c("0", "4", "5", "6", "7", "8", "9"), paste0(seq_len(length(Ordered_Grouping_Combinations_Moments)), "th"), ifelse(substr(seq_len(length(Ordered_Grouping_Combinations_Moments)), nchar(seq_len(length(Ordered_Grouping_Combinations_Moments))), nchar(seq_len(length(Ordered_Grouping_Combinations_Moments)))) %in% c("1"), paste0(seq_len(length(Ordered_Grouping_Combinations_Moments)), "st"), ifelse(substr(seq_len(length(Ordered_Grouping_Combinations_Moments)), nchar(seq_len(length(Ordered_Grouping_Combinations_Moments))), nchar(seq_len(length(Ordered_Grouping_Combinations_Moments)))) %in% c("2"), paste0(seq_len(length(Ordered_Grouping_Combinations_Moments)), "nd"), ifelse(substr(seq_len(length(Ordered_Grouping_Combinations_Moments)), nchar(seq_len(length(Ordered_Grouping_Combinations_Moments))), nchar(seq_len(length(Ordered_Grouping_Combinations_Moments)))) %in% c("3"), paste0(seq_len(length(Ordered_Grouping_Combinations_Moments)), "rd"), seq_len(length(Ordered_Grouping_Combinations_Moments)))))), "-Best Combination")
    names(Optimal_Combinations)[1] <- "The Best Combination"
    names(Optimal_Combinations)[length(Optimal_Combinations)] <- "The Worst Combination"
    Optimal_Combinations
  }, u = Data_Split_by_Block, v = Rescaled_Data_by_Block, SIMPLIFY = FALSE)
}
