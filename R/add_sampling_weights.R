#' Add column with sampling weights to a data frame
#'
#' @description 
#' This function adds a new column `sampling_weights` to a data frame. The purpose of sampling weights is to adjust for a mismatch between sample and target population with regard to the distribution of a particular categorical variable. The population distribution can be specified, and sampling weights are then calculated to work as adjustment factors. The default is to assume a balanced population distribution (all levels represented equally), but custom population distributions can be specified. 
#'
#' @param data A data frame
#' @param variable Character string indicating the variable for whose levels sampling weights should be calculated
#' @param population_distribution List (or data frame) specifying the population distribution of the levels; default is `NULL`, meaning that all levels are assumed to be represented equally
#'
#' @author Lukas Soenning
#' 
#' @details
#' This function takes as input a data frame, where observations (rows) represent a sample from a population. If the distribution of a categorical variable in the sample does not match the (known or assumed) distribution in the population, the function calculated sampling weights for the observations (rows). These account for the mismatch by up-weighting rows of those level that are underrepresented in the sample (relative to the population) and down-weighting rows belonging to levels that are overrepresented in the sample (relative to the population). If no population distribution is specified, all levels are assumed to be represented equally in the target population. Sampling weights are calculated on the basis of (i) the observed distribution of the variable in the sample, and (ii) the population distribution. For instance, if a specific subgroup (i.e. level) has a share of 10% in the sample, compared to 20% in the population, the sampling weight is 2.0 (20% divided by 10%). Sampling weights above 1 indicate up-weighting, sampling weights below 1 indicate down-weighting. The function prints out information on the sample and population distribution and the resulting weights.
#' 
#' 
#' @returns A data frame
#' 
#' @export
#'
#' @examples
#' add_sampling_weights(
#'   data = metadata_ice_gb,
#'   variable = "mode")
add_sampling_weights <- function(data, 
                                 variable, 
                                 population_distribution = NULL){
  if(!(variable %in% colnames(data))){
    stop("No column found with the name provided to \`variable\`")
  }
  if(!(class(data[[variable]]) %in% c("character", "factor"))){
    stop("\`variable\` must be a factor or character vector")
  }
  n_lvls <- length(unique(data[[variable]]))
  if(n_lvls <= 1){
    stop("\`variable\` must have at least two levels")
  }
  
  data$original_order <- 1:nrow(data)
  
  df_weights <- data.frame(prop.table(table(data[[variable]])))
  colnames(df_weights) <- c("var_level", "sample_distr")
  
  if(is.null(population_distribution)){
    df_weights$population_distr <- 1/n_lvls
  } else {
    if(isFALSE(is.list(population_distribution))){
      stop("The argument \`population_distribution\` must be a list or data frame")
    }
    if(isFALSE(sum(names(population_distribution) %in% data[[variable]]) == n_lvls)){
      stop("The names in \`population_distribution\` do not match those in \`variable\`")
    }
    df_custom_weights <- data.frame(unlist(population_distribution))
    df_custom_weights$var_level <- rownames(df_custom_weights)
    colnames(df_custom_weights)[1] <- "population_distr"
    df_weights <- merge(df_weights, df_custom_weights, by = "var_level")
  }
  df_weights$sampling_weight <- df_weights$population_distr/df_weights$sample_distr
  
  data <- merge(data, df_weights[,c("var_level", "sampling_weight")], 
                by.x = variable,
                by.y = "var_level")
  
  data <- data[order(data$original_order),]
  data <- subset(data, select = -data$original_order)
  
  print_info <- cbind(
    substr(df_weights$var_level,1,17),
    round(df_weights$sample_distr*100),
    round(df_weights$population_distr*100),
    format(round(df_weights$sampling_weight, 2), nsmall = 2)
  )
  
  message(paste0("Sampling weights added for \`", variable, "\`"))
  
  message("\n                    Distribution in")
  message("                  -------------------   Sampling")
  message("Levels             Sample  Population     weight")
  message("----------------- ------- ----------- ----------")
  for(i in 1:n_lvls){
    message(paste0(print_info[i,1], 
                   strrep(" ", 17 - nchar(print_info[i,1])),
                   strrep(" ", 7 - nchar(print_info[i,2])), print_info[i,2], "%",
                   strrep(" ", 11 - nchar(print_info[i,3])), print_info[i,3], "%",
                   "       ", print_info[i,4]))
  }
  message("------------------------------------------------")
  message("\nEffect of sampling weights:")
  message("  if > 1, then rows representing this level will be up-weighted")
  message("  if < 1, then rows representing this level will be down-weighted")
  
  return(data)
}