#' Calculate the dispersion measure *D~MB~*
#'
#' @description
#' This function calculates *D~MB~*, a generalized version of the Poisson-based dispersion measure *MB* proposed by Nelson (2025). It allows the user to choose the directionality of scaling, i.e. whether higher values denote a more even or a less even distribution, and it returns confidence intervals for *D~MB~*.
#'
#' @inheritParams disp_DP
#' @param conf_int Logical. Whether a (profile likelihood) confidence interval should be computed; default: `FALSE`
#' @param conf_level Scalar giving the confidence level; default `0.95` for a 95% CI
#'
#' @author Lukas Soenning
#' 
#' @details The function calculates the dispersion measure *D~MB~* based on a set of subfrequencies (number of occurrences of the item in each corpus part) and a matching set of part sizes (the size of the corpus parts, i.e. number of word tokens). *D~MB~* can be considered a generalization of the method proposed by Nelson (2025). In contrast to the original measure, *MB*, *D~MB~* works with a pre-determined set of corpus parts, which may also differ in size. To provide this additional flexibility, *D~MB~* is constructed based on a Poisson regression model that considers the corpus parts as observations and allows them to differ in length through its incorporation of an offset parameter.
#' 
#' - Directionality: *D~MB~* ranges from 0 to 1. The conventional scaling of dispersion measures (see Juilland & Chang-Rodriguez 1964; Carroll 1970; Rosengren 1971) assigns higher values to more even/dispersed/balanced distributions of subfrequencies across corpus parts. This is the default. Gries (2008) uses the reverse scaling, with higher values denoting a more uneven/bursty/concentrated distribution; use `directionality = "gries"` to choose this option.
#' 
#' @returns A vector of numeric values
#' 
#' @references 
#' 
#' Carroll, John B. 1970. An alternative to Juilland’s usage coefficient for lexical frequencies and a proposal for a standard frequency index. \emph{Computer Studies in the Humanities and Verbal Behaviour} 3(2). 61--65. \doi{doi:10.1002/j.2333-8504.1970.tb00778.x}
#' 
#' Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. \emph{International Journal of Corpus Linguistics} 13(4). 403--437. \doi{doi:10.1075/ijcl.13.4.02gri}
#' 
#' Juilland, Alphonse G. & Eugenio Chang-Rodríguez. 1964. \emph{Frequency dictionary of Spanish words.} The Hague: Mouton de Gruyter. \doi{doi:10.1515/9783112415467}
#' 
#' Nelson, Robset N. Jr. 2025. Groundhog Day is not a good model for corpus dispersion. Journal of Quantitative Linguistics 32(2). 103--127. \doi{10.1080/09296174.2024.2423415}
#' 
#' Rosengren, Inger. 1971. The quantitative concept of language and its relation to the structure of frequency dictionaries. \emph{Études de linguistique appliquée (Nouvelle Série)} 1. 103--127.
#' 
#' @export
#'
#' @examples
#' disp_DMB(
#'   subfreq = c(0,0,1,2,5), 
#'   partsize = rep(1000, 5),
#'   directionality = "conventional",
#'   conf_int = TRUE)
disp_DMB <- function(subfreq, 
                     partsize,
                     directionality = "conventional", 
                     conf_int = FALSE,
                     conf_level = .95,
                     digits = NULL,
                     verbose = TRUE,
                     print_score = TRUE,
                     suppress_warning = FALSE){
  
  if (length(subfreq) != length(partsize)){
    stop("Lengths of the variables 'subfreq' and 'partsize' differ.")
  }
  
  if (sum(subfreq) == 0){
    output <- NA
  } else {
    m <- stats::glm(subfreq ~ offset(log(partsize)), family = stats::poisson())
    
    Pr_exp_0 <- mean(stats::dpois(0, exp(stats::coef(m))*partsize))
    
    Pr_obs_0 <- mean(subfreq == 0)
    
    if(Pr_obs_0 <= Pr_exp_0){
      DMB <- 1
    } else {
      DMB <- 1 - ((abs(Pr_obs_0 - Pr_exp_0)) / (1 - Pr_exp_0))
    }
    output <- DMB
    names(output) <- "DMB"
    
    if(isTRUE(conf_int)){
      lambda_ci <- suppressMessages(
        stats::confint(m, trace = FALSE, level = conf_level))
      
      Pr_exp_0_upper <- mean(stats::dpois(0, exp(lambda_ci[1])*partsize))
      Pr_exp_0_lower <- mean(stats::dpois(0, exp(lambda_ci[2])*partsize))
      
      if(Pr_obs_0 <= Pr_exp_0_lower){
        DMB_lower <- 1
      } else {
        DMB_lower <- 1 - ((abs(Pr_obs_0 - Pr_exp_0_lower)) / (1 - Pr_exp_0_lower))
      }
      
      if(Pr_obs_0 <= Pr_exp_0_upper){
        DMB_upper <- 1
      } else {
        DMB_upper <- 1 - ((abs(Pr_obs_0 - Pr_exp_0_upper)) / (1 - Pr_exp_0_upper))
      }
      
      output <- c(output, DMB_lower, DMB_upper)
      names(output) <- c("DMB", "ci_lower", "ci_upper")
      
    }
  }
  
  if(directionality == "gries"){
    output <- 1 - output
    output <- output[c(1,3,2)]
  } 
    
  
  if (!is.null(digits)) output <- round(output, digits)
  
  if (print_score != FALSE) print(output)
  
  if (sum(subfreq) == 0 & suppress_warning == FALSE){
    warning("All subfrequencies are 0; returning NA.")
  } else {
    if (verbose) {
      if(isTRUE(conf_int)){
        message("\nConfidence level: ", round(conf_level*100), "%")
      }
      message("\nDispersion is calculated with a generalized version of the method")
      message("  propsed by Nelson (2025), relying on a Poisson regression model; see")
      message("  https://lsoenning.github.io/posts/2025-10-23_dispersion_nelson_poissonness/")

      if (directionality == "gries") {
        message("\nScores follow scaling used by Gries (2008):")
        message("  0 = maximally even/dispersed/balanced distribution (optimum)")
        message("  1 = maximally uneven/bursty/concentrated distribution (pessimum)")
      } else {
        message("\nScores follow conventional scaling:")
        message("  0 = maximally uneven/bursty/concentrated distribution (pessimum)")
        message("  1 = maximally even/dispersed/balanced distribution (optimum)")
      }
    }
  }
  invisible(output)
}