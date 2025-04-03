#' Calculate Gries's *deviation of proportions*
#'
#' @description
#' This function calculates Gries's dispersion measure DP (deviation of proportions). It offers three different formulas and allows the user to choose the directionality of scaling, i.e. whether higher values denote a more even or a less even distribution. It also offers the option of calculating frequency-adjusted dispersion scores.
#' 
#'
#' @inheritParams disp
#' @param formula Character string indicating which formula to use for the calculation of DP. See details below. Possible values are \code{'egbert_etal_2020'} (default), \code{'gries_2008'}, \code{'lijffit_gries_2012'}
#'
#' @author Lukas Soenning
#' 
#' @details The function calculates the dispersion measure DP based on a set of subfrequencies (number of occurrences of the item in each corpus part) and a matching set of part sizes (the size of the corpus parts, i.e. number of word tokens).
#' 
#' - Directionality: DP ranges from 0 to 1. The \code{conventional} scaling of dispersion measures (see Juilland & Chang-Rodriguez 1964; Carroll 1970; Rosengren 1971) assigns higher values to more even/dispersed/balanced distributions of subfrequencies across corpus parts. Gries (2008) uses the reverse scaling, with higher values denoting a more uneven/bursty/concentrated distribution; this is implemented by the value \code{gries}.
#' 
#' - Formula: Irrespective of the directionality of scaling, four formulas for DP exist in the literature (see below for details). This is because the original version proposed by Gries (2008: 415), which is commonly denoted as \eqn{DP} (and here referenced by the value \code{gries_2008}) does not always reach its theoretical limits of 0 and 1. For this reason, modifications have been suggested, starting with Gries (2008: 419) himself, who referred to this version as \eqn{DPnorm}. This version is not implemented in the current package, because Lijffit & Gries (2012) updated \eqn{DPnorm} to ensure that it also works as intended when corpus parts differ in size; this version is represented by the value \code{lijffit_gries_2012} and often denoted using subscript notation \eqn{DP_{norm}} . Finally, Egbert et al. (2020: 99) suggest a further modification to ensure proper behavior in settings where the item occurs in only one corpus part. They label this version \eqn{D_P}. In the current function, it is the default and represented by the value \code{egbert_etal_2020}.
#' 
#' - Frequency adjustment: Dispersion scores can be adjusted for frequency using a variant of the min-max transformation proposed by Gries (2022, 2024). The frequency-adjusted score for a specific item considers its dispersion potential, the lowest and highest possible score it can obtain given its overall corpus frequency as well as the number and size of corpus parts. The unadjusted score is then expressed relative to these endpoints, where the dispersion pessimum is set to 0, and the dispersion optimum to 1 (expressed in terms of conventional scaling). The frequency-adjusted dispersion score falls between these bounds and expresses how close the observed distribution is to the theoretical maximum and minimum. This adjustment therefore requires a maximally and a minimally dispersed distribution of the item across the parts. These hypothetical extremes can be built in different ways. In particular, different strategies exist for finding a distribution that yields the dispersion maximum. The method used by Gries (2024) uses a computationally expensive procedure that finds the distribution that produces the highest value on the dispersion measure of interest. The current function constructs extreme distributions independently of the dispersion measures used and therefore only proved approximations to the upper bound yielded by Gries's method. 
#' 
#'    - To obtain the lowest possible level of dispersion, the occurrences are allocated to the smallest corpus parts. The function starts by filling in the smallest part. If the total number of occurrences of the items is greater than the size of this part, it then continues with the second smallest corpus part and so on. This approach is very similar to that used in Gries (2024).
#'    - To obtain the highest possible level of dispersion, two methods are available, and these can be set with the argument \code{freq_adjust_method}. The choice between these methods is particularly relevant if corpus parts differ considerably in size. See documentation for \code{find_max_disp()}.
#' 
#' In the formulas given below, the following notation is used:
#' 
#' - \eqn{k} the number of corpus parts
#' - \eqn{t_i} a proportional quantity; the subfrequency in part \eqn{i} divided by the total number of occurrences of the item in the corpus (i.e. the sum of all subfrequencies)
#' - \eqn{w_i} a proportional quantity; the size of corpus part \eqn{i} divided by the size of the corpus (i.e. the sum of a part sizes) 
#' 
#' The value \code{gries_2008} implements the original version proposed by Gries (2008: 415). Note that while the following formula represents \code{gries} scaling (0 = even, 1 = uneven), in the current function the directionality is controlled separately using the argument \code{directionality}.
#' 
#' &emsp; \eqn{\frac{\sum_i^k |t_i - w_i|}{2}} (Gries 2008)
#' 
#' The value \code{lijffit_gries_2012} implements the modified version described by Lijffit & Gries (2012). Again, the following formula represents \code{gries} scaling (0 = even, 1 = uneven), but the directionality is handled separately in the current function. The notation \eqn{min\{w_i\}} refers to the \eqn{w_i} value of the smallest corpus part.
#' 
#' &emsp; \eqn{\frac{\sum_i^k |t_i - w_i|}{2} \times \frac{1}{1 - min\{w_i\}}} (Lijffijt & Gries 2012)
#'  
#' The value \code{egbert_etal_2020} (default) selects the modification suggested by Egbert et al. (2020: 99). The following formula represents \code{conventional} scaling (0 = uneven, 1 = even). The notation \eqn{min\{w_i: t_i > 0\}} refers to the \eqn{w_i} value among those corpus parts that include at least one occurrence of the item.
#' 
#' &emsp; \eqn{1 - \frac{\sum_i^k |t_i - w_i|}{2} \times \frac{1}{1 - min\{w_i: t_i > 0\}}} (Egbert et al. 2020)
#'    
#'  
#' @return A numeric vector
#' 
#' @references 
#' 
#' - Carroll, John B. 1970. An alternative to Juilland’s usage coefficient for lexical frequencies and a proposal for a standard frequency index. \emph{Computer Studies in the Humanities and Verbal Behaviour} 3(2). 61–65. \url{https://doi.org/10.1002/j.2333-8504.1970.tb00778.x}
#' 
#' - Egbert, Jesse, Brent Burch & Douglas Biber. 2020. Lexical dispersion and corpus design. \emph{International Journal of Corpus Linguistics} 25(1). 89–115. \url{https://doi.org/10.1075/ijcl.18010.egb}
#' 
#' - Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. \emph{International Journal of Corpus Linguistics} 13(4). 403–437. \url{https://doi.org/10.1075/ijcl.13.4.02gri}
#' 
#' - Juilland, Alphonse G. & Eugenio Chang-Rodríguez. 1964. \emph{Frequency dictionary of Spanish words.} The Hague: Mouton de Gruyter. \url{https://doi.org/10.1515/9783112415467}
#' 
#' Lijffijt, Jefrey & Stefan Th. Gries. 2012. Correction to Stefan Th. Gries’ ‘Dispersions and adjusted frequencies in corpora’. \emph{International Journal of Corpus Linguistics} 17(1). 147–149. \url{https://doi.org/10.1075/ijcl.17.1.08lij}
#' 
#' - Rosengren, Inger. 1971. The quantitative concept of language and its relation to the structure of frequency dictionaries. \emph{Études de linguistique appliquée (Nouvelle Série)} 1. 103–127.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # not run
#' disp_DP(
#'   subfreq = c(0,0,1,2,5), 
#'   partsize = rep(1000, 5),
#'   directionality = "conventional",
#'   formula = "gries_2008",
#'   freq_adjust = FALSE)
#' }
#' 
disp_DP <- function(subfreq,
                    partsize,
                    directionality = "conventional",
                    formula = "egbert_etal_2020",
                    freq_adjust = FALSE,
                    freq_adjust_method = "pervasive",
                    digits = NULL,
                    verbose = TRUE,
                    print_score = TRUE) {
  
  calculate_DP <- function(subfreq, partsize){
    w_i <- partsize / sum(partsize)
    t_i <- subfreq / sum(subfreq)
    
    if (formula == "gries_2008") {
      DP <- 1 - sum(abs(t_i - w_i)) / 2
    } else if (formula == "lijffit_gries_2012") {
      DP <- 1 - (sum(abs(t_i - w_i)) / 2) / (1 - min(w_i))
    } else {
      DP <- 1 - (sum(abs(t_i - w_i)) / 2) / (1 - min(w_i[t_i > 0]))
    }
  }
  
  if (sum(subfreq) == 0){
    output <- NA
    
  } else {
    
    DP_score <- calculate_DP(subfreq, partsize)
    output <- DP_score
    
    if (directionality == "gries") DP_score <- 1 - DP_score
    
    if (freq_adjust == TRUE){
      
      subfreq_min_disp <- find_min_disp(
        subfreq, 
        partsize)
      
      subfreq_max_disp <- find_max_disp(
        subfreq, 
        partsize)
      
      DP_min <- calculate_DP(subfreq_min_disp, partsize)
      DP_max <- calculate_DP(subfreq_max_disp, partsize)
      
      if (directionality == "gries") {
        DP_min <- 1 - DP_min
        DP_max <- 1 - DP_max
      } 
      
      # frequency-adjusted version
      output <- (DP_score - DP_min) / (DP_max - DP_min)
    }
  }
  
  if (freq_adjust == TRUE){
    names(output) <- "DP_nofreq"
  } else {
    names(output) <- "DP"
  }
  
  if (!is.null(digits)) output <- round(output, digits)

  if (print_score == TRUE) print(output)
  
  if (verbose) {
    if (freq_adjust == TRUE){
      message("\nDispersion scores are adjusted for frequency using the min-max")
      message("  transformation (see Gries 2024: 196-208)")
    }
    if (directionality == "gries") {
      message("\nScores follow scaling used by Gries (2008):")
      message("  0 = maximally even/dispersed/balanced distribution (optimum)")
      message("  1 = maximally uneven/bursty/concentrated distribution (pessimum)")
    } else {
      message("\nScores follow conventional scaling:")
      message("  0 = maximally uneven/bursty/concentrated distribution (pessimum)")
      message("  1 = maximally even/dispersed/balanced distribution (optimum)")
    }
    
    if (formula == "gries_2008") {
      message("\nComputed using the original version proposed by Gries (2008)")
    } else if (formula == "lijffit_gries_2012") {
      message("\nComputed using the modification suggested by Lijffit & Gries (2012)")
    } else {
      message("\nComputed using the modification suggested by Egbert et al. (2020)")
    }
  }
  invisible(output)
}


#' Calculate Gries's *deviation of proportions* for a term-document matrix
#'
#' @description
#' This function calculates Gries's dispersion measure DP (deviation of proportions). It offers three different formulas and allows the user to choose the directionality of scaling, i.e. whether higher values denote a more even or a less even distribution. It also offers the option of calculating frequency-adjusted dispersion scores.
#' 
#' @inheritParams disp_tdm
#' @inheritParams disp_DP
#' 
#'
#' @author Lukas Soenning
#' 
#' @details 
#' 
#' This function takes as input a term-document matrix and returns, for each item (i.e. each row) the dispersion measure DP. The rows in the matrix represent the items, and the columns the corpus parts. Importantly, the term-document matrix must include an additional row that records the size of the corpus parts. For a proper term-document matrix, which includes all items that appear in the corpus, this can be added as a column margin, which sums the frequencies in each column. If the matrix only includes a selection of items drawn from the corpus, this information cannot be derived from the matrix and must be provided as a separate row.
#' 
#' - Directionality: DP ranges from 0 to 1. The \code{conventional} scaling of dispersion measures (see Juilland & Chang-Rodriguez 1964; Carroll 1970; Rosengren 1971) assigns higher values to more even/dispersed/balanced distributions of subfrequencies across corpus parts. Gries (2008) uses the reverse scaling, with higher values denoting a more uneven/bursty/concentrated distribution; this is implemented by the value \code{gries}.
#' 
#' - Formula: Irrespective of the directionality of scaling, four formulas for DP exist in the literature (see below for details). This is because the original version proposed by Gries (2008: 415), which is commonly denoted as \eqn{DP} (and here referenced by the value \code{gries_2008}) does not always reach its theoretical limits of 0 and 1. For this reason, modifications have been suggested, starting with Gries (2008: 419) himself, who referred to this version as \eqn{DPnorm}. This version is not implemented in the current package, because Lijffit & Gries (2012) updated \eqn{DPnorm} to ensure that it also works as intended when corpus parts differ in size; this version is represented by the value \code{lijffit_gries_2012} and often denoted using subscript notation \eqn{DP_{norm}} . Finally, Egbert et al. (2020: 99) suggest a further modification to ensure proper behavior in settings where the item occurs in only one corpus part. They label this version \eqn{D_P}. In the current function, it is the default and represented by the value \code{egbert_etal_2020}.
#' 
#' - Frequency adjustment: Dispersion scores can be adjusted for frequency using a variant of the min-max transformation proposed by Gries (2022, 2024). The frequency-adjusted score for a specific item considers its dispersion potential, the lowest and highest possible score it can obtain given its overall corpus frequency as well as the number and size of corpus parts. The unadjusted score is then expressed relative to these endpoints, where the dispersion pessimum is set to 0, and the dispersion optimum to 1 (expressed in terms of conventional scaling). The frequency-adjusted dispersion score falls between these bounds and expresses how close the observed distribution is to the theoretical maximum and minimum. This adjustment therefore requires a maximally and a minimally dispersed distribution of the item across the parts. These hypothetical extremes can be built in different ways. In particular, different strategies exist for finding a distribution that yields the dispersion maximum. The method used by Gries (2024) uses a computationally expensive procedure that finds the distribution that produces the highest value on the dispersion measure of interest. The current function constructs extreme distributions independently of the dispersion measures used and therefore only proved approximations to the upper bound yielded by Gries's method. 
#' 
#'    - To obtain the lowest possible level of dispersion, the occurrences are allocated to the smallest corpus parts. The function starts by filling in the smallest part. If the total number of occurrences of the items is greater than the size of this part, it then continues with the second smallest corpus part and so on. This approach is very similar to that used in Gries (2024).
#'    - To obtain the highest possible level of dispersion, two methods are available, and these can be set with the argument \code{freq_adjust_method}. The choice between these methods is particularly relevant if corpus parts differ considerably in size. See documentation for \code{find_max_disp()}.
#' 
#' In the formulas given below, the following notation is used:
#' 
#' - \eqn{k} the number of corpus parts
#' - \eqn{t_i} a proportional quantity; the subfrequency in part \eqn{i} divided by the total number of occurrences of the item in the corpus (i.e. the sum of all subfrequencies)
#' - \eqn{w_i} a proportional quantity; the size of corpus part \eqn{i} divided by the size of the corpus (i.e. the sum of a part sizes) 
#' 
#' The value \code{gries_2008} implements the original version proposed by Gries (2008: 415). Note that while the following formula represents \code{gries} scaling (0 = even, 1 = uneven), in the current function the directionality is controlled separately using the argument \code{directionality}.
#' 
#' &emsp; \eqn{\frac{\sum_i^k |t_i - w_i|}{2}} (Gries 2008)
#' 
#' The value \code{lijffit_gries_2012} implements the modified version described by Lijffit & Gries (2012). Again, the following formula represents \code{gries} scaling (0 = even, 1 = uneven), but the directionality is handled separately in the current function. The notation \eqn{min\{w_i\}} refers to the \eqn{w_i} value of the smallest corpus part.
#' 
#' &emsp; \eqn{\frac{\sum_i^k |t_i - w_i|}{2} \times \frac{1}{1 - min\{w_i\}}} (Lijffijt & Gries 2012)
#'  
#' The value \code{egbert_etal_2020} (default) selects the modification suggested by Egbert et al. (2020: 99). The following formula represents \code{conventional} scaling (0 = uneven, 1 = even). The notation \eqn{min\{w_i: t_i > 0\}} refers to the \eqn{w_i} value among those corpus parts that include at least one occurrence of the item.
#' 
#' &emsp; \eqn{1 - \frac{\sum_i^k |t_i - w_i|}{2} \times \frac{1}{1 - min\{w_i: t_i > 0\}}} (Egbert et al. 2020)
#'    
#'  
#' @return A numeric matrix
#' 
#' @references 
#' 
#' - Carroll, John B. 1970. An alternative to Juilland’s usage coefficient for lexical frequencies and a proposal for a standard frequency index. \emph{Computer Studies in the Humanities and Verbal Behaviour} 3(2). 61–65. \url{https://doi.org/10.1002/j.2333-8504.1970.tb00778.x}
#' 
#' - Egbert, Jesse, Brent Burch & Douglas Biber. 2020. Lexical dispersion and corpus design. \emph{International Journal of Corpus Linguistics} 25(1). 89–115. \url{https://doi.org/10.1075/ijcl.18010.egb}
#' 
#' - Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. \emph{International Journal of Corpus Linguistics} 13(4). 403–437. \url{https://doi.org/10.1075/ijcl.13.4.02gri}
#' 
#' - Juilland, Alphonse G. & Eugenio Chang-Rodríguez. 1964. \emph{Frequency dictionary of Spanish words.} The Hague: Mouton de Gruyter. \url{https://doi.org/10.1515/9783112415467}
#' 
#' Lijffijt, Jefrey & Stefan Th. Gries. 2012. Correction to Stefan Th. Gries’ ‘Dispersions and adjusted frequencies in corpora’. \emph{International Journal of Corpus Linguistics} 17(1). 147–149. \url{https://doi.org/10.1075/ijcl.17.1.08lij}
#' 
#' - Rosengren, Inger. 1971. The quantitative concept of language and its relation to the structure of frequency dictionaries. \emph{Études de linguistique appliquée (Nouvelle Série)} 1. 103–127.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # not run
#' disp_DP_tdm(
#'   tdm = biber150_spokenBNC2014[1:20,]
#'   row_partsize = "first_row",
#'   directionality = "conventional",
#'   formula = "gries_2008",
#'   freq_adjust = FALSE)
#' }
#' 
disp_DP_tdm <- function(tdm,
                        row_partsize = "first_row",
                        directionality = "conventional",
                        formula = "egbert_etal_2020",
                        freq_adjust = FALSE,
                        freq_adjust_method = "pervasive",
                        digits = NULL,
                        verbose = TRUE,
                        print_score = TRUE) {
  
  if (row_partsize == "first_row"){
    
    DP_score <- apply(
      tdm[-1,],
      1,
      function(x){
        disp_DP(subfreq = x, 
                partsize = tdm[1,],
                verbose = FALSE,
                print_score = FALSE)
      })
    
  } else if (row_partsize == "last_row"){
    
    DP_score <- apply(
      tdm[-nrow(tdm),],
      1,
      function(x){
        disp_DP(subfreq = x, 
                partsize = tdm[nrow(tdm),],
                verbose = FALSE,
                print_score = FALSE)
      })
  }
  
  if (freq_adjust == TRUE){
    
    min_disp_tdm <- find_min_disp_tdm(tdm)
    max_disp_tdm <- find_max_disp_tdm(tdm)
    
    if (row_partsize == "first_row"){
      
      DP_min <- apply(
        min_disp_tdm[-1,],
        1,
        function(x){
          disp_DP(subfreq = x, 
                  partsize = min_disp_tdm[1,],
                  verbose = FALSE,
                  print_score = FALSE)
        })
      
      DP_max <- apply(
        max_disp_tdm[-1,],
        1,
        function(x){
          disp_DP(subfreq = x, 
                  partsize = max_disp_tdm[1,],
                  verbose = FALSE,
                  print_score = FALSE)
        })
      
    } else if (row_partsize == "last_row"){
      
      DP_min <- apply(
        min_disp_tdm[-nrow(min_disp_tdm),],
        1,
        function(x){
          disp_DP(subfreq = x, 
                  partsize = min_disp_tdm[nrow(min_disp_tdm),],
                  verbose = FALSE,
                  print_score = FALSE)
        })
      
      DP_max <- apply(
        max_disp_tdm[-nrow(max_disp_tdm),],
        1,
        function(x){
          disp_DP(sbfreq = x, 
                  partsize = max_disp_tdm[nrow(max_disp_tdm),],
                  verbose = FALSE,
                  print_score = FALSE)
        })
    }
    output <- (DP_score - DP_min) / (DP_max - DP_min)
    
  } else {
    output <- DP_score
  }
  
  output <- t(output)
  
  if (!is.null(digits)) output <- round(output, digits)
  
  if (print_score == TRUE) print(output)
  
  if (verbose) {
    if (freq_adjust == TRUE){
      message("\nDispersion scores are adjusted for frequency using the min-max")
      message("  transformation (see Gries 2024: 196-208)")
    }
    if (directionality == "gries") {
      message("\nScores follow scaling used by Gries (2008):")
      message("  0 = maximally even/dispersed/balanced distribution (optimum)")
      message("  1 = maximally uneven/bursty/concentrated distribution (pessimum)\n")
    } else {
      message("\nScores follow conventional scaling:")
      message("  0 = maximally uneven/bursty/concentrated distribution (pessimum)")
      message("  1 = maximally even/dispersed/balanced distribution (optimum)\n")
    }
    
    if (formula == "gries_2008") {
      message("\nComputed using the original version proposed by Gries (2008)")
    } else if (formula == "lijffit_gries_2012") {
      message("\nComputed using the modification suggested by Lijffit & Gries (2012)")
    } else {
      message("\nComputed using the modification suggested by Egbert et al. (2020)")
    }
  }
  invisible(output)
}
