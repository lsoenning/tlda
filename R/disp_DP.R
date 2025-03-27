#' Calculate Gries's DP (deviation of proportions)
#'
#' @description
#' This function calculates Gries's dispersion measure DP (deviation of proportions). It offers three different formulas and allows the user to choose the directionality of scaling, i.e. whether higher values denote a more even or a less even distribution.
#' 
#'
#' @param subfreq A numeric vector of subfrequencies, i.e. the number of occurrences of the item in each corpus part
#' @param partsize A numeric vector with the size of the corpus parts
#' @param formula Character string indicating which formula to use for the calculation of DP. See details below. Possible values are \code{'egbert_etal_2020'} (default), \code{'gries_2008'}, \code{'lijffit_gries_2012'}
#' @param directionality Character string indicating the directionality of scaling. See details below. Possible values are \code{'conventional'} (default), \code{'gries'}
#' @param verbose Logical. Whether additional information on scaling and formulas should be printed, default is TRUE
#'
#' @author Lukas Soenning
#' 
#' @details The function calculates the dispersion measure DP based on a set of subfrequencies (number of occurrences of the item in each corpus part) and a matching set of part sizes (the size of the corpus parts, i.e. number of word tokens).
#' 
#' - Directionality: DP ranges from 0 to 1. The \code{conventional} scaling of dispersion measures (see Juilland & Chang-Rodriguez 1964; Carroll 1970; Rosengren 1971) assigns higher values to more even/dispersed/balanced distributions of subfrequencies across corpus parts. Gries (2008) uses the reverse scaling, with higher values denoting a more uneven/bursty/concentrated distribution; this is implemented by the value \code{gries}.
#' 
#' - Formula: Irrespective of the directionality of scaling, four formulas for DP exist in the literature (see below for details). This is because the original version proposed by Gries (2008: 415), which is commonly denoted as \eqn{DP} (and here referenced by the value \code{gries_2008}) does not always reach its theoretical limits of 0 and 1. For this reason, modifications have been suggested, starting with Gries (2008: 419) himself, who referred to this version as \eqn{DPnorm}. This version is not implemented in the current package, because Lijffit & Gries (2012) updated \eqn{DPnorm} to ensure that it also works as intended when corpus parts differ in size; this version is represented by the value \code{lijffit_gries_2012} and often denoted using subscript notation \eqn{DP_{norm}} . Finally, Egbert et al. (2020: 99) suggest a further modification to ensure proper behavior in settings where the item occurs in only one corpus part. They label this version \eqn{D_P}. In the current function, it is the default and represented by the value \code{egbert_etal_2020}.
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
#' @return Returns a numeric value, the dispersion score.
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
#'   formula = "gries_2008",
#'   directionality = "conventional")
#' }
#' 
disp_DP <- function(subfreq,
                    partsize,
                    formula = "egbert_etal_2020",
                    directionality = "conventional",
                    verbose = TRUE) {
  w_i <- partsize / sum(partsize)
  t_i <- subfreq / sum(subfreq)

  if (formula == "gries_2008") {
    DP <- 1 - sum(abs(t_i - w_i)) / 2
  } else if (formula == "lijffit_gries_2012") {
    DP <- 1 - (sum(abs(t_i - w_i)) / 2) / (1 - min(w_i))
  } else {
    DP <- 1 - (sum(abs(t_i - w_i)) / 2) / (1 - min(w_i[t_i > 0]))
  }

  if (directionality == "gries") DP <- 1 - DP

  names(DP) <- c("DP")

  print(DP)

  if (verbose) {
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
      message("Computed using the original version of DP proposed by\n  Gries (2008) [https://doi.org/10.1075/ijcl.13.4.02gri]\n")
    } else if (formula == "lijffit_gries_2012") {
      message("Computed using the modified version of DP suggested by\n  Lijffit & Gries (2012) [https://doi.org/10.1075/ijcl.17.1.08lij]\n")
    } else {
      message("Computed using the modified version of DP suggested by\n  Egbert et al. (2020) [https://doi.org/10.1075/ijcl.18010.egb]\n")
    }

    message("For background on the formulas for DP, see:")
    message("  Gries 2020 [https://doi.org/10.1007/978-3-030-46216-1_5]")
    message("  Soenning 2025 []")
  }

  invisible(DP)
}
