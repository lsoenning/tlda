#' Calculate the dispersion measure DA
#'
#' @description
#' This function calculates the dispersion measure DA. It offers two different formulas, the basic version as well as a computational shortcut. It also allows the user to choose the directionality of scaling, i.e. whether higher values denote a more even or a less even distribution.
#'
#' @param subfreq A numeric vector of subfrequencies, i.e. the number of occurrences of the item in each corpus part
#' @param partsize A numeric vector with the size of the corpus parts
#' @param formula Character string indicating which formula to use for the calculation of DA. See details below. Possible values are \code{'basic'} (default), \code{'shortcut'}
#' @param directionality Character string indicating the directionality of scaling. See details below. Possible values are \code{'conventional'} (default), \code{'gries'}
#' @param verbose Logical. Whether additional information on scaling and formulas should be printed, default is TRUE
#' @param print_score Logical. Whether the dispersion score should be printed to the console, default is TRUE
#' 
#' @author Lukas Soenning
#' 
#' @details The function calculates the dispersion measure DA based on a set of subfrequencies (number of occurrences of the item in each corpus part) and a matching set of part sizes (the size of the corpus parts, i.e. number of word tokens).
#' 
#' - Directionality: DA ranges from 0 to 1. The \code{conventional} scaling of dispersion measures (see Juilland & Chang-Rodriguez 1964; Carroll 1970; Rosengren 1971) assigns higher values to more even/dispersed/balanced distributions of subfrequencies across corpus parts. Gries (2008) uses the reverse scaling, with higher values denoting a more uneven/bursty/concentrated distribution; this is implemented by the value \code{gries}.
#' 
#' - Formula: Irrespective of the directionality of scaling, two formulas for DA exist (see below for details). Both appear in Wilcox (1973), where the measure is referred to as "MDA". The basic formula (represented by the value \code{basic}) carries out the full set of computations required by the composition of the formula. As the number of corpus parts grows, this can become computationally very expensive. Wilcox (1973) also gives a "computational" formula, which is a shortcut that is much quicker and closely approximates the scores produced by the basic formula. This version is represented by the value \code{shortcut}.
#' 
#' 
#' In the formulas given below, the following notation is used:
#' 
#' - \eqn{k} the number of corpus parts
#' - \eqn{R_i} the normalized subfrequency in part \eqn{i}, i.e. the number of occurrences of the item divided by the size of the part
#' - \eqn{r_i} a proportional quantity; the normalized subfrequency in part \eqn{i} (\eqn{R_i}) divided by the sum of all normalized subfrequencies
#' 
#' The value \code{basic} implements the basic version of the formula (see Wilcox 1973: 329, 343; Burch et al. 2017: 194; Egbert et al. 2020: 98). The basic version of the formula can be applied to absolute frequencies and normalized frequencies. For dispersion analysis, absolute frequencies only make sense if the corpus parts are identical in size. Wilcox (1973: 343, 'MDA', column 1 and 2) gives both variants of the basic version. The first use of DA for corpus-linguistic dispersion analysis appears in Burch et al. (2017: 194), a paper that deals with equal-sized parts and therefore uses the variant for absolute frequencies. Egbert et al. (2020: 98) rely on the variant using normalized frequencies. Since this variant of the basic version of DA works irrespective of the length of the corpus parts (equal or variable), we will only give this version of the formula. Note that while the formula represents \code{conventional} scaling (0 = uneven, 1 = even), in the current function the directionality is controlled separately using the argument \code{directionality}.
#' 
#' &emsp; \eqn{1 - \frac{\sum_{i = 1}^{k-1} \sum_{j = i+1}^{k} |R_i - R_j|}{\frac{k(k-1)}{2}} \times \frac{1}{2\frac{\sum_i^k R_i}{k}}} (Egbert et al. 2020: 98)
#' 
#' The function uses a different version of the same formula, which relies on the proportional \eqn{r_i} values instead of the normalized subfrequencies \eqn{R_i}. This version yields the identical result; the \eqn{r_i} quantities are also the key to using the computational shortcut given in Wilcox (1973: 343). This is the basic formula for DA using \eqn{r_i} instead of \eqn{R_i} values: 
#' 
#' &emsp; \eqn{1 - \frac{\sum_{i = 1}^{k-1} \sum_{j = i+1}^{k} |r_i - r_j|}{k-1}} (Wilcox 1973: 343; see also Sönning 2022)
#' 
#' The value \code{shortcut} implements the computational formula given in Wilcox (1973: 343). Critically, the proportional quantities \eqn{r_i} must first be sorted in decreasing order. Only after this rearrangement can the shortcut version be applied. We will refer to this rearranged version of \eqn{r_i} as \eqn{r_i^{sorted}}:
#' 
#' &emsp; \eqn{\frac{2\left(\sum_{i = 1}^{k} (i \times r_i^{sorted}) - 1\right)}{k-1}} (Wilcox 1973: 343)
#'
#' The value \code{shortcut_mod} adds a minor modification to the computational shortcut to ensure DA does not exceed 1 (on the conventional dispersion scale):
#' 
#' &emsp; \eqn{\frac{2\left(\sum_{i = 1}^{k} (i \times r_i^{sorted}) - 1\right)}{k-1} \times \frac{k}{k - 1}}
#' 
#' @return Returns a numeric value, the dispersion score.
#' 
##' @references 
#' 
#' - Burch, Brent, Jesse Egbert & Douglas Biber. 2017. Measuring and interpreting lexical dispersion in corpus linguistics. \emph{Journal of Research Design and Statistics in Linguistics and Communication Science} 3(2). 189–216. \url{https://doi.org/10.1558/jrds.33066}
#' 
#' - Carroll, John B. 1970. An alternative to Juilland’s usage coefficient for lexical frequencies and a proposal for a standard frequency index. \emph{Computer Studies in the Humanities and Verbal Behaviour} 3(2). 61–65. \url{https://doi.org/10.1002/j.2333-8504.1970.tb00778.x}
#' 
#' - Egbert, Jesse, Brent Burch & Douglas Biber. 2020. Lexical dispersion and corpus design. \emph{International Journal of Corpus Linguistics} 25(1). 89–115. \url{https://doi.org/10.1075/ijcl.18010.egb}
#' 
#' - Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. \emph{International Journal of Corpus Linguistics} 13(4). 403–437. \url{https://doi.org/10.1075/ijcl.13.4.02gri}
#' 
#' - Juilland, Alphonse G. & Eugenio Chang-Rodríguez. 1964. \emph{Frequency dictionary of Spanish words.} The Hague: Mouton de Gruyter. \url{https://doi.org/10.1515/9783112415467}
#' 
#' - Rosengren, Inger. 1971. The quantitative concept of language and its relation to the structure of frequency dictionaries. \emph{Études de linguistique appliquée (Nouvelle Série)} 1. 103–127.
#' 
#' - Sönning, Lukas. 2022. Evaluation of text-level measures of lexical dispersion: Robustness and consistency. \emph{PsyArXiv preprint}. \url{https://psyarxiv.com/h9mvs_v1/}
#' - Wilcox, Allen R. 1973. Indices of qualitative variation and political measurement. The Western Political Quarterly 26 (2). 325–343. \url{https://doi.org/10.2307/446831}
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # not run
#' disp_DA(
#'   subfreq = c(0,0,1,2,5), 
#'   partsize = rep(1000, 5),
#'   formula = "basic",
#'   directionality = "conventional")
#' }
#' 
disp_DA <- function(subfreq,
                    partsize,
                    formula = "basic",
                    directionality = "conventional",
                    verbose = TRUE,
                    print_score = TRUE) {
  R_i <- subfreq / partsize
  r_i <- R_i / sum(R_i)
  k <- length(partsize)

  if (formula == "basic") {
    dist_r <- as.matrix(stats::dist(r_i, method = "manhattan"))
    DA <- 1 - (mean(dist_r[lower.tri(dist_r)]) / (2 / k))
  } else if (formula == "shortcut") {
    DA <- (2 * sum(sort(r_i, decreasing = TRUE) * 1:k) - 1) / (k - 1)
  } else if (formula == "shortcut_mod") {
    DA <- ((2 * sum(sort(r_i, decreasing = TRUE) * 1:k) - 1) / (k - 1)) / (k/(k-1))
  }

  if (directionality == "gries") DA <- 1 - DA

  names(DA) <- c("DA")

  if (print_score == TRUE) print(DA)
  
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

    if (formula == "basic") {
      message("Computed using the basic formula for DA, see:")
      message("  Wilcox (1967: 343, 'MDA', column 2) [https://doi.org/10.2307/446831]")
      message("  Burch et al. (2017: 194-196) [ https://doi.org/10.1558/jrds.33066]\n")
    } else if (formula == "shortcut") {
      message("Computed using the computational shortcut suggested by")
      message("  Wilcox (1967: 343, 'MDA', column 4) [https://doi.org/10.2307/446831]\n")
    } else if (formula == "shortcut_mod") {
      message("Computed using the computational shortcut suggested by")
      message("  Wilcox (1967: 343, 'MDA', column 4) [https://doi.org/10.2307/446831]\n")
      message("  with a minor correction to ensure DA does not exceed 1 (conventional)")
    }

    message("For background on the formula for DA, see:")
    message("  Soenning 2025 []")
  }

  invisible(DA)
}
