#' Calculate the dispersion measure \eqn{D_{A}}
#'
#' @description
#' This function calculates the dispersion measure \eqn{D_{A}}. It offers two different computational procedures, the basic version as well as a computational shortcut. It also allows the user to choose the directionality of scaling, i.e. whether higher values denote a more even or a less even distribution. It also provides the option of calculating frequency-adjusted dispersion scores.
#'
#' @inheritParams disp
#' @param procedure Character string indicating which procedure to use for the calculation of \eqn{D_{A}}. See details below. Possible values are \code{'basic'} (default), \code{'shortcut'}
#' 
#' @author Lukas Soenning
#' 
#' @details The function calculates the dispersion measure \eqn{D_{A}} based on a set of subfrequencies (number of occurrences of the item in each corpus part) and a matching set of part sizes (the size of the corpus parts, i.e. number of word tokens).
#' 
#' - Directionality: \eqn{D_{A}} ranges from 0 to 1. The \code{conventional} scaling of dispersion measures (see Juilland & Chang-Rodriguez 1964; Carroll 1970; Rosengren 1971) assigns higher values to more even/dispersed/balanced distributions of subfrequencies across corpus parts. Gries (2008) uses the reverse scaling, with higher values denoting a more uneven/bursty/concentrated distribution; this is implemented by the value \code{gries}.
#' 
#' - Procedure: Irrespective of the directionality of scaling, two computational procedures for \eqn{D_{A}} exist (see below for details). Both appear in Wilcox (1973), where the measure is referred to as "MDA". The basic version (represented by the value \code{basic}) carries out the full set of computations required by the composition of the formula. As the number of corpus parts grows, this can become computationally very expensive. Wilcox (1973) also gives a "computational" procedure, which is a shortcut that is much quicker and closely approximates the scores produced by the basic formula. This version is represented by the value \code{shortcut}.
#' 
#' - Frequency adjustment: Dispersion scores can be adjusted for frequency using a variant of the min-max transformation proposed by Gries (2022, 2024). The frequency-adjusted score for a specific item considers its dispersion potential, the lowest and highest possible score it can obtain given its overall corpus frequency as well as the number and size of corpus parts. The unadjusted score is then expressed relative to these endpoints, where the dispersion pessimum is set to 0, and the dispersion optimum to 1 (expressed in terms of conventional scaling). The frequency-adjusted dispersion score falls between these bounds and expresses how close the observed distribution is to the theoretical maximum and minimum. This adjustment therefore requires a maximally and a minimally dispersed distribution of the item across the parts. These hypothetical extremes can be built in different ways. In particular, different strategies exist for finding a distribution that yields the dispersion maximum. The method used by Gries (2024) uses a computationally expensive procedure that finds the distribution that produces the highest value on the dispersion measure of interest. The current function constructs extreme distributions independently of the dispersion measures used and therefore only proved approximations to the upper bound yielded by Gries's method. 
#' 
#'    - To obtain the lowest possible level of dispersion, the occurrences are allocated to the smallest corpus parts. The function starts by filling in the smallest part. If the total number of occurrences of the items is greater than the size of this part, it then continues with the second smallest corpus part and so on. This approach is very similar to that used in Gries (2024).
#'    - To obtain the highest possible level of dispersion, two methods are available, and these can be set with the argument \code{freq_adjust_method}. The choice between these methods is particularly relevant if corpus parts differ considerably in size. See documentation for \code{find_max_disp()}.
#'    
#'    
#' In the formulas given below, the following notation is used:
#' 
#' - \eqn{k} the number of corpus parts
#' - \eqn{R_i} the normalized subfrequency in part \eqn{i}, i.e. the number of occurrences of the item divided by the size of the part
#' - \eqn{r_i} a proportional quantity; the normalized subfrequency in part \eqn{i} (\eqn{R_i}) divided by the sum of all normalized subfrequencies
#' 
#' The value \code{basic} implements the basic computational procedure (see Wilcox 1973: 329, 343; Burch et al. 2017: 194; Egbert et al. 2020: 98). The basic version can be applied to absolute frequencies and normalized frequencies. For dispersion analysis, absolute frequencies only make sense if the corpus parts are identical in size. Wilcox (1973: 343, 'MDA', column 1 and 2) gives both variants of the basic version. The first use of \eqn{D_{A}} for corpus-linguistic dispersion analysis appears in Burch et al. (2017: 194), a paper that deals with equal-sized parts and therefore uses the variant for absolute frequencies. Egbert et al. (2020: 98) rely on the variant using normalized frequencies. Since this variant of the basic version of \eqn{D_{A}} works irrespective of the length of the corpus parts (equal or variable), we will only give this version of the formula. Note that while the formula represents \code{conventional} scaling (0 = uneven, 1 = even), in the current function the directionality is controlled separately using the argument \code{directionality}.
#' 
#' &emsp; \eqn{1 - \frac{\sum_{i = 1}^{k-1} \sum_{j = i+1}^{k} |R_i - R_j|}{\frac{k(k-1)}{2}} \times \frac{1}{2\frac{\sum_i^k R_i}{k}}} (Egbert et al. 2020: 98)
#' 
#' The function uses a different version of the same formula, which relies on the proportional \eqn{r_i} values instead of the normalized subfrequencies \eqn{R_i}. This version yields the identical result; the \eqn{r_i} quantities are also the key to using the computational shortcut given in Wilcox (1973: 343). This is the basic formula for \eqn{D_{A}} using \eqn{r_i} instead of \eqn{R_i} values: 
#' 
#' &emsp; \eqn{1 - \frac{\sum_{i = 1}^{k-1} \sum_{j = i+1}^{k} |r_i - r_j|}{k-1}} (Wilcox 1973: 343; see also Sönning 2022)
#' 
#' The value \code{shortcut} implements the computational shortcut given in Wilcox (1973: 343). Critically, the proportional quantities \eqn{r_i} must first be sorted in decreasing order. Only after this rearrangement can the shortcut version be applied. We will refer to this rearranged version of \eqn{r_i} as \eqn{r_i^{sorted}}:
#' 
#' &emsp; \eqn{\frac{2\left(\sum_{i = 1}^{k} (i \times r_i^{sorted}) - 1\right)}{k-1}} (Wilcox 1973: 343)
#'
#' The value \code{shortcut_mod} adds a minor modification to the computational shortcut to ensure \eqn{D_{A}} does not exceed 1 (on the conventional dispersion scale):
#' 
#' &emsp; \eqn{\frac{2\left(\sum_{i = 1}^{k} (i \times r_i^{sorted}) - 1\right)}{k-1} \times \frac{k}{k - 1}}
#' 
#' @return A numeric value
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
#'   procedure = "basic",
#'   directionality = "conventional",
#'   freq_adjust = FALSE)
#' }
#' 
disp_DA <- function(subfreq,
                    partsize,
                    procedure = "basic",
                    directionality = "conventional",
                    freq_adjust = FALSE,
                    freq_adjust_method = "pervasive_even",
                    digits = NULL,
                    verbose = TRUE,
                    print_score = TRUE) {
  
  calculate_DA <- function(subfreq, partsize){
    R_i <- subfreq / partsize
    r_i <- R_i / sum(R_i)
    k <- length(partsize)
    
    if (procedure == "basic") {
      dist_r <- as.matrix(stats::dist(r_i, method = "manhattan"))
      DA <- 1 - (mean(dist_r[lower.tri(dist_r)]) / (2 / k))
    } else if (procedure == "shortcut") {
      DA <- (2 * sum(sort(r_i, decreasing = TRUE) * 1:k) - 1) / (k - 1)
    } else if (procedure == "shortcut_mod") {
      DA <- ((2 * sum(sort(r_i, decreasing = TRUE) * 1:k) - 1) / (k - 1)) / (k/(k-1))
    }
  }

  if (sum(subfreq) == 0){
    output <- NA
    
  } else {
    
    DA_score <- calculate_DA(subfreq, partsize)
    output <- DA_score
    
    if (directionality == "gries") DA_score <- 1 - DA_score
    
    if (freq_adjust == TRUE){
      
      subfreq_min_disp <- find_min_disp(
        subfreq, 
        partsize)
      
      subfreq_max_disp <- find_max_disp(
        subfreq, 
        partsize)
      
      DA_min <- calculate_DA(subfreq_min_disp, partsize)
      DA_max <- calculate_DA(subfreq_max_disp, partsize)
      
      if (directionality == "gries") {
        DA_min <- 1 - DA_min
        DA_max <- 1 - DA_max
      } 
      
      # frequency-adjusted version
      output <- (DA_score - DA_min) / (DA_max - DA_min)
    }
  }
  
  if (freq_adjust == TRUE){
    names(output) <- "DA_nofreq"
  } else {
    names(output) <- "DA"
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

    if (procedure == "basic") {
      message("\nComputed using the basic formula for DA, see:")
      message("  Wilcox (1967: 343, 'MDA', column 2), Burch et al. (2017: 194-196)")
    } else if (procedure == "shortcut") {
      message("\nComputed using the computational shortcut suggested by")
      message("  Wilcox (1967: 343, 'MDA', column 4)")
    } else if (procedure == "shortcut_mod") {
      message("\nComputed using the computational shortcut suggested by")
      message("  Wilcox (1967: 343, 'MDA', column 4) with a minor")
      message("  correction to ensure DA does not exceed 1 (conventional)")
    }
  }

  invisible(output)
}


#' Calculate the dispersion measure \eqn{D_{A}} for a term-document matrix
#'
#' @description
#' This function calculates the dispersion measure \eqn{D_{A}}. It offers two different computational procedures, the basic version as well as a computational shortcut. It also allows the user to choose the directionality of scaling, i.e. whether higher values denote a more even or a less even distribution. It also provides the option of calculating frequency-adjusted dispersion scores.
#'
#' @inheritParams disp_tdm
#' @inheritParams disp_DA
#' 
#' @author Lukas Soenning
#' 
#' @details 
#' This function takes as input a term-document matrix and returns, for each item (i.e. each row) the dispersion measure \eqn{D_{A}}. The rows in the matrix represent the items, and the columns the corpus parts. Importantly, the term-document matrix must include an additional row that records the size of the corpus parts. For a proper term-document matrix, which includes all items that appear in the corpus, this can be added as a column margin, which sums the frequencies in each column. If the matrix only includes a selection of items drawn from the corpus, this information cannot be derived from the matrix and must be provided as a separate row.
#' 
#' - Directionality: \eqn{D_{A}} ranges from 0 to 1. The \code{conventional} scaling of dispersion measures (see Juilland & Chang-Rodriguez 1964; Carroll 1970; Rosengren 1971) assigns higher values to more even/dispersed/balanced distributions of subfrequencies across corpus parts. Gries (2008) uses the reverse scaling, with higher values denoting a more uneven/bursty/concentrated distribution; this is implemented by the value \code{gries}.
#' 
#' - Procedure: Irrespective of the directionality of scaling, two computational procedures for \eqn{D_{A}} exist (see below for details). Both appear in Wilcox (1973), where the measure is referred to as "MDA". The basic version (represented by the value \code{basic}) carries out the full set of computations required by the composition of the formula. As the number of corpus parts grows, this can become computationally very expensive. Wilcox (1973) also gives a "computational" procedure, which is a shortcut that is much quicker and closely approximates the scores produced by the basic formula. This version is represented by the value \code{shortcut}.
#' 
#' - Frequency adjustment: Dispersion scores can be adjusted for frequency using a variant of the min-max transformation proposed by Gries (2022, 2024). The frequency-adjusted score for a specific item considers its dispersion potential, the lowest and highest possible score it can obtain given its overall corpus frequency as well as the number and size of corpus parts. The unadjusted score is then expressed relative to these endpoints, where the dispersion pessimum is set to 0, and the dispersion optimum to 1 (expressed in terms of conventional scaling). The frequency-adjusted dispersion score falls between these bounds and expresses how close the observed distribution is to the theoretical maximum and minimum. This adjustment therefore requires a maximally and a minimally dispersed distribution of the item across the parts. These hypothetical extremes can be built in different ways. In particular, different strategies exist for finding a distribution that yields the dispersion maximum. The method used by Gries (2024) uses a computationally expensive procedure that finds the distribution that produces the highest value on the dispersion measure of interest. The current function constructs extreme distributions independently of the dispersion measures used and therefore only proved approximations to the upper bound yielded by Gries's method. 
#' 
#'    - To obtain the lowest possible level of dispersion, the occurrences are allocated to the smallest corpus parts. The function starts by filling in the smallest part. If the total number of occurrences of the items is greater than the size of this part, it then continues with the second smallest corpus part and so on. This approach is very similar to that used in Gries (2024).
#'    - To obtain the highest possible level of dispersion, two methods are available, and these can be set with the argument \code{freq_adjust_method}. The choice between these methods is particularly relevant if corpus parts differ considerably in size. See documentation for \code{find_max_disp()}.
#'    
#'    
#' In the formulas given below, the following notation is used:
#' 
#' - \eqn{k} the number of corpus parts
#' - \eqn{R_i} the normalized subfrequency in part \eqn{i}, i.e. the number of occurrences of the item divided by the size of the part
#' - \eqn{r_i} a proportional quantity; the normalized subfrequency in part \eqn{i} (\eqn{R_i}) divided by the sum of all normalized subfrequencies
#' 
#' The value \code{basic} implements the basic computational procedure (see Wilcox 1973: 329, 343; Burch et al. 2017: 194; Egbert et al. 2020: 98). The basic version can be applied to absolute frequencies and normalized frequencies. For dispersion analysis, absolute frequencies only make sense if the corpus parts are identical in size. Wilcox (1973: 343, 'MDA', column 1 and 2) gives both variants of the basic version. The first use of \eqn{D_{A}} for corpus-linguistic dispersion analysis appears in Burch et al. (2017: 194), a paper that deals with equal-sized parts and therefore uses the variant for absolute frequencies. Egbert et al. (2020: 98) rely on the variant using normalized frequencies. Since this variant of the basic version of \eqn{D_{A}} works irrespective of the length of the corpus parts (equal or variable), we will only give this version of the formula. Note that while the formula represents \code{conventional} scaling (0 = uneven, 1 = even), in the current function the directionality is controlled separately using the argument \code{directionality}.
#' 
#' &emsp; \eqn{1 - \frac{\sum_{i = 1}^{k-1} \sum_{j = i+1}^{k} |R_i - R_j|}{\frac{k(k-1)}{2}} \times \frac{1}{2\frac{\sum_i^k R_i}{k}}} (Egbert et al. 2020: 98)
#' 
#' The function uses a different version of the same formula, which relies on the proportional \eqn{r_i} values instead of the normalized subfrequencies \eqn{R_i}. This version yields the identical result; the \eqn{r_i} quantities are also the key to using the computational shortcut given in Wilcox (1973: 343). This is the basic formula for \eqn{D_{A}} using \eqn{r_i} instead of \eqn{R_i} values: 
#' 
#' &emsp; \eqn{1 - \frac{\sum_{i = 1}^{k-1} \sum_{j = i+1}^{k} |r_i - r_j|}{k-1}} (Wilcox 1973: 343; see also Sönning 2022)
#' 
#' The value \code{shortcut} implements the computational shortcut given in Wilcox (1973: 343). Critically, the proportional quantities \eqn{r_i} must first be sorted in decreasing order. Only after this rearrangement can the shortcut procedure be applied. We will refer to this rearranged version of \eqn{r_i} as \eqn{r_i^{sorted}}:
#' 
#' &emsp; \eqn{\frac{2\left(\sum_{i = 1}^{k} (i \times r_i^{sorted}) - 1\right)}{k-1}} (Wilcox 1973: 343)
#'
#' The value \code{shortcut_mod} adds a minor modification to the computational shortcut to ensure \eqn{D_{A}} does not exceed 1 (on the conventional dispersion scale):
#' 
#' &emsp; \eqn{\frac{2\left(\sum_{i = 1}^{k} (i \times r_i^{sorted}) - 1\right)}{k-1} \times \frac{k}{k - 1}}
#' 
#' @return A numeric vector
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
#' disp_DA_tdm(
#'   tdm = biber150_spokenBNC2014[1:20,]
#'   row_partsize = "first_row",
#'   procedure = "basic",
#'   directionality = "conventional",
#'   freq_adjust = FALSE)
#' }
#' 
disp_DA_tdm <- function(tdm,
                        row_partsize = "first_row",
                    directionality = "conventional",
                    procedure = "basic",
                    freq_adjust = FALSE,
                    freq_adjust_method = "pervasive_even",
                    digits = NULL,
                    verbose = TRUE,
                    print_score = TRUE) {
  
  if (row_partsize == "first_row"){
    
    DA_score <- apply(
      tdm[-1,],
      1,
      function(x){
        disp_DA(subfreq = x, 
                partsize = tdm[1,],
                verbose = FALSE,
                print_score = FALSE)
      })
    
  } else if (row_partsize == "last_row"){
    
    DA_score <- apply(
      tdm[-nrow(tdm),],
      1,
      function(x){
        disp_DA(subfreq = x, 
                partsize = tdm[nrow(tdm),],
                verbose = FALSE,
                print_score = FALSE)
      })
  }
  
  if (freq_adjust == TRUE){
    
    min_disp_tdm <- find_min_disp_tdm(tdm)
    max_disp_tdm <- find_max_disp_tdm(tdm)
    
    if (row_partsize == "first_row"){
      
      DA_min <- apply(
        min_disp_tdm[-1,],
        1,
        function(x){
          disp_DA(subfreq = x, 
                  partsize = min_disp_tdm[1,],
                  verbose = FALSE,
                  print_score = FALSE)
        })
      
      DA_max <- apply(
        max_disp_tdm[-1,],
        1,
        function(x){
          disp_DA(subfreq = x, 
                  partsize = max_disp_tdm[1,],
                  verbose = FALSE,
                  print_score = FALSE)
        })
      
    } else if (row_partsize == "last_row"){
      
      DA_min <- apply(
        min_disp_tdm[-nrow(min_disp_tdm),],
        1,
        function(x){
          disp_DA(subfreq = x, 
                  partsize = min_disp_tdm[nrow(min_disp_tdm),],
                  verbose = FALSE,
                  print_score = FALSE)
        })
      
      DA_max <- apply(
        max_disp_tdm[-nrow(max_disp_tdm),],
        1,
        function(x){
          disp_DA(sbfreq = x, 
                  partsize = max_disp_tdm[nrow(max_disp_tdm),],
                  verbose = FALSE,
                  print_score = FALSE)
        })
    }
    output <- (DA_score - DA_min) / (DA_max - DA_min)
    
  } else {
    output <- DA_score
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
    
    if (procedure == "basic") {
      message("\nComputed using the basic formula for DA, see:")
      message("  Wilcox (1967: 343, 'MDA', column 2), Burch et al. (2017: 194-196)")
    } else if (procedure == "shortcut") {
      message("\nComputed using the computational shortcut suggested by")
      message("  Wilcox (1967: 343, 'MDA', column 4)")
    } else if (procedure == "shortcut_mod") {
      message("\nComputed using the computational shortcut suggested by")
      message("  Wilcox (1967: 343, 'MDA', column 4) with a minor")
      message("  correction to ensure DA does not exceed 1 (conventional)")
    }
  }
  invisible(output)
}
