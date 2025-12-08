#' Calculate parts-based dispersion measures
#'
#' @description
#' This function calculates a number of parts-based dispersion measures and allows the user to choose the directionality of scaling, i.e. whether higher values denote a more even or a less even distribution. It also offers the option of calculating frequency-adjusted dispersion scores.
#' 
#' @param subfreq A numeric vector of subfrequencies, i.e. the number of occurrences of the item in each corpus part
#' @param partsize A numeric vector specifying the size of the corpus parts
#' @param directionality Character string indicating the directionality of scaling. See details below. Possible values are `"conventional"` (default) and `"gries"`
#' @param freq_adjust Logical. Whether dispersion score should be adjusted for frequency (i.e. whether frequency should be 'partialed out'); default is `FALSE`
#' @param freq_adjust_method Character string indicating which method to use for devising dispersion extremes. See details below. Possible values are `"even"` (default) and `"pervasive"`
#' @param unit_interval Logical. Whether frequency-adjusted scores that exceed the limits of the unit interval should be replaced by 0 and 1; default is `TRUE`
#' @param digits Rounding: Integer value specifying the number of decimal places to retain (default: no rounding)
#' @param verbose Logical. Whether additional information (on directionality, formulas, frequency adjustment) should be printed; default is `TRUE`
#' @param print_score Logical. Whether the dispersion score should be printed to the console; default is `TRUE`
#' @param suppress_warning Logical. Whether warning messages should be suppressed; default is `FALSE`
#' 
#' 
#' @author Lukas Soenning
#' 
#' @details This function calculates dispersion measures based on two vectors: a set of subfrequencies (number of occurrences of the item in each corpus part) and a matching set of part sizes (the size of the corpus parts, i.e. number of word tokens). 
#' 
#' - Directionality: The scores for all measures range from 0 to 1. The conventional scaling of dispersion measures (see Juilland & Chang-Rodriguez 1964; Carroll 1970; Rosengren 1971) assigns higher values to more even/dispersed/balanced distributions of subfrequencies across corpus parts. This is the default. Gries (2008) uses the reverse scaling, with higher values denoting a more uneven/bursty/concentrated distribution; use `directionality = "gries"` to choose this option.
#' 
#' - Frequency adjustment: Dispersion scores can be adjusted for frequency using the min-max transformation proposed by Gries (2022: 184-191; 2024: 196-208). The frequency-adjusted score for an  item considers the lowest and highest possible level of dispersion it can obtain given its overall corpus frequency as well as the number (and size) of corpus parts. The unadjusted score is then expressed relative to these endpoints, where the dispersion minimum is set to 0, and the dispersion maximum to 1 (expressed in terms of conventional scaling). The frequency-adjusted score falls between these bounds and expresses how close the observed distribution is to the theoretical maximum and minimum. This adjustment therefore requires a maximally and a minimally dispersed distribution of the item across the parts. These hypothetical extremes can be built in different ways. The method used by Gries (2022, 2024) uses a computationally expensive procedure that finds the distribution that produces the highest value on the dispersion measure of interest. The current function constructs extreme distributions in a different way, based on the distributional features pervasiveness (`"pervasive"`) or evenness (`"even"`). You can choose between these with the argument `freq_adjust_method`; the default is `even`. For details and explanations, see `vignette("frequency-adjustment")`. 
#' 
#'    - To obtain the lowest possible level of dispersion, the occurrences are either allocated to as few corpus parts as possible (`"pervasive"`), or they are assigned to the smallest corpus part(s) (`"even"`).
#'    - To obtain the highest possible level of dispersion, the occurrences are either spread as broadly across corpus parts as possible (`"pervasive"`), or they are allocated to corpus parts in proportion to their size (`"even"`). The choice between these methods is particularly relevant if corpus parts differ considerably in size. See documentation for `find_max_disp()` and `vignette("frequency-adjustment")`.
#' 
#' The following measures are computed, listed in chronological order (see details below):
#' 
#' - \eqn{R_{rel}} (Keniston 1920)
#' - \eqn{D} (Juilland & Chang-Rodriguez 1964)
#' - \eqn{D_2} (Carroll 1970)
#' - \eqn{S} (Rosengren 1971)
#' - \eqn{D_P} (Gries 2008; modification: Egbert et al. 2020)
#' - \eqn{D_A} (Burch et al. 2017)
#' - \eqn{D_{KL}} (Gries 2024)
#' 
#' In the formulas given below, the following notation is used:
#' 
#' - \eqn{k} the number of corpus parts
#' - \eqn{T_i} the absolute subfrequency in part \eqn{i}
#' - \eqn{t_i} a proportional quantity; the subfrequency in part \eqn{i} divided by the total number of occurrences of the item in the corpus (i.e. the sum of all subfrequencies)
#' - \eqn{W_i} the absolute size of corpus part \eqn{i}
#' - \eqn{w_i} a proportional quantity; the size of corpus part \eqn{i} divided by the size of the corpus (i.e. the sum of the part sizes) 
#' - \eqn{R_i} the normalized subfrequency in part \eqn{i}, i.e. the subfrequency divided by the size of the corpus part
#' - \eqn{r_i} a proportional quantity; the normalized subfrequency in part \eqn{i} divided by the sum of all normalized subfrequencies
#' - \eqn{N} corpus frequency, i.e. the total number of occurrence of the item in the corpus
#' 
#' Note that the formulas cited below differ in their scaling, i.e. whether 1 reflects an even or an uneven distribution. In the current function, this behavior is overridden by the argument `directionality`. The specific scaling used in the formulas below is therefore irrelevant.
#' 
#' \eqn{R_{rel}} refers to the relative range, i.e. the proportion of corpus parts containing at least one occurrence of the item.
#' 
#' \eqn{D} denotes Juilland's D and is calculated as follows (this formula uses conventional scaling); \eqn{\bar{R_i}} refers to the average over the normalized subfrequencies:
#' 
#'    \eqn{1 - \sqrt{\frac{\sum_{i = 1}^k (R_i - \bar{R_i})^2}{k}} \times \frac{1}{\bar{R_i} \sqrt{k - 1}}}
#' 
#' \eqn{D_2} denotes the index proposed by Carroll (1970); the following formula uses conventional scaling:
#' 
#'    \eqn{\frac{\sum_i^k r_i \log_2{\frac{1}{r_i}}}{\log_2{k}}}
#' 
#' \eqn{S} is the dispersion measure proposed by Rosengren (1971); the formula uses conventional scaling:
#' 
#'    \eqn{\frac{(\sum_i^k r_i \sqrt{w_i T_i}}{N}}
#' 
#' \eqn{D_P} represents Gries's deviation of proportions; the following formula is the modified version suggested by Egbert et al. (2020: 99); it implements conventional scaling (0 = uneven, 1 = even) and the notation \eqn{min\{w_i: t_i > 0\}} refers to the \eqn{w_i} value among those corpus parts that include at least one occurrence of the item.
#' 
#'    \eqn{1 - \frac{\sum_i^k |t_i - w_i|}{2} \times \frac{1}{1 - min\{w_i: t_i > 0\}}}
#' 
#' \eqn{D_A} is a measure introduced into dispersion analysis by Burch et al. (2017). The following formula is the one used by Egbert et al. (2020: 98); it relies on normalized frequencies and therefore works with corpus parts of different size. The formula represents conventional scaling (0 = uneven, 1 = even):
#' 
#'    \eqn{1 - \frac{\sum_{i = 1}^{k-1} \sum_{j = i+1}^{k} |R_i - R_j|}{\frac{k(k-1)}{2}} \times \frac{1}{2\frac{\sum_i^k R_i}{k}}}
#' 
#' The current function uses a formula that may be found in Wilcox (1973: 343). It relies on the proportional \eqn{r_i} values instead of the normalized subfrequencies \eqn{R_i}: 
#' 
#'    \eqn{1 - \frac{\sum_{i = 1}^{k-1} \sum_{j = i+1}^{k} |r_i - r_j|}{k-1}}
#' 
#' Since this formula is computationally expensive, the function actually uses the computational shortcut given in Wilcox (1973: 343). Critically, the proportional quantities \eqn{r_i} must first be sorted in decreasing order. Only after this rearrangement can the shortcut version be applied. We will refer to this rearranged version of \eqn{r_i} as \eqn{r_i^{sorted}}:
#' 
#'    \eqn{\frac{2\left(\sum_{i = 1}^{k} (i \times r_i^{sorted}) - 1\right)}{k-1}} (Wilcox 1973: 343) 
#' 
#' \eqn{D_{KL}} refers to a measure proposed by Gries (2020, 2021); for standardization, it uses the odds-to-probability transformation (Gries 2024: 90) and represents Gries scaling (0 = even, 1 = uneven):
#' 
#'    \eqn{\frac{\sum_i^k t_i \log_2{\frac{t_i}{w_i}}}{1 + \sum_i^k t_i \log_2{\frac{t_i}{w_i}}}}
#' 
#' 
#' @returns A numeric vector of seven dispersion scores
#' 
#' @seealso For finer control over the calculation of several dispersion measures:
#' - [disp_R()] for \eqn{Range} 
#' - [disp_DP()] for \eqn{D_P}
#' - [disp_DA()] for \eqn{D_A}
#' - [disp_DKL()] for \eqn{D_{KL}}
#' 
#' 
#' @references 
#' 
#' Burch, Brent, Jesse Egbert & Douglas Biber. 2017. Measuring and interpreting lexical dispersion in corpus linguistics. \emph{Journal of Research Design and Statistics in Linguistics and Communication Science} 3(2). 189--216. \doi{doi:10.1558/jrds.33066}
#' 
#' Carroll, John B. 1970. An alternative to Juilland’s usage coefficient for lexical frequencies and a proposal for a standard frequency index. \emph{Computer Studies in the Humanities and Verbal Behaviour} 3(2). 61--65. \doi{doi:10.1002/j.2333-8504.1970.tb00778.x}
#' 
#' Egbert, Jesse, Brent Burch & Douglas Biber. 2020. Lexical dispersion and corpus design. \emph{International Journal of Corpus Linguistics} 25(1). 89--115. \doi{doi:10.1075/ijcl.18010.egb}
#' 
#' Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. \emph{International Journal of Corpus Linguistics} 13(4). 403--437. \doi{doi:10.1075/ijcl.13.4.02gri}
#' 
#' Gries, Stefan Th. 2020. Analyzing dispersion. In Magali Paquot & Stefan Th. Gries (eds.), \emph{A practical handbook of corpus linguistics}, 99--118. New York: Springer. \doi{doi:10.1007/978-3-030-46216-1_5}
#' 
#' Gries, Stefan Th. 2021. A new approach to (key) keywords analysis: Using frequency, and now also dispersion. \emph{Research in Corpus Linguistics} 9(2). 1--33. \doi{doi:10.32714/ricl.09.02.02}
#' 
#' Gries, Stefan Th. 2022. What do (most of) our dispersion measures measure (most)? Dispersion? \emph{Journal of Second Language Studies} 5(2). 171--205. \doi{doi:10.1075/jsls.21029.gri}
#' 
#' Gries, Stefan Th. 2024. \emph{Frequency, dispersion, association, and keyness: Revising and tupleizing corpus-linguistic measures}. Amsterdam: Benjamins. \doi{doi:10.1075/scl.115}
#' 
#' Juilland, Alphonse G. & Eugenio Chang-Rodríguez. 1964. \emph{Frequency dictionary of Spanish words.} The Hague: Mouton de Gruyter. \doi{doi:10.1515/9783112415467}
#' 
#' Keniston, Hayward. 1920. Common words in Spanish. \emph{Hispania} 3(2). 85--96. \doi{doi:10.2307/331305}
#' 
#' Lijffijt, Jefrey & Stefan Th. Gries. 2012. Correction to Stefan Th. Gries’ ‘Dispersions and adjusted frequencies in corpora’. \emph{International Journal of Corpus Linguistics} 17(1). 147--149. \doi{doi:10.1075/ijcl.17.1.08lij}
#' 
#' Rosengren, Inger. 1971. The quantitative concept of language and its relation to the structure of frequency dictionaries. \emph{Études de linguistique appliquée (Nouvelle Série)} 1. 103--127.
#' 
#' 
#' @export
#'
#' @examples
#' disp_DP(
#'   subfreq = c(0,0,1,2,5), 
#'   partsize = rep(1000, 5),
#'   directionality = "conventional",
#'   freq_adjust = FALSE)
#'
disp <- function(subfreq,
                 partsize,
                 directionality = "conventional",
                 freq_adjust = FALSE,
                 freq_adjust_method = "even",
                 unit_interval = TRUE,
                 digits = NULL,
                 verbose = TRUE,
                 print_score = TRUE,
                 suppress_warning = FALSE) {
  
  if (length(subfreq) != length(partsize)){
    stop("Lengths of the variables 'subfreq' and 'partsize' differ.")
  }
  
  calculate_dispersion <- function(subfreq,
                                   partsize){
    W_i <- partsize
    w_i <- W_i / sum(W_i)
    T_i <- subfreq
    t_i <- T_i / sum(T_i)
    R_i <- T_i / W_i
    r_i <- R_i / sum(R_i)
    k <- length(T_i)
    
    Rrel <- sum(T_i != 0) / length(T_i)
    
    D    <- 1 - (sqrt(sum((R_i - mean(R_i))^2) / k) / (mean(R_i) * sqrt(k - 1)))
    
    D2 <- -sum(r_i * ifelse(r_i == 0, 0, log2(r_i))) / log2(k)
    
    S    <- (sum(sqrt(w_i * T_i))^2) / sum(T_i)
    
    DP   <- 1 - ((sum(abs(t_i - w_i)) * (sum(W_i) / (sum(W_i) - min(W_i[T_i > 0], fill = 0)))) / 2)
    
    DA <-  (2 * (sum(sort(r_i, decreasing = TRUE) * 1:k) - 1)) / (k - 1)
    
    KLD  <- sum(t_i * ifelse(t_i == 0, 0, log2(t_i / w_i)))
    DKL  <- 1 - (KLD / (1 + KLD))
    
    output <- c(Rrel, D, D2, S, DP, DA, DKL)
    
    return(output)
  }
  
  
  if (sum(subfreq) == 0){
    output <- rep(NA, 7)
    
  } else {
    
    disp_score <- calculate_dispersion(subfreq, partsize)
    output <- disp_score
    
    if (freq_adjust == TRUE){
      
      subfreq_min_disp <- find_min_disp(
        subfreq,
        partsize,
        freq_adjust_method)
      
      subfreq_max_disp <- find_max_disp(
        subfreq,
        partsize,
        freq_adjust_method)
      
      
      disp_min <- calculate_dispersion(subfreq_min_disp, partsize)
      disp_max <- calculate_dispersion(subfreq_max_disp, partsize)
      
      if (directionality == "gries") {
        disp_min <- 1 - disp_min
        disp_max <- 1 - disp_max
      } 
      
      output <- (disp_score - disp_min) / (disp_max - disp_min)
      output[is.nan(output)] <- disp_score[is.nan(output)]
      
      item_exceeds_limits <- FALSE
      if (unit_interval){
        item_exceeds_limits <- sum(output < 0 | output > 1) > 0
        output[output > 1] <- 1
        output[output < 0] <- 0
      }
    }
  }
  
  if (freq_adjust == TRUE){
    names(output) <- c(
      "Rrel_nofreq", "D_nofreq", "D2_nofreq", "S_nofreq", 
      "DP_nofreq", "DA_nofreq", "DKL_nofreq")    
  } else {
    names(output) <- c("Rrel", "D", "D2", "S", "DP", "DA", "DKL")
  }
  
  if (directionality == "gries") output <- 1 - output
  
  if (!is.null(digits)) output <- round(output, digits)
  
  if (print_score != FALSE) print(output)
  
  if (sum(subfreq) == 0 & suppress_warning != TRUE){
    warning("All subfrequencies are 0; returning NA.")
  } else {
    if (verbose) {
      log_buffer <- character()
      
      logmsg <- function(x) {
        log_buffer <<- c(log_buffer, x)
      }
      
      if (freq_adjust == TRUE){
        logmsg("\nDispersion scores are adjusted for frequency using the min-max")
        logmsg("  transformation (see Gries 2024: 196-208); please note that the")
        logmsg("  method implemented here does not work well if corpus parts differ")
        logmsg("  considerably in size; see vignette('frequency-adjustment')")
        
        if (unit_interval & item_exceeds_limits){
          logmsg("\nSome frequency-adjusted score(s) exceed(s) the limits of the")
          logmsg("  unit interval [0,1], and was/were replaced by 0 or 1")
        }
      }
      if (directionality == "gries") {
        logmsg("\nScores follow scaling used by Gries (2008):")
        logmsg("  0 = maximally even/dispersed/balanced distribution (optimum)")
        logmsg("  1 = maximally uneven/bursty/concentrated distribution (pessimum)")
      } else {
        logmsg("\nScores follow conventional scaling:")
        logmsg("  0 = maximally uneven/bursty/concentrated distribution (pessimum)")
        logmsg("  1 = maximally even/dispersed/balanced distribution (optimum)")
      }
      
      logmsg("\nFor Gries's DP, the function uses the modified version suggested by")
      logmsg("  Egbert et al. (2020)")
      
      logmsg("\nDA is calculated using the computational shortcut suggested by")
      logmsg("  Wilcox (1973: 343, 'MDA', column 4)")         
      
      logmsg("\nFor DKL, standardization to the unit interval [0,1] is based on the")
      logmsg("  odds-to-probability transformation, see Gries (2024: 90)")

      cat(paste(log_buffer, collapse = "\n"))
    }
  }
  invisible(output)
}


#' Calculate parts-based dispersion measures for a term-document matrix
#'
#' @description
#' This function calculates a number of parts-based dispersion measures and allows the user to choose the directionality of scaling, i.e. whether higher values denote a more even or a less even distribution. It also offers the option of calculating frequency-adjusted dispersion scores.
#'
#' @inheritParams disp
#' @param tdm A term-document matrix, where rows represent items and columns represent corpus parts; must also contain a row giving the size of the corpus parts (first or last row in the term-document matrix)
#' @param add_frequency Logical. Whether to add a column that gives the total number of occurrences of the item across a corpus parts; default is `TRUE` 
#' @param row_partsize Character string indicating which row in the term-document matrix contains the size of the corpus parts. Possible values are `"first"` (default) and `"last"`
#' @param print_scores Logical. Whether the dispersion scores should be printed to the console; default is `TRUE`
#' 
#' @author Lukas Soenning
#' 
#' @details This function takes as input a term-document matrix and returns, for each item (i.e. each row) a variety of dispersion measures. The rows in the input matrix represent the items, and the columns the corpus parts. Importantly, the term-document matrix must include an additional row that records the size of the corpus parts. For a proper term-document matrix, which includes all items that appear in the corpus, this can be added as a column margin, which sums the frequencies in each column. If the matrix only includes a selection of items drawn from the corpus, this information cannot be derived from the matrix and must be provided as a separate row.
#' 
#' - Directionality: The scores for all measures range from 0 to 1. The conventional scaling of dispersion measures (see Juilland & Chang-Rodriguez 1964; Carroll 1970; Rosengren 1971) assigns higher values to more even/dispersed/balanced distributions of subfrequencies across corpus parts. Gries (2008) uses the reverse scaling, with higher values denoting a more uneven/bursty/concentrated distribution; this is implemented by the value `gries`.
#' 
#' - Frequency adjustment: Dispersion scores can be adjusted for frequency using the min-max transformation proposed by Gries (2022: 184-191; 2024: 196-208). The frequency-adjusted score for an  item considers the lowest and highest possible level of dispersion it can obtain given its overall corpus frequency as well as the number (and size) of corpus parts. The unadjusted score is then expressed relative to these endpoints, where the dispersion minimum is set to 0, and the dispersion maximum to 1 (expressed in terms of conventional scaling). The frequency-adjusted score falls between these bounds and expresses how close the observed distribution is to the theoretical maximum and minimum. This adjustment therefore requires a maximally and a minimally dispersed distribution of the item across the parts. These hypothetical extremes can be built in different ways. The method used by Gries (2022, 2024) uses a computationally expensive procedure that finds the distribution that produces the highest value on the dispersion measure of interest. The current function constructs extreme distributions in a different way, based on the distributional features pervasiveness (`"pervasive"`) or evenness (`"even"`). You can choose between these with the argument `freq_adjust_method`; the default is `even`. For details and explanations, see `vignette("frequency-adjustment")`. 
#' 
#'    - To obtain the lowest possible level of dispersion, the occurrences are either allocated to as few corpus parts as possible (`"pervasive"`), or they are assigned to the smallest corpus part(s) (`"even"`).
#'    - To obtain the highest possible level of dispersion, the occurrences are either spread as broadly across corpus parts as possible (`"pervasive"`), or they are allocated to corpus parts in proportion to their size (`"even"`). The choice between these methods is particularly relevant if corpus parts differ considerably in size. See documentation for `find_max_disp()` and `vignette("frequency-adjustment")`.
#'    
#' The following measures are computed, listed in chronological order (see details below):
#' 
#' - \eqn{R_{rel}} (Keniston 1920)
#' - \eqn{D} (Juilland & Chang-Rodriguez 1964)
#' - \eqn{D_2} (Carroll 1970)
#' - \eqn{S} (Rosengren 1971)
#' - \eqn{D_P} (Gries 2008; modification: Egbert et al. 2020)
#' - \eqn{D_A} (Burch et al. 2017)
#' - \eqn{D_{KL}} (Gries 2024)
#' 
#' In the formulas given below, the following notation is used:
#' 
#' - \eqn{k} the number of corpus parts
#' - \eqn{T_i} the absolute subfrequency in part \eqn{i}
#' - \eqn{t_i} a proportional quantity; the subfrequency in part \eqn{i} divided by the total number of occurrences of the item in the corpus (i.e. the sum of all subfrequencies)
#' - \eqn{W_i} the absolute size of corpus part \eqn{i}
#' - \eqn{w_i} a proportional quantity; the size of corpus part \eqn{i} divided by the size of the corpus (i.e. the sum of the part sizes) 
#' - \eqn{R_i} the normalized subfrequency in part \eqn{i}, i.e. the subfrequency divided by the size of the corpus part
#' - \eqn{r_i} a proportional quantity; the normalized subfrequency in part \eqn{i} divided by the sum of all normalized subfrequencies
#' - \eqn{N} corpus frequency, i.e. the total number of occurrence of the item in the corpus
#' 
#' Note that the formulas cited below differ in their scaling, i.e. whether 1 reflects an even or an uneven distribution. In the current function, this behavior is overridden by the argument `directionality`. The specific scaling used in the formulas below is therefore irrelevant.
#' 
#' \eqn{R_{rel}} refers to the relative range, i.e. the proportion of corpus parts containing at least one occurrence of the item
#' 
#' \eqn{D} denotes Juilland's D and is calculated as follows (this formula uses conventional scaling); \eqn{\bar{R_i}} denotes the average over the normalized subfrequencies:
#' 
#'    \eqn{1 - \sqrt{\frac{\sum_{i = 1}^k (R_i - \bar{R_i})^2}{k}} \times \frac{1}{\bar{R_i} \sqrt{k - 1}}}
#' 
#' \eqn{D_2} denotes the index proposed by Carroll (1970); the following formula uses conventional scaling:
#' 
#'    \eqn{\frac{\sum_i^k r_i \log_2{\frac{1}{r_i}}}{\log_2{k}}}
#' 
#' \eqn{S} is the dispersion measure proposed by Rosengren (1971); the formula uses conventional scaling:
#' 
#'    \eqn{\frac{(\sum_i^k r_i \sqrt{w_i T_i}}{N}}
#' 
#' \eqn{D_P} represents Gries's \emph{deviation of proportions}; the following formula is the modified version suggested by Egbert et al. (2020: 99); it implements conventional scaling (0 = uneven, 1 = even) and the notation \eqn{min\{w_i: t_i > 0\}} refers to the \eqn{w_i} value among those corpus parts that include at least one occurrence of the item.
#' 
#'    \eqn{1 - \frac{\sum_i^k |t_i - w_i|}{2} \times \frac{1}{1 - min\{w_i: t_i > 0\}}}
#' 
#' \eqn{D_A} is a measure introduced into dispersion analysis by Burch et al. (2017). The following formula is the one used by Egbert et al. (2020: 98); it relies on normalized frequencies and therefore works with corpus parts of different size. The formula represents conventional scaling (0 = uneven, 1 = even):
#' 
#'    \eqn{1 - \frac{\sum_{i = 1}^{k-1} \sum_{j = i+1}^{k} |R_i - R_j|}{\frac{k(k-1)}{2}} \times \frac{1}{2\frac{\sum_i^k R_i}{k}}}
#' 
#' The current function uses a formula that may be found in Wilcox (1973: 343). It relies on the proportional \eqn{r_i} values instead of the normalized subfrequencies \eqn{R_i}: 
#' 
#'    \eqn{1 - \frac{\sum_{i = 1}^{k-1} \sum_{j = i+1}^{k} |r_i - r_j|}{k-1}}
#' 
#' Since this formula is computationally expensive, the function actually uses the computational shortcut given in Wilcox (1973: 343). Critically, the proportional quantities \eqn{r_i} must first be sorted in decreasing order. Only after this rearrangement can the shortcut version be applied. We will refer to this rearranged version of \eqn{r_i} as \eqn{r_i^{sorted}}:
#' 
#'    \eqn{\frac{2\left(\sum_{i = 1}^{k} (i \times r_i^{sorted}) - 1\right)}{k-1}} (Wilcox 1973: 343) 
#' 
#' \eqn{D_{KL}} denotes a measure proposed by Gries (2020, 2021); for standardization, it uses the odds-to-probability transformation (Gries 2024: 90) and represents Gries scaling (0 = even, 1 = uneven):
#' 
#'    \eqn{\frac{\sum_i^k t_i \log_2{\frac{t_i}{w_i}}}{1 + \sum_i^k t_i \log_2{\frac{t_i}{w_i}}}}
#' 
#' 
#' @returns A data frame with one row per item
#' 
#' 
#' @seealso For finer control over the calculation of several dispersion measures:
#' - [disp_R_tdm()] for \eqn{Range} 
#' - [disp_DP_tdm()] for \eqn{D_P}
#' - [disp_DA_tdm()] for \eqn{D_A}
#' - [disp_DKL_tdm()] for \eqn{D_{KL}}
#' 
#' 
#' @references 
#' 
#' Burch, Brent, Jesse Egbert & Douglas Biber. 2017. Measuring and interpreting lexical dispersion in corpus linguistics. \emph{Journal of Research Design and Statistics in Linguistics and Communication Science} 3(2). 189--216. \doi{doi:10.1558/jrds.33066}
#' 
#' Carroll, John B. 1970. An alternative to Juilland’s usage coefficient for lexical frequencies and a proposal for a standard frequency index. \emph{Computer Studies in the Humanities and Verbal Behaviour} 3(2). 61--65. \doi{doi:10.1002/j.2333-8504.1970.tb00778.x}
#' 
#' Egbert, Jesse, Brent Burch & Douglas Biber. 2020. Lexical dispersion and corpus design. \emph{International Journal of Corpus Linguistics} 25(1). 89--115. \doi{doi:10.1075/ijcl.18010.egb}
#' 
#' Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. \emph{International Journal of Corpus Linguistics} 13(4). 403--437. \doi{doi:10.1075/ijcl.13.4.02gri}
#' 
#' Gries, Stefan Th. 2020. Analyzing dispersion. In Magali Paquot & Stefan Th. Gries (eds.), \emph{A practical handbook of corpus linguistics}, 99--118. New York: Springer. \doi{doi:10.1007/978-3-030-46216-1_5}
#' 
#' Gries, Stefan Th. 2021. A new approach to (key) keywords analysis: Using frequency, and now also dispersion. \emph{Research in Corpus Linguistics} 9(2). 1--33. \doi{doi:10.32714/ricl.09.02.02}
#' 
#' Gries, Stefan Th. 2022. What do (most of) our dispersion measures measure (most)? Dispersion? \emph{Journal of Second Language Studies} 5(2). 171--205. \doi{doi:10.1075/jsls.21029.gri}
#' 
#' Gries, Stefan Th. 2024. \emph{Frequency, dispersion, association, and keyness: Revising and tupleizing corpus-linguistic measures}. Amsterdam: Benjamins. \doi{doi:10.1075/scl.115}
#' 
#' Juilland, Alphonse G. & Eugenio Chang-Rodríguez. 1964. \emph{Frequency dictionary of Spanish words.} The Hague: Mouton de Gruyter. \doi{doi:10.1515/9783112415467}
#' 
#' Keniston, Hayward. 1920. Common words in Spanish. \emph{Hispania} 3(2). 85--96. \doi{doi:10.2307/331305}
#' 
#' Lijffijt, Jefrey & Stefan Th. Gries. 2012. Correction to Stefan Th. Gries’ ‘Dispersions and adjusted frequencies in corpora’. \emph{International Journal of Corpus Linguistics} 17(1). 147--149. \doi{doi:10.1075/ijcl.17.1.08lij}
#' 
#' Rosengren, Inger. 1971. The quantitative concept of language and its relation to the structure of frequency dictionaries. \emph{Études de linguistique appliquée (Nouvelle Série)} 1. 103--127.
#' 
#' @export
#'
#' @examples
#' disp_tdm(
#'   tdm = biber150_spokenBNC2014[1:20,],
#'   row_partsize = "first",
#'   directionality = "conventional",
#'   freq_adjust = FALSE)
#'
disp_tdm <- function(tdm,
                     row_partsize = "first",
                     directionality = "conventional",
                     freq_adjust = FALSE,
                     freq_adjust_method = "even",
                     add_frequency = TRUE,
                     unit_interval = TRUE,
                     digits = NULL,
                     verbose = TRUE,
                     print_scores = TRUE,
                     suppress_warning = FALSE) {
  
  if(missing(row_partsize)){
    stop("Please indicate which row in the term-document matrix includes the part sizes.\n  Use argument 'row_partsize' to locate the correct row ('first' or 'last').")
  }
  
  if ("data.frame" %in% class(tdm)){
    if (inherits(unlist(tdm[,1]), "character") | inherits(unlist(tdm[,1]), "factor")){
      tdm <- as.matrix(tdm[,-1])
      rownames(tdm) <- tdm[,1]
    } else {
      row_names <- rownames(tdm)
      tdm <- as.matrix(tdm)
      rownames(tdm) <- row_names      
    }
  }
  
  calculate_dispersion <- function(subfreq,
                                   partsize){
    W_i <- partsize
    w_i <- W_i / sum(W_i)
    T_i <- subfreq
    t_i <- T_i / sum(T_i)
    R_i <- T_i / W_i
    r_i <- R_i / sum(R_i)
    k <- length(T_i)
    
    Rrel <- sum(T_i != 0) / length(T_i)
    
    D    <- 1 - (sqrt(sum((R_i - mean(R_i))^2) / k) / (mean(R_i) * sqrt(k - 1)))
    
    D2 <- -sum(r_i * ifelse(r_i == 0, 0, log2(r_i))) / log2(k)
    
    S    <- (sum(sqrt(w_i * T_i))^2) / sum(T_i)
    
    DP   <- 1 - ((sum(abs(t_i - w_i)) * (sum(W_i) / (sum(W_i) - min(W_i[T_i > 0], fill = 0)))) / 2)
    
    DA <-  (2 * (sum(sort(r_i, decreasing = TRUE) * 1:k) - 1)) / (k - 1)
    
    KLD  <- sum(t_i * ifelse(t_i == 0, 0, log2(t_i / w_i)))
    DKL  <- 1 - (KLD / (1 + KLD))
    
    output <- c(Rrel, D, D2, S, DP, DA, DKL)
    
    return(output)
  }
  
  if (row_partsize == "first"){
    
    if (!all(colSums(tdm[-1,]) <= tdm[1,])){
      stop("The row you indicated (first row) does not contain the (correct) part sizes.\n  Use argument 'row_partsize' to locate the correct row or check content of\n  first row. At the moment, (some) counts in the first row are too small.")
    }
    
    disp_score <- apply(
      tdm[-1,],
      1,
      function(x){
        calculate_dispersion(
          subfreq = x, 
          partsize = tdm[1,])
      })
    
  } else if (row_partsize == "last"){
    
    if (!all(colSums(tdm[-nrow(tdm),]) <= tdm[nrow(tdm),])){
      stop("The row you indicated (last row) does not contain the (correct) part sizes.\n  Use argument 'row_partsize' to locate the correct row or check content of\n  last row. At the moment, (some) counts in the last row are too small.")
    }
    
    disp_score <- apply(
      tdm[-nrow(tdm),],
      1,
      function(x){
        calculate_dispersion(
          subfreq = x, 
          partsize = tdm[nrow(tdm),])
      })
  }
  
  if (freq_adjust == TRUE){
    
    min_disp_tdm <- find_min_disp_tdm(
      tdm, 
      row_partsize = row_partsize,
      freq_adjust_method = freq_adjust_method)
    
    max_disp_tdm <- find_max_disp_tdm(
      tdm, 
      row_partsize = row_partsize,
      freq_adjust_method = freq_adjust_method)
    
    
    if (row_partsize == "first"){
      
      disp_min <- apply(
        min_disp_tdm[-1,],
        1,
        function(x){
          calculate_dispersion(x, 
                               min_disp_tdm[1,])
        })
      
      disp_max <- apply(
        max_disp_tdm[-1,],
        1,
        function(x){
          calculate_dispersion(x, 
                               max_disp_tdm[1,])
        })
      
      
    } else if (row_partsize == "last"){
      
      disp_min <- apply(
        min_disp_tdm[-nrow(min_disp_tdm),],
        1,
        function(x){
          calculate_dispersion(
            x, 
            min_disp_tdm[nrow(min_disp_tdm),])
        })
      
      disp_max <- apply(
        max_disp_tdm[-nrow(max_disp_tdm),],
        1,
        function(x){
          calculate_dispersion(
            x, 
            max_disp_tdm[nrow(max_disp_tdm),])
        })
    }
    output <- (disp_score - disp_min) / (disp_max - disp_min)
    output[is.nan(output)] <- disp_score[is.nan(output)]
    
    if (unit_interval){
      
      n_items_exceeding_limits_measure <- apply(
        output, 1, function(x){
          sum(x < 0 | x > 1, na.rm = TRUE)
        })
      
      output[output > 1] <- 1
      output[output < 0] <- 0
    }
    
  } else {
    output <- disp_score
  }
  
  if (freq_adjust == TRUE){
    rownames(output) <- c(
      "Rrel_nofreq", "D_nofreq", "D2_nofreq", "S_nofreq", 
      "DP_nofreq", "DA_nofreq", "DKL_nofreq")    
  } else {
    rownames(output) <- c("Rrel", "D", "D2", "S", "DP", "DA", "DKL")
  }
  
  if (directionality == "gries") output <- 1 - output 
  
  if (!is.null(digits)) output <- round(output, digits)
  
  output <- t(output)
  
  if (add_frequency == TRUE){
    if (row_partsize == "first"){
      output <- cbind(output, rowSums(tdm[-1,]))
    }
    if (row_partsize == "last"){
      output <- cbind(output, rowSums(tdm[-nrow(tdm),]))
    }
    colnames(output)[8] <- "frequency"
  }
  output <- data.frame(output)
  
  if (row_partsize == "first"){
    output <- data.frame(
      item = rownames(tdm[-1,]),
      output
    )
  }
  if (row_partsize == "last"){
    output <- data.frame(
      item = rownames(tdm[-nrow(tdm),]),
      output
    )
  }
  rownames(output) <- 1:nrow(output)

  
  if (print_scores != FALSE) print(output)
  
  if (verbose) {
    log_buffer <- character()
    
    logmsg <- function(x) {
      log_buffer <<- c(log_buffer, x)
    }
    if (freq_adjust == TRUE){
      logmsg("\nDispersion scores are adjusted for frequency using the min-max")
      logmsg("  transformation (see Gries 2024: 196-208); please note that the")
      logmsg("  method implemented here does not work well if corpus parts differ")
      logmsg("  considerably in size; see vignette('frequency-adjustment')")
      
      if (unit_interval){
        logmsg(paste0(
          "\nFrequency-adjusted scores that exceed the limits of the unit interval [0,1]"
        ))
        logmsg("  were replaced by 0 or 1; number of items affected, by measure:")
        logmsg(paste0("  - Rrel: ", n_items_exceeding_limits_measure[1]))
        logmsg(paste0("  - D   : ", n_items_exceeding_limits_measure[2]))
        logmsg(paste0("  - D2  : ", n_items_exceeding_limits_measure[3]))
        logmsg(paste0("  - S   : ", n_items_exceeding_limits_measure[4]))
        logmsg(paste0("  - DP  : ", n_items_exceeding_limits_measure[5]))
        logmsg(paste0("  - DA  : ", n_items_exceeding_limits_measure[6]))
        logmsg(paste0("  - DKL : ", n_items_exceeding_limits_measure[7]))
      }
    }
    
    if (directionality == "gries") {
      logmsg("\nScores follow scaling used by Gries (2008):")
      logmsg("  0 = maximally even/dispersed/balanced distribution (optimum)")
      logmsg("  1 = maximally uneven/bursty/concentrated distribution (pessimum)")
    } else {
      logmsg("\nScores follow conventional scaling:")
      logmsg("  0 = maximally uneven/bursty/concentrated distribution (pessimum)")
      logmsg("  1 = maximally even/dispersed/balanced distribution (optimum)")
    }
    
    logmsg("\nFor Gries's DP, the function uses the modified version suggested by")
    logmsg("  Egbert et al. (2020)")
    
    logmsg("\nDA is calculated using the computational shortcut suggested by")
    logmsg("  Wilcox (1973: 343, 'MDA', column 4)")  
    
    logmsg("\nFor DKL, standardization to the unit interval [0,1] is based on the")
    logmsg("  odds-to-probability transformation, see Gries (2024: 90)")
  
    cat(paste(log_buffer, collapse = "\n"))
  }
  
  
  if (row_partsize == "first" & suppress_warning != TRUE){
    if (sum(rowSums(tdm[-1,]) == 0) > 0){
      warning("\n  For some item(s), all subfrequencies are 0; returning NA in this case\n\n")
    }  
    if (sum(rowSums(is.na(tdm[-1,])) > 0) > 0){
      warning("\n  For some item(s), the subfrequency is NA; treating this as 0\n\n")
    }  
  }
  if (row_partsize == "last" & suppress_warning != TRUE){
    if (sum(rowSums(tdm[-nrow(tdm),]) == 0) > 0){
      warning("\n  For some item(s), all subfrequencies are 0; returning NA in this case\n\n")
    }
    if (sum(rowSums(is.na(tdm[-nrow(tdm),])) > 0) > 0){
      warning("\n  For some item(s), the subfrequency is NA; treating this as 0\n\n")
    }  
  }
  if (sum(rowSums(tdm) == 1) > 0 & freq_adjust == TRUE & suppress_warning  != TRUE){
    warning("\n  For some item(s), the corpus frequency is 1; no frequency adjustment\n  made in this case; function returns unadjusted dispersion score")
  }
  invisible(output)
}

