#' Calculate parts-based dispersion measures
#'
#' @description
#' This function calculates a number of parts-based dispersion measures and allows the user to choose the directionality of scaling, i.e. whether higher values denote a more even or a less even distribution. It also offers the option of calculating frequency-adjusted dispersion scores.
#' 
#' @param subfreq A numeric vector of subfrequencies, i.e. the number of occurrences of the item in each corpus part
#' @param partsize A numeric vector with the size of the corpus parts
#' @param directionality Character string indicating the directionality of scaling. See details below. Possible values are \code{'conventional'} (default) and \code{'gries'}
#' @param freq_adjust Logical. Whether dispersion score should be adjusted for frequency (i.e. whether frequency should be 'partialed out'); default is \code{FALSE}
#' @param freq_adjust_method Character string indicating which method to used for devising a 'maximally dispersed' distribution of the item, which is necessary for frequency adjustment. See details below. Possible values are \code{'pervasive'} (default) and \code{'pervasive_even'}
#' @param digits Rounding: Integer value specifying the number of decimal places to retain (default: no rounding)
#' @param verbose Logical. Whether additional information (directionality, formulas, frequency adjustment) should be printed; default is \code{TRUE}
#' @param print_score Logical. Whether the dispersion score should be printed to the console; default is \code{TRUE}
#'
#' @author Lukas Soenning
#' 
#' @details This function calculates dispersion measures based on two vectors: a set of subfrequencies (number of occurrences of the item in each corpus part) and a matching set of part sizes (the size of the corpus parts, i.e. number of word tokens). 
#' 
#' - Directionality: The scores for all measures range from 0 to 1. The \code{conventional} scaling of dispersion measures (see Juilland & Chang-Rodriguez 1964; Carroll 1970; Rosengren 1971) assigns higher values to more even/dispersed/balanced distributions of subfrequencies across corpus parts. Gries (2008) uses the reverse scaling, with higher values denoting a more uneven/bursty/concentrated distribution; this is implemented by the value \code{gries}.
#' 
#' - Frequency adjustment: Dispersion scores can be adjusted for frequency using a variant of the min-max transformation proposed by Gries (2022, 2024). The frequency-adjusted score for a specific item considers its dispersion potential, the lowest and highest possible score it can obtain given its overall corpus frequency as well as the number and size of corpus parts. The unadjusted score is then expressed relative to these endpoints, where the dispersion pessimum is set to 0, and the dispersion optimum to 1 (expressed in terms of conventional scaling). The frequency-adjusted dispersion score falls between these bounds and expresses how close the observed distribution is to the theoretical maximum and minimum. This adjustment therefore requires a maximally and a minimally dispersed distribution of the item across the parts. These hypothetical extremes can be built in different ways. In particular, different strategies exist for finding a distribution that yields the dispersion maximum. The method used by Gries (2024) uses a computationally expensive procedure that finds the distribution that produces the highest value on the dispersion measure of interest. The current function constructs extreme distributions independently of the dispersion measures used and therefore only proved approximations to the upper bound yielded by Gries's method. 
#' 
#'    - To obtain the lowest possible level of dispersion, the occurrences are allocated to the smallest corpus parts. The function starts by filling in the smallest part. If the total number of occurrences of the items is greater than the size of this part, it then continues with the second smallest corpus part and so on. This approach is very similar to that used in Gries (2024).
#'    - To obtain the highest possible level of dispersion, two methods are available, and these can be set with the argument \code{freq_adjust_method}. The choice between these methods is particularly relevant if corpus parts differ considerably in size. See documentation for \code{find_max_disp()}.
#' 
#' The following measures are computed, listed in chronological order (see details below):
#' 
#' - \eqn{R_{rel}} &ensp; (Keniston 1920)
#' - \eqn{D} &ensp; (Juilland & Chang-Rodriguez 1964)
#' - \eqn{D_2} &ensp; (Carroll 1970)
#' - \eqn{S} &ensp; (Rosengren 1971)
#' - \eqn{D_P} &ensp; (Gries 2008; modification: Egbert et al. 2020)
#' - \eqn{D_A} &ensp; (Burch et al. 2017)
#' - \eqn{D_{KL}} &ensp; (Gries 2024)
#' 
#' In the formulas given below, the following notation is used:
#' 
#' - \eqn{k} &ensp; the number of corpus parts
#' - \eqn{T_i} &ensp; the absolute subfrequency in part \eqn{i}
#' - \eqn{t_i} &ensp; a proportional quantity; the subfrequency in part \eqn{i} divided by the total number of occurrences of the item in the corpus (i.e. the sum of all subfrequencies)
#' - \eqn{W_i} &ensp; the absolute size of corpus part \eqn{i}
#' - \eqn{w_i} &ensp; a proportional quantity; the size of corpus part \eqn{i} divided by the size of the corpus (i.e. the sum of a part sizes) 
#' - \eqn{R_i} &ensp; the normalized subfrequency in part \eqn{i}, i.e. the subfrequency divided by the size of the corpus part
#' - \eqn{r_i} &ensp; a proportional quantity; the normalized subfrequency in part \eqn{i} divided by the sum of all normalized subfrequencies
#' 
#' Note that the formulas cited below differ in their scaling, i.e. whether 1 reflects an even or an uneven distribution. In the current function, this behavior is overridden by the argument \code{directionality}. The specific scaling used in the formulas below is therefore irrelevant.
#' 
#' \eqn{R_{rel}} refers to the relative range, i.e. the proportion of corpus parts containing at least one occurrence of the item
#' \eqn{D} denotes Juilland's D and is calculated as follows (this formula uses conventional scaling); \eqn{\bar{R_i}} denotes the average over the normalized subfrequencies:
#' 
#' &emsp; \eqn{1 - \sqrt{\frac{\sum_{i = 1}^k (R_i - \bar{R_i})^2}{k}} \times \frac{1}{\bar{R_i} \sqrt{k - 1}}}
#' 
#' \eqn{D_2} denotes the index proposed by Carroll (1970); the following formula uses conventional scaling:
#' 
#' &emsp; \eqn{\frac{\sum_i^k r_i \log_2{\frac{1}{r_i}}}{\log_2{k}}}
#' 
#' \eqn{S} is the dispersion measure proposed by Rosengren (1971); the formula uses conventional scaling:
#' 
#' &emsp; \eqn{\frac{(\sum_i^k r_i \sqrt{w_i T_i}}{N}}
#' 
#' \eqn{D_P} represents Gries's deviation of proportions; the following formula is the modified version suggested by Egbert et al. (2020: 99); it implements conventional scaling (0 = uneven, 1 = even) and the notation \eqn{min\{w_i: t_i > 0\}} refers to the \eqn{w_i} value among those corpus parts that include at least one occurrence of the item.
#' 
#' &emsp; \eqn{1 - \frac{\sum_i^k |t_i - w_i|}{2} \times \frac{1}{1 - min\{w_i: t_i > 0\}}}
#' 
#' \eqn{D_A} refers is a measure introduced into dispersion analysis by Burch et al. (2017). The following formula is the one used by Egbert et al. (2020: 98); it relies on normalized frequencies and therefore works with corpus parts of different size. The formula represents conventional scaling (0 = uneven, 1 = even):
#' 
#' &emsp; \eqn{1 - \frac{\sum_{i = 1}^{k-1} \sum_{j = i+1}^{k} |R_i - R_j|}{\frac{k(k-1)}{2}} \times \frac{1}{2\frac{\sum_i^k R_i}{k}}} (Egbert et al. 2020: 98)
#' 
#' The current function uses a different version of the same formula, which relies on the proportional \eqn{r_i} values instead of the normalized subfrequencies \eqn{R_i}. This version yields the identical result: 
#' 
#' &emsp; \eqn{1 - \frac{\sum_{i = 1}^{k-1} \sum_{j = i+1}^{k} |r_i - r_j|}{k-1}}
#' 
#' \eqn{D_{KL}} denotes a measure proposed by Gries (2020, 2021); for standardization, it uses the odds-to-probability transformation (Gries 2024: 90) and represents "Gries scaling" (0 = even, 1 = uneven):
#' 
#' &emsp; \eqn{\frac{\sum_i^k t_i \log_2{\frac{t_i}{w_i}}}{1 + \sum_i^k t_i \log_2{\frac{t_i}{w_i}}}}
#' 
#' 
#' 
#' @return Returns a numeric vector with seven dispersion scores, one for each measure (in the order listed above).
#' 
#' @references 
#' 
#' - Burch, Brent, Jesse Egbert & Douglas Biber. 2017. Measuring and interpreting lexical dispersion in corpus linguistics. \emph{Journal of Research Design and Statistics in Linguistics and Communication Science} 3(2). 189–216. \url{https://doi.org/10.1558/jrds.33066}
#' 
#' - Carroll, John B. 1970. An alternative to Juilland’s usage coefficient for lexical frequencies and a proposal for a standard frequency index. \emph{Computer Studies in the Humanities and Verbal Behaviour} 3(2). 61–65. \url{https://doi.org/10.1002/j.2333-8504.1970.tb00778.x}
#' 
#' - Egbert, Jesse, Brent Burch & Douglas Biber. 2020. Lexical dispersion and corpus design. \emph{International Journal of Corpus Linguistics} 25(1). 89–115. \url{https://doi.org/10.1075/ijcl.18010.egb}
#' 
#' - Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. \emph{International Journal of Corpus Linguistics} 13(4). 403–437. \url{https://doi.org/10.1075/ijcl.13.4.02gri}
#' 
#' - Gries, Stefan Th. 2020. Analyzing dispersion. In Magali Paquot & Stefan Th. Gries (eds.), \emph{A practical handbook of corpus linguistics}, 99–118. New York: Springer. \url{https://doi.org/10.1007/978-3-030-46216-1_5}
#' 
#' - Gries, Stefan Th. 2021. A new approach to (key) keywords analysis: Using frequency, and now also dispersion. \emph{Research in Corpus Linguistics} 9(2). 1−33. \url{https://doi.org/10.32714/ricl.09.02.02}
#' 
#' - Gries, Stefan Th. 2024. \emph{Frequency, dispersion, association, and keyness: Revising and tupleizing corpus-linguistic measures}. Amsterdam: Benjamins.
#' 
#' - Juilland, Alphonse G. & Eugenio Chang-Rodríguez. 1964. \emph{Frequency dictionary of Spanish words.} The Hague: Mouton de Gruyter. \url{https://doi.org/10.1515/9783112415467}
#' 
#' - Keniston, Hayward. 1920. Common words in Spanish. \emph{Hispania} 3(2). 85–96. \url{https://doi.org/10.2307/331305}
#' 
#' - Lijffijt, Jefrey & Stefan Th. Gries. 2012. Correction to Stefan Th. Gries’ ‘Dispersions and adjusted frequencies in corpora’. \emph{International Journal of Corpus Linguistics} 17(1). 147–149. \url{https://doi.org/10.1075/ijcl.17.1.08lij}
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
#'   freq_adjust = FALSE)
#' }
#'
disp <- function(subfreq,
                 partsize,
                 directionality = "conventional",
                 freq_adjust = FALSE,
                 freq_adjust_method = "pervasive",
                 digits = NULL,
                 verbose = TRUE,
                 print_score = TRUE) {
  
  calculate_dispersion <- function(subfreq,
                                   partsize){
    W_i <- partsize
    w_i <- W_i / sum(W_i)
    T_i <- subfreq
    t_i <- T_i / sum(T_i)
    R_i <- T_i / W_i
    r_i <- R_i / sum(R_i)
    k <- length(T_i)
    
    dist_r <- as.matrix(stats::dist(r_i, method = "manhattan"))
    
    Rrel <- sum(T_i != 0) / length(T_i)
    
    D    <- 1 - (sqrt(sum((R_i - mean(R_i))^2) / k) / (mean(R_i) * sqrt(k - 1)))
    
    D2   <- (log(sum(R_i)) - sum(ifelse(R_i == 0, 0, R_i * log(R_i))) / sum(R_i)) / log(k)
    
    S    <- (sum(sqrt(w_i * T_i))^2) / sum(T_i)
    
    DP   <- 1 - ((sum(abs(t_i - w_i)) * (sum(W_i) / (sum(W_i) - min(W_i[T_i > 0], fill = 0)))) / 2)
    
    DA   <- 1 - (mean(dist_r[lower.tri(dist_r)]) / (2 / k))
    
    KLD  <- sum(t_i * ifelse(t_i == 0, 0, log2(t_i / w_i)))
    DKL  <- 1 - (KLD / (1 + KLD))
    
    output <- c(Rrel, D, D2, S, DP, DA, DKL)
    names(output) <- c("Rrel", "D", "D2", "S", "DP", "DA", "DKL")
    
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
        partsize)
      
      subfreq_max_disp <- find_max_disp(
        subfreq, 
        partsize)
      
      disp_min <- calculate_dispersion(subfreq_min_disp, partsize)
      disp_max <- calculate_dispersion(subfreq_max_disp, partsize)
      
      if (directionality == "gries") {
        disp_min <- 1 - disp_min
        disp_max <- 1 - disp_max
      } 
      
      # frequency-adjusted version
      output <- (disp_score - disp_min) / (disp_max - disp_min)
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
    message("\nFor Gries's DP, the function uses the modified version suggested by")
    message("  Egbert et al. (2020)")
    message("\nFor DKL, standardization to the unit interval [0,1] is based on the")
    message("  odds-to-probability transformation, see Gries (2024: 90)")
  }
  invisible(output)
}


#' Calculate parts-based dispersion measures for a term-document matrix
#'
#' @description
#' This function calculates a number of parts-based dispersion measures and allows the user to choose the directionality of scaling, i.e. whether higher values denote a more even or a less even distribution. It also offers the option of calculating frequency-adjusted dispersion scores.
#'
#' @inheritParams disp
#' @param tdm A term-document matrix, where rows represent items and columns corpus parts; must also contain a row giving the size of the corpus parts (first or last row in the term-document matrix)
#' @param row_partsize Character string indicating which row in the term-document matrix contains the size of the corpus parts. Possible values are \code{'first_row'}, \code{'last_row'} (default)
#' 
#' @author Lukas Soenning
#' 
#' @details This function takes as input a term-document matrix and returns, for each item (i.e. each row) a variety of dispersion measures. The rows in the matrix represent the items, and the columns the corpus parts. Importantly, the term-document matrix must include an additional row that records the size of the corpus parts. For a proper term-document matrix, which includes all items that appear in the corpus, this can be added as a column margin, which sums the frequencies in each column. If the matrix only includes a selection of items drawn from the corpus, this information cannot be derived from the matrix and must be provided as a separate row.
#' 
#' - Directionality: The scores for all measures range from 0 to 1. The \code{conventional} scaling of dispersion measures (see Juilland & Chang-Rodriguez 1964; Carroll 1970; Rosengren 1971) assigns higher values to more even/dispersed/balanced distributions of subfrequencies across corpus parts. Gries (2008) uses the reverse scaling, with higher values denoting a more uneven/bursty/concentrated distribution; this is implemented by the value \code{gries}.
#' 
#' - Frequency adjustment: Dispersion scores can be adjusted for frequency using a variant of the min-max transformation proposed by Gries (2022, 2024). The frequency-adjusted score for a specific item considers its dispersion potential - this is the lowest and highest possible score it can obtain given its overall corpus frequency as well as the number and size of corpus parts. It is then expressed relative to these endpoints, where the dispersion pessimum is set to 0, and the dispersion optimum to 1 (expressed in terms of conventional scaling). The frequency-adjusted dispersion score falls between these bounds and expresses how close the observed distribution is to the theoretical maximum and minimum. This adjustment therefore requires maximally and a minimally dispersed distribution of the item across the parts. These are determined as follows:
#' 
#'    - To obtain the highest possible level of concentration (i.e. minimum dispersion), the occurrences are allocated to the smallest corpus parts. The function starts by filling in the smallest corpus part. If the total number of occurrences of the items is greater than the size of the smallest corpus part, it then continues with the second smallest part and so on. 
#'    - To obtain the highest possible level of dispersion, two methods are available, and these can be set with the argument \code{freq_adjust_method}. The choice between these methods is particularly relevant if corpus parts differ considerably in size. See documentation for \code{find_max_disp()}.
#'    
#' The following measures are computed, listed in chronological order (see details below):
#' 
#' - \eqn{R_{rel}} &ensp; (Keniston 1920)
#' - \eqn{D} &ensp; (Juilland & Chang-Rodriguez 1964)
#' - \eqn{D_2} &ensp; (Carroll 1970)
#' - \eqn{S} &ensp; (Rosengren 1971)
#' - \eqn{D_P} &ensp; (Gries 2008; modification: Egbert et al. 2020)
#' - \eqn{D_A} &ensp; (Burch et al. 2017)
#' - \eqn{D_{KL}} &ensp; (Gries 2024)
#' 
#' In the formulas given below, the following notation is used:
#' 
#' - \eqn{k} &ensp; the number of corpus parts
#' - \eqn{T_i} &ensp; the absolute subfrequency in part \eqn{i}
#' - \eqn{t_i} &ensp; a proportional quantity; the subfrequency in part \eqn{i} divided by the total number of occurrences of the item in the corpus (i.e. the sum of all subfrequencies)
#' - \eqn{W_i} &ensp; the absolute size of corpus part \eqn{i}
#' - \eqn{w_i} &ensp; a proportional quantity; the size of corpus part \eqn{i} divided by the size of the corpus (i.e. the sum of a part sizes) 
#' - \eqn{R_i} &ensp; the normalized subfrequency in part \eqn{i}, i.e. the subfrequency divided by the size of the corpus part
#' - \eqn{r_i} &ensp; a proportional quantity; the normalized subfrequency in part \eqn{i} divided by the sum of all normalized subfrequencies
#' 
#' Note that the formulas cited below differ in their scaling, i.e. whether 1 reflects an even or an uneven distribution. In the current function, this behavior is overridden by the argument \code{directionality}. The specific scaling used in the formulas below is therefore irrelevant.
#' 
#' \eqn{R_{rel}} refers to the relative range, i.e. the proportion of corpus parts containing at least one occurrence of the item
#' \eqn{D} denotes Juilland's D and is calculated as follows (this formula uses conventional scaling); \eqn{\bar{R_i}} denotes the average over the normalized subfrequencies:
#' 
#' &emsp; \eqn{1 - \sqrt{\frac{\sum_{i = 1}^k (R_i - \bar{R_i})^2}{k}} \times \frac{1}{\bar{R_i} \sqrt{k - 1}}}
#' 
#' \eqn{D_2} denotes the index proposed by Carroll (1970); the following formula uses conventional scaling:
#' 
#' &emsp; \eqn{\frac{\sum_i^k r_i \log_2{\frac{1}{r_i}}}{\log_2{k}}}
#' 
#' \eqn{S} is the dispersion measure proposed by Rosengren (1971); the formula uses conventional scaling:
#' 
#' &emsp; \eqn{\frac{(\sum_i^k r_i \sqrt{w_i T_i}}{N}}
#' 
#' \eqn{D_P} represents Gries's deviation of proportions; the following formula is the modified version suggested by Egbert et al. (2020: 99); it implements conventional scaling (0 = uneven, 1 = even) and the notation \eqn{min\{w_i: t_i > 0\}} refers to the \eqn{w_i} value among those corpus parts that include at least one occurrence of the item.
#' 
#' &emsp; \eqn{1 - \frac{\sum_i^k |t_i - w_i|}{2} \times \frac{1}{1 - min\{w_i: t_i > 0\}}}
#' 
#' \eqn{D_A} refers is a measure introduced into dispersion analysis by Burch et al. (2017). The following formula is the one used by Egbert et al. (2020: 98); it relies on normalized frequencies and therefore works with corpus parts of different size. The formula represents conventional scaling (0 = uneven, 1 = even):
#' 
#' &emsp; \eqn{1 - \frac{\sum_{i = 1}^{k-1} \sum_{j = i+1}^{k} |R_i - R_j|}{\frac{k(k-1)}{2}} \times \frac{1}{2\frac{\sum_i^k R_i}{k}}} (Egbert et al. 2020: 98)
#' 
#' The current function uses a different version of the same formula, which relies on the proportional \eqn{r_i} values instead of the normalized subfrequencies \eqn{R_i}. This version yields the identical result: 
#' 
#' &emsp; \eqn{1 - \frac{\sum_{i = 1}^{k-1} \sum_{j = i+1}^{k} |r_i - r_j|}{k-1}}
#' 
#' \eqn{D_{KL}} denotes a measure proposed by Gries (2020, 2021); for standardization, it uses the odds-to-probability transformation (Gries 2024: 90) and represents "Gries scaling" (0 = even, 1 = uneven):
#' 
#' &emsp; \eqn{\frac{\sum_i^k t_i \log_2{\frac{t_i}{w_i}}}{1 + \sum_i^k t_i \log_2{\frac{t_i}{w_i}}}}
#' 
#' 
#' @return A numeric matrix
#' 
#' @references 
#' 
#' - Burch, Brent, Jesse Egbert & Douglas Biber. 2017. Measuring and interpreting lexical dispersion in corpus linguistics. \emph{Journal of Research Design and Statistics in Linguistics and Communication Science} 3(2). 189–216. \url{https://doi.org/10.1558/jrds.33066}
#' 
#' - Carroll, John B. 1970. An alternative to Juilland’s usage coefficient for lexical frequencies and a proposal for a standard frequency index. \emph{Computer Studies in the Humanities and Verbal Behaviour} 3(2). 61–65. \url{https://doi.org/10.1002/j.2333-8504.1970.tb00778.x}
#' 
#' - Egbert, Jesse, Brent Burch & Douglas Biber. 2020. Lexical dispersion and corpus design. \emph{International Journal of Corpus Linguistics} 25(1). 89–115. \url{https://doi.org/10.1075/ijcl.18010.egb}
#' 
#' - Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. \emph{International Journal of Corpus Linguistics} 13(4). 403–437. \url{https://doi.org/10.1075/ijcl.13.4.02gri}
#' 
#' - Gries, Stefan Th. 2020. Analyzing dispersion. In Magali Paquot & Stefan Th. Gries (eds.), \emph{A practical handbook of corpus linguistics}, 99–118. New York: Springer. \url{https://doi.org/10.1007/978-3-030-46216-1_5}
#' 
#' - Gries, Stefan Th. 2021. A new approach to (key) keywords analysis: Using frequency, and now also dispersion. \emph{Research in Corpus Linguistics} 9(2). 1−33. \url{https://doi.org/10.32714/ricl.09.02.02}
#' 
#' - Gries, Stefan Th. 2024. \emph{Frequency, dispersion, association, and keyness: Revising and tupleizing corpus-linguistic measures}. Amsterdam: Benjamins.
#' 
#' - Juilland, Alphonse G. & Eugenio Chang-Rodríguez. 1964. \emph{Frequency dictionary of Spanish words.} The Hague: Mouton de Gruyter. \url{https://doi.org/10.1515/9783112415467}
#' 
#' - Keniston, Hayward. 1920. Common words in Spanish. \emph{Hispania} 3(2). 85–96. \url{https://doi.org/10.2307/331305}
#' 
#' - Lijffijt, Jefrey & Stefan Th. Gries. 2012. Correction to Stefan Th. Gries’ ‘Dispersions and adjusted frequencies in corpora’. \emph{International Journal of Corpus Linguistics} 17(1). 147–149. \url{https://doi.org/10.1075/ijcl.17.1.08lij}
#' 
#' - Rosengren, Inger. 1971. The quantitative concept of language and its relation to the structure of frequency dictionaries. \emph{Études de linguistique appliquée (Nouvelle Série)} 1. 103–127.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # not run
#' disp_tdm(
#'   tdm = biber150_spokenBNC2014[1:20,]
#'   row_partsize = "first_row",
#'   directionality = "conventional",
#'   freq_adjust = FALSE)
#' }
#'
disp_tdm <- function(tdm,
                     row_partsize = "first_row",
                     directionality = "conventional",
                     freq_adjust = FALSE,
                     freq_adjust_method = "pervasive",
                     digits = NULL,
                     verbose = TRUE,
                     print_score = TRUE) {
  
  if (row_partsize == "first_row"){
    
    disp_score <- apply(
      tdm[-1,],
      1,
      function(x){
        disp(subfreq = x, 
             partsize = tdm[1,],
             verbose = FALSE,
             print_score = FALSE)
      })
    
  } else if (row_partsize == "last_row"){
    
    disp_score <- apply(
      tdm[-nrow(tdm),],
      1,
      function(x){
        disp(x, tdm[nrow(tdm),],
             verbose = FALSE,
             print_score = FALSE)
      })
  }
  
  if (freq_adjust == TRUE){
    
    min_disp_tdm <- find_min_disp_tdm(tdm)
    max_disp_tdm <- find_max_disp_tdm(tdm)
    
    if (row_partsize == "first_row"){
      
      disp_min <- apply(
        min_disp_tdm[-1,],
        1,
        function(x){
          disp(x, min_disp_tdm[1,],
               verbose = FALSE,
               print_score = FALSE)
        })
      
      disp_max <- apply(
        max_disp_tdm[-1,],
        1,
        function(x){
          disp(x, max_disp_tdm[1,],
               verbose = FALSE,
               print_score = FALSE)
        })
      
    } else if (row_partsize == "last_row"){
      
      disp_min <- apply(
        min_disp_tdm[-nrow(min_disp_tdm),],
        1,
        function(x){
          disp(x, min_disp_tdm[nrow(min_disp_tdm),],
               verbose = FALSE,
               print_score = FALSE)
        })
      
      disp_max <- apply(
        max_disp_tdm[-nrow(max_disp_tdm),],
        1,
        function(x){
          disp(x, max_disp_tdm[nrow(max_disp_tdm),],
               verbose = FALSE,
               print_score = FALSE)
        })
    }
    output <- (disp_score - disp_min) / (disp_max - disp_min)

  } else {
    output <- disp_score
  }
  
  if (directionality == "gries") output <- 1 - output
  
  if (!is.null(digits)) output <- round(output, digits)

  output <- t(output)
  
  if (print_score == TRUE) print(output)
  
  if (verbose) {
    if (directionality == "gries") {
      message("\nScores follow scaling used by Gries (2008):")
      message("  0 = maximally even/dispersed/balanced distribution (optimum)")
      message("  1 = maximally uneven/bursty/concentrated distribution (pessimum)")
    } else {
      message("\nScores follow conventional scaling:")
      message("  0 = maximally uneven/bursty/concentrated distribution (pessimum)")
      message("  1 = maximally even/dispersed/balanced distribution (optimum)")
    }
    message("\nFor Gries's DP, the function uses the modified version suggested by")
    message("  Egbert et al. (2020)")
    message("\nFor DKL, standardization to the unit interval [0,1] is based on the")
    message("  odds-to-probability transformation, see Gries (2024: 90)")
  }
  invisible(output)
}

