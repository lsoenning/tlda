#' Calculate the dispersion measure \eqn{D_{KL}}
#'
#' @description
#' This function calculates the dispersion measure \eqn{D_{KL}}, which is based on the Kullback-Leibler divergence (Gries 2020, 2021, 2024). It offers three different options for standardization to the unit interval \[0,1\] (see Gries 2024: 90-92) and allows the user to choose the directionality of scaling, i.e. whether higher values denote a more even or a less even distribution. It also offers the option of calculating frequency-adjusted dispersion scores.
#'
#' @inheritParams disp
#' @param standardization Character string indicating which standardization strategy to use. See details below. Possible values are \code{'o2p'} (default), \code{'base_e'}, and \code{'base_2'}
#'
#' @author Lukas Soenning
#' 
#' @details The function calculates the dispersion measure \eqn{D_{KL}} based on a set of subfrequencies (number of occurrences of the item in each corpus part) and a matching set of part sizes (the size of the corpus parts, i.e. number of word tokens).
#' 
#' - Directionality: \eqn{D_{KL}} ranges from 0 to 1. The \code{conventional} scaling of dispersion measures (see Juilland & Chang-Rodriguez 1964; Carroll 1970; Rosengren 1971) assigns higher values to more even/dispersed/balanced distributions of subfrequencies across corpus parts. Gries (2008) uses the reverse scaling, with higher values denoting a more uneven/bursty/concentrated distribution; this is implemented by the value \code{gries}.
#' 
#' - Standardization: Irrespective of the directionality of scaling, three ways of standardizing the Kullback-Leibler divergence to the unit interval \[0;1\] are mentioned in Gries (2024: 90-92). The choice between these transformations can have an appreciable effect on the standardized dispersion score. In Gries (2020: 103-104), the Kullback-Leibler divergence is not standardized. In Gries (2021: 20), the transformation \code{'base_e'} is used (see (1) below), and in Gries (2024), the default strategy is \code{'o2p'}, the odds-to-probability transformation (see (3) below). 
#' 
#' - Frequency adjustment: Dispersion scores can be adjusted for frequency using a variant of the min-max transformation proposed by Gries (2022, 2024). The frequency-adjusted score for a specific item considers its dispersion potential, the lowest and highest possible score it can obtain given its overall corpus frequency as well as the number and size of corpus parts. The unadjusted score is then expressed relative to these endpoints, where the dispersion pessimum is set to 0, and the dispersion optimum to 1 (expressed in terms of conventional scaling). The frequency-adjusted dispersion score falls between these bounds and expresses how close the observed distribution is to the theoretical maximum and minimum. This adjustment therefore requires a maximally and a minimally dispersed distribution of the item across the parts. These hypothetical extremes can be built in different ways. In particular, different strategies exist for finding a distribution that yields the dispersion maximum. The method used by Gries (2024) uses a computationally expensive procedure that finds the distribution that produces the highest value on the dispersion measure of interest. The current function constructs extreme distributions independently of the dispersion measures used and therefore only proved approximations to the upper bound yielded by Gries's method. 
#' 
#'    - To obtain the lowest possible level of dispersion, the occurrences are allocated to the smallest corpus parts. The function starts by filling in the smallest part. If the total number of occurrences of the items is greater than the size of this part, it then continues with the second smallest corpus part and so on. This approach is very similar to that used in Gries (2024).
#'    - To obtain the highest possible level of dispersion, two methods are available, and these can be set with the argument \code{freq_adjust_method}. The choice between these methods is particularly relevant if corpus parts differ considerably in size. See documentation for \code{find_max_disp()}.
#'    
#' In the formulas given below, the following notation is used:
#' 
#' - \eqn{t_i} a proportional quantity; the subfrequency in part \eqn{i} divided by the total number of occurrences of the item in the corpus (i.e. the sum of all subfrequencies)
#' - \eqn{w_i} a proportional quantity; the size of corpus part \eqn{i} divided by the size of the corpus (i.e. the sum of a part sizes) 
#' 
#' The first step is to calculate the Kullback-Leibler divergence based on the proportional subfrequencies (\eqn{t_i}) and the size of the corpus parts (\eqn{w_i}):
#' 
#' &emsp; \eqn{KLD = \sum_i^k t_i \log_2{\frac{t_i}{w_i}}} &emsp; with \eqn{\log_2(0) = 0}
#' 
#' This KLD score is then standardized (i.e. transformed) to the conventional unit interval \[0,1\]. Three options are discussed in Gries (2024: 90-92). The following formulas represents "gries scaling" (0 = even, 1 = uneven):
#' 
#' &emsp; (1) &emsp; \eqn{e^{-KLD}} &emsp; (Gries 2021: 20), represented by the value \code{'base_e'}
#'
#' &emsp; (2) &emsp; \eqn{2^{-KLD}} &emsp; (Gries 2024: 90), represented by the value \code{'base_2'}
#'
#' &emsp; (3) &emsp; \eqn{\frac{KLD}{1+KLD}} &emsp; (Gries 2024: 90), represented by the value \code{'o2p'} (default)
#' 
#'
#' @return A numeric value
#' 
#' @references 
#' 
#' - Carroll, John B. 1970. An alternative to Juilland’s usage coefficient for lexical frequencies and a proposal for a standard frequency index. \emph{Computer Studies in the Humanities and Verbal Behaviour} 3(2). 61–65. \url{https://doi.org/10.1002/j.2333-8504.1970.tb00778.x}
#' 
#' - Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. \emph{International Journal of Corpus Linguistics} 13(4). 403–437. \url{https://doi.org/10.1075/ijcl.13.4.02gri}
#' 
#' - Gries, Stefan Th. 2024. \emph{Frequency, dispersion, association, and keyness: Revising and tupleizing corpus-linguistic measures}. Amsterdam: Benjamins.
#' 
#' - Juilland, Alphonse G. & Eugenio Chang-Rodríguez. 1964. \emph{Frequency dictionary of Spanish words.} The Hague: Mouton de Gruyter. \url{https://doi.org/10.1515/9783112415467}
#' 
#' - Rosengren, Inger. 1971. The quantitative concept of language and its relation to the structure of frequency dictionaries. \emph{Études de linguistique appliquée (Nouvelle Série)} 1. 103–127.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # not run
#' disp_DKL(
#'   subfreq = c(0,0,1,2,5), 
#'   partsize = rep(1000, 5),
#'   standardization = "base_e",
#'   directionality = "conventional")
#' }
#' 
disp_DKL <- function(subfreq,
                    partsize,
                    directionality = "conventional",
                    standardization = "o2p",
                    freq_adjust = FALSE,
                    freq_adjust_method = "pervasive",
                    verbose = TRUE,
                    print_score = TRUE) {
  
  calculate_DKL <- function(subfreq, partsize){
    w_i <- partsize / sum(partsize)
    t_i <- subfreq / sum(subfreq)
    
    KLD <- sum(t_i * ifelse(t_i == 0, 0, log2(t_i / w_i)))
    
    if (standardization == "base_e") {
      DKL <- exp(-KLD)
    } else if (standardization == "base_2") {
      DKL <- 2^(-KLD)
    } else {
      DKL <- 1 - (KLD / (1 + KLD))
    }    
  }
  
  if (sum(subfreq) == 0){
    output <- NA
    
  } else {
    
    DKL_score <- calculate_DKL(subfreq, partsize)
    output <- DKL_score
    
    if (directionality == "gries") DKL_score <- 1 - DKL_score
    
    if (freq_adjust == TRUE){
      
      subfreq_min_disp <- find_min_disp(
        subfreq, 
        partsize)
      
      subfreq_max_disp <- find_max_disp(
        subfreq, 
        partsize)
      
      DKL_min <- calculate_DKL(subfreq_min_disp, partsize)
      DKL_max <- calculate_DKL(subfreq_max_disp, partsize)
      
      if (directionality == "gries") {
        DKL_min <- 1 - DKL_min
        DKL_max <- 1 - DKL_max
      } 
      
      # frequency-adjusted version
      output <- (DKL_score - DKL_min) / (DKL_max - DKL_min)
    }
  }
  
  if (freq_adjust == TRUE){
    names(output) <- "DKL_nofreq"
  } else {
    names(output) <- "DKL"
  }
  
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
    
    if (standardization == "base_e") {
      message("\nStandardization to the unit interval [0,1] using base e,")
      message("  see Gries (2021: 20)")
    } else if (standardization == "base_2") {
      message("\nStandardization to the unit interval [0,1] using base 2,")
      message("  see Gries (2024: 90)")
    } else {
      message("\nStandardization to the unit interval [0,1] using the odds-to-probability")
      message("  transformation, see Gries (2024: 90)")
    }
  }
  invisible(output)
}


#' Calculate the dispersion measure \eqn{D_{KL}} for a term-document matrix
#'
#' @description
#' This function calculates the dispersion measure \eqn{D_{KL}}, which is based on the Kullback-Leibler divergence (Gries 2020, 2021, 2024). It offers three different options for standardization to the unit interval \[0,1\] (see Gries 2024: 90-92) and allows the user to choose the directionality of scaling, i.e. whether higher values denote a more even or a less even distribution. It also offers the option of calculating frequency-adjusted dispersion scores.
#'
#' @inheritParams disp_tdm
#' @inheritParams disp_DKL
#'
#' @author Lukas Soenning
#' 
#' @details 
#' This function takes as input a term-document matrix and returns, for each item (i.e. each row) the dispersion measure \eqn{D_{KL}}. The rows in the matrix represent the items, and the columns the corpus parts. Importantly, the term-document matrix must include an additional row that records the size of the corpus parts. For a proper term-document matrix, which includes all items that appear in the corpus, this can be added as a column margin, which sums the frequencies in each column. If the matrix only includes a selection of items drawn from the corpus, this information cannot be derived from the matrix and must be provided as a separate row.
#' 
#' - Directionality: \eqn{D_{KL}} ranges from 0 to 1. The \code{conventional} scaling of dispersion measures (see Juilland & Chang-Rodriguez 1964; Carroll 1970; Rosengren 1971) assigns higher values to more even/dispersed/balanced distributions of subfrequencies across corpus parts. Gries (2008) uses the reverse scaling, with higher values denoting a more uneven/bursty/concentrated distribution; this is implemented by the value \code{gries}.
#' 
#' - Standardization: Irrespective of the directionality of scaling, three ways of standardizing the Kullback-Leibler divergence to the unit interval \[0;1\] are mentioned in Gries (2024: 90-92). The choice between these transformations can have an appreciable effect on the standardized dispersion score. In Gries (2020: 103-104), the Kullback-Leibler divergence is not standardized. In Gries (2021: 20), the transformation \code{'base_e'} is used (see (1) below), and in Gries (2024), the default strategy is \code{'o2p'}, the odds-to-probability transformation (see (3) below). 
#' 
#' - Frequency adjustment: Dispersion scores can be adjusted for frequency using a variant of the min-max transformation proposed by Gries (2022, 2024). The frequency-adjusted score for a specific item considers its dispersion potential, the lowest and highest possible score it can obtain given its overall corpus frequency as well as the number and size of corpus parts. The unadjusted score is then expressed relative to these endpoints, where the dispersion pessimum is set to 0, and the dispersion optimum to 1 (expressed in terms of conventional scaling). The frequency-adjusted dispersion score falls between these bounds and expresses how close the observed distribution is to the theoretical maximum and minimum. This adjustment therefore requires a maximally and a minimally dispersed distribution of the item across the parts. These hypothetical extremes can be built in different ways. In particular, different strategies exist for finding a distribution that yields the dispersion maximum. The method used by Gries (2024) uses a computationally expensive procedure that finds the distribution that produces the highest value on the dispersion measure of interest. The current function constructs extreme distributions independently of the dispersion measures used and therefore only proved approximations to the upper bound yielded by Gries's method. 
#' 
#'    - To obtain the lowest possible level of dispersion, the occurrences are allocated to the smallest corpus parts. The function starts by filling in the smallest part. If the total number of occurrences of the items is greater than the size of this part, it then continues with the second smallest corpus part and so on. This approach is very similar to that used in Gries (2024).
#'    - To obtain the highest possible level of dispersion, two methods are available, and these can be set with the argument \code{freq_adjust_method}. The choice between these methods is particularly relevant if corpus parts differ considerably in size. See documentation for \code{find_max_disp()}.
#'    
#' In the formulas given below, the following notation is used:
#' 
#' - \eqn{t_i} a proportional quantity; the subfrequency in part \eqn{i} divided by the total number of occurrences of the item in the corpus (i.e. the sum of all subfrequencies)
#' - \eqn{w_i} a proportional quantity; the size of corpus part \eqn{i} divided by the size of the corpus (i.e. the sum of a part sizes) 
#' 
#' The first step is to calculate the Kullback-Leibler divergence based on the proportional subfrequencies (\eqn{t_i}) and the size of the corpus parts (\eqn{w_i}):
#' 
#' &emsp; \eqn{KLD = \sum_i^k t_i \log_2{\frac{t_i}{w_i}}} &emsp; with \eqn{\log_2(0) = 0}
#' 
#' This KLD score is then standardized (i.e. transformed) to the conventional unit interval \[0,1\]. Three options are discussed in Gries (2024: 90-92). The following formulas represents "gries scaling" (0 = even, 1 = uneven):
#' 
#' &emsp; (1) &emsp; \eqn{e^{-KLD}} &emsp; (Gries 2021: 20), represented by the value \code{'base_e'}
#'
#' &emsp; (2) &emsp; \eqn{2^{-KLD}} &emsp; (Gries 2024: 90), represented by the value \code{'base_2'}
#'
#' &emsp; (3) &emsp; \eqn{\frac{KLD}{1+KLD}} &emsp; (Gries 2024: 90), represented by the value \code{'o2p'} (default)
#' 
#'
#' @return A numeric value
#' 
#' @references 
#' 
#' - Carroll, John B. 1970. An alternative to Juilland’s usage coefficient for lexical frequencies and a proposal for a standard frequency index. \emph{Computer Studies in the Humanities and Verbal Behaviour} 3(2). 61–65. \url{https://doi.org/10.1002/j.2333-8504.1970.tb00778.x}
#' 
#' - Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. \emph{International Journal of Corpus Linguistics} 13(4). 403–437. \url{https://doi.org/10.1075/ijcl.13.4.02gri}
#' 
#' - Gries, Stefan Th. 2024. \emph{Frequency, dispersion, association, and keyness: Revising and tupleizing corpus-linguistic measures}. Amsterdam: Benjamins.
#' 
#' - Juilland, Alphonse G. & Eugenio Chang-Rodríguez. 1964. \emph{Frequency dictionary of Spanish words.} The Hague: Mouton de Gruyter. \url{https://doi.org/10.1515/9783112415467}
#' 
#' - Rosengren, Inger. 1971. The quantitative concept of language and its relation to the structure of frequency dictionaries. \emph{Études de linguistique appliquée (Nouvelle Série)} 1. 103–127.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # not run
#' disp_DKL_tdm(
#'   tdm = biber150_spokenBNC2014[1:20,]
#'   row_partsize = "first_row",
#'   standardization = "base_e",
#'   directionality = "conventional")
#' }
#' 
disp_DKL_tdm <- function(tdm,
                         row_partsize = "first_row",
                     directionality = "conventional",
                     standardization = "o2p",
                     freq_adjust = FALSE,
                     freq_adjust_method = "pervasive",
                     verbose = TRUE,
                     print_score = TRUE) {
  
  if (row_partsize == "first_row"){
    
    DKL_score <- apply(
      tdm[-1,],
      1,
      function(x){
        disp_DKL(subfreq = x, 
                partsize = tdm[1,],
                verbose = FALSE,
                print_score = FALSE)
      })
    
  } else if (row_partsize == "last_row"){
    
    DKL_score <- apply(
      tdm[-nrow(tdm),],
      1,
      function(x){
        disp_DKL(subfreq = x, 
                partsize = tdm[nrow(tdm),],
                verbose = FALSE,
                print_score = FALSE)
      })
  }
  
  if (freq_adjust == TRUE){
    
    min_disp_tdm <- find_min_disp_tdm(tdm)
    max_disp_tdm <- find_max_disp_tdm(tdm)
    
    if (row_partsize == "first_row"){
      
      DKL_min <- apply(
        min_disp_tdm[-1,],
        1,
        function(x){
          disp_DKL(subfreq = x, 
                  partsize = min_disp_tdm[1,],
                  verbose = FALSE,
                  print_score = FALSE)
        })
      
      DKL_max <- apply(
        max_disp_tdm[-1,],
        1,
        function(x){
          disp_DKL(subfreq = x, 
                  partsize = max_disp_tdm[1,],
                  verbose = FALSE,
                  print_score = FALSE)
        })
      
    } else if (row_partsize == "last_row"){
      
      DKL_min <- apply(
        min_disp_tdm[-nrow(min_disp_tdm),],
        1,
        function(x){
          disp_DKL(subfreq = x, 
                  partsize = min_disp_tdm[nrow(min_disp_tdm),],
                  verbose = FALSE,
                  print_score = FALSE)
        })
      
      DKL_max <- apply(
        max_disp_tdm[-nrow(max_disp_tdm),],
        1,
        function(x){
          disp_DKL(sbfreq = x, 
                  partsize = max_disp_tdm[nrow(max_disp_tdm),],
                  verbose = FALSE,
                  print_score = FALSE)
        })
    }
    output <- (DKL_score - DKL_min) / (DKL_max - DKL_min)
    
  } else {
    output <- DKL_score
  }
  
  output <- t(output)

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
    
    if (standardization == "base_e") {
      message("\nStandardization to the unit interval [0,1] using base e,")
      message("  see Gries (2021: 20)")
    } else if (standardization == "base_2") {
      message("\nStandardization to the unit interval [0,1] using base 2,")
      message("  see Gries (2024: 90)")
    } else {
      message("\nStandardization to the unit interval [0,1] using the odds-to-probability")
      message("  transformation, see Gries (2024: 90)")
    }
  }
  invisible(output)
}

