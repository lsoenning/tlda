#' Calculate the dispersion measure \eqn{D_{KL}}
#'
#' @description
#' This function calculates the dispersion measure \eqn{D_{KL}}, which is based on the Kullback-Leibler divergence (Gries 2020, 2021, 2024). It offers three options for standardization to the unit interval \[0,1\] (see Gries 2024: 90-92) and allows the user to choose the directionality of scaling, i.e. whether higher values denote a more even or a less even distribution. It also offers the option of calculating frequency-adjusted dispersion scores.
#'
#' @inheritParams disp
#' @param standardization Character string indicating which standardization method to use. See details below. Possible values are `"o2p"` (default), `"base_e"`, and `"base_2"`.
#'
#' @author Lukas Soenning
#' 
#' @details The function calculates the dispersion measure \eqn{D_{KL}} based on a set of subfrequencies (number of occurrences of the item in each corpus part) and a matching set of part sizes (the size of the corpus parts, i.e. number of word tokens).
#' 
#' - Directionality: \eqn{D_{KL}} ranges from 0 to 1. The conventional scaling of dispersion measures (see Juilland & Chang-Rodriguez 1964; Carroll 1970; Rosengren 1971) assigns higher values to more even/dispersed/balanced distributions of subfrequencies across corpus parts. This is the default. Gries (2008) uses the reverse scaling, with higher values denoting a more uneven/bursty/concentrated distribution; use `directionality = "gries"` to choose this option.
#'
#' - Standardization: Irrespective of the directionality of scaling, three ways of standardizing the Kullback-Leibler divergence to the unit interval \[0;1\] are mentioned in Gries (2024: 90-92). The choice between these transformations can have an appreciable effect on the standardized dispersion score. In Gries (2020: 103-104), the Kullback-Leibler divergence is not standardized. In Gries (2021: 20), the transformation `"base_e"` is used (see (1) below), and in Gries (2024), the default strategy is `"o2p"`, the odds-to-probability transformation (see (3) below). 
#' 
#' - Frequency adjustment: Dispersion scores can be adjusted for frequency using the min-max transformation proposed by Gries (2022: 184-191; 2024: 196-208). The frequency-adjusted score for an  item considers the lowest and highest possible level of dispersion it can obtain given its overall corpus frequency as well as the number (and size) of corpus parts. The unadjusted score is then expressed relative to these endpoints, where the dispersion minimum is set to 0, and the dispersion maximum to 1 (expressed in terms of conventional scaling). The frequency-adjusted score falls between these bounds and expresses how close the observed distribution is to the theoretical maximum and minimum. This adjustment therefore requires a maximally and a minimally dispersed distribution of the item across the parts. These hypothetical extremes can be built in different ways. The method used by Gries (2022, 2024) uses a computationally expensive procedure that finds the distribution that produces the highest value on the dispersion measure of interest. The current function constructs extreme distributions in a different way, based on the distributional features pervasiveness (`"pervasive"`) or evenness (`"even"`). You can choose between these with the argument `freq_adjust_method`; the default is `even`. For details and explanations, see `vignette("frequency-adjustment")`. 
#' 
#'    - To obtain the lowest possible level of dispersion, the occurrences are either allocated to as few corpus parts as possible (`"pervasive"`), or they are assigned to the smallest corpus part(s) (`"even"`).
#'    - To obtain the highest possible level of dispersion, the occurrences are either spread as broadly across corpus parts as possible (`"pervasive"`), or they are allocated to corpus parts in proportion to their size (`"even"`). The choice between these methods is particularly relevant if corpus parts differ considerably in size. See documentation for `find_max_disp()` and `vignette("frequency-adjustment")`.
#'    
#' In the formulas given below, the following notation is used:
#' 
#' - \eqn{t_i} a proportional quantity; the subfrequency in part \eqn{i} divided by the total number of occurrences of the item in the corpus (i.e. the sum of all subfrequencies)
#' - \eqn{w_i} a proportional quantity; the size of corpus part \eqn{i} divided by the size of the corpus (i.e. the sum of the part sizes) 
#' 
#' The first step is to calculate the Kullback-Leibler divergence based on the proportional subfrequencies (\eqn{t_i}) and the size of the corpus parts (\eqn{w_i}):
#' 
#'    \eqn{KLD = \sum_i^k t_i \log_2{\frac{t_i}{w_i}}}    with \eqn{\log_2(0) = 0}
#' 
#' This KLD score is then standardized (i.e. transformed) to the conventional unit interval \[0,1\]. Three options are discussed in Gries (2024: 90-92). The following formulas represent Gries scaling (0 = even, 1 = uneven):
#' 
#'    (1) \eqn{e^{-KLD}} (Gries 2021: 20), represented by the value `"base_e"`
#'
#'    (2) \eqn{2^{-KLD}} (Gries 2024: 90), represented by the value" `"base_2"`
#'
#'    (3) \eqn{\frac{KLD}{1+KLD}} (Gries 2024: 90), represented by the value `"o2p"` (default)
#' 
#'
#' @returns A numeric value
#' 
#' @references 
#' 
#' Carroll, John B. 1970. An alternative to Juilland’s usage coefficient for lexical frequencies and a proposal for a standard frequency index. \emph{Computer Studies in the Humanities and Verbal Behaviour} 3(2). 61--65. \doi{doi:10.1002/j.2333-8504.1970.tb00778.x}
#' 
#' Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. \emph{International Journal of Corpus Linguistics} 13(4). 403--437. \doi{doi:10.1075/ijcl.13.4.02gri}
#' 
#' Gries, Stefan Th. 2022. What do (most of) our dispersion measures measure (most)? Dispersion? \emph{Journal of Second Language Studies} 5(2). 171--205. \doi{doi:10.1075/jsls.21029.gri}
#' 
#' Gries, Stefan Th. 2024. \emph{Frequency, dispersion, association, and keyness: Revising and tupleizing corpus-linguistic measures}. Amsterdam: Benjamins. \doi{doi:10.1075/scl.115}
#' 
#' Juilland, Alphonse G. & Eugenio Chang-Rodríguez. 1964. \emph{Frequency dictionary of Spanish words.} The Hague: Mouton de Gruyter. \doi{doi:10.1515/9783112415467}
#' 
#' Rosengren, Inger. 1971. The quantitative concept of language and its relation to the structure of frequency dictionaries. \emph{Études de linguistique appliquée (Nouvelle Série)} 1. 103--127.
#' 
#' @export
#'
#' @examples
#' disp_DKL(
#'   subfreq = c(0,0,1,2,5), 
#'   partsize = rep(1000, 5),
#'   standardization = "base_e",
#'   directionality = "conventional")
#' 
disp_DKL <- function(subfreq,
                     partsize,
                     directionality = "conventional",
                     standardization = "o2p",
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
  
  calculate_DKL <- function(subfreq, partsize, standardization){
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
    
    DKL_score <- calculate_DKL(subfreq, partsize, standardization)
    output <- DKL_score
  
    if (freq_adjust == TRUE){
      
      subfreq_min_disp <- find_min_disp(
        subfreq, 
        partsize,
        freq_adjust_method)
      
      subfreq_max_disp <- find_max_disp(
        subfreq, 
        partsize,
        freq_adjust_method)
      
      DKL_min <- calculate_DKL(subfreq_min_disp, partsize, standardization)
      DKL_max <- calculate_DKL(subfreq_max_disp, partsize, standardization)

      output <- (DKL_score - DKL_min) / (DKL_max - DKL_min)
      
      item_exceeds_limits <- FALSE
      if (unit_interval){
        item_exceeds_limits <- sum(output < 0 | output > 1) > 0
        output[output > 1] <- 1
        output[output < 0] <- 0
      }
    }
  }
  
  if (directionality == "gries") output <- 1 - output
  
  if (freq_adjust == TRUE){
    names(output) <- "DKL_nofreq"
  } else {
    names(output) <- "DKL"
  }
  
  if (!is.null(digits)) output <- round(output, digits)
  
  if (print_score != FALSE) print(output)
  
  if (sum(subfreq) == 0 & suppress_warning != TRUE){
    warning("All subfrequencies are 0; returning NA.")
  } else {
    if (verbose) {
      if (freq_adjust == TRUE){
        message("\nThe dispersion score is adjusted for frequency using the min-max")
        message("  transformation (see Gries 2024: 196-208); please note that the")
        message("  method implemented here does not work well if corpus parts differ")
        message("  considerably in size; see vignette('frequency-adjustment')")
        
        if (unit_interval & item_exceeds_limits){
          message("\nThe frequency-adjusted score exceeds the limits of the unit")
          message("  interval [0,1] and was replaced by 0 or 1")
        }
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
#' This function takes as input a term-document matrix and returns, for each item (i.e. each row) the dispersion measure \eqn{D_{KL}}. The rows in the input matrix represent the items, and the columns the corpus parts. Importantly, the term-document matrix must include an additional row that records the size of the corpus parts. For a proper term-document matrix, which includes all items that appear in the corpus, this can be added as a column margin, which sums the frequencies in each column. If the matrix only includes a selection of items drawn from the corpus, this information cannot be derived from the matrix and must be provided as a separate row.
#' 
#' - Directionality: \eqn{D_{KL}} ranges from 0 to 1. The conventional scaling of dispersion measures (see Juilland & Chang-Rodriguez 1964; Carroll 1970; Rosengren 1971) assigns higher values to more even/dispersed/balanced distributions of subfrequencies across corpus parts. This is the default. Gries (2008) uses the reverse scaling, with higher values denoting a more uneven/bursty/concentrated distribution; use `directionality = 'gries'` to choose this option.
#' 
#' - Standardization: Irrespective of the directionality of scaling, three ways of standardizing the Kullback-Leibler divergence to the unit interval \[0;1\] are mentioned in Gries (2024: 90-92). The choice between these transformations can have an appreciable effect on the standardized dispersion score. In Gries (2020: 103-104), the Kullback-Leibler divergence is not standardized. In Gries (2021: 20), the transformation `'base_e'` is used (see (1) below), and in Gries (2024), the default strategy is `'o2p'`, the odds-to-probability transformation (see (3) below). 
#' 
#' - Frequency adjustment: Dispersion scores can be adjusted for frequency using the min-max transformation proposed by Gries (2022: 184-191; 2024: 196-208). The frequency-adjusted score for an  item considers the lowest and highest possible level of dispersion it can obtain given its overall corpus frequency as well as the number (and size) of corpus parts. The unadjusted score is then expressed relative to these endpoints, where the dispersion minimum is set to 0, and the dispersion maximum to 1 (expressed in terms of conventional scaling). The frequency-adjusted score falls between these bounds and expresses how close the observed distribution is to the theoretical maximum and minimum. This adjustment therefore requires a maximally and a minimally dispersed distribution of the item across the parts. These hypothetical extremes can be built in different ways. The method used by Gries (2022, 2024) uses a computationally expensive procedure that finds the distribution that produces the highest value on the dispersion measure of interest. The current function constructs extreme distributions in a different way, based on the distributional features pervasiveness (`pervasive`) or evenness (`even`). You can choose between these with the argument `freq_adjust_method`; the default is `even`. For details and explanations, see `vignette("frequency-adjustment")`. 
#' 
#'    - To obtain the lowest possible level of dispersion, the occurrences are either allocated to as few corpus parts as possible (`pervasive`), or they are assigned to the smallest corpus part(s) (`even`).
#'    - To obtain the highest possible level of dispersion, the occurrences are either spread as broadly across corpus parts as possible (`pervasive`), or they are allocated to corpus parts in proportion to their size (`even`). The choice between these methods is particularly relevant if corpus parts differ considerably in size. See documentation for `find_max_disp()`.
#'    
#' In the formulas given below, the following notation is used:
#' 
#' - \eqn{t_i} a proportional quantity; the subfrequency in part \eqn{i} divided by the total number of occurrences of the item in the corpus (i.e. the sum of all subfrequencies)
#' - \eqn{w_i} a proportional quantity; the size of corpus part \eqn{i} divided by the size of the corpus (i.e. the sum of the part sizes) 
#' 
#' The first step is to calculate the Kullback-Leibler divergence based on the proportional subfrequencies (\eqn{t_i}) and the size of the corpus parts (\eqn{w_i}):
#' 
#'    \eqn{KLD = \sum_i^k t_i \log_2{\frac{t_i}{w_i}}}    with \eqn{\log_2(0) = 0}
#' 
#' This KLD score is then standardized (i.e. transformed) to the conventional unit interval \[0,1\]. Three options are discussed in Gries (2024: 90-92). The following formulas represents Gries scaling (0 = even, 1 = uneven):
#' 
#'    (1) \eqn{e^{-KLD}} (Gries 2021: 20), represented by the value `'base_e'`
#'
#'    (2) \eqn{2^{-KLD}} (Gries 2024: 90), represented by the value `'base_2'`
#'
#'    (3) \eqn{\frac{KLD}{1+KLD}} (Gries 2024: 90), represented by the value `'o2p'` (default)
#' 
#'
#' @returns A data frame with one row per item
#' 
#' @references 
#' 
#' Carroll, John B. 1970. An alternative to Juilland’s usage coefficient for lexical frequencies and a proposal for a standard frequency index. \emph{Computer Studies in the Humanities and Verbal Behaviour} 3(2). 61--65. \doi{doi:10.1002/j.2333-8504.1970.tb00778.x}
#' 
#' Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. \emph{International Journal of Corpus Linguistics} 13(4). 403--437. \doi{doi:10.1075/ijcl.13.4.02gri}
#' 
#' Gries, Stefan Th. 2022. What do (most of) our dispersion measures measure (most)? Dispersion? \emph{Journal of Second Language Studies} 5(2). 171--205. \doi{doi:10.1075/jsls.21029.gri}
#' 
#' Gries, Stefan Th. 2024. \emph{Frequency, dispersion, association, and keyness: Revising and tupleizing corpus-linguistic measures}. Amsterdam: Benjamins. \doi{doi:10.1075/scl.115}
#' 
#' Juilland, Alphonse G. & Eugenio Chang-Rodríguez. 1964. \emph{Frequency dictionary of Spanish words.} The Hague: Mouton de Gruyter. \doi{doi:10.1515/9783112415467}
#' 
#' Rosengren, Inger. 1971. The quantitative concept of language and its relation to the structure of frequency dictionaries. \emph{Études de linguistique appliquée (Nouvelle Série)} 1. 103--127.
#' 
#' 
#' @export
#'
#' @examples
#' disp_DKL_tdm(
#'   tdm = biber150_spokenBNC2014[1:20,],
#'   row_partsize = "first",
#'   standardization = "base_e",
#'   directionality = "conventional")
#' 
disp_DKL_tdm <- function(tdm,
                         row_partsize = "first",
                         directionality = "conventional",
                         standardization = "o2p",
                         freq_adjust = FALSE,
                         freq_adjust_method = "even",
                         add_frequency = TRUE,
                         unit_interval = TRUE,
                         digits = NULL,
                         verbose = TRUE,
                         print_scores = TRUE) {
  
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
  
  if (row_partsize == "first"){
    
    if (!all(colSums(tdm[-1,]) <= tdm[1,])){
      stop("The row you indicated (first row) does not contain the (correct) part sizes.\n  Use argument 'row_partsize' to locate the correct row or check content of\n  first row. At the moment, (some) counts in the first row are too small.")
    }
    
    DKL_score <- apply(
      tdm[-1,],
      1,
      function(x){
        disp_DKL(subfreq = x, 
                 partsize = tdm[1,],
                 directionality,
                 standardization,
                 freq_adjust = FALSE,
                 unit_interval = FALSE,
                 digits = NULL,
                 verbose = FALSE,
                 print_score = FALSE,
                 suppress_warning = TRUE)
      })
    
  } else if (row_partsize == "last"){
    
    if (!all(colSums(tdm[-nrow(tdm),]) <= tdm[nrow(tdm),])){
      stop("The row you indicated (last row) does not contain the (correct) part sizes.\n  Use argument 'row_partsize' to locate the correct row or check content of\n  last row. At the moment, (some) counts in the last row are too small.")
    }
    
    DKL_score <- apply(
      tdm[-nrow(tdm),],
      1,
      function(x){
        disp_DKL(subfreq = x, 
                 partsize = tdm[nrow(tdm),],
                 directionality,
                 standardization,
                 freq_adjust = FALSE,
                 unit_interval = FALSE,
                 digits = NULL,
                 verbose = FALSE,
                 print_score = FALSE,
                 suppress_warning = TRUE)
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
      
      DKL_min <- apply(
        min_disp_tdm[-1,],
        1,
        function(x){
          disp_DKL(subfreq = x, 
                   partsize = min_disp_tdm[1,],
                   directionality,
                   standardization,
                   freq_adjust = FALSE,
                   unit_interval = FALSE,
                   digits = NULL,
                   verbose = FALSE,
                   print_score = FALSE,
                   suppress_warning = TRUE)
        })
      
      DKL_max <- apply(
        max_disp_tdm[-1,],
        1,
        function(x){
          disp_DKL(subfreq = x, 
                   partsize = max_disp_tdm[1,],
                   directionality,
                   standardization,
                   freq_adjust = FALSE,
                   unit_interval = FALSE,
                   digits = NULL,
                   verbose = FALSE,
                   print_score = FALSE,
                   suppress_warning = TRUE)
        })
      
    } else if (row_partsize == "last"){
      
      DKL_min <- apply(
        min_disp_tdm[-nrow(min_disp_tdm),],
        1,
        function(x){
          disp_DKL(subfreq = x, 
                   partsize = min_disp_tdm[nrow(min_disp_tdm),],
                   directionality,
                   standardization,
                   freq_adjust = FALSE,
                   unit_interval = FALSE,
                   digits = NULL,
                   verbose = FALSE,
                   print_score = FALSE,
                   suppress_warning = TRUE)
        })
      
      DKL_max <- apply(
        max_disp_tdm[-nrow(max_disp_tdm),],
        1,
        function(x){
          disp_DKL(subfreq = x, 
                   partsize = max_disp_tdm[nrow(max_disp_tdm),],
                   directionality,
                   standardization,
                   freq_adjust = FALSE,
                   unit_interval = FALSE,
                   digits = NULL,
                   verbose = FALSE,
                   print_score = FALSE,
                   suppress_warning = TRUE)
        })
    }
    output <- (DKL_score - DKL_min) / (DKL_max - DKL_min)
    
    if (unit_interval){
      
      n_items_exceeding_limits <- sum(
        output < 0 | output > 1, na.rm = TRUE) 
      
      output[output > 1] <- 1
      output[output < 0] <- 0
    }
    
  } else {
    output <- DKL_score
  }
  
  output <- t(output)
  
  if (!is.null(digits)) output <- round(output, digits)
  
  output <- data.frame(
    item = colnames(output),
    DKL = as.numeric(output)
  )
  
  if (freq_adjust == TRUE){
    colnames(output)[2] <- "DKL_nofreq"
  }
  
  if (add_frequency == TRUE){
    if (row_partsize == "first"){
      output$frequency <- rowSums(tdm[-1,])
    }
    if (row_partsize == "last"){
      output$frequency <- rowSums(tdm[-nrow(tdm),])
    }
  }
  
  if (print_scores != FALSE) print(output)
  
  if (verbose) {
    if (freq_adjust == TRUE){
      message("\nDispersion scores are adjusted for frequency using the min-max")
      message("  transformation (see Gries 2024: 196-208); please note that the")
      message("  method implemented here does not work well if corpus parts differ")
      message("  considerably in size; see vignette('frequency-adjustment')")
      
      if (unit_interval){
        message(paste0(
          "\nFor ", n_items_exceeding_limits, " items, the frequency-adjusted score exceeds the limits of the"
        ))
        message("  unit interval [0,1]; these scores were replaced by 0 or 1")
      }
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
      message("  see Gries (2021: 20)\n")
    } else if (standardization == "base_2") {
      message("\nStandardization to the unit interval [0,1] using base 2,")
      message("  see Gries (2024: 90)\n")
    } else {
      message("\nStandardization to the unit interval [0,1] using the odds-to-probability")
      message("  transformation, see Gries (2024: 90)\n")
    }
  }
  if (row_partsize == "first"){
    if (sum(rowSums(tdm[-1,]) == 0) > 0){
      warning("\n  For some item(s), all subfrequencies are 0; returning NA in this case\n\n")
    }  
  }
  if (row_partsize == "last"){
    if (sum(rowSums(tdm[-nrow(tdm),]) == 0) > 0){
      warning("\n  For some item(s), all subfrequencies are 0; returning NA in this case\n\n")
    } 
  }
  if (sum(rowSums(tdm) == 1) > 0 & freq_adjust == TRUE){
    warning("\n  For some item(s), the corpus frequency is 1; no frequency adjustment\n  made in this case; function returns unadjusted dispersion score")
  }
  invisible(output)
}

