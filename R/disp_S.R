#' Calculate the dispersion measure \eqn{S}
#'
#' @description
#' This function calculates the dispersion measure \eqn{S} (Rosengren 1971) and allows the user to choose the directionality of scaling, i.e. whether higher values denote a more even or a less even distribution. It also offers the option of calculating frequency-adjusted dispersion scores.
#'
#' @inheritParams disp
#'
#' @author Lukas Soenning
#' 
#' @details The function calculates the dispersion measure \eqn{S} based on a set of subfrequencies (number of occurrences of the item in each corpus part) and a matching set of part sizes (the size of the corpus parts, i.e. number of word tokens).
#' 
#' - Directionality: \eqn{S} ranges from 0 to 1. The conventional scaling of dispersion measures (see Juilland & Chang-Rodriguez 1964; Carroll 1970; Rosengren 1971) assigns higher values to more even/dispersed/balanced distributions of subfrequencies across corpus parts. This is the default. Gries (2008) uses the reverse scaling, with higher values denoting a more uneven/bursty/concentrated distribution; use `directionality = "gries"` to choose this option.
#' 
#' - Frequency adjustment: Dispersion scores can be adjusted for frequency using the min-max transformation proposed by Gries (2022: 184-191; 2024: 196-208). The frequency-adjusted score for an  item considers the lowest and highest possible level of dispersion it can obtain given its overall corpus frequency as well as the number (and size) of corpus parts. The unadjusted score is then expressed relative to these endpoints, where the dispersion minimum is set to 0, and the dispersion maximum to 1 (expressed here in terms of conventional scaling). The frequency-adjusted score falls between these bounds and expresses how close the observed distribution is to the theoretical maximum and minimum. This adjustment therefore requires a maximally and a minimally dispersed distribution of the item across the parts. These hypothetical extremes can be built in different ways. The method used by Gries (2022, 2024) uses a computationally expensive procedure that finds the distribution that produces the highest value on the dispersion measure of interest. The current function constructs extreme distributions in a different way, based on the distributional features pervasiveness (`"pervasive"`) or evenness (`"even"`). You can choose between these with the argument `freq_adjust_method`; the default is `even`. For details and explanations, see `vignette("frequency-adjustment")`. 
#' 
#'    - To obtain the lowest possible level of dispersion, the occurrences are either allocated to as few corpus parts as possible (`"pervasive"`), or they are assigned to the smallest corpus part(s) (`"even"`).
#'    - To obtain the highest possible level of dispersion, the occurrences are either spread as broadly across corpus parts as possible (`"pervasive"`), or they are allocated to corpus parts in proportion to their size (`"even"`). The choice between these methods is particularly relevant if corpus parts differ considerably in size. See documentation for `find_max_disp()` and `vignette("frequency-adjustment")`.
#'    
#' In the formulas given below, the following notation is used:
#' 
#' - \eqn{k} the number of corpus parts
#' - \eqn{T_i} the absolute subfrequency in part \eqn{i}
#' - \eqn{w_i} a proportional quantity; the size of corpus part \eqn{i} divided by the size of the corpus (i.e. the sum of the part sizes) 
#' 
#' \eqn{S} is the dispersion measure proposed by Rosengren (1971); the formula uses conventional scaling:
#' 
#'    \eqn{\frac{(\sum_i^k r_i \sqrt{w_i T_i}}{N}}
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
#' disp_S(
#'   subfreq = c(0,0,1,2,5), 
#'   partsize = rep(1000, 5),
#'   directionality = "conventional")
#' 
disp_S <- function(subfreq,
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
  
  calculate_S <- function(subfreq, partsize){
    w_i <- partsize / sum(partsize)

    S   <- (sum(sqrt(w_i * subfreq))^2) / sum(subfreq)
  }
  
  if (sum(subfreq) == 0){
    output <- NA
    
  } else {
    
    S_score <- calculate_S(subfreq, partsize)
    output <- S_score
    
    if (freq_adjust == TRUE){
      
      subfreq_min_disp <- find_min_disp(
        subfreq, 
        partsize,
        freq_adjust_method)
      
      subfreq_max_disp <- find_max_disp(
        subfreq, 
        partsize,
        freq_adjust_method)
      
      S_min <- calculate_S(subfreq_min_disp, partsize)
      S_max <- calculate_S(subfreq_max_disp, partsize)
      
      output <- (S_score - S_min) / (S_max - S_min)
      
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
    names(output) <- "S_nofreq"
  } else {
    names(output) <- "S"
  }
  
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
        logmsg("\nThe dispersion score is adjusted for frequency using the min-max")
        logmsg("  transformation (see Gries 2024: 196-208); please note that the")
        logmsg("  method implemented here does not work well if corpus parts differ")
        logmsg("  considerably in size; see vignette('frequency-adjustment')")
        
        if (unit_interval & item_exceeds_limits){
          logmsg("\nThe frequency-adjusted score exceeds the limits of the unit")
          logmsg("  interval [0,1] and was replaced by 0 or 1")
        }
      }
      if (directionality == "gries") {
        logmsg("\nScores follow scaling used by Gries (2008):")
        logmsg("  0 = maximally even/dispersed/balanced distribution (optimum)")
        logmsg("  1 = maximally uneven/bursty/concentrated distribution (pessimum)\n")
      } else {
        logmsg("\nScores follow conventional scaling:")
        logmsg("  0 = maximally uneven/bursty/concentrated distribution (pessimum)")
        logmsg("  1 = maximally even/dispersed/balanced distribution (optimum)\n")
      }
      cat(paste(log_buffer, collapse = "\n"))
      
    }
  }
  invisible(output)
}


#' Calculate the dispersion measure \eqn{S} for a term-document matrix
#'
#' @description
#' This function calculates the dispersion measure \eqn{S} (Rosengren 1971) and allows the user to choose the directionality of scaling, i.e. whether higher values denote a more even or a less even distribution. It also offers the option of calculating frequency-adjusted dispersion scores.
#'
#' @inheritParams disp_tdm
#' @inheritParams disp_S
#'
#' @author Lukas Soenning
#' 
#' @details 
#' This function takes as input a term-document matrix and returns, for each item (i.e. each row) the dispersion measure \eqn{S}. The rows in the input matrix represent the items, and the columns the corpus parts. Importantly, the term-document matrix must include an additional row that records the size of the corpus parts. For a proper term-document matrix, which includes all items that appear in the corpus, this can be added as a column margin, which sums the frequencies in each column. If the matrix only includes a selection of items drawn from the corpus, this information cannot be derived from the matrix and must be provided as a separate row.
#' 
#' - Directionality: \eqn{S} ranges from 0 to 1. The conventional scaling of dispersion measures (see Juilland & Chang-Rodriguez 1964; Carroll 1970; Rosengren 1971) assigns higher values to more even/dispersed/balanced distributions of subfrequencies across corpus parts. This is the default. Gries (2008) uses the reverse scaling, with higher values denoting a more uneven/bursty/concentrated distribution; use `directionality = 'gries'` to choose this option.
#' 
#' - Frequency adjustment: Dispersion scores can be adjusted for frequency using the min-max transformation proposed by Gries (2022: 184-191; 2024: 196-208). The frequency-adjusted score for an  item considers the lowest and highest possible level of dispersion it can obtain given its overall corpus frequency as well as the number (and size) of corpus parts. The unadjusted score is then expressed relative to these endpoints, where the dispersion minimum is set to 0, and the dispersion maximum to 1 (expressed in terms of conventional scaling). The frequency-adjusted score falls between these bounds and expresses how close the observed distribution is to the theoretical maximum and minimum. This adjustment therefore requires a maximally and a minimally dispersed distribution of the item across the parts. These hypothetical extremes can be built in different ways. The method used by Gries (2022, 2024) uses a computationally expensive procedure that finds the distribution that produces the highest value on the dispersion measure of interest. The current function constructs extreme distributions in a different way, based on the distributional features pervasiveness (`pervasive`) or evenness (`even`). You can choose between these with the argument `freq_adjust_method`; the default is `even`. For details and explanations, see `vignette("frequency-adjustment")`. 
#' 
#'    - To obtain the lowest possible level of dispersion, the occurrences are either allocated to as few corpus parts as possible (`pervasive`), or they are assigned to the smallest corpus part(s) (`even`).
#'    - To obtain the highest possible level of dispersion, the occurrences are either spread as broadly across corpus parts as possible (`pervasive`), or they are allocated to corpus parts in proportion to their size (`even`). The choice between these methods is particularly relevant if corpus parts differ considerably in size. See documentation for `find_max_disp()`.
#'    
#' In the formulas given below, the following notation is used:
#' 
#' - \eqn{k} the number of corpus parts
#' - \eqn{T_i} the absolute subfrequency in part \eqn{i}
#' - \eqn{w_i} a proportional quantity; the size of corpus part \eqn{i} divided by the size of the corpus (i.e. the sum of the part sizes) 
#' 
#' \eqn{S} is the dispersion measure proposed by Rosengren (1971); the formula uses conventional scaling:
#' 
#'    \eqn{\frac{(\sum_i^k r_i \sqrt{w_i T_i}}{N}}
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
#' disp_S_tdm(
#'   tdm = biber150_spokenBNC2014[1:20,],
#'   row_partsize = "first",
#'   directionality = "conventional")
#' 
disp_S_tdm <- function(tdm,
                       row_partsize = "first",
                       directionality = "conventional",
                       freq_adjust = FALSE,
                       freq_adjust_method = "even",
                       unit_interval = TRUE,
                       add_frequency = TRUE,
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
    
    S_score <- apply(
      tdm[-1,],
      1,
      function(x){
        disp_S(subfreq = x, 
                 partsize = tdm[1,],
                 directionality,
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
    
    S_score <- apply(
      tdm[-nrow(tdm),],
      1,
      function(x){
        disp_S(subfreq = x, 
                 partsize = tdm[nrow(tdm),],
                 directionality,
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
      
      S_min <- apply(
        min_disp_tdm[-1,],
        1,
        function(x){
          disp_S(subfreq = x, 
                   partsize = min_disp_tdm[1,],
                   directionality,
                   freq_adjust = FALSE,
                   unit_interval = FALSE,
                   digits = NULL,
                   verbose = FALSE,
                   print_score = FALSE,
                   suppress_warning = TRUE)
        })
      
      S_max <- apply(
        max_disp_tdm[-1,],
        1,
        function(x){
          disp_S(subfreq = x, 
                   partsize = max_disp_tdm[1,],
                   directionality,
                   freq_adjust = FALSE,
                   unit_interval = FALSE,
                   digits = NULL,
                   verbose = FALSE,
                   print_score = FALSE,
                   suppress_warning = TRUE)
        })
      
    } else if (row_partsize == "last"){
      
      S_min <- apply(
        min_disp_tdm[-nrow(min_disp_tdm),],
        1,
        function(x){
          disp_S(subfreq = x, 
                   partsize = min_disp_tdm[nrow(min_disp_tdm),],
                   directionality,
                   freq_adjust = FALSE,
                   unit_interval = FALSE,
                   digits = NULL,
                   verbose = FALSE,
                   print_score = FALSE,
                   suppress_warning = TRUE)
        })
      
      S_max <- apply(
        max_disp_tdm[-nrow(max_disp_tdm),],
        1,
        function(x){
          disp_S(subfreq = x, 
                   partsize = max_disp_tdm[nrow(max_disp_tdm),],
                   directionality,
                   freq_adjust = FALSE,
                   unit_interval = FALSE,
                   digits = NULL,
                   verbose = FALSE,
                   print_score = FALSE,
                   suppress_warning = TRUE)
        })
    }
    output <- (S_score - S_min) / (S_max - S_min)
    
    if (unit_interval){
      
      n_items_exceeding_limits <- sum(
        output < 0 | output > 1, na.rm = TRUE) 
      
      output[output > 1] <- 1
      output[output < 0] <- 0
    }
    
  } else {
    output <- S_score
  }
  
  output <- t(output)
  
  if (!is.null(digits)) output <- round(output, digits)
  
  output <- data.frame(
    item = colnames(output),
    S = as.numeric(output)
  )
  
  if (freq_adjust == TRUE){
    colnames(output)[2] <- "S_nofreq"
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
          "\nFor ", n_items_exceeding_limits, " items, the frequency-adjusted score exceeds the limits of the"
        ))
        logmsg("  unit interval [0,1]; these scores were replaced by 0 or 1")
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
    cat(paste(log_buffer, collapse = "\n"))
    
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

