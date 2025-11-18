#' Calculate Gries's *deviation of proportions*
#'
#' @description
#' This function calculates Gries's dispersion measure DP (deviation of proportions). It offers three different formulas and allows the user to choose the directionality of scaling, i.e. whether higher values denote a more even or a less even distribution. It also offers the option of calculating frequency-adjusted dispersion scores.
#' 
#'
#' @inheritParams disp
#' @param formula Character string indicating which formula to use for the calculation of DP. See details below. Possible values are `"egbert_etal_2020"` (default), `"gries_2008"`, `"lijffit_gries_2012"`.
#'
#' @author Lukas Soenning
#' 
#' @details The function calculates the dispersion measure DP based on a set of subfrequencies (number of occurrences of the item in each corpus part) and a matching set of part sizes (the size of the corpus parts, i.e. number of word tokens).
#' 
#' - Directionality: DP ranges from 0 to 1. The conventional scaling of dispersion measures (see Juilland & Chang-Rodriguez 1964; Carroll 1970; Rosengren 1971) assigns higher values to more even/dispersed/balanced distributions of subfrequencies across corpus parts. This is the default. Gries (2008) uses the reverse scaling, with higher values denoting a more uneven/bursty/concentrated distribution; use `directionality = "gries"` to choose this option.
#' 
#' - Formula: Irrespective of the directionality of scaling, four formulas for DP exist in the literature (see below for details). This is because the original version proposed by Gries (2008: 415), which is commonly denoted as \eqn{DP} (and here referenced by the value `"gries_2008"`) does not always reach its theoretical limits of 0 and 1. For this reason, modifications have been suggested, starting with Gries (2008: 419) himself, who referred to this version as \eqn{DPnorm}. This version is not implemented in the current package, because Lijffit & Gries (2012) updated \eqn{DPnorm} to ensure that it also works as intended when corpus parts differ in size; this version is represented by the value `"lijffit_gries_2012"` and often denoted using subscript notation \eqn{DP_{norm}} . Finally, Egbert et al. (2020: 99) suggest a further modification to ensure proper behavior in settings where the item occurs in only one corpus part. They label this version \eqn{D_P}. In the current function, it is the default and represented by the value `"egbert_etal_2020"`.
#' 
#' - Frequency adjustment: Dispersion scores can be adjusted for frequency using the min-max transformation proposed by Gries (2022: 184-191; 2024: 196-208). The frequency-adjusted score for an  item considers the lowest and highest possible level of dispersion it can obtain given its overall corpus frequency as well as the number (and size) of corpus parts. The unadjusted score is then expressed relative to these endpoints, where the dispersion minimum is set to 0, and the dispersion maximum to 1 (expressed in terms of conventional scaling). The frequency-adjusted score falls between these bounds and expresses how close the observed distribution is to the theoretical maximum and minimum. This adjustment therefore requires a maximally and a minimally dispersed distribution of the item across the parts. These hypothetical extremes can be built in different ways. The method used by Gries (2022, 2024) uses a computationally expensive procedure that finds the distribution that produces the highest value on the dispersion measure of interest. The current function constructs extreme distributions in a different way, based on the distributional features pervasiveness (`"pervasive"`) or evenness (`"even"`). You can choose between these with the argument `freq_adjust_method`; the default is `even`. For details and explanations, see `vignette("frequency-adjustment")`. 
#' 
#'    - To obtain the lowest possible level of dispersion, the occurrences are either allocated to as few corpus parts as possible (`"pervasive"`), or they are assigned to the smallest corpus part(s) (`"even"`).
#'    - To obtain the highest possible level of dispersion, the occurrences are either spread as broadly across corpus parts as possible (`"pervasive"`), or they are allocated to corpus parts in proportion to their size (`"even"`). The choice between these methods is particularly relevant if corpus parts differ considerably in size. See documentation for `find_max_disp()`.
#' 
#' In the formulas given below, the following notation is used:
#' 
#' - \eqn{k} the number of corpus parts
#' - \eqn{t_i} a proportional quantity; the subfrequency in part \eqn{i} divided by the total number of occurrences of the item in the corpus (i.e. the sum of all subfrequencies)
#' - \eqn{w_i} a proportional quantity; the size of corpus part \eqn{i} divided by the size of the corpus (i.e. the sum of thea part sizes) 
#' 
#' The value `"gries_2008"` implements the original version proposed by Gries (2008: 415). Note that while the following formula represents Gries scaling (0 = even, 1 = uneven), in the current function the directionality is controlled separately using the argument `directionality`.
#' 
#'    \eqn{\frac{\sum_i^k |t_i - w_i|}{2}} (Gries 2008)
#' 
#' The value `"lijffit_gries_2012"` implements the modified version described by Lijffit & Gries (2012). Again, the following formula represents Gries scaling (0 = even, 1 = uneven), but the directionality is handled separately in the current function. The notation \eqn{min\{w_i\}} refers to the \eqn{w_i} value of the smallest corpus part.
#' 
#'    \eqn{\frac{\sum_i^k |t_i - w_i|}{2} \times \frac{1}{1 - min\{w_i\}}} (Lijffijt & Gries 2012)
#'  
#' The value `"egbert_etal_2020"` (default) selects the modification suggested by Egbert et al. (2020: 99). The following formula represents conventional scaling (0 = uneven, 1 = even). The notation \eqn{min\{w_i: t_i > 0\}} refers to the \eqn{w_i} value among those corpus parts that include at least one occurrence of the item.
#' 
#'    \eqn{1 - \frac{\sum_i^k |t_i - w_i|}{2} \times \frac{1}{1 - min\{w_i: t_i > 0\}}} (Egbert et al. 2020)
#'    
#'  
#' @returns A numeric value
#' 
#' @references 
#' 
#' Carroll, John B. 1970. An alternative to Juilland’s usage coefficient for lexical frequencies and a proposal for a standard frequency index. \emph{Computer Studies in the Humanities and Verbal Behaviour} 3(2). 61--65. \doi{doi:10.1002/j.2333-8504.1970.tb00778.x}
#' 
#' Egbert, Jesse, Brent Burch & Douglas Biber. 2020. Lexical dispersion and corpus design. \emph{International Journal of Corpus Linguistics} 25(1). 89--115. \doi{doi:10.1075/ijcl.18010.egb}
#' 
#' Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. \emph{International Journal of Corpus Linguistics} 13(4). 403--437. \doi{doi:10.1075/ijcl.13.4.02gri}
#' 
#' Gries, Stefan Th. 2022. What do (most of) our dispersion measures measure (most)? Dispersion? \emph{Journal of Second Language Studies} 5(2). 171--205. \doi{doi:10.1075/jsls.21029.gri}
#' 
#' Gries, Stefan Th. 2024. \emph{Frequency, dispersion, association, and keyness: Revising and tupleizing corpus-linguistic measures}. Amsterdam: Benjamins. \doi{doi:10.1075/scl.115}
#' 
#' Juilland, Alphonse G. & Eugenio Chang-Rodríguez. 1964. \emph{Frequency dictionary of Spanish words.} The Hague: Mouton de Gruyter. \doi{doi:10.1515/9783112415467}
#' 
#' Lijffijt, Jefrey & Stefan Th. Gries. 2012. Correction to Stefan Th. Gries’ ‘Dispersions and adjusted frequencies in corpora’. \emph{International Journal of Corpus Linguistics} 17(1). 147--149. \doi{doi:10.1075/ijcl.17.1.08lij}
#' 
#' Rosengren, Inger. 1971. The quantitative concept of language and its relation to the structure of frequency dictionaries. \emph{Études de linguistique appliquée (Nouvelle Série)} 1. 103--127.
#' 
#' @export
#'
#' @examples
#' disp_DP(
#'   subfreq = c(0,0,1,2,5), 
#'   partsize = rep(1000, 5),
#'   directionality = "conventional",
#'   formula = "gries_2008",
#'   freq_adjust = FALSE)
#' 
disp_DP <- function(subfreq,
                    partsize,
                    directionality = "conventional",
                    formula = "egbert_etal_2020",
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
  
  calculate_DP <- function(subfreq, partsize, formula){
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
    
    DP_score <- calculate_DP(subfreq, partsize, formula)
    output <- DP_score
    
    if (freq_adjust == TRUE){
      
      subfreq_min_disp <- find_min_disp(
        subfreq, 
        partsize,
        freq_adjust_method)
      
      subfreq_max_disp <- find_max_disp(
        subfreq, 
        partsize,
        freq_adjust_method)
      
      DP_min <- calculate_DP(subfreq_min_disp, partsize, formula)
      DP_max <- calculate_DP(subfreq_max_disp, partsize, formula)
      
      output <- (DP_score - DP_min) / (DP_max - DP_min)
      
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
    names(output) <- "DP_nofreq"
  } else {
    names(output) <- "DP"
  }
  
  if (!is.null(digits)) output <- round(output, digits)
  
  if (print_score != FALSE) print(output)
  
  if (sum(subfreq) == 0 & suppress_warning == FALSE){
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
        logmsg("  1 = maximally uneven/bursty/concentrated distribution (pessimum)")
      } else {
        logmsg("\nScores follow conventional scaling:")
        logmsg("  0 = maximally uneven/bursty/concentrated distribution (pessimum)")
        logmsg("  1 = maximally even/dispersed/balanced distribution (optimum)")
      }
      
      if (formula == "gries_2008") {
        logmsg("\nComputed using the original version proposed by Gries (2008)")
      } else if (formula == "lijffit_gries_2012") {
        logmsg("\nComputed using the modification suggested by Lijffijt & Gries (2012)")
      } else {
        logmsg("\nComputed using the modification suggested by Egbert et al. (2020)")
      }
      cat(paste(log_buffer, collapse = "\n"))
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
#' This function takes as input a term-document matrix and returns, for each item (i.e. each row) the dispersion measure DP. The rows in the input matrix represent the items, and the columns the corpus parts. Importantly, the term-document matrix must include an additional row that records the size of the corpus parts. For a proper term-document matrix, which includes all items that appear in the corpus, this can be added as a column margin, which sums the frequencies in each column. If the matrix only includes a selection of items drawn from the corpus, this information cannot be derived from the matrix and must be provided as a separate row.
#' 
#' - Directionality: DP ranges from 0 to 1. The conventional scaling of dispersion measures (see Juilland & Chang-Rodriguez 1964; Carroll 1970; Rosengren 1971) assigns higher values to more even/dispersed/balanced distributions of subfrequencies across corpus parts. This is the default. Gries (2008) uses the reverse scaling, with higher values denoting a more uneven/bursty/concentrated distribution; use `directionality = "gries"` to choose this option.
#' 
#' - Formula: Irrespective of the directionality of scaling, four formulas for DP exist in the literature (see below for details). This is because the original version proposed by Gries (2008: 415), which is commonly denoted as \eqn{DP} (and here referenced by the value `"gries_2008"`) does not always reach its theoretical limits of 0 and 1. For this reason, modifications have been suggested, starting with Gries (2008: 419) himself, who referred to this version as \eqn{DPnorm}. This version is not implemented in the current package, because Lijffit & Gries (2012) updated \eqn{DPnorm} to ensure that it also works as intended when corpus parts differ in size; this version is represented by the value `"lijffit_gries_2012"` and often denoted using subscript notation \eqn{DP_{norm}} . Finally, Egbert et al. (2020: 99) suggest a further modification to ensure proper behavior in settings where the item occurs in only one corpus part. They label this version \eqn{D_P}. In the current function, it is the default and represented by the value `"egbert_etal_2020"`.
#' 
#' - Frequency adjustment: Dispersion scores can be adjusted for frequency using the min-max transformation proposed by Gries (2022: 184-191; 2024: 196-208). The frequency-adjusted score for an  item considers the lowest and highest possible level of dispersion it can obtain given its overall corpus frequency as well as the number (and size) of corpus parts. The unadjusted score is then expressed relative to these endpoints, where the dispersion minimum is set to 0, and the dispersion maximum to 1 (expressed in terms of conventional scaling). The frequency-adjusted score falls between these bounds and expresses how close the observed distribution is to the theoretical maximum and minimum. This adjustment therefore requires a maximally and a minimally dispersed distribution of the item across the parts. These hypothetical extremes can be built in different ways. The method used by Gries (2022, 2024) uses a computationally expensive procedure that finds the distribution that produces the highest value on the dispersion measure of interest. The current function constructs extreme distributions in a different way, based on the distributional features pervasiveness (`"pervasive"`) or evenness (`"even"`). You can choose between these with the argument `freq_adjust_method`; the default is `even`. For details and explanations, see `vignette("frequency-adjustment")`. 
#' 
#'    - To obtain the lowest possible level of dispersion, the occurrences are either allocated to as few corpus parts as possible (`"pervasive"`), or they are assigned to the smallest corpus part(s) (`"even"`).
#'    - To obtain the highest possible level of dispersion, the occurrences are either spread as broadly across corpus parts as possible (`"pervasive"`), or they are allocated to corpus parts in proportion to their size (`"even"`). The choice between these methods is particularly relevant if corpus parts differ considerably in size. See documentation for `find_max_disp()`.
#' 
#' In the formulas given below, the following notation is used:
#' 
#' - \eqn{k}    the number of corpus parts
#' - \eqn{t_i}    a proportional quantity; the subfrequency in part \eqn{i} divided by the total number of occurrences of the item in the corpus (i.e. the sum of all subfrequencies)
#' - \eqn{w_i}    a proportional quantity; the size of corpus part \eqn{i} divided by the size of the corpus (i.e. the sum of the part sizes) 
#' 
#' The value `"gries_2008"` implements the original version proposed by Gries (2008: 415). Note that while the following formula represents Gries scaling (0 = even, 1 = uneven), in the current function the directionality is controlled separately using the argument `directionality`.
#' 
#'    \eqn{\frac{\sum_i^k |t_i - w_i|}{2}} (Gries 2008)
#' 
#' The value `"lijffit_gries_2012"` implements the modified version described by Lijffit & Gries (2012). Again, the following formula represents Gries scaling (0 = even, 1 = uneven), but the directionality is handled separately in the current function. The notation \eqn{min\{w_i\}} refers to the \eqn{w_i} value of the smallest corpus part.
#' 
#'    \eqn{\frac{\sum_i^k |t_i - w_i|}{2} \times \frac{1}{1 - min\{w_i\}}} (Lijffijt & Gries 2012)
#'  
#' The value `"egbert_etal_2020"` (default) selects the modification suggested by Egbert et al. (2020: 99). The following formula represents conventional scaling (0 = uneven, 1 = even). The notation \eqn{min\{w_i: t_i > 0\}} refers to the \eqn{w_i} value among those corpus parts that include at least one occurrence of the item.
#' 
#'    \eqn{1 - \frac{\sum_i^k |t_i - w_i|}{2} \times \frac{1}{1 - min\{w_i: t_i > 0\}}} (Egbert et al. 2020)
#'    
#'  
#' @returns A data frame with one row per item
#' 
#' @references 
#' 
#' Carroll, John B. 1970. An alternative to Juilland’s usage coefficient for lexical frequencies and a proposal for a standard frequency index. \emph{Computer Studies in the Humanities and Verbal Behaviour} 3(2). 61--65. \doi{doi:10.1002/j.2333-8504.1970.tb00778.x}
#' 
#' Egbert, Jesse, Brent Burch & Douglas Biber. 2020. Lexical dispersion and corpus design. \emph{International Journal of Corpus Linguistics} 25(1). 89--115. \doi{doi:10.1075/ijcl.18010.egb}
#' 
#' Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. \emph{International Journal of Corpus Linguistics} 13(4). 403--437. \doi{doi:10.1075/ijcl.13.4.02gri}
#' 
#' Gries, Stefan Th. 2022. What do (most of) our dispersion measures measure (most)? Dispersion? \emph{Journal of Second Language Studies} 5(2). 171--205. \doi{doi:10.1075/jsls.21029.gri}
#' 
#' Gries, Stefan Th. 2024. \emph{Frequency, dispersion, association, and keyness: Revising and tupleizing corpus-linguistic measures}. Amsterdam: Benjamins. \doi{doi:10.1075/scl.115}
#' 
#' Juilland, Alphonse G. & Eugenio Chang-Rodríguez. 1964. \emph{Frequency dictionary of Spanish words.} The Hague: Mouton de Gruyter. \doi{doi:10.1515/9783112415467}
#' 
#' Lijffijt, Jefrey & Stefan Th. Gries. 2012. Correction to Stefan Th. Gries’ ‘Dispersions and adjusted frequencies in corpora’. \emph{International Journal of Corpus Linguistics} 17(1). 147--149. \doi{doi:10.1075/ijcl.17.1.08lij}
#' 
#' Rosengren, Inger. 1971. The quantitative concept of language and its relation to the structure of frequency dictionaries. \emph{Études de linguistique appliquée (Nouvelle Série)} 1. 103--127.
#' 
#' @export
#'
#' @examples
#' disp_DP_tdm(
#'   tdm = biber150_spokenBNC2014[1:20,],
#'   row_partsize = "first",
#'   directionality = "conventional",
#'   formula = "gries_2008",
#'   freq_adjust = FALSE)
#' 
disp_DP_tdm <- function(tdm,
                        row_partsize = "first",
                        directionality = "conventional",
                        formula = "egbert_etal_2020",
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
    
    DP_score <- apply(
      tdm[-1,],
      1,
      function(x){
        disp_DP(subfreq = x, 
                partsize = tdm[1,],
                directionality,
                formula,
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
    
    DP_score <- apply(
      tdm[-nrow(tdm),],
      1,
      function(x){
        disp_DP(subfreq = x, 
                partsize = tdm[nrow(tdm),],
                directionality,
                formula,
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
      freq_adjust_method = freq_adjust_method, 
      row_partsize = row_partsize)
    
    max_disp_tdm <- find_max_disp_tdm(
      tdm, 
      freq_adjust_method = freq_adjust_method, 
      row_partsize = row_partsize)
    
    
    if (row_partsize == "first"){
      
      DP_min <- apply(
        min_disp_tdm[-1,],
        1,
        function(x){
          disp_DP(subfreq = x, 
                  partsize = min_disp_tdm[1,],
                  directionality,
                  formula,
                  freq_adjust = FALSE,
                  unit_interval = FALSE,
                  digits = NULL,
                  verbose = FALSE,
                  print_score = FALSE,
                  suppress_warning = TRUE)
        })
      
      DP_max <- apply(
        max_disp_tdm[-1,],
        1,
        function(x){
          disp_DP(subfreq = x, 
                  partsize = max_disp_tdm[1,],
                  directionality,
                  formula,
                  freq_adjust = FALSE,
                  unit_interval = FALSE,
                  digits = NULL,
                  verbose = FALSE,
                  print_score = FALSE,
                  suppress_warning = TRUE)
        })
      
    } else if (row_partsize == "last"){
      
      DP_min <- apply(
        min_disp_tdm[-nrow(min_disp_tdm),],
        1,
        function(x){
          disp_DP(subfreq = x, 
                  partsize = min_disp_tdm[nrow(min_disp_tdm),],
                  directionality,
                  formula,
                  freq_adjust = FALSE,
                  unit_interval = FALSE,
                  digits = NULL,
                  verbose = FALSE,
                  print_score = FALSE,
                  suppress_warning = TRUE)
        })
      
      DP_max <- apply(
        max_disp_tdm[-nrow(max_disp_tdm),],
        1,
        function(x){
          disp_DP(subfreq = x, 
                  partsize = max_disp_tdm[nrow(max_disp_tdm),],
                  directionality,
                  formula,
                  freq_adjust = FALSE,
                  unit_interval = FALSE,
                  digits = NULL,
                  verbose = FALSE,
                  print_score = FALSE,
                  suppress_warning = TRUE)
        })
    }
    output <- (DP_score - DP_min) / (DP_max - DP_min)
    
    if (unit_interval){
      
      n_items_exceeding_limits <- sum(
        output < 0 | output > 1, na.rm = TRUE) 
      
      output[output > 1] <- 1
      output[output < 0] <- 0
    }
    
  } else {
    output <- DP_score
  }
  
  output <- t(output)
  
  if (!is.null(digits)) output <- round(output, digits)
  
  output <- data.frame(
    item = colnames(output),
    DP = as.numeric(output)
  )
  
  if (freq_adjust == TRUE){
    colnames(output)[2] <- "DP_nofreq"
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
    
    if (formula == "gries_2008") {
      message("\nComputed using the original version proposed by Gries (2008)\n")
    } else if (formula == "lijffit_gries_2012") {
      message("\nComputed using the modification suggested by Lijffit & Gries (2012)\n")
    } else {
      message("\nComputed using the modification suggested by Egbert et al. (2020)\n")
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




#' Bootstrap (and weight) Gries's *deviation of proportions*
#'
#' @description
#' This function offers facilities for bootstrapping and weighting Gries's dispersion measure DP (deviation of proportions). In addition to the full functionality offered by the function `disp_DP()`, it can be used to obtain weighted dispersion scores as well as bootstrap confidence intervals.
#' 
#' @inheritParams disp_DP
#' @param n_replicates Integer value specifying the number of bootstrap replicates to draw; default: `500`
#' @param strata Variable indicating the subgroups for stratified bootstrap sampling
#' @param boot_ci Logical. Whether a percentile bootstrap confidence interval should be computed; default: `FALSE`
#' @param conf_level Scalar giving the confidence level; default `0.95` for a 95% percentile CI
#' @param return_distribution Logical. Whether the function should return a vector of all `n_replicates` bootstrap statistics instead of a summary measure
#' @param partweight A numeric vector specifying the weights of the corpus parts; if not specified, function returns unweighted estimate
#'
#' @author Lukas Soenning
#' 
#' @details 
#' This function calculates weighted dispersion measures and bootstrap confidence intervals.
#' 
#' @seealso [disp_DP()] for finer control over the calculation of DP
#' 
#' @export
#'
#' @examples
#' disp_DP_boot(
#'   subfreq = biber150_ice_gb[3,], 
#'   partsize = biber150_ice_gb[1,], 
#'   digits = 2,
#'   freq_adjust = TRUE,
#'   directionality = "conventional",
#'   formula = "gries_2008")
#' 
disp_DP_boot <- function(subfreq,
                         partsize,
                         n_replicates = 500,
                         strata = NULL,
                         boot_ci = FALSE,
                         conf_level = .95,
                         return_distribution = FALSE,
                         partweight = NULL,
                         directionality = "conventional",
                         formula = "egbert_etal_2020",
                         freq_adjust = FALSE,
                         freq_adjust_method = "even",
                         unit_interval = TRUE,
                         digits = NULL,
                         verbose = TRUE,
                         print_score = TRUE,
                         suppress_warning = FALSE){
  if (length(subfreq) != length(partsize)){
    stop("Lengths of the variables 'subfreq', 'partsize', and 'partweight' differ.")
  }
  
  if (!is.null(partweight)) {
    if(length(subfreq) != length(partweight)){
      stop("Lengths of the variables 'subfreq', 'partsize', and 'partweight' differ.")
    }
  }
  
  if (sum(subfreq) == 0){
    output <- NA
    
  } else {
    
    if (is.null(strata)){
      
      boot_DP <- function(data, 
                          indices)
      {
        d <- data[indices,]
        
        disp_score <- disp_DP(
          subfreq = d$subfreq, 
          partsize = d$partsize,
          directionality = directionality,
          formula = formula,
          freq_adjust = freq_adjust,
          freq_adjust_method = freq_adjust_method,
          unit_interval = FALSE,
          digits = digits,
          verbose = FALSE,
          print_score = FALSE,
          suppress_warning = TRUE)
        
        return(disp_score)
      }
      if (is.null(partweight)) {
        partweight <- rep(1, length(subfreq))
      }
      data <- data.frame(
        subfreq,
        partsize,
        partweight
      )
      boot_results <- boot::boot(
        data, 
        statistic = boot_DP,
        R = n_replicates,
        weights = data$partweight)
    }
  }
  
  if(!is.null(strata)){
    
    boot_DP_strat <- function(data, 
                              idx){
      
      get_partsize <- stats::aggregate(
        partsize ~ strata, 
        data = data,
        subset = idx,
        FUN = sum)
      
      get_subfreq <- stats::aggregate(
        subfreq ~ strata, 
        data = data,
        subset = idx,
        FUN = sum)
      
      disp_score <- disp_DP(
        subfreq = get_subfreq[,2],
        partsize = get_partsize[,2],
        directionality = directionality,
        formula = formula,
        freq_adjust = freq_adjust,
        freq_adjust_method = freq_adjust_method,
        unit_interval = FALSE,
        digits = digits,
        verbose = FALSE,
        print_score = FALSE,
        suppress_warning = TRUE)
      
      return(disp_score)
    }
    if (is.null(partweight)) {
      partweight <- rep(1, length(subfreq))
    }
    
    d <- data.frame(
      subfreq = subfreq,
      partsize = partsize,
      strata = strata,
      partweight = partweight
    )
    
    boot_results <- boot::boot(
      d, 
      statistic = boot_DP_strat, 
      strata = d$strata, 
      R = n_replicates,
      weights = d$partweight)
  }
  
  
  
  boot_output <- boot_results$t
  
  if (unit_interval){
    
    n_replicates_exceeding_limits <- sum(
      boot_output < 0 | boot_output > 1, na.rm = TRUE) 
    
    boot_output[boot_output > 1] <- 1
    boot_output[boot_output < 0] <- 0
  }
  
  if(boot_ci){
    output <- stats::quantile(
      boot_output, 
      probs = c(.5, (1 - conf_level)/2, 1 - (1 - conf_level)/2))
    
    if (freq_adjust == TRUE){
      names(output) <- c("DP_nofreq", "conf.low", "conf.high")
    } else {
      names(output) <- c("DP", "conf.low", "conf.high")
    }
  } else {
    output <- stats::median(boot_output)
    
    if (freq_adjust == TRUE){
      names(output) <- "DP_nofreq"
    } else {
      names(output) <- "DP"
    }
  }
  
  if (return_distribution == TRUE){
    if (print_score != FALSE) print(boot_output)
  } else {
    if (print_score != FALSE) print(output)
  }
  
  if (sum(subfreq) == 0 & suppress_warning == FALSE){
    warning("All subfrequencies are 0; returning NA.")
  } else {
    if (verbose) {
      log_buffer <- character()
      
      logmsg <- function(x) {
        log_buffer <<- c(log_buffer, x)
      }
      
      message(paste0("\nBased on ", n_replicates, " bootstrap replicates"))
      if(boot_ci){
        message(paste0("  Median and ", conf_level*100, "% percentile confidence interval limits"))
      } else {
        message(paste0("  Dispersion score is the median over the ", n_replicates, " replicates"))
      }
      if (!is.null(partweight) | length(unique(partweight)) == 1) {
        message("  Unweighted estimate (all corpus parts weighted equally)")
      } else {
        message("  Weighted estimate (corpus parts weighted differently)")
      }
      if (freq_adjust == TRUE){
        message("\nThe dispersion score is adjusted for frequency using the min-max")
        message("  transformation (see Gries 2024: 196-208); please note that the")
        message("  method implemented here does not work well if corpus parts differ")
        message("  considerably in size; see vignette('frequency-adjustment')")
        
        if (unit_interval & n_replicates_exceeding_limits > 0){
          message("\nThe frequency-adjusted score exceeds the limits of the unit")
          message("  interval [0,1] and was replaced by 0 or 1")
        }
      }
      if (unit_interval){
        message(paste0(
          "\nFor ", n_replicates_exceeding_limits, " replicates, the frequency-adjusted score exceeds the limits of the"
        ))
        message("  unit interval [0,1]; these scores were replaced by 0 or 1")
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
      cat(paste(log_buffer, collapse = "\n"))
      
    }
  }
  if (return_distribution == TRUE){
    invisible(boot_output)
  } else {
    invisible(output)
  }
}
