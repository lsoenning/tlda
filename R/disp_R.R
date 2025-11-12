#' Calculate the dispersion measure 'range'
#'
#' @description
#' This function calculates the dispersion measure 'range'. It offers three different versions: 'absolute range' (the number of corpus parts containing at least one occurrence of the item), 'relative range' (the proportion of corpus parts containing at least one occurrence of the item), and 'relative range with size' (relative range that takes into account the size of the corpus parts). The function also offers the option of calculating frequency-adjusted dispersion scores.
#'
#' @inheritParams disp
#' @param type Character string indicating which type of range to calculate. See details below. Possible values are `"relative"` (default), `"absolute"`, `"relative_withsize"`
#' @param freq_adjust_method Character string indicating which method to use for devising dispersion extremes. See details below. Possible values are `"pervasive"` (default) and `"even"`
#'
#' @author Lukas Soenning
#'
#' @details The function calculates the dispersion measure 'range' based on a set of subfrequencies (number of occurrences of the item in each corpus part) and a matching set of part sizes (the size of the corpus parts, i.e. number of word tokens). Three different types of range measures can be calculated:
#'
#' - Absolute range: The number of corpus parts containing at least one occurrence of the item
#' - Relative range: The proportion of corpus parts containing at least one occurrence of the item; this version of 'range' follows the conventional scaling of dispersion measures (1 = widely dispersed)
#' - Relative range with size (see Gries 2022: 179-180; Gries 2024: 27-28): Relative range that takes into account the size of the corpus parts. Each corpus part contributes to this version of range in proportion to its size. Suppose there are 100 corpus parts, and part 1 is relatively short, accounting for 1/200 of the words in the whole corpus. If the item occurs in part 1, ordinary relative range increases by 1/100, since each part receives the same weight. Relative range with size, on the other hand, increases by 1/200, i.e. the relative size of the corpus part; this version of range weights corpus parts proportionate to their size.
#'
#' - Frequency adjustment: Dispersion scores can be adjusted for frequency using the min-max transformation proposed by Gries (2022: 184-191; 2024: 196-208). The frequency-adjusted score for an  item considers the lowest and highest possible level of dispersion it can obtain given its overall corpus frequency as well as the number (and size) of corpus parts. The unadjusted score is then expressed relative to these endpoints, where the dispersion minimum is set to 0, and the dispersion maximum to 1 (expressed in terms of conventional scaling). The frequency-adjusted score falls between these bounds and expresses how close the observed distribution is to the theoretical maximum and minimum. This adjustment therefore requires a maximally and a minimally dispersed distribution of the item across the parts. These hypothetical extremes can be built in different ways. The method used by Gries (2022, 2024) uses a computationally expensive procedure that finds the distribution that produces the highest value on the dispersion measure of interest. The current function constructs extreme distributions in a different way, based on the distributional features pervasiveness (`"pervasive"`) or evenness (`"even"`). You can choose between these with the argument `freq_adjust_method`; the default is `even`. For details and explanations, see `vignette("frequency-adjustment")`. 
#' 
#'    - To obtain the lowest possible level of dispersion, the occurrences are either allocated to as few corpus parts as possible (`"pervasive"`), or they are assigned to the smallest corpus part(s) (`"even"`).
#'    - To obtain the highest possible level of dispersion, the occurrences are either spread as broadly across corpus parts as possible (`"pervasive"`), or they are allocated to corpus parts in proportion to their size (`"even"`). The choice between these methods is particularly relevant if corpus parts differ considerably in size. See documentation for `find_max_disp()`.
#' 
#' 
#' @returns A numeric value
#'
#' @references
#' 
#' Gries, Stefan Th. 2022. What do (most of) our dispersion measures measure (most)? Dispersion? \emph{Journal of Second Language Studies} 5(2). 171--205. \doi{doi:10.1075/jsls.21029.gri}
#'
#' Gries, Stefan Th. 2024. \emph{Frequency, dispersion, association, and keyness: Revising and tupleizing corpus-linguistic measures}. Amsterdam: Benjamins. \doi{doi:10.1075/scl.115}
#'
#'
#' @export
#'
#' @examples
#' disp_R(
#'   subfreq = c(0, 0, 1, 2, 5),
#'   partsize = rep(1000, 5),
#'   type = "relative",
#'   freq_adjust = FALSE)
#'
disp_R <- function(subfreq,
                   partsize,
                   type = "relative",
                   freq_adjust = FALSE,
                   freq_adjust_method = "pervasive",
                   unit_interval = TRUE,
                   digits = NULL,
                   verbose = TRUE,
                   print_score = TRUE,
                   suppress_warning = FALSE) {
  
  if (length(subfreq) != length(partsize)){
    stop("Lengths of the variables 'subfreq' and 'partsize' differ.")
  }
  
  calculate_R <- function(subfreq, partsize, type){
    w_i <- partsize / sum(partsize)
    
    if (type == "absolute") {
      R <- sum(subfreq > 0, na.rm = TRUE)
    } else if (type == "relative_withsize") {
      R <- sum(w_i[subfreq > 0])
    } else {
      R <- sum(subfreq > 0, na.rm = TRUE) / length(subfreq)
    }
  }
  
  if (sum(subfreq) == 0){
    output <- NA
    
  } else {
    
    R_score <- calculate_R(subfreq, partsize, type)
    output <- R_score
    
    if (freq_adjust == TRUE){
      
      subfreq_min_disp <- find_min_disp(
        subfreq, 
        partsize,
        freq_adjust_method)
      
      subfreq_max_disp <- find_max_disp(
        subfreq, 
        partsize,
        freq_adjust_method)
      
      R_min <- calculate_R(subfreq_min_disp, partsize, type)
      R_max <- calculate_R(subfreq_max_disp, partsize, type)
      
      output <- (R_score - R_min) / (R_max - R_min)
      
      item_exceeds_limits <- FALSE
      if (unit_interval){
        item_exceeds_limits <- sum(output < 0 | output > 1, na.rm = TRUE) > 0
        output[output > 1] <- 1
        output[output < 0] <- 0
      }
      
    }
  }
  
  if (freq_adjust == TRUE){
    if (type == "absolute") names(output) <- "Rabs_nofreq"
    else if (type == "relative_withsize") names(output) <- "Rrel_withsize_nofreq"
    else names(output) <- "Rrel_nofreq"
  } else {
    if (type == "absolute") names(output) <- "Rabs"
    else if (type == "relative_withsize") names(output) <- "Rrel_withsize"
    else names(output) <- "Rrel"
  }
  
  if (!is.null(digits)) output <- round(output, digits)
  
  if (print_score == TRUE) print(output)
  
  if (sum(subfreq) == 0 & suppress_warning == FALSE){
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
      if (type == "absolute") {
        message("\nScores represent absolute range, i.e. the number of corpus parts")
        message("  containing at least one occurrence of the item.\n")
      } else if (type == "relative_withsize") {
        message("\nScores represent relative range, i.e. the proportion of corpus parts")
        message("  containing at least one occurrence of the item. The size of the")
        message("  corpus parts is taken into account, see Gries (2022: 179-180),")
        message("  Gries (2024: 27-28)\n")
      } else {
        message("\nScores represent relative range, i.e. the proportion of corpus parts")
        message("  containing at least one occurrence of the item. The size of the")
        message("  corpus parts is not taken into account.\n")
      } 
    }
  }
  invisible(output)
}

#' Calculate the dispersion measure 'range' for a term-document matrix
#'
#' @description
#' This function calculates the dispersion measure 'range'. It offers three different versions: 'absolute range' (the number of corpus parts containing at least one occurrence of the item), 'relative range' (the proportion of corpus parts containing at least one occurrence of the item), and 'relative range with size' (relative range that takes into account the size of the corpus parts). The function also offers the option of calculating frequency-adjusted dispersion scores.
#'
#' @inheritParams disp_tdm
#' @inheritParams disp_R
#' @param freq_adjust_method Character string indicating which method to use for devising dispersion extremes. See details below. Possible values are `"pervasive"` (default) and `"even"`
#'
#' @author Lukas Soenning
#'
#' @details 
#' This function takes as input a term-document matrix and returns, for each item (i.e. each row) the dispersion measure 'range'. The rows in the input matrix represent the items, and the columns the corpus parts. Importantly, the term-document matrix must include an additional row that records the size of the corpus parts. For a proper term-document matrix, which includes all items that appear in the corpus, this can be added as a column margin, which sums the frequencies in each column. If the matrix only includes a selection of items drawn from the corpus, this information cannot be derived from the matrix and must be provided as a separate row.
#' 
#' Three different types of range measures can be calculated:
#'
#' - Absolute range: The number of corpus parts containing at least one occurrence of the item
#' - Relative range: The proportion of corpus parts containing at least one occurrence of the item; this version of 'range' follows the conventional scaling of dispersion measures (1 = widely dispersed)
#' - Relative range with size (see Gries 2022: 179-180; Gries 2024: 27-28): Relative range that takes into account the size of the corpus parts. Each corpus part contributes to this version of range in proportion to its size. Suppose there are 100 corpus parts, and part 1 is relatively short, accounting for 1/200 of the words in the whole corpus. If the item occurs in part 1, ordinary relative range increases by 1/100, since each part receives the same weight. Relative range with size, on the other hand, increases by 1/200, i.e. the relative size of the corpus part; this version of range weights corpus parts proportionate to their size.
#'
#' - Frequency adjustment: Dispersion scores can be adjusted for frequency using the min-max transformation proposed by Gries (2022: 184-191; 2024: 196-208). The frequency-adjusted score for an  item considers the lowest and highest possible level of dispersion it can obtain given its overall corpus frequency as well as the number (and size) of corpus parts. The unadjusted score is then expressed relative to these endpoints, where the dispersion minimum is set to 0, and the dispersion maximum to 1 (expressed in terms of conventional scaling). The frequency-adjusted score falls between these bounds and expresses how close the observed distribution is to the theoretical maximum and minimum. This adjustment therefore requires a maximally and a minimally dispersed distribution of the item across the parts. These hypothetical extremes can be built in different ways. The method used by Gries (2022, 2024) uses a computationally expensive procedure that finds the distribution that produces the highest value on the dispersion measure of interest. The current function constructs extreme distributions in a different way, based on the distributional features pervasiveness (`"pervasive"`) or evenness (`"even"`). You can choose between these with the argument `freq_adjust_method`; the default is `"even"`. For details and explanations, see `vignette("frequency-adjustment")`. 
#' 
#'    - To obtain the lowest possible level of dispersion, the occurrences are either allocated to as few corpus parts as possible (`"pervasive"`), or they are assigned to the smallest corpus part(s) (`"even"`).
#'    - To obtain the highest possible level of dispersion, the occurrences are either spread as broadly across corpus parts as possible (`"pervasive"`), or they are allocated to corpus parts in proportion to their size (`"even"`). The choice between these methods is particularly relevant if corpus parts differ considerably in size. See documentation for `find_max_disp()`.
#' 
#' 
#' @returns A data frame with one row per item
#'
#' @references
#' 
#' Gries, Stefan Th. 2022. What do (most of) our dispersion measures measure (most)? Dispersion? \emph{Journal of Second Language Studies} 5(2). 171--205. \doi{doi:10.1075/jsls.21029.gri}
#'
#' Gries, Stefan Th. 2024. \emph{Frequency, dispersion, association, and keyness: Revising and tupleizing corpus-linguistic measures}. Amsterdam: Benjamins. \doi{doi:10.1075/scl.115}
#'
#'
#' @export
#'
#' @examples
#' disp_R_tdm(
#'   tdm = biber150_spokenBNC2014[1:20,],
#'   row_partsize = "first",
#'   type = "relative",
#'   freq_adjust = FALSE)
#'
disp_R_tdm <- function(tdm,
                       row_partsize = "first",
                       type = "relative",
                       freq_adjust = FALSE,
                       freq_adjust_method = "pervasive",
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
    
    R_score <- apply(
      tdm[-1,],
      1,
      function(x){
        disp_R(subfreq = x, 
               partsize = tdm[1,],
               type,
               freq_adjust = FALSE,
               unit_interval = FALSE,
               verbose = FALSE,
               digits = NULL,
               print_score = FALSE,
               suppress_warning = TRUE)
      })
    
  } else if (row_partsize == "last"){
    
    if (!all(colSums(tdm[-nrow(tdm),]) <= tdm[nrow(tdm),])){
      stop("The row you indicated (last row) does not contain the (correct) part sizes.\n  Use argument 'row_partsize' to locate the correct row or check content of\n  last row. At the moment, (some) counts in the last row are too small.")
    }
    
    R_score <- apply(
      tdm[-nrow(tdm),],
      1,
      function(x){
        disp_R(subfreq = x, 
               partsize = tdm[nrow(tdm),],
               type,
               freq_adjust = FALSE,
               unit_interval = FALSE,
               verbose = FALSE,
               digits = NULL,
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
      
      R_min <- apply(
        min_disp_tdm[-1,],
        1,
        function(x){
          disp_R(subfreq = x, 
                 partsize = min_disp_tdm[1,],
                 type,
                 freq_adjust = FALSE,
                 unit_interval = FALSE,
                 verbose = FALSE,
                 digits = NULL,
                 print_score = FALSE,
                 suppress_warning = TRUE)
        })
      
      R_max <- apply(
        max_disp_tdm[-1,],
        1,
        function(x){
          disp_R(subfreq = x, 
                 partsize = max_disp_tdm[1,],
                 type,
                 freq_adjust = FALSE,
                 unit_interval = FALSE,
                 verbose = FALSE,
                 digits = NULL,
                 print_score = FALSE,
                 suppress_warning = TRUE)
        })
      
    } else if (row_partsize == "last"){
      
      R_min <- apply(
        min_disp_tdm[-nrow(min_disp_tdm),],
        1,
        function(x){
          disp_R(subfreq = x, 
                 partsize = min_disp_tdm[nrow(min_disp_tdm),],
                 type,
                 freq_adjust = FALSE,
                 unit_interval = FALSE,
                 verbose = FALSE,
                 digits = NULL,
                 print_score = FALSE,
                 suppress_warning = TRUE)
        })
      
      R_max <- apply(
        max_disp_tdm[-nrow(max_disp_tdm),],
        1,
        function(x){
          disp_R(subfreq = x, 
                 partsize = max_disp_tdm[nrow(max_disp_tdm),],
                 type,
                 freq_adjust = FALSE,
                 unit_interval = FALSE,
                 verbose = FALSE,
                 digits = NULL,
                 print_score = FALSE,
                 suppress_warning = TRUE)
        })
    }
    output <- (R_score - R_min) / (R_max - R_min)
    
    if (unit_interval){
      
      n_items_exceeding_limits <- sum(
        output < 0 | output > 1, na.rm = TRUE) 
      
      output[output > 1] <- 1
      output[output < 0] <- 0
    }
    
  } else {
    output <- R_score
  }
  
  output <- t(output)
  
  if (!is.null(digits)) output <- round(output, digits)
  
  output <- data.frame(
    item = colnames(output),
    Rrel = as.numeric(output)
  )
  
  if (freq_adjust == TRUE & type == "relative"){
    colnames(output)[2] <- "Rrel_nofreq"
  }
  if (freq_adjust == FALSE & type == "abolute"){
    colnames(output)[2] <- "Rabs"
  }
  if (freq_adjust == TRUE & type == "abolute"){
    colnames(output)[2] <- "Rabs_nofreq"
  }
  if (freq_adjust == FALSE & type == "relative_withsize"){
    colnames(output)[2] <- "Rrel_withsize"
  }
  if (freq_adjust == TRUE & type == "relative_withsize"){
    colnames(output)[2] <- "Rrel_withsize_nofreq"
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
    if (type == "absolute") {
      message("\nScores represent absolute range, i.e. the number of corpus parts")
      message("  containing at least one occurrence of the item.\n")
    } else if (type == "relative") {
      message("\nScores represent relative range, i.e. the proportion of corpus parts")
      message("  containing at least one occurrence of the item. The size of the")
      message("  corpus parts is not taken into account.\n")
    } else if (type == "relative_withsize") {
      message("\nScores represent relative range, i.e. the proportion of corpus parts")
      message("  containing at least one occurrence of the item. The size of the")
      message("  corpus parts is taken into account, see Gries (2022: 179-180),")
      message("  Gries (2024: 27-28)\n")
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
