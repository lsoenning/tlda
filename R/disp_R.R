#' Calculate the dispersion measure 'range'
#'
#' @description
#' This function calculates the dispersion measure 'range'. It offers three different versions: 'absolute range' (the number of corpus parts containing at least one occurrence of the item), 'relative range' (the proportion of corpus parts containing at least one occurrence of the item), and 'relative range with size' (relative range that takes into account the size of the corpus parts). The function also offers the option of calculating frequency-adjusted dispersion scores.
#'
#' @inheritParams disp
#' @param type Character string indicating which type of range to calculate. See details below. Possible values are \code{'relative'} (default), \code{'absolute'}, \code{'relative_withsize'}
#'
#' @author Lukas Soenning
#'
#' @details The function calculates the dispersion measure 'range' based on a set of subfrequencies (number of occurrences of the item in each corpus part) and a matching set of part sizes (the size of the corpus parts, i.e. number of word tokens). Three different types of range measures can be calculated:
#'
#' - Absolute range: The number of corpus parts containing at least one occurrence of the item
#' - Relative range: The proportion of corpus parts containing at least one occurrence of the item; this version of 'range' follows the conventional scaling of dispersion measures (1 = widely dispersed)
#' - Relative range with size (see Gries 2022: 179-180; Gries 2024: 27-28): Relative range that takes into account the size of the corpus parts. Each corpus part contributes to this version of range in proportion to its size. Suppose there are 100 corpus parts, and part 1 is relatively short, accounting for 1/200 of the words in the whole corpus. If the item occurs in part 1, ordinary relative range increases by 1/100, since each part receives the same weight. Relative range with size, on the other hand, increases by 1/200, i.e. the relative size of the corpus part; this version of range weights corpus parts proportionate to their size.
#'
#' - Frequency adjustment: Dispersion scores can be adjusted for frequency using a variant of the min-max transformation proposed by Gries (2022, 2024). The frequency-adjusted score for a specific item considers its dispersion potential, the lowest and highest possible score it can obtain given its overall corpus frequency as well as the number and size of corpus parts. The unadjusted score is then expressed relative to these endpoints, where the dispersion pessimum is set to 0, and the dispersion optimum to 1 (expressed in terms of conventional scaling). The frequency-adjusted dispersion score falls between these bounds and expresses how close the observed distribution is to the theoretical maximum and minimum. This adjustment therefore requires a maximally and a minimally dispersed distribution of the item across the parts. These hypothetical extremes can be built in different ways. In particular, different strategies exist for finding a distribution that yields the dispersion maximum. The method used by Gries (2024) uses a computationally expensive procedure that finds the distribution that produces the highest value on the dispersion measure of interest. The current function constructs extreme distributions independently of the dispersion measures used and therefore only proved approximations to the upper bound yielded by Gries's method. 
#' 
#'    - To obtain the lowest possible level of dispersion, the occurrences are allocated to the smallest corpus parts. The function starts by filling in the smallest part. If the total number of occurrences of the items is greater than the size of this part, it then continues with the second smallest corpus part and so on. This approach is very similar to that used in Gries (2024).
#'    - To obtain the highest possible level of dispersion, two methods are available, and these can be set with the argument \code{freq_adjust_method}. The choice between these methods is particularly relevant if corpus parts differ considerably in size. See documentation for \code{find_max_disp()}.
#' 
#' 
#' @return A numeric value
#'
#' @references
#' 
#' - Gries, Stefan Th. 2022. What do (most of) our dispersion measures measure (most)? Dispersion? \emph{Journal of Second Language Studies} 5(2). 171–205. \url{https://doi.org/10.1075/jsls.21029.gri}
#'
#' - Gries, Stefan Th. 2024. \emph{Frequency, dispersion, association, and keyness: Revising and tupleizing corpus-linguistic measures}. Amsterdam: Benjamins. \url{https://doi.org/10.1075/scl.115}
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # not run
#' disp_R(
#'   subfreq = c(0, 0, 1, 2, 5),
#'   partsize = rep(1000, 5),
#'   type = "relative",
#'   freq_adjust = FALSE)
#' }
#'
disp_R <- function(subfreq,
                   partsize,
                   type = "relative",
                   freq_adjust = FALSE,
                   freq_adjust_method = "pervasive",
                   digits = NULL,
                   verbose = TRUE,
                   print_score = TRUE) {
  
  calculate_R <- function(subfreq, partsize){
    w_i <- partsize / sum(partsize)
    
    if (type == "absolute") {
      R <- sum(subfreq > 0)
    } else if (type == "relative") {
      R <- sum(subfreq > 0) / length(subfreq)
    } else if (type == "relative_withsize") {
      R <- sum(w_i[subfreq > 0])
    }
  }
    
  if (sum(subfreq) == 0){
    output <- NA
    
  } else {
    
    R_score <- calculate_R(subfreq, partsize)
    output <- R_score
    
    if (freq_adjust == TRUE){
      
      subfreq_min_disp <- find_min_disp(
        subfreq, 
        partsize)
      
      subfreq_max_disp <- find_max_disp(
        subfreq, 
        partsize)
      
      R_min <- calculate_R(subfreq_min_disp, partsize)
      R_max <- calculate_R(subfreq_max_disp, partsize)
      
      # frequency-adjusted version
      output <- (R_score - R_min) / (R_max - R_min)
      
    }
  }
  
  if (freq_adjust == TRUE){
    if (type == "absolute") names(output) <- "Rabs_nofreq"
    if (type == "relative") names(output) <- "Rrel_nofreq"
    if (type == "relative_withsize") names(output) <- "Rrel_withsize_nofreq"
  } else {
    if (type == "absolute") names(output) <- "Rabs"
    if (type == "relative") names(output) <- "Rrel"
    if (type == "relative_withsize") names(output) <- "Rrel_withsize"
    }

  if (!is.null(digits)) output <- round(output, digits)
  
  if (print_score == TRUE) print(output)
  
  if (verbose) {
    if (freq_adjust == TRUE){
      message("\nDispersion scores are adjusted for frequency using the min-max")
      message("  transformation (see Gries 2024: 196-208)")
    }
    if (type == "absolute") {
      message("\nScores represent absolute range, i.e. the number of corpus parts")
      message("  containing at least one occurrence of the item.")
    } else if (type == "relative") {
      message("\nScores represent relative range, i.e. the proportion of corpus parts")
      message("  containing at least one occurrence of the item.")
      message("The size of the corpus parts is not taken into account.")
    } else if (type == "relative_withsize") {
      message("\nScores represent relative range, i.e. the proportion of corpus parts")
      message("  containing at least one occurrence of the item.")
      message("Size of corpus parts is taken into account, see Gries (2022: 179-180),")
      message("  Gries (2024: 27-28)")
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
#'
#' @author Lukas Soenning
#'
#' @details 
#' This function takes as input a term-document matrix and returns, for each item (i.e. each row) the dispersion measure 'range'. The rows in the matrix represent the items, and the columns the corpus parts. Importantly, the term-document matrix must include an additional row that records the size of the corpus parts. For a proper term-document matrix, which includes all items that appear in the corpus, this can be added as a column margin, which sums the frequencies in each column. If the matrix only includes a selection of items drawn from the corpus, this information cannot be derived from the matrix and must be provided as a separate row.
#' 
#' Three different types of range measures can be calculated:
#'
#' - Absolute range: The number of corpus parts containing at least one occurrence of the item
#' - Relative range: The proportion of corpus parts containing at least one occurrence of the item; this version of 'range' follows the conventional scaling of dispersion measures (1 = widely dispersed)
#' - Relative range with size (see Gries 2022: 179-180; Gries 2024: 27-28): Relative range that takes into account the size of the corpus parts. Each corpus part contributes to this version of range in proportion to its size. Suppose there are 100 corpus parts, and part 1 is relatively short, accounting for 1/200 of the words in the whole corpus. If the item occurs in part 1, ordinary relative range increases by 1/100, since each part receives the same weight. Relative range with size, on the other hand, increases by 1/200, i.e. the relative size of the corpus part; this version of range weights corpus parts proportionate to their size.
#'
#' - Frequency adjustment: Dispersion scores can be adjusted for frequency using a variant of the min-max transformation proposed by Gries (2022, 2024). The frequency-adjusted score for a specific item considers its dispersion potential, the lowest and highest possible score it can obtain given its overall corpus frequency as well as the number and size of corpus parts. The unadjusted score is then expressed relative to these endpoints, where the dispersion pessimum is set to 0, and the dispersion optimum to 1 (expressed in terms of conventional scaling). The frequency-adjusted dispersion score falls between these bounds and expresses how close the observed distribution is to the theoretical maximum and minimum. This adjustment therefore requires a maximally and a minimally dispersed distribution of the item across the parts. These hypothetical extremes can be built in different ways. In particular, different strategies exist for finding a distribution that yields the dispersion maximum. The method used by Gries (2024) uses a computationally expensive procedure that finds the distribution that produces the highest value on the dispersion measure of interest. The current function constructs extreme distributions independently of the dispersion measures used and therefore only proved approximations to the upper bound yielded by Gries's method. 
#' 
#'    - To obtain the lowest possible level of dispersion, the occurrences are allocated to the smallest corpus parts. The function starts by filling in the smallest part. If the total number of occurrences of the items is greater than the size of this part, it then continues with the second smallest corpus part and so on. This approach is very similar to that used in Gries (2024).
#'    - To obtain the highest possible level of dispersion, two methods are available, and these can be set with the argument \code{freq_adjust_method}. The choice between these methods is particularly relevant if corpus parts differ considerably in size. See documentation for \code{find_max_disp()}.
#' 
#' 
#' @return A numeric matrix
#'
#' @references
#' 
#' - Gries, Stefan Th. 2022. What do (most of) our dispersion measures measure (most)? Dispersion? \emph{Journal of Second Language Studies} 5(2). 171–205. \url{https://doi.org/10.1075/jsls.21029.gri}
#'
#' - Gries, Stefan Th. 2024. \emph{Frequency, dispersion, association, and keyness: Revising and tupleizing corpus-linguistic measures}. Amsterdam: Benjamins. \url{https://doi.org/10.1075/scl.115}
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # not run
#' disp_R_tdm(
#'   tdm = biber150_spokenBNC2014[1:20,]
#'   row_partsize = "first_row",
#'   type = "relative",
#'   freq_adjust = FALSE)
#' }
#'
disp_R_tdm <- function(tdm,
                       row_partsize = "first_row",
                   type = "relative",
                   freq_adjust = FALSE,
                   freq_adjust_method = "pervasive",
                   digits = NULL,
                   verbose = TRUE,
                   print_score = TRUE) {
  
  if (row_partsize == "first_row"){
    
    R_score <- apply(
      tdm[-1,],
      1,
      function(x){
        disp_R(subfreq = x, 
                partsize = tdm[1,],
                verbose = FALSE,
                print_score = FALSE)
      })
    
  } else if (row_partsize == "last_row"){
    
    R_score <- apply(
      tdm[-nrow(tdm),],
      1,
      function(x){
        disp_R(subfreq = x, 
                partsize = tdm[nrow(tdm),],
                verbose = FALSE,
                print_score = FALSE)
      })
  }
  
  if (freq_adjust == TRUE){
    
    min_disp_tdm <- find_min_disp_tdm(tdm)
    max_disp_tdm <- find_max_disp_tdm(tdm)
    
    if (row_partsize == "first_row"){
      
      R_min <- apply(
        min_disp_tdm[-1,],
        1,
        function(x){
          disp_R(subfreq = x, 
                  partsize = min_disp_tdm[1,],
                  verbose = FALSE,
                  print_score = FALSE)
        })
      
      R_max <- apply(
        max_disp_tdm[-1,],
        1,
        function(x){
          disp_R(subfreq = x, 
                  partsize = max_disp_tdm[1,],
                  verbose = FALSE,
                  print_score = FALSE)
        })
      
    } else if (row_partsize == "last_row"){
      
      R_min <- apply(
        min_disp_tdm[-nrow(min_disp_tdm),],
        1,
        function(x){
          disp_R(subfreq = x, 
                  partsize = min_disp_tdm[nrow(min_disp_tdm),],
                  verbose = FALSE,
                  print_score = FALSE)
        })
      
      R_max <- apply(
        max_disp_tdm[-nrow(max_disp_tdm),],
        1,
        function(x){
          disp_R(sbfreq = x, 
                  partsize = max_disp_tdm[nrow(max_disp_tdm),],
                  verbose = FALSE,
                  print_score = FALSE)
        })
    }
    output <- (R_score - R_min) / (R_max - R_min)
    
  } else {
    output <- R_score
  }
  
  output <- t(output)

  if (!is.null(digits)) output <- round(output, digits)
  
  if (print_score == TRUE) print(output)
  
  if (verbose) {
    if (freq_adjust == TRUE){
      message("\nDispersion scores are adjusted for frequency using the min-max")
      message("  transformation (see Gries 2024: 196-208)")
    }
    if (type == "absolute") {
      message("\nScores represent absolute range, i.e. the number of corpus parts")
      message("  containing at least one occurrence of the item.")
    } else if (type == "relative") {
      message("\nScores represent relative range, i.e. the proportion of corpus parts")
      message("  containing at least one occurrence of the item.")
      message("The size of the corpus parts is not taken into account.")
    } else if (type == "relative_withsize") {
      message("\nScores represent relative range, i.e. the proportion of corpus parts")
      message("  containing at least one occurrence of the item.")
      message("Size of corpus parts is taken into account, see Gries (2022: 179-180),")
      message("  Gries (2024: 27-28)")
    }
  }
  invisible(output)
}
