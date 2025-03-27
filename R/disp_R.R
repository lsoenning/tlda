#' Calculate the dispersion measure 'range'
#'
#' @description
#' This function calculates the dispersion measure 'range'. It offers three different versions: 'absolute range' (the number of corpus parts containing at least one occurrence of the item), 'relative range' (the proportion of corpus parts containing at least one occurrence of the item), and 'relative range with size' (relative range that takes into account the size of the corpus parts).
#'
#' @param subfreq A numeric vector of subfrequencies, i.e. the number of occurrences of the item in each corpus part
#' @param partsize A numeric vector with the size of the corpus parts
#' @param type Character string indicating which type of range to calculate. See details below. Possible values are \code{'relative'} (default), \code{'absolute'}, \code{'relative_withsize'}
#' @param verbose Logical. Whether additional information should be printed, default is TRUE
#'
#' @author Lukas Soenning
#'
#' @details The function calculates the dispersion measure 'range' based on a set of subfrequencies (number of occurrences of the item in each corpus part) and a matching set of part sizes (the size of the corpus parts, i.e. number of word tokens). Three different types of range measures can be calculated:
#'
#' - Absolute range: The number of corpus parts containing at least one occurrence of the item
#' - Relative range: The proportion of corpus parts containing at least one occurrence of the item; this version of 'range' follows the conventional scaling of dispersion measures (1 = widely dispersed)
#' - Relative range with size (see Gries 2022: 179-180; Gries 2024: 27-28): Relative range that takes into account the size of the corpus parts. Each corpus part contributes to this version of range in proportion to its size. Suppose there are 100 corpus parts, and part 1 is relatively short, accounting for 1/200 of the words in the whole corpus. If the item occurs in part 1, ordinary relative range increases by 1/100, since each part receives the same weight. Relative range with size, on the other hand, increases by 1/200, i.e. the relative size of the corpus part; this version of range weights corpus parts proportionate to their size.
#'

#' @return Returns a numeric value, the dispersion score.
#'
##' @references
#'
#' - Gries, Stefan Th. 2024. \emph{Frequency, dispersion, association, and keyness: Revising and tupleizing corpus-linguistic measures}. Amsterdam: Benjamins. \url{https://doi.org/10.1075/scl.115}
#'
#' - Gries, Stefan Th. 2022. What do (most of) our dispersion measures measure (most)? Dispersion? \emph{Journal of Second Language Studies} 5(2). 171–205. \url{https://doi.org/10.1075/jsls.21029.gri}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # not run
#' disp_R(
#'   subfreq = c(0, 0, 1, 2, 5),
#'   partsize = rep(1000, 5),
#'   type = "relative"
#' )
#' }
#'
disp_R <- function(subfreq,
                   partsize,
                   type = "relative",
                   verbose = TRUE) {
  w_i <- partsize / sum(partsize)

  if (type == "absolute") {
    R <- sum(subfreq > 0)
    names(R) <- c("R_abs")
  } else if (type == "relative") {
    R <- sum(subfreq > 0) / length(subfreq)
    names(R) <- c("R_rel")
  } else if (type == "relative_withsize") {
    R <- sum(w_i[subfreq > 0])
    names(R) <- c("R_rel_withsize")
  }

  print(R)

  if (verbose) {
    if (type == "absolute") {
      message("Scores represent absolute range, i.e. the number of corpus parts in which the item")
      message("  appears at least once.")
    } else if (type == "relative") {
      message("Scores represent relative range, i.e. the proportion of corpus parts in which the")
      message("  item appears at least once.")
      message("The size of the corpus parts is not taken into account.\n")
    } else if (type == "relative_withsize") {
      message("Scores represent relative range, i.e. the proportion of corpus parts in which the")
      message("  item appears at least once.")
      message("Size of corpus parts is taken into account, see:")
      message("  Gries (2022: 179-180) [https://doi.org/10.1075/jsls.21029.gri]")
      message("  Gries (2024: 27-28) [https://doi.org/10.1075/scl.115]")
    }
  }

  invisible(R)
}
