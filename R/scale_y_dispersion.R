#' Add annotation to tick marks on y-axis (ggplot2) to clarify the directionality of scaling for dispersion scores
#'
#' @description
#' This function can be used when plotting dispersion scores with ggplot2. It forces the y-axis to extend from 0 to 1 and adds verbal information at the endpoints to clarify the directionality of scaling. For `conventional` scaling, these are "(uneven) 0" and "(even) 1", for `gries` scaling, these are "(even) 0" and "(uneven) 1".
#' 
#' @param directionality Character string indicating the directionality of scaling. Must match the way the dispersion scores were calculated. See details below. Possible values are `"conventional"` and `"gries"`
#' @param n_breaks Number of major scale breaks: Integer value specifying the number of tick marks to display (default: `5`)
#' @param leading_zero Logical. Whether the tick mark labels should include a leading 0 ("0.50") or not (".50"); default is `TRUE`
#' 
#' @author Lukas Soenning
#' 
#' @details This function modifies the y-axis in a ggplot2 object. It forces the axis to extend from 0 to 1 and adds the labels "(even)" and "(uneven)" at the endpoints of the scale (0 and 1), to make clear which value (0 or 1) denotes a maximally even/dispersed/balanced distribution of subfrequencies across corpus parts. The conventional scaling of dispersion measures (see Juilland & Chang-Rodriguez 1964; Carroll 1970; Rosengren 1971) assigns higher values to more even/dispersed/balanced distributions of subfrequencies across corpus parts. In the `{tlda}` package, this is the default setting for all measures. Gries (2008) uses the reverse scaling, with higher values denoting a more uneven/bursty/concentrated distribution; use `directionality = "gries"` to choose this option. The function implements no default, so the user must specify which directionality was used when calculating the scores.
#' 
#' @returns The ggplot2 function scale_y_continuous with the appropriate settings for the argument `limits`, `breaks`, and `labels`.
#'
#' @references 
#' 
#' Carroll, John B. 1970. An alternative to Juilland’s usage coefficient for lexical frequencies and a proposal for a standard frequency index. \emph{Computer Studies in the Humanities and Verbal Behaviour} 3(2). 61--65. \doi{doi:10.1002/j.2333-8504.1970.tb00778.x}
#' 
#' Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. \emph{International Journal of Corpus Linguistics} 13(4). 403--437. \doi{doi:10.1075/ijcl.13.4.02gri}
#' 
#' Juilland, Alphonse G. & Eugenio Chang-Rodríguez. 1964. \emph{Frequency dictionary of Spanish words.} The Hague: Mouton de Gruyter. \doi{doi:10.1515/9783112415467}
#' 
#' Rosengren, Inger. 1971. The quantitative concept of language and its relation to the structure of frequency dictionaries. \emph{Études de linguistique appliquée (Nouvelle Série)} 1. 103--127.
#' 
#' @export
#' 
#' @examples
#' if (require("ggplot2")) {
#'   ggplot(
#'     data = data.frame(
#'       frequency = c(1, 1.5, 2, 2.5, 3, 4, 5),
#'       dispersion = c(.25, .8, .34, .53, .88, .57, .9)),
#'     aes(x = frequency, 
#'         y = dispersion)) +
#'     geom_point() +
#'     scale_y_dispersion(
#'       directionality = "conventional",
#'       n_breaks = 3)
#' }
#' 
scale_y_dispersion <- function(directionality, 
                               n_breaks = 5, 
                               leading_zero = TRUE){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }
  breaks_positions <- seq(0, 1, length = n_breaks)
  
  if(isTRUE(leading_zero)){
    breaks_labels <- format(round(breaks_positions, 2), nsmall = 2)
  } else {
    breaks_labels <- 
      {ncode <- paste0("%.", 2, "f")
      sub("^(-?)0.", "\\1.", sprintf(ncode, breaks_positions))}
  }
  
  
  if(directionality == "conventional"){
    breaks_labels[1] <- "(uneven) 0"
    breaks_labels[n_breaks] <- "(even) 1"
    
  } else if (directionality == "gries"){
    breaks_labels[1] <- "(even) 0"
    breaks_labels[n_breaks] <- "(uneven) 1"
  }
  
  ggplot2::scale_y_continuous(
    limits = c(0, 1), 
    breaks = breaks_positions,
    labels = breaks_labels)	
}


#' Add annotation to tick marks on x-axis (ggplot2) to clarify the directionality of scaling for dispersion scores
#'
#' @description
#' This function can be used when plotting dispersion scores with ggplot2. It forces the x-axis to extend from 0 to 1 and adds verbal information at the endpoints to clarify the directionality of scaling. For `conventional` scaling, these are "(uneven) 0" and "(even) 1", for `gries` scaling, these are "(even) 0" and "(uneven) 1".
#' 
#' @param directionality Character string indicating the directionality of scaling. Must match the way the dispersion scores were calculated. See details below. Possible values are `"conventional"` and `"gries"`
#' @param n_breaks Number of major scale breaks: Integer value specifying the number of tick marks to display (default: `5`)
#' @param leading_zero Logical. Whether the tick mark labels should include a leading 0 ("0.50") or not (".50"); default is `TRUE`
#' 
#' @author Lukas Soenning
#' 
#' @details This function modifies the x-axis in a ggplot2 object. It forces the axis to extend from 0 to 1 and adds the labels "(even)" and "(uneven)" at the endpoints of the scale (0 and 1), to make clear which value (0 or 1) denotes a maximally even/dispersed/balanced distribution of subfrequencies across corpus parts. The conventional scaling of dispersion measures (see Juilland & Chang-Rodriguez 1964; Carroll 1970; Rosengren 1971) assigns higher values to more even/dispersed/balanced distributions of subfrequencies across corpus parts. In the `{tlda}` package, this is the default setting for all measures. Gries (2008) uses the reverse scaling, with higher values denoting a more uneven/bursty/concentrated distribution; use `directionality = "gries"` to choose this option. The function implements no default, so the user must specify which directionality was used when calculating the scores.
#' 
#' @returns The ggplot2 function scale_x_continuous with the appropriate settings for the argument `limits`, `breaks`, and `labels`.
#'
#' @references 
#' 
#' Carroll, John B. 1970. An alternative to Juilland’s usage coefficient for lexical frequencies and a proposal for a standard frequency index. \emph{Computer Studies in the Humanities and Verbal Behaviour} 3(2). 61--65. \doi{doi:10.1002/j.2333-8504.1970.tb00778.x}
#' 
#' Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. \emph{International Journal of Corpus Linguistics} 13(4). 403--437. \doi{doi:10.1075/ijcl.13.4.02gri}
#' 
#' Juilland, Alphonse G. & Eugenio Chang-Rodríguez. 1964. \emph{Frequency dictionary of Spanish words.} The Hague: Mouton de Gruyter. \doi{doi:10.1515/9783112415467}
#' 
#' Rosengren, Inger. 1971. The quantitative concept of language and its relation to the structure of frequency dictionaries. \emph{Études de linguistique appliquée (Nouvelle Série)} 1. 103--127.
#' 
#' @export
#' 
#' @examples
#' if (require("ggplot2")) {
#'   ggplot(
#'     data = data.frame(
#'       dispersion = stats::runif(100, 0, 1)),
#'     aes(x = dispersion)) +
#'     geom_dotplot() +
#'     scale_x_dispersion(
#'       directionality = "conventional",
#'       n_breaks = 5)
#' }
#' 
scale_x_dispersion <- function(directionality, 
                               n_breaks = 5, 
                               leading_zero = TRUE){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }
  breaks_positions <- seq(0, 1, length = n_breaks)
  
  if(isTRUE(leading_zero)){
    breaks_labels <- format(round(breaks_positions, 2), nsmall = 2)
  } else {
    breaks_labels <- 
      {ncode <- paste0("%.", 2, "f")
      sub("^(-?)0.", "\\1.", sprintf(ncode, breaks_positions))}
  }
  
  
  if(directionality == "conventional"){
    breaks_labels[1] <- "0\n(uneven)"
    for(i in 2:(n_breaks-1)){
      for(i in 2:(n_breaks-1)) breaks_labels[i] <- paste0(breaks_labels[i], "\n")
    }
    breaks_labels[n_breaks] <- "1\n(even)"
    
  } else if (directionality == "gries"){
    breaks_labels[1] <- "0\n(even)"
    if(n_breaks >=3){
      for(i in 2:(n_breaks-1)){
        breaks_labels[i] <- paste0(breaks_labels[i], "\n")
      } 
    }
    breaks_labels[n_breaks] <- "1\n(uneven)"
  }
  
  ggplot2::scale_x_continuous(
    limits = c(0, 1), 
    breaks = breaks_positions,
    labels = breaks_labels,
    ...)
}
