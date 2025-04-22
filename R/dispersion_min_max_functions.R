#' Find the minimally dispersed distribution of an item across corpus parts
#' 
#' @description
#' This function returns the (hypothetical) distribution of subfrequencies that represents the smallest possible level of dispersion for a given item across a particular set of corpus parts. It requires a vector of subfrequencies and a vector of corpus part sizes. This distribution is required for the min-max transformation proposed by Gries (2022: 184-191; 2024: 196-208) to obtain frequency-adjusted dispersion scores.
#' 
#' @inheritParams disp
#'
#' @returns An integer vector the same length as `partsize`
#' 
#' 
#' @references
#' 
#' Gries, Stefan Th. 2022. What do (most of) our dispersion measures measure (most)? Dispersion? \emph{Journal of Second Language Studies} 5(2). 171--205. \doi{doi:10.1075/jsls.21029.gri}
#' 
#' Gries, Stefan Th. 2024. \emph{Frequency, dispersion, association, and keyness: Revising and tupleizing corpus-linguistic measures}. Amsterdam: Benjamins. \doi{doi:10.1075/scl.115}
#' 
#' Gries, Stefan Th. 2025. \emph{KLD4C: Gries 2024: Tupleization of corpus linguistics}. R package version 1.01. (available from https://www.stgries.info/research/kld4c/kld4c.html)
#' 
#' 
#' @author Lukas Soenning
#' 
#' @details This function creates a hypothetical distribution of the total number of occurrences of the item (i.e. the sum of its subfrequencies) across corpus parts. To obtain the lowest possible level of dispersion, the argument `freq_adjust_method` allows the user to choose between two distributional features: pervasiveness (`pervasive`) or evenness (`even`). For details and explanations, see `vignette("frequency-adjustment")`. To obtain the lowest possible level of dispersion, the occurrences are either allocated to as few corpus parts as possible (`pervasiveness`), or they are assigned to the smallest corpus part(s) (`even`). Since the dispersion of items that occur only once in the corpus (hapaxes) cannot be sensibly measured or manipulated, such items are disregarded; the function returns their observed subfrequencies. The function reuses code segments from Gries's (2025) 'KLD4C' package (from the function `most.uneven.distr()`).
#' 
#' @export
#'
#' @examples
#' find_min_disp(
#'   subfreq = c(0,0,1,2,5), 
#'   partsize = rep(1000, 5),
#'   freq_adjust_method = "even")
#'
find_min_disp <- function(
    subfreq, 
    partsize,
    freq_adjust_method = "even"){
  
  if (length(subfreq) != length(partsize)){
    stop("Lengths of the variables 'subfreq' and 'partsize' differ.")
  }
  
  if (sum(subfreq) == 1) {
    output <- subfreq
    
  } else {
    
    if (freq_adjust_method == "pervasive"){
      
      output <- rep(0, length(partsize))
      
      occurrences_to_allocate <- sum(subfreq)
      
      if (occurrences_to_allocate <= max(partsize)) {
        output[which.max(partsize)] <- occurrences_to_allocate
      }
      else {
        partsize_order <- order(partsize, decreasing = TRUE)

        for (i in seq(partsize_order)) {
          curr <- partsize_order[i]
          output[curr] <- min(partsize[curr], occurrences_to_allocate)
          occurrences_to_allocate <- occurrences_to_allocate - partsize[curr]
          if (occurrences_to_allocate < 0) {
            break
          }
        }
      }
    } else if (freq_adjust_method == "even"){
      
      output <- rep(0, length(partsize))
      
      occurrences_to_allocate <- sum(subfreq)
      
      if (occurrences_to_allocate <= min(partsize)) {
        output[which.min(partsize)] <- occurrences_to_allocate
      }
      else {
        partsize_order <- order(partsize)
        for (i in seq(partsize_order)) {
          curr <- partsize_order[i]
          output[curr] <- min(partsize[curr], occurrences_to_allocate)
          occurrences_to_allocate <- occurrences_to_allocate - partsize[curr]
          if (occurrences_to_allocate < 0) {
            break
          }
        }
      }
    }
  }
  return(output)
}


#' Find the maximally dispersed distribution of an item across corpus parts
#' 
#' @description
#' This function returns the (hypothetical) distribution of subfrequencies that represents the highest possible level of dispersion for a given item across a particular set of corpus parts. It requires a vector of subfrequencies and a vector of corpus part sizes. This distribution is required for the min-max transformation proposed by Gries (2022: 184-191; 2024: 196-208) to obtain frequency-adjusted dispersion scores.
#' 
#' @inheritParams disp
#'
#' @returns An integer vector the same length as `partsize`
#' 
#'
#' @references
#' 
#' Gries, Stefan Th. 2022. What do (most of) our dispersion measures measure (most)? Dispersion? \emph{Journal of Second Language Studies} 5(2). 171--205. \doi{doi:10.1075/jsls.21029.gri}
#' 
#' Gries, Stefan Th. 2024. \emph{Frequency, dispersion, association, and keyness: Revising and tupleizing corpus-linguistic measures}. Amsterdam: Benjamins. \doi{doi:10.1075/scl.115}
#' 
#' 
#' @author Lukas Soenning
#' 
#' @details This function creates a hypothetical distribution of the total number of occurrences of the item (i.e. the sum of its subfrequencies) across corpus parts. To obtain the highest possible level of dispersion, the argument `freq_adjust_method` allows the user to choose between two distributional features: pervasiveness (`pervasive`) or evenness (`even`). For details and explanations, see `vignette("frequency-adjustment")`. To obtain the highest possible level of dispersion, the occurrences are either spread as broadly across corpus parts as possible (`pervasive`), or they are allocated to corpus parts in proportion to their size (`even`). The choice between these methods is particularly relevant if corpus parts differ considerably in size. Since the dispersion of an item that occurs only once in the corpus (hapaxes) cannot be sensibly measured or manipulated, such items are disregarded; the function returns their observed subfrequencies. 
#' 
#' 
#' @export
#'
#' @examples
#' find_max_disp(
#'   subfreq = c(0,0,1,2,5), 
#'   partsize = c(100, 100, 100, 500, 1000),
#'   freq_adjust_method = "pervasive")
#'
find_max_disp <- function(
    subfreq, 
    partsize,
    freq_adjust_method = "even"){
  
  if (length(subfreq) != length(partsize)){
    stop("Lengths of the variables 'subfreq' and 'partsize' differ.")
  }
  
  if (sum(subfreq) == 1) {
    output <- subfreq
    
  } else {
    sorted_partsize_decreasing <- sort(partsize, decreasing = TRUE)
    original_order <- rank(-partsize, ties.method = "first")
    
    
    if (freq_adjust_method == "pervasive") {
      
      base_rows <- sapply(1:stats::median(sorted_partsize_decreasing), function(x){
        sum(sorted_partsize_decreasing >= x) 
      })
      
      n_full <- sum(cumsum(base_rows) < sum(subfreq))
      
      n_next <- sum(subfreq) - sum(base_rows[0:n_full])
      
      aux_matrix <- matrix(0, nrow = n_full + 1, ncol = length(partsize))
      
      for(i in 1:(n_full+1)){
        aux_matrix[i, 0:c(base_rows[0:n_full], n_next)[i]] <- 1
      }
      output <- colSums(aux_matrix)[original_order]
      
    } else if (freq_adjust_method == "even"){
      
      expected <- as.numeric(sorted_partsize_decreasing/sum(sorted_partsize_decreasing)*sum(subfreq))
      after_rounding <- round(expected)
      
      n_deviations <- sum(subfreq) - sum(after_rounding)
      deviations <- expected - after_rounding
      
      output <- after_rounding
      
      if (n_deviations > 0){
        plus_one <- order(expected - after_rounding, decreasing = TRUE)[1:n_deviations]
        output[plus_one] <- output[plus_one] + 1
      }
      if (n_deviations < 0){
        n_deviations <- abs(n_deviations)
        minus_one <- order(expected - after_rounding)[1:n_deviations]
        output[minus_one] <- output[minus_one] - 1
      }
      output <- output[original_order] 
    }
  }
  return(output)
}




#' Find the minimally dispersed distribution of each item in a term-document matrix
#' 
#' @description
#' This function takes as input a term-document matrix and returns, for each item (i.e. row), the (hypothetical) distribution of subfrequencies that represents the smallest possible level of dispersion for the item across the corpus parts. This distribution is required for the min-max transformation proposed by Gries (2022: 184-191; 2024: 196-208) to obtain frequency-adjusted dispersion scores. 
#' 
#' @inheritParams disp_tdm
#' 
#' @returns A matrix of integers with one row per item and one column per corpus part
#' 
#' @seealso [find_min_disp()]
#' 
#' @author Lukas Soenning 
#'
#' @references
#' 
#' Gries, Stefan Th. 2022. What do (most of) our dispersion measures measure (most)? Dispersion? \emph{Journal of Second Language Studies} 5(2). 171--205. \doi{doi:10.1075/jsls.21029.gri}
#' 
#' Gries, Stefan Th. 2024. \emph{Frequency, dispersion, association, and keyness: Revising and tupleizing corpus-linguistic measures}. Amsterdam: Benjamins. \doi{doi:10.1075/scl.115}
#' 
#' Gries, Stefan Th. 2025. \emph{KLD4C: Gries 2024: Tupleization of corpus linguistics}. R package version 1.01. (available from https://www.stgries.info/research/kld4c/kld4c.html)
#' 
#' 
#' @details This function takes as input a term-document matrix and creates, for each item in the matrix, a hypothetical distribution of the total number of occurrences of the item (i.e. the sum of the subfrequencies) across corpus parts. To obtain the lowest possible level of dispersion, the argument `freq_adjust_method` allows the user to choose between two distributional features: pervasiveness (`pervasive`) or evenness (`even`). For details and explanations, see `vignette("frequency-adjustment")`. To obtain the lowest possible level of dispersion, the occurrences are either allocated to as few corpus parts as possible (`pervasiveness`), or they are assigned to the smallest corpus part(s) (`even`).  Since the dispersion of an item that occurs only once in the corpus (hapaxes) cannot be sensibly measured or manipulated, such items are disregarded; the function returns their observed subfrequencies. The function reuses code segments from Gries's (2025) 'KLD4C' package (from the function `most.uneven.distr()`).
#' 
#' @export
#'
#' @examples
#' find_min_disp_tdm(
#'   tdm = biber150_spokenBNC2014[1:10,],
#'   row_partsize = "first",
#'   freq_adjust_method = "even")
#'
find_min_disp_tdm <- function(
    tdm,
    row_partsize = "first",
    freq_adjust_method = freq_adjust_method){
  
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
  
  if (row_partsize == "last"){
    
    output <- apply(tdm[-nrow(tdm),], 1, function(x){
      find_min_disp(
        subfreq = x, 
        partsize = tdm[nrow(tdm),], 
        freq_adjust_method = freq_adjust_method)
    })      
    output <- rbind(t(output), "word_count" = tdm[nrow(tdm),])  
    
  } else {
    
    output <- apply(tdm[-1,], 1, function(x){
      find_min_disp(
        subfreq = x, 
        partsize = tdm[1,], 
        freq_adjust_method = freq_adjust_method)
    })
    output <- rbind("word_count" = tdm[1,], t(output))
  }
  return(output)
}


#' Find the maximally dispersed distribution of each item in a term-document matrix
#' 
#' @description
#' This function takes as input a term-document matrix and returns, for each item (i.e. row), the (hypothetical) distribution of subfrequencies that represents the highest possible level of dispersion for the item across the corpus parts. This distribution is required for the min-max transformation proposed by Gries (2022: 184-191; 2024: 196-208) to obtain frequency-adjusted dispersion scores.
#' 
#' @inheritParams disp
#' @inheritParams disp_tdm
#' 
#' @returns A matrix of integers with one row per item and one column per corpus part
#' 
#' @seealso [find_max_disp()]
#' 
#' @references
#'
#' Gries, Stefan Th. 2022. What do (most of) our dispersion measures measure (most)? Dispersion? \emph{Journal of Second Language Studies} 5(2). 171--205. \doi{doi:10.1075/jsls.21029.gri}
#'  
#' Gries, Stefan Th. 2024. \emph{Frequency, dispersion, association, and keyness: Revising and tupleizing corpus-linguistic measures}. Amsterdam: Benjamins.
#' 
#' @author Lukas Soenning
#' 
#' @details This function takes as input a term-document matrix and creates, for each item in the matrix, a hypothetical distribution of the total number of occurrences of the item (i.e. the sum of the subfrequencies) across corpus parts. To obtain the highest possible level of dispersion, the argument `freq_adjust_method` allows the user to choose between two distributional features: pervasiveness (`pervasive`) or evenness (`even`). For details and explanations, see `vignette("frequency-adjustment")`. To obtain the highest possible level of dispersion, the occurrences are either spread as broadly across corpus parts as possible (`pervasive`), or they are allocated to corpus parts in proportion to their size (`even`). The choice between these methods is particularly relevant if corpus parts differ considerably in size. Since the dispersion of items that occur only once in the corpus (hapaxes) cannot be sensibly measured or manipulated, such items are disregarded; the function returns their observed subfrequencies. 
#' 
#' @export
#'
#' @examples
#' find_max_disp_tdm(
#'   tdm = biber150_spokenBNC2014[1:10,],
#'   row_partsize = "first",
#'   freq_adjust_method = "even")
#'
find_max_disp_tdm <- function(
    tdm,
    row_partsize = "first",
    freq_adjust_method = freq_adjust_method){
  
  if(missing(row_partsize)){
    stop("Please indicate which row in the term-document matrix inludes the part sizes.\n  Use argument 'row_partsize' to locate the correct row ('first' or 'last').")
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
  
  if (row_partsize == "last"){
    
    output <- apply(tdm[-nrow(tdm),], 1, function(x){
      find_max_disp(
        subfreq = x, 
        partsize = tdm[nrow(tdm),], 
        freq_adjust_method = freq_adjust_method)
    })      
    output <- rbind(t(output), "word_count" = tdm[nrow(tdm),], freq_adjust_method)    
    
  } else {
    
    output <- apply(tdm[-1,], 1, function(x){
      find_max_disp(
        subfreq = x, 
        partsize = tdm[1,], 
        freq_adjust_method = freq_adjust_method)
    })
    output <- rbind("word_count" = tdm[1,], t(output))

  }
  return(output)
}
