#' Find the pessimum distribution (dispersion minimum) for a vector of subfrequencies and part sizes
#' 
#' @description
#' This function returns the (hypothetical) distribution of subfrequencies that represents the smallest possible level of dispersion for the item across the corpus parts. It requires a vector of subfrequencies and a vector of corpus part sizes. This distribution is required for the min-max transformation proposed by Gries (2024: 196-208) to obtain frequency-adjusted dispersion scores.
#' 
#' @inheritParams disp
#'
#' @return An integer vector of the same length as the number of corpus parts.
#' 
#' @author Lukas Soenning
#' 
#' @details This function creates a hypothetical distribution of the total number of occurrences of the item (i.e. the sum of the subfrequencies) across the corpus parts. To obtain the highest possible level of concentration (i.e. minimum dispersion), the occurrences are allocated to the smallest corpus parts. The function starts by filling in the smallest corpus part. If the total number of occurrences of the items is greater than the size of the smallest corpus part, it then continues with the second smallest part and so on. The function returns a vector of length \eqn{k} (where \eqn{k} is the number of corpus parts), whose order matches that of the vector of corpus parts provided to the argument \code{partsize}.
#' @export
#'
#' @examples
#' \dontrun{
#' # not run
#' find_min_disp(
#'   subfreq = c(0,0,1,2,5), 
#'   partsize = rep(1000, 5))
#' }
#'
find_min_disp <- function(subfreq, partsize){
  
  token_string <- c(rep(1, sum(subfreq)), rep(0, sum(partsize) - sum(subfreq)))
  part_string <- rep(1:length(partsize), sort(partsize))
  
  output <- as.vector(
    tapply(token_string,           
           part_string, 
           sum))[rank(partsize, ties.method = "first")]
  
  return(output)
}



#' Find the optimum distribution (dispersion maximum) for a vector of subfrequencies and part sizes
#' 
#' @description
#' This function returns the (hypothetical) distribution of subfrequencies that represents the highest possible level of dispersion for the item across the corpus parts. It requires a vector of subfrequencies and a vector of corpus part sizes. This distribution is required for the min-max transformation proposed by Gries (2024: 196-208) to obtain frequency-adjusted dispersion scores.
#' 
#' @inheritParams disp
#'
#' @return An integer vector of the same length as the number of corpus parts.
#' 
#' @author Lukas Soenning
#' 
#' @details This function creates a hypothetical distribution of the total number of occurrences of the item (i.e. the sum of the subfrequencies) across the corpus parts. To obtain the highest possible level of dispersion, two methods are available. The choice between these methods is particularly relevant if corpus parts differ considerably in size.
#' 
#' The first method prioritizes pervasiveness (\code{pervasive}), which means that it creates a distribution with the widest possible spread of the item across corpus parts. It first arranges the corpus parts by size, in decreasing order. Then one occurrence of the item is assigned to each part, starting with the largest part, and continuing to(wards) the smallest part. The allocation of occurrences to parts therefore largely disregards the size of the parts. If the number of occurrences of the item exceeds the number of corpus parts, we start afresh with the largest corpus part. If the smallest corpus part(s) cannot hold any further occurrences (i.e. if they are "full"), the allocation returns to the largest corpus part.
#' 
#' The second method, which is EXPERIMENTAL, strives for a balance between pervasiveness and evenness (\code{pervasive_even}). This means that the allocation of occurrences also takes into account the size of the corpus parts. During the allocation process, an occurrence may therefore not be assigned to the next-smaller corpus part if a larger part makes an extra occurrence of the item more likely due to its length. In principle, weights can be assigned to pervasiveness and evenness to represent their relative importance. At the moment, no weighting scheme has been devised, which is why this method is currently EXPERIMENTAL. 
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # not run
#' find_max_disp(
#'   subfreq = c(0,0,1,2,5), 
#'   partsize = c(100, 100, 100, 500, 1000),
#'   freq_adjust_method = "pervasive")
#' }
#'
find_max_disp <- function(
    subfreq, 
    partsize,
    freq_adjust_method = "pervasive_even"){
  
  if (freq_adjust_method == "pervasive_even") {
    
    sorted_partsize_decreasing <- sort(partsize, decreasing = TRUE)

    aux_matrix <- sapply(sorted_partsize_decreasing, function(x) {
        c((x:1)/x - x^-1, rep(0, max(sorted_partsize_decreasing) - x))
      })[1:median(sorted_partsize_decreasing),]
    
    aux_matrix_ranks <- matrix(
      rank(-aux_matrix, ties.method = "first"),
      ncol = length(partsize), 
      nrow = median(partsize))
    
    aux_matrix_ranks[aux_matrix_ranks <= sum(subfreq)] <- 1
    aux_matrix_ranks[aux_matrix_ranks > sum(subfreq)] <- 0
    
    output <- rev(colSums(aux_matrix_ranks))[rank(partsize, ties.method = "first")]
    
  } else if (freq_adjust_method == "pervasive") {
    
    sorted_partsize_decreasing <- sort(partsize, decreasing = TRUE)
    
    fill_string <- as.integer(t(
      sapply(sorted_partsize_decreasing, function(x) {
        c(rep(1, x), rep(0, max(sorted_partsize_decreasing) - x))
      }))[1:median(sorted_partsize_decreasing),])
    
    fill_string <- fill_string[1:which(cumsum(fill_string) == sum(subfreq))]
    
    output <- rev(
      colSums(
        matrix(c(fill_string, rep(0, length(subfreq) * max(tdm[1,]) - length(fill_string))), 
               ncol = length(subfreq), byrow = TRUE)))[rank(partsize, ties.method = "first")]  
  }
  return(output)
}


#' Find the pessimum distribution (dispersion minimum) for each item in a term-document matrix
#' 
#' @description
#' This function takes as input a term-document matrix and returns, for each item (i.e. row), the (hypothetical) distribution of subfrequencies that represents the smallest possible level of dispersion for the item across the corpus parts. This distribution is required for the min-max transformation proposed by Gries (2024: 196-208) to obtain frequency-adjusted dispersion scores.
#' 
#' @inheritParams disp_tdm
#'
#' @return A matrix of integers
#' 
#' @author Lukas Soenning
#' 
#' @details This function takes as input a term-document matrix and creates, for each item in the matrix, a hypothetical distribution of the total number of occurrences of the item (i.e. the sum of the subfrequencies) across the corpus parts. To obtain the highest possible level of concentration (i.e. minimum dispersion) for each item, its occurrences are allocated to the smallest corpus parts. The function starts by filling in the smallest part. If the total number of occurrences of the item is greater than the size of this part, it continues with the second smallest corpus part, and so on. The function returns a matrix of integers which has the same structure as the term-document matrix supplied to the function.
#' @export
#'
#' @examples
#' \dontrun{
#' # not run
#' find_min_disp(
#'   subfreq = matrix(
#'     c(0,0,1,2,5,
#'       0,1,2,4,4), 
#'       nrow = 2, 
#'       byrow = TRUE),
#'   partsize = c(100, 100, 100, 500, 1000),
#'   freq_adjust_method = "pervasive")
#' }
#'
find_min_disp_tdm <- function(
    tdm,
    row_partsize = "first_row"){
  
  if (row_partsize == "first_row"){
    
    token_string_0 <- rep(0, sum(tdm[1,]))
    part_string <- rep(1:ncol(tdm), sort(tdm[1,]))
    
    output <- apply(tdm[-1,], 1, function(x){
      token_string <- token_string_0
      token_string[1:sum(x)] <- 1
      
      output <- as.integer(
        tapply(token_string,           
               part_string, 
               sum))[rank(tdm[1,], ties.method = "first")]
    })
    output <- rbind("word_count" = tdm[1,], t(output))
    
  } else if (row_partsize == "last_row"){
    
    token_string_0 <- rep(0, sum(tdm[nrow(tdm),]))
    part_string <- rep(1:ncol(tdm), sort(tdm[nrow(tdm),]))
    
    output <- apply(tdm[-nrow(tdm),], 1, function(x){
      token_string <- token_string_0
      token_string_0[1:sum(x)] <- 1
      
      output <- as.integer(
        tapply(token_string,           
               part_string, 
               sum))[rank(tdm[nrow(tdm),], ties.method = "first")]
    })
    output <- rbind("word_count" = tdm[nrow(tdm),], t(output))
  }
  return(output)
}


#' Find the optimum distribution (dispersion maximum) for each item in a term-document matrix
#' 
#' @description
#' This function takes as input a term-document matrix and returns, for each item (i.e. row), the (hypothetical) distribution of subfrequencies that represents the highest possible level of dispersion for the item across the corpus parts. This distribution is required for the min-max transformation proposed by Gries (2024: 196-208) to obtain frequency-adjusted dispersion scores.
#' 
#' @inheritParams disp
#' @inheritParams disp_tdm
#' 
#' @return A matrix of integers
#' 
#' @author Lukas Soenning
#' 
#' @details This function takes as input a term-document matrix and creates, for each item in the matrix, a hypothetical distribution of the total number of occurrences of the item (i.e. the sum of the subfrequencies) across the corpus parts. To obtain the highest possible level of dispersion, two methods are available. The choice between these methods is particularly relevant if corpus parts differ considerably in size.
#' 
#' The first method prioritizes pervasiveness (\code{pervasive}), which means that it creates a distribution with the widest possible spread of the item across corpus parts. It first arranges the corpus parts by size, in decreasing order. Then one occurrence of the item is assigned to each part, starting with the largest part, and continuing to(wards) the smallest part. The allocation of occurrences to parts therefore largely disregards the size of the parts. If the number of occurrences of the item exceeds the number of corpus parts, we start afresh with the largest corpus part. If the smallest corpus part(s) cannot hold any further occurrences (i.e. if they are "full"), the allocation returns to the largest corpus part.
#' 
#' The second method, which is EXPERIMENTAL, strives for a balance between pervasiveness and evenness (\code{pervasive_even}). This means that the allocation of occurrences also takes into account the size of the corpus parts. During the allocation process, an occurrence may therefore not be assigned to the next-smaller corpus part if a larger part makes an extra occurrence of the item more likely due to its length. In principle, weights can be assigned to pervasiveness and evenness to represent their relative importance. At the moment, no weighting scheme has been devised, which is why this method is currently EXPERIMENTAL. 
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # not run
#' find_max_disp(
#'   subfreq = matrix(
#'     c(0,0,1,2,5,
#'       0,1,2,4,4), 
#'       nrow = 2, 
#'       byrow = TRUE),
#'   partsize = c(100, 100, 100, 500, 1000),
#'   freq_adjust_method = "pervasive")
#' }
#'
find_max_disp_tdm <- function(
    tdm,
    row_partsize = "first_row",
    freq_adjust_method = "pervasive_even"){
  
  
  if (row_partsize == "first_row"){
    
    sorted_partsize_decreasing <- as.integer(sort(tdm[1,], decreasing = TRUE))
    
    
    if (freq_adjust_method == "pervasive_even") {

      aux_matrix <- sapply(sorted_partsize_decreasing, function(x) {
          c((x:1)/x - x^-1, rep(0, max(sorted_partsize_decreasing) - x))
        })[1:median(sorted_partsize_decreasing),]
      
      aux_matrix_ranks <- matrix(
        rank(-aux_matrix, ties.method = "first"),
        ncol = length(sorted_partsize_decreasing), 
        nrow = median(sorted_partsize_decreasing))
      
      output <- apply(tdm[-1,], 1, function(x){
        
        aux_matrix_ranks_item <- aux_matrix_ranks
        
        aux_matrix_ranks_item[aux_matrix_ranks_item <= sum(x)] <- 1
        aux_matrix_ranks_item[aux_matrix_ranks_item > sum(x)] <- 0
        
        rev(colSums(aux_matrix_ranks_item))[rank(tdm[1,], ties.method = "first")]
      })
      
    } else if (freq_adjust_method == "pervasive") {
      
      fill_string <- as.integer(t(
        sapply(matrix(sorted_partsize_decreasing), 1, function(x) {
          c(rep(1, x), rep(0, max(sorted_partsize_decreasing) - x))
        }))[1:median(sorted_partsize_decreasing),])

      output <- apply(tdm[-1,], 1, function(x){
        
        fill_string_item <- fill_string
        fill_string_item <- fill_string_item[1:which(cumsum(fill_string_item) == sum(x))]
        
        output <- rev(
          colSums(
            matrix(c(fill_string_item, rep(0, length(x) * max(tdm[1,]) - length(fill_string_item))), 
                   ncol = length(subfreq), byrow = TRUE)))[rank(tdm[1,], ties.method = "first")]  
      })
    }
    output <- rbind("word_count" = tdm[1,], t(output))
    
  } else if (row_partsize == "last_row"){
    
    if (freq_adjust_method == "pervasive_even") {
      
      sorted_partsize_decreasing <- as.integer(
        sort(tdm[nrow(tdm),], decreasing = TRUE))

      aux_matrix <- sapply(sorted_partsize_decreasing, function(x) {
          c((x:1)/x - x^-1, rep(0, max(sorted_partsize_decreasing) - x))
        })[1:median(sorted_partsize_decreasing),]
      
      aux_matrix_ranks <- matrix(
        rank(-aux_matrix, ties.method = "first"),
        ncol = length(sorted_partsize_decreasing), 
        nrow = median(sorted_partsize_decreasing))
      
      output <- apply(tdm[-nrow(tdm),], 1, function(x){
        
        aux_matrix_ranks_item <- aux_matrix_ranks
        
        aux_matrix_ranks_item[aux_matrix_ranks_item <= sum(x)] <- 1
        aux_matrix_ranks_item[aux_matrix_ranks_item > sum(x)] <- 0
        
        rev(colSums(aux_matrix_ranks_item))[rank(tdm[nrow(tdm),], ties.method = "first")]
      })     
      
      
      
    } else if (freq_adjust_method == "pervasive") {
      
      sorted_partsize_decreasing <- as.integer(
        sort(tdm[nrow(tdm),], decreasing = TRUE))
      
      fill_string <- as.integer(t(
        sapply(sorted_partsize_decreasing, function(x) {
          c(rep(1, x), rep(0, max(sorted_partsize_decreasing) - x))
        }))[1:median(sorted_partsize_decreasing),])

      output <- apply(tdm[-1,], 1, function(x){
        
        fill_string_item <- fill_string
        fill_string_item <- fill_string_item[1:which(cumsum(fill_string_item) == sum(x))]
        
        output <- rev(
          colSums(
            matrix(c(fill_string_item, rep(0, length(x) * max(tdm[nrow(tdm),]) - length(fill_string_item))), 
                   ncol = length(subfreq), byrow = TRUE)))[rank(tdm[nrow(tdm),], ties.method = "first")]  
      })
    }
    output <- rbind("word_count" = tdm[nrow(tdm),], t(output))
  }
  return(output)
}


