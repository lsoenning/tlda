#' Re-express proportions using the folded power transformation
#'
#' @description
#' This function takes as input a vector of proportions (or, more generally, scores in the unit interval \[0,1\]) and re-expresses them using Tukey's folded power transformation. It allows the user to decide whether the transformed scores should be mapped to the \[-1, +1\] interval (default), or whether they may extend beyond these limits.
#'
#' @param x A numeric vector of scores in the unit interval \[0,1\]; 0 and 1 are allowed but throw an error message when lambda = 0
#' @param lambda Numeric value of the power transformation, which can range between 0 (limiting case: logit transformation) and 1 (no transformation) 
#' @param scaling Character string indicating whether scores should be re-expressed to the \[-1, 1\] interval (`plus_minus_1`) or allowed to stretch beyond these limits (`free`)
#'
#' @author Lukas Soenning
#' 
#' @details
#' This function allows the user to apply a variety of folded power transformations to quantities bounded between 0 and 1. Different values may be specified for the power of the transformation (\eqn{\lambda} `lambda`), but only powers between 0 and 1 are supported. Two versions of the folded power transformation are available. The first maps transformed values to the \[-1, +1\] interval:
#' 
#' \eqn{x^\lambda - (1-x)^\lambda}
#' 
#' The second version does not impose these limits:
#' 
#' \eqn{(x^\lambda - (1-x)^\lambda) / \lambda}
#' 
#' For `lambda` equal to 0, the logit transformation is implemented as a limiting case; note that input scores of 0 and 1 are not allowed when `lambda` is set to 0
#' `lambda` = 0.14 gives a close approximation to the probit transformation (see Fox 2016: 74) while accepting input score of 0 and 1
#' `lambda` = 1/3 implements folded cube roots
#' `lambda` = 0.41 gives a close approximation to the arcsine-square-root (or angular) transformation (see Fox 2016: 74)
#' `lambda` = 0.5 implements folded roots
#' 
#' This function was written with the help of ChatGPT (version GPT-5.1; OpenAI 2025)
#' 
#' @returns A numeric vector
#' 
#' @references 
#' OpenAI. (2025). ChatGPT (GPT-5.1) Large language model. https://chat.openai.com
#' 
#' @export
#'
#' @examples
#' fpower(
#'   seq(0, 1, .1),
#'   lambda = .14,
#'   scaling = "plus_minus_1")
fpower <- function(x, lambda = 1, scaling = "plus_minus_1") {
  
  safe_numeric <- function(x) {
    if (is.factor(x)) {
      x_num <- as.numeric(as.character(x))
    } else if (!is.numeric(x)) {
      x_num <- suppressWarnings(as.numeric(x))
    } else {
      x_num <- x
    }
    x_num
  }
  
  if (lambda < 0 | lambda > 1){
    stop("Lambda must not be smaller than 0 or greater than 1.")
  }
  if (is.null(x)) return(x)
  if (any(is.na(x))) {
    warning("Some values are NA and will be returned as NA.")
  }
  x_num <- safe_numeric(x)
  
  
  if(scaling == "plus_minus_1"){
    # Check domain
    if (any(x_num < 0 | x_num > 1, na.rm = TRUE)) {
      stop("Input values must be within [0, 1].")
    }
    
    # λ = 0 cannot handle 0 or 1 because it uses the logit
    if (lambda == 0 && any(x_num %in% c(0, 1), na.rm = TRUE)) {
      stop("Input contains 0 or 1; folded powers with lambda = 0 (logit transformation) are undefined for 0 and 1; choose a different value for lambda (e.g. .14, which approximates a multiple of the probit transformation).")
    }
    
    res <- rep(NA_real_, length(x_num))
    
    # Valid points
    ok <- !is.na(x_num)
    
    if (lambda == 0) {
      # logit/4 version, continuous λ→0 limit
      res[ok] <- log(x_num[ok] / (1 - x_num[ok])) / 4
    } else {
      # folded-power without division by λ
      res[ok] <- x_num[ok]^lambda - (1 - x_num[ok])^lambda
    }
  }
  
  if (scaling == "free"){
    # Check domain
    if (any(x_num < 0 | x_num > 1, na.rm = TRUE)) {
      stop("Input values must be within [0, 1].")
    }
    
    # λ = 0 cannot handle 0 or 1 because it uses the logit
    if (lambda == 0 && any(x_num %in% c(0, 1), na.rm = TRUE)) {
      stop("Input contains 0 or 1; folded powers with lambda = 0 (logit transformation) are undefined for 0 and 1; choose a different value for lambda (e.g. .14, which approximates a multiple of the probit transformation).")
    }
    
    res <- rep(NA_real_, length(x_num))
    
    # Valid points
    ok <- !is.na(x_num)
    
    
    if (lambda == 0) {
      res[ok] <- log(x_num[ok] / (1 - x_num[ok]))
    } else {
      res[ok] <- (x_num[ok]^lambda - (1 - x_num[ok])^lambda) / lambda
    }
  }
  res
}



#' Back-transform folded-power-transformed scores to the unit interval \[0,1\]
#'
#' @description
#' This function takes as input a vector of transformed scores, i.e. values that were originally in the unit interval \[0, 1\] but which were re-expressed using Tukey's folded power transformation. It allows back-transformation of two versions of folded powers: Those that are mapped to the \[-1, +1\] interval and those that aren't.
#'
#' @param y A numeric vector of folded-power-transformed scores
#' @param lambda Numeric value of the applied power transformation, which can range between 0 (limiting case: logit transformation) and 1 (no transformation) 
#' @param scaling Character string indicating whether scores were re-expressed to the \[-1, 1\] interval (`plus_minus_1`) or not (`free`)
#'
#' @author Lukas Soenning
#' 
#' @details
#' This function was written with the help of ChatGPT (version GPT-5.1; OpenAI 2025)
#'
#' @returns A numeric vector
#' 
#' @references 
#' OpenAI. (2025). ChatGPT (GPT-5.1) Large language model. https://chat.openai.com
#' 
#' @export
#'
#' @examples
#' invfpower(
#'   seq(-1, 1, .1),
#'   lambda = .14,
#'   scaling = "plus_minus_1")
invfpower <- function(y, lambda = 1, scaling = "plus_minus_1") {
  
  safe_numeric <- function(x) {
    if (is.factor(x)) {
      x_num <- as.numeric(as.character(x))
    } else if (!is.numeric(x)) {
      x_num <- suppressWarnings(as.numeric(x))
    } else {
      x_num <- x
    }
    x_num
  }
  
  if (lambda < 0 | lambda > 1){
    stop("Lambda must not be smaller than 0 or greater than 1.")
  }
  
  if (is.null(y)) return(y)
  y_num <- safe_numeric(y)
  
  # Range check: y must be within [-1, 1]
  if (any(y_num < -1 | y_num > 1, na.rm = TRUE)) {
    stop("Transformed values must lie within [-1, 1].")
  }
  
  res <- rep(NA_real_, length(y_num))
  ok <- !is.na(y_num)
  
  if (lambda == 0) {
    # inverse of logit/4
    res[ok] <- exp(4 * y_num[ok]) / (1 + exp(4 * y_num[ok]))
  } else {
    # numerical inversion for λ != 0
    res[ok] <- sapply(y_num[ok], function(yi) {
      f <- function(p) p^lambda - (1 - p)^lambda - yi
      stats::uniroot(f, c(0, 1), tol = .Machine$double.eps^0.5)$root
    })
  }
  res
}


#' Tukey's folded power transformation
#'
#' @param lambda Numeric value of the applied power transformation, which can range between 0 (limiting case: logit transformation) and 1 (no transformation) 
#'
#'
#' @details
#' This function was written with the help of ChatGPT (version GPT-5.1; OpenAI 2025)
#' 
#' @returns A numeric vector
#' 
#' @references 
#' OpenAI. (2025). ChatGPT (GPT-5.1) Large language model. https://chat.openai.com
#' 
#' @export
#'
#' @examples
#' plot(fpower_trans(lambda = .5), xlim = c(0, 1))
fpower_trans <- function(lambda = 0) {
  
  safe_numeric <- function(x) {
    if (is.factor(x)) {
      x_num <- as.numeric(as.character(x))
    } else if (!is.numeric(x)) {
      x_num <- suppressWarnings(as.numeric(x))
    } else {
      x_num <- x
    }
    x_num
  }
  
  # ------------------------------------------------------------------
  # Forward transformation
  # ------------------------------------------------------------------
  forward <- function(p) {
    if (is.null(p)) return(p)
    p_num <- safe_numeric(p)
    
    if (any(p_num < 0 | p_num > 1, na.rm = TRUE)) {
      stop("Input values must lie within [0, 1].")
    }
    
    if (lambda == 0 && any(p_num %in% c(0, 1), na.rm = TRUE)) {
      stop("Input contains 0 or 1, which are undefined for lambda = 0.")
    }
    
    res <- rep(NA_real_, length(p_num))
    ok  <- !is.na(p_num)
    
    if (lambda == 0) {
      res[ok] <- log(p_num[ok] / (1 - p_num[ok]))
    } else {
      res[ok] <- (p_num[ok]^lambda - (1 - p_num[ok])^lambda) / lambda
    }
    
    res
  }
  
  # ------------------------------------------------------------------
  # Inverse with automatic domain adjustment
  # ------------------------------------------------------------------
  inverse <- function(y) {
    if (is.null(y)) return(y)
    y_num <- safe_numeric(y)
    
    res <- rep(NA_real_, length(y_num))
    ok  <- !is.na(y_num)
    
    # handle lambda = 0 directly (inverse logit)
    if (lambda == 0) {
      res[ok] <- exp(y_num[ok]) / (1 + exp(y_num[ok]))
      return(res)
    }
    
    # Pre-compute endpoint values
    f0 <- (0^lambda - (1-0)^lambda) / lambda     # = -1/lambda
    f1 <- (1^lambda - (1-1)^lambda) / lambda     # = +1/lambda
    
    # ensure f0 <= f1
    lo <- min(f0, f1)
    hi <- max(f0, f1)
    
    # small tolerance to account for floating-point drift
    eps <- 1e-12
    
    # Solve each y_i
    res[ok] <- sapply(y_num[ok], function(yi) {
      
      # If yi below possible range → clamp to p=0
      if (yi < lo - eps) return(0)
      
      # If yi above possible range → clamp to p=1
      if (yi > hi + eps) return(1)
      
      # If yi extremely close to endpoints → snap
      if (abs(yi - lo) < eps) return(0)
      if (abs(yi - hi) < eps) return(1)
      
      # Function for root-finding
      f <- function(p) (p^lambda - (1 - p)^lambda) / lambda - yi
      
      # Safe uniroot call: domain is always [0,1]
      out <- try(
        stats::uniroot(f, interval = c(0, 1), tol = .Machine$double.eps^0.5)$root,
        silent = TRUE
      )
      
      # If uniroot still fails for any pathological reason → return NA
      if (inherits(out, "try-error")) return(NA_real_)
      
      out
    })
    
    res
  }
  
  # axis helpers
  breaks <- function(x) pretty(c(0, 1), n = 5)
  labels <- scales::label_number(accuracy = 0.01)
  
  scales::trans_new(
    name      = paste0("folded_power_", lambda),
    transform = forward,
    inverse   = inverse,
    breaks    = breaks,
    format    = labels,
    domain    = c(0, 1)
  )
}



#' Position scales for Tukey's folded power transformation
#' 
#' @param lambda Numeric value of the applied power transformation
#' @param ... Other argument passed on to `scale_(x|y)_continuous()`
#'
#' @details
#' This function was written with the help of ChatGPT (version GPT-5.1; OpenAI 2025)
#' 
#' @returns The ggplot2 function `scale_(x|y)_continuous()` with the appropriate transformation
#' 
#' @references 
#' OpenAI. (2025). ChatGPT (GPT-5.1) Large language model. https://chat.openai.com
#' 
#' @export
#'
#' @examples
#' if (require("ggplot2")) {
#'   ggplot(
#'     data = data.frame(
#'       dispersion = seq(0, 1, .01)),
#'     aes(x = dispersion)) +
#'     geom_dotplot() +
#'     scale_x_fpower(lambda = .5)
#' }
scale_x_fpower <- function(lambda = 0, ...) {
  

  ggplot2::scale_x_continuous(
    trans  = fpower_trans(lambda),
    ...,
    breaks = pretty(c(0, 1), n = 6),
    labels = scales::label_number(accuracy = 0.01)
  )
}

scale_y_fpower <- function(lambda = 0, ...) {
  
  
  ggplot2::scale_y_continuous(
    trans  = fpower_trans(lambda),
    ...,
    breaks = pretty(c(0, 1), n = 6),
    labels = scales::label_number(accuracy = 0.01)
  )
}