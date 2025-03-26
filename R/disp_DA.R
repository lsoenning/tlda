disp_DA <- function(subfreq,
                    partsize,
                    verbose = TRUE,
                    formula = "basic",
                    directionality = "conventional") {
  R_i <- subfreq / partsize
  r_i <- R_i / sum(R_i)
  k <- length(partsize)

  if (formula == "basic") {
    dist_r <- as.matrix(dist(r_i, method = "manhattan"))
    DA <- 1 - (mean(dist_r[lower.tri(dist_r)]) / (2 / k))
  } else if (formula == "shortcut") {
    R_i <- n_tokens / word_count
    r_i <- R_i / sum(R_i)
    k <- length(n_tokens)

    DA <- (2 * sum((sort(r_i, decreasing = TRUE) * 1:k)) - 1) / (k - 1)
  }

  if (directionality == "gries") DA <- 1 - DA

  names(DA) <- c("DA")

  print(DA)

  if (verbose) {
    if (directionality == "gries") {
      message("\nThe scores follow the scaling used by Gries (2008):")
      message("  0 = maximally even/dispersed/balanced distribution (optimum)")
      message("  1 = maximally uneven/bursty/concentrated distribution (pessimum)\n")
    } else {
      message("\nThe scores follow the conventional scaling:")
      message("  0 = maximally uneven/bursty/concentrated distribution (pessimum)")
      message("  1 = maximally even/dispersed/balanced distribution (optimum)\n")
    }

    if (formula == "basic") {
      message("Scores are computed using the basic formula for DA, see:")
      message("  Wilcox (1967: 343, 'MDA', column 2) [https://doi.org/10.2307/446831]")
      message("  Burch et al. (2017: 194-196) [ https://doi.org/10.1558/jrds.33066]\n")
    } else if (formula == "shortcut") {
      message("Scores are computed using the computational shortcut suggested by")
      message("  Wilcox (1967: 343, 'MDA', column 4) [https://doi.org/10.2307/446831]\n")
    }

    message("For background on the formula for DA, see:")
    message("  Soenning 2025 []")
  }

  invisible(DA)
}
