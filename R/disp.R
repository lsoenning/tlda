disp <- function(subfreq,
                 partsize,
                 verbose = TRUE) {
  W_i <- partsize
  w_i <- W_i / sum(W_i)
  T_i <- subfreq
  t_i <- T_i / sum(T_i)
  R_i <- T_i / W_i
  r_i <- R_i / sum(R_i)
  k <- length(T_i)

  dist_r <- as.matrix(dist(r_i, method = "manhattan"))

  Rrel <- sum(T_i != 0) / length(T_i)

  D <- 1 - (sqrt(sum((R_i - mean(R_i))^2) / k) / (mean(R_i) * sqrt(k - 1)))

  S <- (sum(sqrt(w_i * T_i))^2) / sum(T_i)

  D2 <- (log(sum(R_i)) - sum(ifelse(
    R_i == 0, 0, R_i * log(R_i)
  )) / sum(R_i)) / log(k)

  DP <- 1 - ((sum(abs(t_i - w_i)) * (sum(W_i) / (sum(W_i) - min(W_i[T_i > 0])))) / 2)

  DA <- 1 - (mean(dist_r[lower.tri(dist_r)]) / (2 / k))

  DKL <- exp(-sum(t_i * ifelse(t_i == 0, 0, log2(t_i / w_i))))


  output <- c(Rrel, D, D2, S, DP, DA, DKL)

  names(output) <- c("Rrel", "D", "D2", "S", "DP", "DA", "DKL")

  print(output)

  if (verbose) {
    message("\nThe scores for all measures follow the conventional scaling:")
    message("  0 = maximally uneven/bursty/concentrated distribution (pessimum)")
    message("  1 = maximally even/dispersed/balanced distribution (optimum)\n")
    message("For background on the formulas, see Soenning (2025) []")
    message("For Gries' DP, the function uses the modified version suggested by\n  Egbert et al. (2020) [https://doi.org/10.1075/ijcl.18010.egb]")
  }

  invisible(output)
}
