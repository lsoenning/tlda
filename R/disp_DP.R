disp_DP <- function(subfreq,
                    partsize,
                    verbose = TRUE,
                    formula = "egbert_etal_2020",
                    directionality = "conventional") {
  w_i <- partsize / sum(partsize)
  t_i <- subfreq / sum(subfreq)

  if (formula == "gries_2008") {
    DP <- 1 - sum(abs(t_i - w_i)) / 2
  } else if (formula == "lijffit_gries_2012") {
    DP <- 1 - (sum(abs(t_i - w_i)) / 2) / (1 - min(w_i))
  } else {
    DP <- 1 - (sum(abs(t_i - w_i)) / 2) / (1 - min(w_i[t_i > 0]))
  }

  if (directionality == "gries") DP <- 1 - DP

  names(DP) <- c("DP")

  print(DP)

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

    if (formula == "gries_2008") {
      message("Scores are computed using the original version of DP proposed by\n  Gries (2008) [https://doi.org/10.1075/ijcl.13.4.02gri]\n")
    } else if (formula == "lijffit_gries_2012") {
      message("Scores are computed using the modified version of DP suggested by\n  Lijffit & Gries (2012) [https://doi.org/10.1075/ijcl.17.1.08lij]\n")
    } else {
      message("Scores are computed using the modified version of DP suggested by\n  Egbert et al. (2020) [https://doi.org/10.1075/ijcl.18010.egb]\n")
    }

    message("For background on the formulas for DP, see:")
    message("  Gries 2020 [https://doi.org/10.1007/978-3-030-46216-1_5]")
    message("  Soenning 2025 []")
  }

  invisible(DP)
}
