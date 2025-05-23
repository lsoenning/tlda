#' Speaker metadata for the Spoken BNC2014
#'
#' This dataset provides some metadata for the speakers in the Spoken BNC2014 (Love et al. 2017), including information on age, gender, and the total number of word tokens contributed to the corpus.
#'
#' @format ## `spokenBNC2014_metadata`
#' A data frame with 668 rows and 6 columns:
#' 
#' \describe{
#'   \item{speaker_id}{Speaker ID (e.g. "S0001", "S0002")}
#'   \item{age_group}{Age group, based on the BNC1994 scheme ("0-14", "15-24", "25-34", "35-44", "45-59", "60+", "Unknown")}
#'   \item{gender}{Speaker gender ("Female" vs. "Male")}
#'   \item{age}{Age of speaker;  if actual age is not available, imputed based on \code{age_group} and \code{age_bin}}
#'   \item{n_tokens}{Number of word tokens the speaker contributed to the corpus}
#'   \item{age_bin}{Age group, based on the BNC2014 scheme ("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+")}
#' }
#' @source 
#' Love, Robbie, Claire Dembry, Andrew Hardie, Vaclav Brezina & Tony McEnery. 2017. The Spoken BNC2014: Designing and building a spoken corpus of everyday conversations. \emph{International Journal of Corpus Linguistics}, 22(3), 319--344.
"spokenBNC2014_metadata"