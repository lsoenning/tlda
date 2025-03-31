#' Speaker metadata for the Spoken BNC1994
#'
#' This dataset provides some metadata for speakers in the demographically sampled part of the Spoken BNC1994 (Crowdy 1995), including information on age, gender, and the total number of word tokens contributed to the corpus.
#'
#' @format ## `spokenBNC1994_metadata`
#' A data frame with 1,017 rows and 7 columns:
#' 
#' \describe{
#'   \item{speaker_id}{Speaker ID (e.g. "PS002", "PS003")}
#'   \item{age_group}{Age group, based on the BNC1994 scheme ("0-14", "15-24", "25-34", "35-44", "45-59", "60+", "Unknown")}
#'   \item{gender}{Speaker gender ("Female" vs. "Male")}
#'   \item{age}{Age of speaker;  if actual age is not available, imputed based on \code{age_group} and \code{age_bin}}
#'   \item{n_tokens}{Number of word tokens the speaker contributed to the corpus}
#'   \item{age_bin}{Age group, based on the BNC2014 scheme ("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+")}
#' }
#' @source 
#' Crowdy, Steve. 1995. The BNC spoken corpus. In Geoffrey Leech, Greg Myers & Jenny Thomas (eds.), \emph{Spoken English on Computer: Transcription, Mark-Up and Annotation}, 224–234. Harlow: Longman.
"spokenBNC1994_metadata"