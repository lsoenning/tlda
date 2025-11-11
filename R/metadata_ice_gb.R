#' Text metadata for ICE-GB
#'
#' This dataset provides metadata for the text files in ICE-GB (Nelson et al. 2002). It maps standardized file names to various textual categories such as mode of production, macro genre and genre, and records the length of each text file (in the total number of word and nonword tokens). Text categories, macro genres and genres are ordered based on the sampling frame informing the design of the ICE family of corpora (see https://www.ice-corpora.uzh.ch/en/design.html).
#'
#' @format ## `metadata_ice`
#' A data frame with 500 rows and 7 columns:
#' 
#' \describe{
#'   \item{text_file}{Standardized name of the text file (e.g. "s1a-001", "w1b-008", "w2d-018")}
#'   \item{mode}{Mode of production ("spoken" vs. "written")}
#'   \item{text_category}{4 higher-level text categories ("dialogues", "monologues", "non-printed", "printed"); ordered factor}
#'   \item{macro_genre}{12 macro genres (e.g. "private_dialogues", "student_writing", "reportage"); ordered factor}
#'   \item{genre}{32 genres (e.g. "phonecalls", "unscripted_speeches", "novels_short_stories"); ordered factor}
#'   \item{genre_short}{Short label for the genre (see Schützler 2023: 228); ordered factor}
#'   \item{word_count}{The length of the text file, expressed as the number of (word and nonword) tokens}
#' }
#' @source 
#' <https://www.ice-corpora.uzh.ch/en/design.html>
#' 
#' Greenbaum, Sidney. 1996. Introducing ICE. In Sidney Greenbaum (ed.), \emph{Comparing English worldwide: The International Corpus of English}, 3--12. Clarendon Press.
#' 
#' Nelson, Gerald, Sean Wallis, and Bas Aarts. 2002. \emph{Exploring Natural Language: Working with the British Component of the International Corpus of English}. John Benjamins.
#' 
#' Schützler, Ole. 2023. \emph{Concessive constructions in varieties of English}. Language Science Press. \doi{doi:10.5281/zenodo.8375010} 
"metadata_ice_gb"