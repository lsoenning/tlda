#' Text metadata for ICE corpora
#'
#' This dataset provides metadata for the text files in the ICE family of 
#' corpora. It maps standardized file names to various textual categories
#' such as mode of production, macro genre and genre.
#'
#' @format ## `ice_metadata`
#' A data frame with 500 rows and 6 columns:
#' 
#' \describe{
#'   \item{text_file}{Standardized name of the text file (e.g. "s1a-001", "w1b-008", "w2d-018")}
#'   \item{mode}{Mode of production ("spoken" vs. "written")}
#'   \item{text_category}{4 higher-level text categories ("dialogues", "monologues", "non-printed", "printed")}
#'   \item{macro_genre}{12 macro genres (e.g. "private_dialogues", "student_writing", "reportage")}
#'   \item{genre}{32 genres (e.g. "phonecalls", "unscripted_speeches", "novels_short_stories")}
#'   \item{genre_short}{Short label for the genre (see Schützler 2023: 228)}
#' }
#' @source 
#' <https://www.ice-corpora.uzh.ch/en/design.html>
#' 
#' Greenbaum, Sidney. 1996. Introducing ICE. In Sidney Greenbaum (ed.), \emph{Comparing English worldwide: The International Corpus of English}, 3--12. Oxford: Clarendon Press.
#' 
#' Schützler, Ole. 2023. \emph{Concessive constructions in varieties of English}. Berlin: Language Science Press. \doi{doi:10.5281/zenodo.8375010} 
"ice_metadata"