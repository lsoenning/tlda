#' Text metadata for Brown corpora
#'
#' This dataset provides metadata for the text files in the Brown family of 
#' corpora. It maps standardized file names to the textual categories genre
#' and subgenre.
#'
#' @format ## `brown_metadata`
#' A data frame with 500 rows and 3 columns:
#' 
#' \describe{
#'   \item{text_file}{Standardized name of the text file (e.g. "A01", "J58", "R07")}
#'   \item{macro_genre}{4 macro genres ("press", "general_prose", "learned", "fiction")}
#'   \item{genre}{15 genres (e.g. "press_editorial", "popular_lore", "adventure_western_fiction"))}
#' }
#' @source 
#' McEnery, Tony & Andrew Hardie. 2012. \emph{Corpus linguistics}. Cambridge: Cambridge University Press.
"brown_metadata"