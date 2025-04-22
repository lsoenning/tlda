#' Distribution of Biber et al.'s (2016) 150 lexical items in the Spoken BNC2014 (term-document matrix)
#'
#' This dataset contains speaker-level frequencies for the Spoken BNC2014 (Love et al. 2017) for a set of 150 word forms. The list of items was compiled by Biber et al. (2016) for methodological purposes, that is, to study the behavior of dispersion measures in different distributional settings. The items are intended to cover a broad range of frequency and dispersion levels.
#'
#' While Biber et al. (2016: 446) used 153 target items, the 150 word forms included in the present data set correspond to the slightly narrower selection of forms used in Burch et al. (2017: 214-216). These 150 word forms are listed next, in alphabetical order: 
#' 
#' \emph{a, able, actually, after, against, ah, aha, all, among, an, and, another, anybody, at, aye, be, became, been, began, bet, between, bloke, both, bringing, brought, but, charles, claimed, cor, corp, cos, da, day, decided, did, do, doo, during, each, economic, eh, eighty, england, er, erm, etcetera, everybody, fall, fig, for, forty, found, from, full, get, government, ha, had, has, have, having, held, hello, himself, hm, however, hundred, i, ibm, if, important, in, inc, including, international, into, it, just, know, large, later, latter, let, life, ltd, made, may, methods, mhm, minus, mm, most, mr, mum, new, nineteen, ninety, nodded, nought, oh, okay, on, ooh, out, pence, percent, political, presence, provides, put, really, reckon, say, seemed, seriously, sixty, smiled, so, social, somebody, system, take, talking, than, the, they, thing, think, thirteen, though, thus, time, tt, tv, twenty, uk, under, urgh, us, usa, wants, was, we, who, with, world, yeah, yes, you, your}
#' 
#' The data are provided in the form of a term-document matrix, where rows denote the 150 items and columns denote the 668 speakers in the corpus. Speakers with the label "UNKFEMALE", "UNKMALE", and "UNKMULTI" are not included in the dataset.
#' 
#' The first row of the term-document matrix gives the total number of words (i.e. number of word tokens) the speaker contributed to the corpus.
#'
#' @format ## `biber150_spokenBNC2014`
#' A matrix with 151 rows and 668 columns
#' 
#' \describe{
#'   \item{rows}{Total number of words by speaker (\code{word_count}), followed by set of 150 items in alphabetical order (\emph{a, able, ..., you, your})}
#'   \item{columns}{668 speakers, ordered by ID ("S0001","S0002", ... , "S0691", "S0692"))}
#' }
#' @source 
#' Biber, Douglas, Randi Reppen, Erin Schnur & Romy Ghanem. 2016. On the (non)utility of Juilland’s D to measure lexical dispersion in large corpora. \emph{International Journal of Corpus Linguistics} 21(4). 439--464. 
#' 
#' Burch, Brent, Jesse Egbert & Douglas Biber. 2017. Measuring and interpreting lexical dispersion in corpus linguistics. \emph{Journal of Research Design and Statistics in Linguistics and Communication Science} 3(2). 189--216. 
#' 
#' Love, Robbie, Claire Dembry, Andrew Hardie, Vaclav Brezina & Tony McEnery. 2017. The Spoken BNC2014: Designing and building a spoken corpus of everyday conversations. \emph{International Journal of Corpus Linguistics}, 22(3), 319--344.
#' 
"biber150_spokenBNC2014"