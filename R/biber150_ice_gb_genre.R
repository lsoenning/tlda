#' Distribution of Biber et al.'s (2016) 150 lexical items in ICE-GB (term-document matrix), using genres as corpus parts
#'
#' This dataset contains text-level frequencies for ICE-GB (Nelson et al. 2002) for a set of 150 word forms. The list of items was compiled by Biber et al. (2016) for methodological purposes, that is, to study the behavior of dispersion measures in different distributional settings. The items are intended to cover a broad range of frequency and dispersion levels.
#'
#' While Biber et al. (2016: 446) used 153 target items, the 150 word forms included in the present data set correspond to the slightly narrower selection of forms used in Burch et al. (2017: 214-216). These 150 word forms are listed next, in alphabetical order: 
#' 
#' \emph{a, able, actually, after, against, ah, aha, all, among, an, and, another, anybody, at, aye, be, became, been, began, bet, between, bloke, both, bringing, brought, but, charles, claimed, cor, corp, cos, da, day, decided, did, do, doo, during, each, economic, eh, eighty, england, er, erm, etcetera, everybody, fall, fig, for, forty, found, from, full, get, government, ha, had, has, have, having, held, hello, himself, hm, however, hundred, i, ibm, if, important, in, inc, including, international, into, it, just, know, large, later, latter, let, life, ltd, made, may, methods, mhm, minus, mm, most, mr, mum, new, nineteen, ninety, nodded, nought, oh, okay, on, ooh, out, pence, percent, political, presence, provides, put, really, reckon, say, seemed, seriously, sixty, smiled, so, social, somebody, system, take, talking, than, the, they, thing, think, thirteen, though, thus, time, tt, tv, twenty, uk, under, urgh, us, usa, wants, was, we, who, with, world, yeah, yes, you, your}
#' 
#' The data are provided in the form of a term-document matrix, where rows denote the 150 items and columns denote the 32 genres in the corpus. Four items do not occur in ICE-GB (\emph{aye, corp, ltd, tt}). These are included in the term-document matrix with frequencies of 0 for all texts.
#' 
#' The first row of the term-document matrix gives the size of the genre (i.e. number of word tokens).
#'
#' @format ## `biber150_ice_gb_genre`
#' A matrix with 151 rows and 32 columns
#' 
#' \describe{
#'   \item{rows}{Size of the genre (\code{word_count}), followed by set of 150 items in alphabetical order (\emph{a, able, ..., you, your})}
#'   \item{columns}{32 genres, ordered alphabetically ("acad_humanities", "acad_natural_sciences", "acad_social_sciences", ... ,"student_essays", "unscripted_speeches"))}
#' }
#' @source 
#' Biber, Douglas, Randi Reppen, Erin Schnur & Romy Ghanem. 2016. On the (non)utility of Juilland’s D to measure lexical dispersion in large corpora. \emph{International Journal of Corpus Linguistics} 21(4). 439--464. 
#' 
#' Burch, Brent, Jesse Egbert & Douglas Biber. 2017. Measuring and interpreting lexical dispersion in corpus linguistics. \emph{Journal of Research Design and Statistics in Linguistics and Communication Science} 3(2). 189--216. 
#' 
#' Nelson, Gerald, Sean Wallis and Bas Aarts. 2002. \emph{Exploring Natural Language: Working with the British Component of the International Corpus of English}. Amsterdam: John Benjamins.
#' 
"biber150_ice_gb_genre"