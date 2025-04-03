
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tlda

<!-- badges: start -->
<!-- badges: end -->

This package includes a number of utility functions and resources for
language data analysis. At the moment, the focus is on corpus-linguistic
**dispersion analysis** (see Gries 2021; Sönning 2025), which quantifies
how widely and/or evenly and item is distributed across corpus parts.
This kind of analysis requires two variables:

- the *subfrequencies*, i.e. the number of times the item is found in
  each corpus part
- the *size of the corpus parts* (number of word tokens)

The package includes functions that allow you to calculate seven
different parts-based dispersion measures, including their
frequency-adjusted version. Subfrequencies and part sizes can be
supplied in two forms, either as vectors or as a term-document matrix.
For some measures, different formulas are found in the literature, and
the user can choose among these. The following indices are implemented:

- *Range* (Keniston 1920)
- *D* (Juilland & Chang-Rodriguez 1964)
- *D<sub>2</sub>* (Carroll 1970)
- *S* (Rosengren 1971)
- *D<sub>P</sub>* (Gries 2008; modification: Egbert et al. 2020)
- *D<sub>A</sub>* (Burch et al. 2017)
- *D<sub>KL</sub>* (Gries 2020, 2021)

## Usage

The function `disp()` calculating seven dispersion measures based on two
vectors:

- `subfreq` a set of subfrequencies, i.e. the number of occurrences of
  the item in each corpus part
- `partsize` a vector with the size of the corpus parts

The argument `directionality` controls the scaling of the scores:

- `conventional`: higher values reflect a **more even** distribution
- `gries`: higher values reflect a **less even** distribution

The function prints information about the directionality of scaling and
details about the formula used.

As an example, we will use data from Lyne’s (1985) classic study and
consider the distribution of the French lemma ALLEMAND across the ten
(nearly) equal-sized parts (‘Tenths’) of his corpus of French business
correspondence. The part sizes are taken from Figure 1 (p. 85) and the
subfrequencies from Appendix I (p. 299).

``` r
library(tlda)

x <- c(2, 0, 1, 1, 3, 0, 3, 0, 0, 0)
y <- c(8143, 8058, 8271, 8125, 7959, 7941, 8146, 8001, 8003, 7930)

disp(
  subfreq = x,
  partsize = y,
  directionality = "conventional"
)
#>      Rrel         D        D2         S        DP        DA       DKL 
#> 0.5000000 0.6038797 0.6521444 0.4763652 0.5009295 0.3081640 0.4665078
#> 
#> Scores follow conventional scaling:
#>   0 = maximally uneven/bursty/concentrated distribution (pessimum)
#>   1 = maximally even/dispersed/balanced distribution (optimum)
#> 
#> For Gries's DP, the function uses the modified version suggested by
#>   Egbert et al. (2020)
#> 
#> For DKL, standardization to the unit interval [0,1] is based on the
#>   odds-to-probability transformation, see Gries (2024: 90)
```

If we prefer the reversed scaling used by Gries (2008), we can change
the value of the argument `directionality`, like so:

``` r
disp(
  subfreq = x,
  partsize = y,
  directionality = "gries"
)
#>      Rrel         D        D2         S        DP        DA       DKL 
#> 0.5000000 0.3961203 0.3478556 0.5236348 0.4990705 0.6918360 0.5334922
#> 
#> Scores follow scaling used by Gries (2008):
#>   0 = maximally even/dispersed/balanced distribution (optimum)
#>   1 = maximally uneven/bursty/concentrated distribution (pessimum)
#> 
#> For Gries's DP, the function uses the modified version suggested by
#>   Egbert et al. (2020)
#> 
#> For DKL, standardization to the unit interval [0,1] is based on the
#>   odds-to-probability transformation, see Gries (2024: 90)
```

To calculate dispersion for multiple items, it makes sense to provide
the data in the form of a term-document matrix. In this tabular
arrangement,

- each row represents an item
- each column represents a corpus part.

A number of example data sets are shipped with the `tlda` package,
including `biber150_ice_gb`, a term-document matrix recording the
text-level subfrequencies for Biber et al.’s (2016) 150 lexical items in
ICE-GB. Importantly, the first row gives the number of word tokens in
the text file. This is an excerpt of the matrix:

``` r
biber150_ice_gb[1:5, 1:5]
#>            s1a-001 s1a-002 s1a-003 s1a-004 s1a-005
#> word_count    2195    2159    2287    2290    2120
#> a               50      38      44      67      35
#> able             2       4       4       0       0
#> actually         3       6       2       2       6
#> after            0       0       0       0       4
```

The function `disp_tdm()` calculates seven dispersion measures for each
item in the matrix. The output is therefore also a matrix. In this
function, the arguments `subfreq` and `partsize` are replaced by the two
following:

- `tdm` A term-document matrix, where rows represent items and columns
  corpus parts; it must also contain a row giving the size of the corpus
  parts (first or last row in the TDM)
- `row_partsize` Character string indicating which row in the TDM
  contains the size of the corpus parts.

The following calculates dispersion score for the first ten items in the
term-document matrix:

``` r
disp_tdm(
    tdm = biber150_ice_gb[1:11,], 
    row_partsize = "first_row",
    print_score = TRUE,
    verbose = FALSE)
#>           Rrel         D        D2           S          DP          DA
#> a        1.000 0.9870710 0.9933147 0.979212413 0.888365693 0.839288095
#> able     0.454 0.9335737 0.8439188 0.418157031 0.455174970 0.308646817
#> actually 0.578 0.9347875 0.8582008 0.481715623 0.463542822 0.317577246
#> after    0.708 0.9523801 0.9098015 0.637161938 0.590496159 0.451164597
#> against  0.384 0.9190571 0.8085548 0.344950569 0.384476586 0.246239297
#> ah       0.170 0.8597923 0.6663541 0.144339551 0.166632009 0.099347394
#> aha      0.008 0.5013281 0.2230140 0.007645153 0.007646451 0.005906302
#> all      0.970 0.9705118 0.9646528 0.876345852 0.736685461 0.635556781
#> among    0.206 0.8932566 0.7232523 0.196528249 0.209653551 0.148815290
#> an       0.984 0.9730939 0.9717710 0.906244754 0.763723695 0.672268981
#>                DKL
#> a        0.9439996
#> able     0.4176334
#> actually 0.4367701
#> after    0.5535184
#> against  0.3686444
#> ah       0.2485908
#> aha      0.1245100
#> all      0.7593061
#> among    0.2894787
#> an       0.7990869
```

## Installation

You can install the development version of `tlda` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("lsoenning/tlda")
```

## References

Burch, Brent, Jesse Egbert & Douglas Biber. 2017. Measuring and
interpreting lexical dispersion in corpus linguistics. 3(2). 189–216.

Carroll, John B. 1970. An alternative to Juilland’s usage coefficient
for lexical frequencies and a proposal for a standard frequency index.
3(2). 61–65.

Egbert, Jesse, Brent Burch & Douglas Biber. 2020. Lexical dispersion and
corpus design. 25(1). 89–115.

Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora.
13(4). 403–437.

Gries, Stefan Th. 2020. Analyzing dispersion. In Magali Paquot & Stefan
Th. Gries (eds.), , 99–118. New York: Springer.

Gries, Stefan Th. 2021. A new approach to (key) keywords analysis: Using
frequency, and now also dispersion. 9(2). 1−33.

Juilland, Alphonse G. & Eugenio Chang-Rodríguez. 1964. The Hague: Mouton
de Gruyter.

Keniston, Hayward. 1920. Common words in Spanish. 3(2). 85–96.

Lijffijt, Jefrey & Stefan Th. Gries. 2012. Correction to Stefan Th.
Gries’ ‘Dispersions and adjusted frequencies in corpora’. 17(1).
147–149.

Lyne, Anthony A. 1985. . Paris: Slatkine-Champion.

Rosengren, Inger. 1971. The quantitative concept of language and its
relation to the structure of frequency dictionaries.  1. 103–127.
