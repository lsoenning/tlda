
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tlda

<!-- badges: start -->
<!-- badges: end -->

This package includes a number of utility functions and resources for
language data analysis, with a focus on corpus-linguistic research
tasks.

## Installation

You can install the development version of tlda from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("lsoenning/tlda")
```

## Example: Dispersion measures

The following example demonstrates the use of the function `disp()` for
calculating various dispersion measures based on two vectors:

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
subfrequencies from Appendix I (p. 299). The following dispersion
measures are calculated (returned in chronological order):

- $R_{rel}$ relative range (Keniston 1920)
- $D$ (Juilland & Chang-Rodriguez 1964)
- $D_2$ (Carroll 1970)
- $S$ (Rosengren 1971)
- $D_P$ (Gries 2008; modification: Egbert et al. 2020)
- $D_A$ (Burch et al. 2017)
- $D_{KL}$ (Gries 2020, 2021)

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
#> 0.5000000 0.6038797 0.6521444 0.4763652 0.4462310 0.3081640 0.3186739
#> 
#> Scores follow conventional scaling:
#>   0 = maximally uneven/bursty/concentrated distribution (pessimum)
#>   1 = maximally even/dispersed/balanced distribution (optimum)
#> For background on the formulas, see Soenning (2025) []
#> For Gries's DP, the function uses the modified version suggested by
#>   Egbert et al. (2020) [https://doi.org/10.1075/ijcl.18010.egb]
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
#> 0.5000000 0.6038797 0.6521444 0.4763652 0.4462310 0.3081640 0.3186739
#> 
#> Scores follow scaling used by Gries (2008):
#>   0 = maximally even/dispersed/balanced distribution (optimum)
#>   1 = maximally uneven/bursty/concentrated distribution (pessimum)
#> For background on the formulas, see Soenning (2025) []
#> For Gries's DP, the function uses the modified version suggested by
#>   Egbert et al. (2020) [https://doi.org/10.1075/ijcl.18010.egb]
```

Critically, the functions offered by the current package for calculating
dispersion **keep apart the formula and the directionality of scaling**.
This means that the `directionality` argument always overrides the
scaling implemented in the original formula; the default is
`conventional`.

For three dispersion measures, separate functions offer more options:

- `disp_R()` for $Range$
- `disp_DP()` for Gries’s *deviation of proportions*
- `disp_DA()` for $D_A$

For $Range$, three choice are available: (i) relative range
(`relative`), i.e. the *proportion* of corpus parts containing at least
one occurrence of the item (default); (ii) absolute range (`absolute`),
i.e. the *number* of corpus parts containing at least one occurrence of
the item; and (iii) relative range with size (`relative_withsize`), the
proportional version that takes into account the size of the corpus
parts (see Gries 2022: 179-180; Gries 2024: 27-28). The following code
returns absolute range:

``` r
disp_R(
  subfreq = x,
  partsize = y,
  type = "absolute"
)
#> R_abs 
#>     5
#> 
#> Scores represent absolute range, i.e. the number of corpus parts containing
#>   at least one occurrence of the item.
```

For Gries’s *deviation of proportions*, the function `disp_DP()` allows
the user to choose from three different formulas that are found in the
literature. The following code uses the original formula in Gries
(2008), but with `conventional` scaling:

``` r
disp_DP(
  subfreq = x,
  partsize = y,
  formula = "gries_2008",
  directionality = "conventional"
)
#>        DP 
#> 0.5009295
#> 
#> Scores follow conventional scaling:
#>   0 = maximally uneven/bursty/concentrated distribution (pessimum)
#>   1 = maximally even/dispersed/balanced distribution (optimum)
#> Computed using the original version of DP proposed by
#>   Gries (2008) [https://doi.org/10.1075/ijcl.13.4.02gri]
#> For background on the formulas for DP, see:
#>   Gries 2020 [https://doi.org/10.1007/978-3-030-46216-1_5]
#>   Soenning 2025 []
```

For $D_A$, three formulas are available, The `basic` version is direct
implementation of the actual formula for this measure, which is
computationally expensive if the number of corpus parts is large. Wilcox
(1973: 343) gives a `shortcut` version, which is much quicker (see this
[blog
post](https://lsoenning.github.io/posts/2023-12-11_computation_DA/)).
Finally, `shortcut_mod` is a slightly adapted form of the shortcut
(experimental), which ensures that scores do not exceed 1 (conventional
scaling). The following code uses Wilcox’s (1973) shortcut version:

``` r
disp_DA(
  subfreq = x,
  partsize = y,
  formula = "shortcut",
  directionality = "conventional"
)
#>        DA 
#> 0.4192751
#> 
#> Scores follow conventional scaling:
#>   0 = maximally uneven/bursty/concentrated distribution (pessimum)
#>   1 = maximally even/dispersed/balanced distribution (optimum)
#> Computed using the computational shortcut suggested by
#>   Wilcox (1967: 343, 'MDA', column 4) [https://doi.org/10.2307/446831]
#> For background on the formula for DA, see:
#>   Soenning 2025 []
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
