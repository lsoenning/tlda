# tlda (development version)

# tlda 0.1.0

* Initial CRAN submission.



* fixed function disp_DP(): default behavior fixed (freq_adjust = FALSE instead of TRUE)

* functions taking as input a term-document matrix (disp_tdm, disp_DP_tdm, disp_DA_tdm, disp_DKL_tdm, disp_S_tdm, disp_R_tdm) now return a data frame with a column "item", and (optionally) with a column giving the sum of all subfrequencies, i.e. the total number of occurrences of the item

* added function: scale_y_dispersion() to draw annotated y-axes when using ggplot2

* added function: add_sampling_weights()

* added function: disp_DMB()

* renamed data objects
  o "brown_metadata" -> "metadata_brown"
  o "ice_metadata" -> "metadata_ice_gb"
  o "spokenBNC1994_metadata" -> "metadata_spokenBNC1994"
  o "spokenBNC2014_metadata" -> "metadata_spokenBNC2014"

* changed contents of data objects
  o metadata_brown: 
    - macro_genre and genre as ordered factors
    - added the variable "Word_count"
  o metadata_ice_gb:  
    - macro_genre and genre as ordered factors
    - added the variable "Word_count"

* added data objects
  o "biber150_ice_gb_genre"
  o "biber150_ice_gb_macro_genre"
  o "biber150_brown
  o "biber150_brown_genre"
  o "biber150_brown_macro_genre"