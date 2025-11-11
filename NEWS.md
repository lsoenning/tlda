# tlda (development version)

# tlda 0.1.0

* Initial CRAN submission.



* fixed function disp_DP(): default behavior fixed (freq_adjust = FALSE instead of TRUE)

* added function: scale_y_dispersion() to draw annotated y-axes when using ggplot2

* added function: add_sampling_weights()

* added function: disp_DMB()

* renamed data objects
  o "brown_metadata" -> "metadata_brown"
  o "ice_metadata" -> "metadata_ice"
  o "spokenBNC1994_metadata" -> "metadata_spokenBNC1994"
  o "spokenBNC2014_metadata" -> "metadata_spokenBNC2014"

* changed contents of data objects
  o metadata_brown: 
    - macro_genre and genre as ordered factors
    - added the variable "Word_count"
  o metadata_ice:  
    - macro_genre and genre as ordered factors
    - added the variable "Word_count"

* added data objects
  o "biber150_ice_gb_genre"
  o "biber150_ice_gb_macro_genre"