# tlda (development version)

# tlda 0.1.0

* Initial CRAN submission.



* fixed function disp_DP(): default behavior fixed (freq_adjust = FALSE instead of TRUE)
* added function: scale_y_dispersion() to draw annotated y-axes when using ggplot2
* added function: add_sampling_weights()
* added function: disp_DMB()
* renamed data objects
  - "brown_metadata" -> "metadata_brown"
  - "ice_metadata" -> "metadata_ice"
  - "spokenBNC1994_metadata" -> "metadata_spokenBNC1994"
  - "spokenBNC2014_metadata" -> "metadata_spokenBNC2014"
* changed contents of data objects
  - metadata_brown: macro_genre and genre as ordered factors
  - metadata_ice: text_category, macro_genre and genre as ordered factors
* added data objects
  - "biber150_ice_gb_genre"
  - "biber150_ice_gb_macro_genre"