# tlda (development version)

# tlda 0.1.0

* Initial CRAN submission.


* bug fixes in functions
  + disp_DP(): default behavior fixed (freq_adjust = FALSE instead of TRUE)
  + disp_DA(): shortcut formula corrected; argument "procedure" removed
  + disp_DA_tdm(): shortcut formula corrected; argument "procedure" removed
  + disp(): now uses shortcut formula for DA

* output of TDM-functions changed
  + affected: disp_tdm(), disp_DP_tdm(), disp_DA_tdm(), disp_DKL_tdm(), disp_S_tdm(), disp_R_tdm()) 
  + change: now return a data frame with a column "item", and (optionally) with a column giving the sum of all subfrequencies, i.e. the total number of occurrences of the item

* added functions 
  + scale_y_dispersion() to draw annotated y-axes when using ggplot2
  + add_sampling_weights()
  + disp_DMB()
  + fpower()
  + invfpower()
  + fpower_trans()
  + scale_x_fpower()
  + scale_y_fpower()

* renamed data objects
  + "brown_metadata" -> "metadata_brown"
  + "ice_metadata" -> "metadata_ice_gb"
  + "spokenBNC1994_metadata" -> "metadata_spokenBNC1994"
  + "spokenBNC2014_metadata" -> "metadata_spokenBNC2014"

* changed contents of data objects
  + metadata_brown: 
    - macro_genre and genre as ordered factors
    - added the variable "Word_count"
  + metadata_ice_gb:  
    - macro_genre and genre as ordered factors
    - added the variable "Word_count"

* added data objects
  + "biber150_ice_gb_genre"
  + "biber150_ice_gb_macro_genre"
  + "biber150_brown"
  + "biber150_brown_genre"
  + "biber150_brown_macro_genre"