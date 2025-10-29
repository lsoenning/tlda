library(tidyverse)

# 1994
#-------------------------------------------------------------------------------

# load biodata
metadata_spokenBNC1994 <- read.csv(
  "./data-raw/files/spokenBNC1994DS_biodata.csv")

# exclude speakers with no information on age and/or gender
metadata_spokenBNC1994 <- subset(
  metadata_spokenBNC1994,
  !is.na(u_age_group) & !is.na(u_sex))


str(metadata_spokenBNC1994)

colnames(metadata_spokenBNC1994) <- c(
  "speaker_id",
  "age_group",
  "gender",
  "age",
  "n_words",
  "n_tokens",
  "age_bin"
)

metadata_spokenBNC1994$age <- as.numeric(str_replace(metadata_spokenBNC1994$age, "\\+", ""))

str(metadata_spokenBNC1994)



# 2014
#-------------------------------------------------------------------------------

# load biodata
metadata_spokenBNC2014 <- read.csv2(
  "./data-raw/files/spokenBNC2014_biodata.csv", row.names = 1)

str(metadata_spokenBNC2014)

metadata_spokenBNC2014 <- metadata_spokenBNC2014 |> 
  select(
    ID, 
    Age..BNC1994.groups.,
    Gender,
    age_numeric,
    total,
    age_bins
  )

colnames(metadata_spokenBNC2014) <- c(
  "speaker_id",
  "age_group",
  "gender",
  "age",
  "n_tokens",
  "age_bin"
)

str(metadata_spokenBNC2014)

