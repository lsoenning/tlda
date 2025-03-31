

# 1994
#-------------------------------------------------------------------------------

# load biodata
spokenBNC1994_metadata <- read.csv(
  "./data-raw/files/spokenBNC1994DS_biodata.csv")

# exclude speakers with no information on age and/or gender
spokenBNC1994_metadata <- subset(
  spokenBNC1994_metadata,
  !is.na(u_age_group) & !is.na(u_sex))


str(spokenBNC1994_metadata)

colnames(spokenBNC1994_metadata) <- c(
  "speaker_id",
  "age_group",
  "gender",
  "age",
  "n_words",
  "n_tokens",
  "age_bin"
)

spokenBNC1994_metadata$age <- as.numeric(str_replace(spokenBNC1994_metadata$age, "\\+", ""))

str(spokenBNC1994_metadata)



# 2014
#-------------------------------------------------------------------------------

# load biodata
spokenBNC2014_metadata <- read.csv2(
  "./data-raw/files/spokenBNC2014_biodata.csv", row.names = 1)

str(spokenBNC2014_metadata)

spokenBNC2014_metadata <- spokenBNC2014_metadata |> 
  select(
    ID, 
    Age..BNC1994.groups.,
    Gender,
    age_numeric,
    total,
    age_bins
  )

colnames(spokenBNC2014_metadata) <- c(
  "speaker_id",
  "age_group",
  "gender",
  "age",
  "n_tokens",
  "age_bin"
)

str(spokenBNC2014_metadata)

