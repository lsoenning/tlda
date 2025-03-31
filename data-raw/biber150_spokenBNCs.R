
library(tidyverse)

# 1994
#-------------------------------------------------------------------------------

# load data
biber150_spokenBNC1994 <- read_tsv(
  "data-raw/files/spoken_bnc_1994_150_tdm.tsv")

# convert to matrix, dropping column 1 (which gives the speaker ids)
d <- as.matrix(biber150_spokenBNC1994[,-1])

# speaker IDs as row names
rownames(d) <- biber150_spokenBNC1994$speaker

# transpose
d <- t(d)

# inspect
d[1:5, 1:5]

# assign new label
biber150_spokenBNC1994 <- d


# load biodata
spokenBNC1994_metadata <- read.csv(
  "./data-raw/files/spokenBNC1994DS_biodata.csv")

# exclude speakers with no information on age and/or gender
spokenBNC1994_metadata <- subset(
  spokenBNC1994_metadata,
  !is.na(u_age_group) & !is.na(u_sex))

# reduce term-document matrix to those speakers with sociodemographic information
biber150_spokenBNC1994 <- biber150_spokenBNC1994[, colnames(biber150_spokenBNC1994) %in% spokenBNC1994_metadata$u_who]

str(biber150_spokenBNC1994)



# 2014
#-------------------------------------------------------------------------------

biber150_spokenBNC2014 <- read_tsv("data-raw/files/spoken_bnc_2014_150_tdm.tsv")

str(biber150_spokenBNC2014)

d <- as.matrix(biber150_spokenBNC2014[,-1])

rownames(d) <- biber150_spokenBNC2014$speaker

colnames(d)
rownames(d)

str(d)
dimnames(d)

d <- t(d)
d[1:5, 1:5]

biber150_spokenBNC2014 <- d

# exclude speakers with no socio-demographic information
biber150_spokenBNC2014 <- biber150_spokenBNC2014[,1:668]


