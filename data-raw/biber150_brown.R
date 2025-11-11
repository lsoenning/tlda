library(tidyverse)

# Biber et al. (2016)'s 150 items

items <- c("a", "able", "actually", "after", "against", "ah", "aha", "all", "among", "an", "and", "another", "anybody", "at", "aye", "be", "became", "been", "began", "bet", "between", "bloke", "both", "bringing", "brought", "but", "charles", "claimed", "cor", "corp", "cos", "da", "day", "decided", "did", "do", "doo", "during", "each", "economic", "eh", "eighty", "england", "er", "erm", "etcetera", "everybody", "fall", "fig", "for", "forty", "found", "from", "full", "get", "government", "ha", "had", "has", "have", "having", "held", "hello", "himself", "hm", "however", "hundred", "i", "ibm", "if", "important", "in", "inc", "including", "international", "into", "it", "just", "know", "large", "later", "latter", "let", "life", "ltd", "made", "may", "methods", "mhm", "minus", "mm", "most", "mr", "mum", "new", "nineteen", "ninety", "nodded", "nought", "oh", "okay", "on", "ooh", "out", "pence", "percent", "political", "presence", "provides", "put", "really", "reckon", "say", "seemed", "seriously", "sixty", "smiled", "so", "social", "somebody", "system", "take", "talking", "than", "the", "they", "thing", "think", "thirteen", "though", "thus", "time", "tt", "tv", "twenty", "uk", "under", "urgh", "us", "usa", "wants", "was", "we", "who", "with", "world", "yeah", "yes", "you", "your")

# load data provided by Gries (2024)
Brown.df <- read.delim("data-raw/files/corp_brown_token_df.tsv", sep = "\t")
Brown.df$token <- tolower(Brown.df$token)
Brown.df$text_id[Brown.df$text_id == " E19"] <- "E19"
str(Brown.df)


items[!(items %in% Brown.df$token)]

# change the spelling for four items that are spelled differently in Brown
items[items == "usa"] <- "u.s.a."
items[items == "inc"] <- "inc."
items[items == "mr"] <- "mr."
items[items == "urgh"] <- "ugh"
items[items == "er"] <- "uh"
items[items == "erm"] <- "um"
items[items == "hm"] <- "hmm"


# reduce data to the subset of items
brown_150 <- subset(
  Brown.df, token %in% items
)


# Corpus parts: Text files
#-------------------------------------------------------------------------------

# cross-tabulate number of occurrence of each item in each text
biber150_brown <- table(brown_150$token, brown_150$text_id)

# convert to matrix
biber150_brown <- as.matrix(biber150_brown)

# add those items that do not occur in Brown
add_0_items <- matrix(
  0, 
  nrow = 7, 
  ncol = 500,
  dimnames = list(
    c("aha", "cor", "cos", "ltd", "mhm", "nought", "pence"),
    colnames(biber150_brown)
  ))

# combine with matrix
biber150_brown <- rbind(
  biber150_brown,
  add_0_items
)

# order items alphabetically
biber150_brown <- biber150_brown[order(rownames(biber150_brown)),]



# add word count
add_word_count <- matrix(
  table(Brown.df$text_id), 
  nrow = 1, 
  ncol = 500,
  dimnames = list(
    "word_count",
    colnames(biber150_brown)
  ))

biber150_brown <- rbind(
  add_word_count,
  biber150_brown
)



# Corpus parts: Genres
#-------------------------------------------------------------------------------

str(brown_150)

# add text metadata
colnames(brown_150)[1] <- "text_file"
brown_150 <- brown_150[,c(1, 3)]
brown_150 <- merge(brown_150, metadata_brown, by = "text_file")

str(brown_150)


# cross-tabulate number of occurrence of each item in each text
biber150_brown_genre <- table(brown_150$token, brown_150$genre)

# convert to matrix
biber150_brown_genre <- as.matrix(biber150_brown_genre)
str(biber150_brown_genre)

# add those items that do not occur in ICE-GB
add_0_items <- matrix(
  0, 
  nrow = 7, 
  ncol = 15,
  dimnames = list(
    c("aha", "cor", "cos", "ltd", "mhm", "nought", "pence"),
    colnames(biber150_brown_genre)
  ))

# combine with matrix
biber150_brown_genre <- rbind(
  biber150_brown_genre,
  add_0_items
)

# order items alphabetically
biber150_brown_genre <- biber150_brown_genre[order(rownames(biber150_brown_genre)),]

# columns ordered based on sampling frame?
colnames(biber150_brown_genre) == levels(metadata_brown$genre)

# add word count
add_word_count <- matrix(
  table(brown_150$genre), 
  nrow = 1, 
  ncol = 15,
  dimnames = list(
    "word_count",
    colnames(biber150_brown_genre)
  ))

biber150_brown_genre <- rbind(
  add_word_count,
  biber150_brown_genre
)

str(biber150_brown_genre)





# Corpus parts: Macro-genres
#-------------------------------------------------------------------------------

# cross-tabulate number of occurrence of each item in each text
biber150_brown_macro_genre <- table(brown_150$token, brown_150$macro_genre)

# convert to matrix
biber150_brown_macro_genre <- as.matrix(biber150_brown_macro_genre)
str(biber150_brown_macro_genre)

# add those items that do not occur in ICE-GB
add_0_items <- matrix(
  0, 
  nrow = 7, 
  ncol = 4,
  dimnames = list(
    c("aha", "cor", "cos", "ltd", "mhm", "nought", "pence"),
    colnames(biber150_brown_macro_genre)
  ))

# combine with matrix
biber150_brown_macro_genre <- rbind(
  biber150_brown_macro_genre,
  add_0_items
)

# order items alphabetically
biber150_brown_macro_genre <- biber150_brown_macro_genre[order(rownames(biber150_brown_macro_genre)),]

# columns ordered based on sampling frame?
str(biber150_brown_macro_genre)
colnames(biber150_brown_macro_genre) == levels(metadata_brown$macro_genre)


# add word count
add_word_count <- matrix(
  table(brown_150$macro_genre), 
  nrow = 1, 
  ncol = 4,
  dimnames = list(
    "word_count",
    colnames(biber150_brown_macro_genre)
  ))

biber150_brown_macro_genre <- rbind(
  add_word_count,
  biber150_brown_macro_genre
)

str(biber150_brown_macro_genre)

usethis::use_data(biber150_brown)
usethis::use_data(biber150_brown_genre)
usethis::use_data(biber150_brown_macro_genre)


