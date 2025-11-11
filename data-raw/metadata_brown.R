## load names of the 500 text files
text_files <- readRDS("./data-raw/files/brown_text_labels.rds")

## initialize vectors for metadata
macro_genre <- rep(NA, 500)
genre <- rep(NA, 500)


## fill in metadata

# genre
macro_genre[substr(text_files, 1, 1) %in% c("A", "B", "C")] <- "press"
macro_genre[substr(text_files, 1, 1) %in% c("D", "E", "F", "G", "H")] <- "general_prose"
macro_genre[substr(text_files, 1, 1) %in% c("J")] <- "learned"
macro_genre[substr(text_files, 1, 1) %in% c("K", "L", "M", "N", "P", "R")] <- "fiction"

# subgenre
genre[substr(text_files, 1, 1) == "A"] <- "press_reportage"
genre[substr(text_files, 1, 1) == "B"] <- "press_editorial"
genre[substr(text_files, 1, 1) == "C"] <- "press_reviews"
genre[substr(text_files, 1, 1) == "D"] <- "religion"
genre[substr(text_files, 1, 1) == "E"] <- "skills_trades_hobbies"
genre[substr(text_files, 1, 1) == "F"] <- "popular_lore"
genre[substr(text_files, 1, 1) == "G"] <- "belles_lettres_biography_essays"
genre[substr(text_files, 1, 1) == "H"] <- "government_official_documents"
genre[substr(text_files, 1, 1) == "J"] <- "learned_and_scientific_writings"
genre[substr(text_files, 1, 1) == "K"] <- "general_fiction"
genre[substr(text_files, 1, 1) == "L"] <- "mystery_detective_fiction"
genre[substr(text_files, 1, 1) == "M"] <- "science_fiction"
genre[substr(text_files, 1, 1) == "N"] <- "adventure_western_fiction"
genre[substr(text_files, 1, 1) == "P"] <- "romance_love_story"
genre[substr(text_files, 1, 1) == "R"] <- "humour"



# Length of text files

tmp <- read.delim("./data-raw/files/corp_brown_token_df.tsv", sep = "\t")
colnames(tmp)[1] <- "text_file"
str(tmp)

text_file_wordcount <- data.frame(with(tmp, table(text_file)))
colnames(text_file_wordcount)[2] <- "word_count"


# combine into data frame
metadata_brown <- data.frame(
  text_file = text_files,
  macro_genre = genre,
  genre = genre,
  word_count = text_file_wordcount$word_count
)


metadata_brown$genre <- factor(
  metadata_brown$genre,
  levels = c("press",
             "general_prose",
             "learned",
             "fiction"),
  ordered = TRUE
)

metadata_brown$macro_genre <- factor(
  metadata_brown$macro_genre,
  levels = c("press_reportage",
             "press_editorial",
             "press_reviews",
             "religion",
             "skills_trades_hobbies",
             "popular_lore",
             "belles_lettres_biography_essays",
             "government_official_documents",
             "learned_and_scientific_writings",
             "general_fiction",
             "mystery_detective_fiction",
             "science_fiction",
             "adventure_western_fiction",
             "romance_love_story",
             "humour"),
  ordered = TRUE
)


save(metadata_brown, file = "data/metadata_brown.rda")




