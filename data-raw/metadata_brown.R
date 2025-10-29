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


# combine into data frame
metadata_brown <- data.frame(
  text_file = text_files,
  macro_genre = genre,
  genre = genre
)

