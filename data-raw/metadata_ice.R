## load names of the 500 text files
text_files <- readRDS("./data-raw/files/ice_text_labels.rds")

## initialize vectors for metadata
mode <- rep(NA, 500)
text_category <- rep(NA, 500)
macro_genre <- rep(NA, 500)
genre <- rep(NA, 500)
genre_short <- rep(NA, 500)


## fill in metadata

# mode
mode[grep("^s", text_files)] <- "spoken"
mode[grep("^w", text_files)] <- "written"

# text type
text_category[grep("^s1", text_files)] <- "dialogues"
text_category[grep("^s2", text_files)] <- "monologues"
text_category[grep("^w1", text_files)] <- "non_printed"
text_category[grep("^w2", text_files)] <- "printed"


# macro genre
macro_genre[grep("^s1a", text_files)] <- "private_dialogues"
macro_genre[grep("^s1b", text_files)] <- "public_dialogues"
macro_genre[grep("^s2a", text_files)] <- "unscripted_monologues"
macro_genre[grep("^s2b", text_files)] <- "scripted_monologues"
macro_genre[grep("^w1a", text_files)] <- "student_writing"
macro_genre[grep("^w1b", text_files)] <- "letters"
macro_genre[grep("^w2a", text_files)] <- "academic_writing"
macro_genre[grep("^w2b", text_files)] <- "popular_writing"
macro_genre[grep("^w2c", text_files)] <- "reportage"
macro_genre[grep("^w2d", text_files)] <- "instructional_writing"
macro_genre[grep("^w2e", text_files)] <- "persuasive_writing"
macro_genre[grep("^w2f", text_files)] <- "creative_writing"

# genre
con    <- paste0("s1a-", sprintf("%03d",  1:90))
ph     <- paste0("s1a-", sprintf("%03d", 91:100))
les    <- paste0("s1b-", sprintf("%03d",  1:20))
bdis   <- paste0("s1b-", sprintf("%03d", 21:40))
bint   <- paste0("s1b-", sprintf("%03d", 41:50))
parl   <- paste0("s1b-", sprintf("%03d", 51:60))
cr     <- paste0("s1b-", sprintf("%03d", 61:70))
btrans <- paste0("s1b-", sprintf("%03d", 71:80))
com    <- paste0("s2a-", sprintf("%03d",  1:20))
unsp   <- paste0("s2a-", sprintf("%03d", 21:50))
dem    <- paste0("s2a-", sprintf("%03d", 51:60))
leg    <- paste0("s2a-", sprintf("%03d", 61:70))
bnew   <- paste0("s2b-", sprintf("%03d",  1:20))
btal   <- paste0("s2b-", sprintf("%03d", 21:40))
nbtal  <- paste0("s2b-", sprintf("%03d", 41:50))
ess    <- paste0("w1a-", sprintf("%03d",  1:10))
ex     <- paste0("w1a-", sprintf("%03d", 11:20))
sl     <- paste0("w1b-", sprintf("%03d",  1:15))
bl     <- paste0("w1b-", sprintf("%03d", 16:30))
Ahum   <- paste0("w2a-", sprintf("%03d",  1:10))
Asoc   <- paste0("w2a-", sprintf("%03d", 11:20))
Anat   <- paste0("w2a-", sprintf("%03d", 21:30))
Atec   <- paste0("w2a-", sprintf("%03d", 31:40))
Phum   <- paste0("w2b-", sprintf("%03d",  1:10))
Psoc   <- paste0("w2b-", sprintf("%03d", 11:20))
Pnat   <- paste0("w2b-", sprintf("%03d", 21:30))
Ptec   <- paste0("w2b-", sprintf("%03d", 31:40))
adm    <- paste0("w2d-", sprintf("%03d",  1:10))
skho   <- paste0("w2d-", sprintf("%03d", 11:20))

# short labels
genre_short[text_files %in% con]      <- "con"
genre_short[text_files %in% ph]       <- "ph"
genre_short[text_files %in% les]      <- "les"
genre_short[text_files %in% bdis]     <- "bdis"
genre_short[text_files %in% bint]     <- "bint"
genre_short[text_files %in% parl]     <- "parl"
genre_short[text_files %in% cr]       <- "cr"
genre_short[text_files %in% btrans]   <- "btrans"
genre_short[text_files %in% com]      <- "com"
genre_short[text_files %in% unsp]     <- "unsp"
genre_short[text_files %in% dem]      <- "dem"
genre_short[text_files %in% leg]      <- "leg"
genre_short[text_files %in% bnew]     <- "bnew"
genre_short[text_files %in% btal]     <- "btal"
genre_short[text_files %in% nbtal]    <- "nbtal"
genre_short[text_files %in% ess]      <- "ess"
genre_short[text_files %in% ex]       <- "ex"
genre_short[text_files %in% sl]       <- "sl"
genre_short[text_files %in% bl]       <- "bl"
genre_short[text_files %in% Ahum]     <- "Ahum"
genre_short[text_files %in% Asoc]     <- "Asoc"
genre_short[text_files %in% Anat]     <- "Anat"
genre_short[text_files %in% Atec]     <- "Atec"
genre_short[text_files %in% Phum]     <- "Phum"
genre_short[text_files %in% Psoc]     <- "Psoc"
genre_short[text_files %in% Pnat]     <- "Pnat"
genre_short[text_files %in% Ptec]     <- "Ptec"
genre_short[grep("^w2c", text_files)] <- "rep"
genre_short[text_files %in% adm]      <- "adm"
genre_short[text_files %in% skho]     <- "skho"
genre_short[grep("^w2e", text_files)] <- "ed"
genre_short[grep("^w2f", text_files)] <- "nov"

# long labels
genre[text_files %in% con]      <- "face_to_face_conversations"
genre[text_files %in% ph]       <- "phonecalls"
genre[text_files %in% les]      <- "classroom_lessons"
genre[text_files %in% bdis]     <- "broadcast_discussions"
genre[text_files %in% bint]     <- "broadcast_interviews"
genre[text_files %in% parl]     <- "parliamentary_debates"
genre[text_files %in% cr]       <- "legal_cross_examinations"
genre[text_files %in% btrans]   <- "business_transactions"
genre[text_files %in% com]      <- "spontaneous_commentaries"
genre[text_files %in% unsp]     <- "unscripted_speeches"
genre[text_files %in% dem]      <- "demonstrations"
genre[text_files %in% leg]      <- "legal_presentations"
genre[text_files %in% bnew]     <- "broadcast_news"
genre[text_files %in% btal]     <- "broadcast_talks"
genre[text_files %in% nbtal]    <- "non_broadcast_talks"
genre[text_files %in% ess]      <- "student_essays"
genre[text_files %in% ex]       <- "exam_scripts"
genre[text_files %in% sl]       <- "social_letters"
genre[text_files %in% bl]       <- "business_letters"
genre[text_files %in% Ahum]     <- "acad_humanities"
genre[text_files %in% Asoc]     <- "acad_social_sciences"
genre[text_files %in% Anat]     <- "acad_natural_sciences"
genre[text_files %in% Atec]     <- "acad_technology"
genre[text_files %in% Phum]     <- "pop_humanities"
genre[text_files %in% Psoc]     <- "pop_social_sciences"
genre[text_files %in% Pnat]     <- "pop_natural_sciences"
genre[text_files %in% Ptec]     <- "pop_technology"
genre[grep("^w2c", text_files)] <- "press_news_reports"
genre[text_files %in% adm]      <- "administrative_writing"
genre[text_files %in% skho]     <- "skills_hobbies"
genre[grep("^w2e", text_files)] <- "press_editorials"
genre[grep("^w2f", text_files)] <- "novels_short_stories"


# combine into data frame
metadata_ice <- data.frame(
  text_file = text_files,
  mode = mode,
  text_category = text_category,
  macro_genre = macro_genre,
  genre = genre,
  genre_short = genre_short
)

metadata_ice$text_category <- factor(
  metadata_ice$text_category,
  levels = c("dialogues",
             "monologues",
             "non_printed",
             "printed"),
  ordered = TRUE
)

metadata_ice$macro_genre <- factor(
  metadata_ice$macro_genre,
  levels = c("private_dialogues",
             "public_dialogues",
             "unscripted_monologues",
             "scripted_monologues",
             "student_writing",
             "letters",
             "academic_writing",
             "popular_writing",
             "reportage",
             "instructional_writing",
             "persuasive_writing",
             "creative_writing"),
  ordered = TRUE
)
  
metadata_ice$genre <- factor(
  metadata_ice$genre,
  levels = c("face_to_face_conversations",
             "phonecalls",
             "classroom_lessons",
             "broadcast_discussions",
             "broadcast_interviews",
             "parliamentary_debates",
             "legal_cross_examinations",
             "business_transactions",
             "spontaneous_commentaries",
             "unscripted_speeches",
             "demonstrations",
             "legal_presentations",
             "broadcast_news",
             "broadcast_talks",
             "non_broadcast_talks",
             "student_essays",
             "exam_scripts",
             "social_letters",
             "business_letters",
             "acad_humanities",
             "acad_social_sciences",
             "acad_natural_sciences",
             "acad_technology",
             "pop_humanities",
             "pop_social_sciences",
             "pop_natural_sciences",
             "pop_technology",
             "press_news_reports",
             "administrative_writing",
             "skills_hobbies",
             "press_editorials",
             "novels_short_stories"),
  ordered = TRUE
)


metadata_ice$genre_short <- factor(
  metadata_ice$genre_short,
  levels = c("con",
             "ph",
             "les",
             "bdis",
             "bint",
             "parl",
             "cr",
             "btrans",
             "com",
             "unsp",
             "dem",
             "leg",
             "bnew",
             "btal",
             "nbtal",
             "ess",
             "ex",
             "sl",
             "bl",
             "Ahum",
             "Asoc",
             "Anat",
             "Atec",
             "Phum",
             "Psoc",
             "Pnat",
             "Ptec",
             "rep",
             "adm",
             "skho",
             "ed",
             "nov"),
  ordered = TRUE
)

save(metadata_ice, file = "data/metadata_ice.rda")
