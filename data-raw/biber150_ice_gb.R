

# Biber et al. (2016)'s 150 items

items <- c("a", "able", "actually", "after", "against", "ah", "aha", "all", "among", "an", "and", "another", "anybody", "at", "aye", "be", "became", "been", "began", "bet", "between", "bloke", "both", "bringing", "brought", "but", "charles", "claimed", "cor", "corp", "cos", "da", "day", "decided", "did", "do", "doo", "during", "each", "economic", "eh", "eighty", "england", "er", "erm", "etcetera", "everybody", "fall", "fig", "for", "forty", "found", "from", "full", "get", "government", "ha", "had", "has", "have", "having", "held", "hello", "himself", "hm", "however", "hundred", "i", "ibm", "if", "important", "in", "inc", "including", "international", "into", "it", "just", "know", "large", "later", "latter", "let", "life", "ltd", "made", "may", "methods", "mhm", "minus", "mm", "most", "mr", "mum", "new", "nineteen", "ninety", "nodded", "nought", "oh", "okay", "on", "ooh", "out", "pence", "percent", "political", "presence", "provides", "put", "really", "reckon", "say", "seemed", "seriously", "sixty", "smiled", "so", "social", "somebody", "system", "take", "talking", "than", "the", "they", "thing", "think", "thirteen", "though", "thus", "time", "tt", "tv", "twenty", "uk", "under", "urgh", "us", "usa", "wants", "was", "we", "who", "with", "world", "yeah", "yes", "you", "your")

# load data provided by Gries (2024)
ICEGB.df <- readRDS("data-raw/files/ICEGB.df.RDS")
str(ICEGB.df)

# change the spelling for four items that ar spelled differently in ICE-GB
items[items == "erm"] <- "uhm"
items[items == "urgh"] <- "ugh"
items[items == "etcetera"] <- "etc"
items[items == "inc"] <- "inc."

# reduce data to the subset of items
ice_gb_150 <- subset(
	ICEGB.df, WORD %in% items
)

# strip from file names the ".cor" suffix
ice_gb_150$PART <- str_split(
	ice_gb_150$PART, 
	pattern = "\\.", 
	simplify = TRUE)[,1]

# cross-tabulate number of occurrence of each item in each text
biber150_ice_gb <- table(ice_gb_150$WORD, ice_gb_150$PART)

# convert to matrix
biber150_ice_gb <- as.matrix(biber150_ice_gb)

# add those items that do not occur in ICE-GB
add_0_items <- matrix(
	0, 
	nrow = 4, 
	ncol = 500,
	dimnames = list(
		c("aye", "corp", "ltd", "tt"),
		colnames(biber150_ice_gb)
	))

# combine with matrix
biber150_ice_gb <- rbind(
	biber150_ice_gb,
	add_0_items
)

# order items alphabetically
biber150_ice_gb <- biber150_ice_gb[order(rownames(biber150_ice_gb)),]



# add word count
add_word_count <- matrix(
  table(ICEGB.df$PART), 
  nrow = 1, 
  ncol = 500,
  dimnames = list(
    "word_count",
    colnames(biber150_ice_gb)
  ))

biber150_ice_gb <- rbind(
  add_word_count,
  biber150_ice_gb
)

