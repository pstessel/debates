setwd("C:/Users/pstessel/Documents/repos/debates/texts/")

rm(list=ls(all = TRUE))

require(dplyr)
require(stringr)

# Read in text
debate.v <- scan("r_121515.1.txt", what="character", sep="\n")

debate.df <- data.frame(do.call('rbind', strsplit(as.character(debate.v),':',fixed=TRUE)))
class(debate.df)

trump_lines <- debate.df[which(debate.df$X1=="TRUMP"),]
trump_lines$X1 <- NULL
trump_lines.v <- unlist(trump_lines)
trump_lines.v <- as.character(trump_lines.v)
class(trump_lines.v)
trump_lines.v

#### Jockers, p. 15 ####

# Convert text to lowercase
trump.lower.v <- tolower(trump_lines.v)

# Split words into a list at spaces
trump.words.l <- strsplit(trump.lower.v, "\\W")
trump.words.l

# Convert list to a vector
trump.words.v <- unlist(trump.words.l)

# Figure out which items in the vector are not blank
not.blanks.v <- which(trump.words.v !="")
not.blanks.v

# Retain only non-blank items
trump.words.v <- trump.words.v[not.blanks.v]
trump.words.v

--------------------------------------------

# Convert vector of character strings to a corpus of text using 'VectorSource' and 'Corpus'
require(tm)
txt <- VectorSource(trump_lines)
txt.corpus <- Corpus(txt); rm(txt)
inspect(txt.corpus)

txt.corpus <- tm_map(txt.corpus, tolower)
inspect(txt.corpus)
txt.corpus <- tm_map(txt.corpus, removePunctuation)
txt.corpus <- tm_map(txt.corpus, removeNumbers)
txt.corpus <- tm_map(txt.corpus, removeWords, stopwords("english"))

# Stem to truncate words
require(SnowballC)
txt.corpus <- tm_map(txt.corpus, stemDocument)
detach("package:SnowballC")
inspect(txt.corpus)

# Remove empty spaces

txt.corpus <- tm_map(txt.corpus, stripWhitespace)
inspect(txt.corpus)

txt.corpus <- tm_map(txt.corpus, PlainTextDocument)

tdm <- TermDocumentMatrix(txt.corpus)
#inspect(tdm)
inspect(tdm)

terms <- inspect(tdm)
terms
terms <- order(terms)
terms

findFreqTerms(x=tdm, lowfreq=8, highfreq=Inf)
