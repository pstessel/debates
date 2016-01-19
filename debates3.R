setwd("C:/Users/pstessel/Documents/repos/debates/texts/")

rm(list=ls(all = TRUE))

require(dplyr)
require(stringr)

# Read in text
debate.v <- scan("r_121515.1.txt", what="character", sep="\n")

debate.df <- data.frame(do.call('rbind', strsplit(as.character(debate.v),':',fixed=TRUE)))
class(debate.df)

can_lines <- debate.df[which(debate.df$X1=="CHRISTIE"),]
can_lines$X1 <- NULL
can_lines.v <- unlist(can_lines)
can_lines.v <- as.character(can_lines.v)
class(can_lines.v)
can_lines.v

#### Jockers, p. 15 ####

# Convert text to lowercase
can.lower.v <- tolower(can_lines.v)
can.lower.v
can.lower <- gsub("[^[:alnum:][:space:]']", " ", can.lower.v)
can.words.l <- strsplit(can.lower, "\\s+")
can.words.l

# Convert list to a vector
can.words.v <- unlist(can.words.l)
can.words.v

# Figure out which items in the vector are not blank
not.blanks.v <- which(can.words.v !="")
not.blanks.v

# Retain only non-blank items
can.words.v <- can.words.v[not.blanks.v]
class(can.words.v)
can.words.v

# Frequency table
can.freqs.t <- table(can.words.v)
sorted.can.freqs.t <- sort(can.freqs.t, decreasing = T)
sorted.can.freqs.t[1:10]

# Remove stopwords
require(tm)

can.words.c <- as.character(can.words.v)
can.words.stopped.c <- removeWords(can.words.c, stopwords('english'))
can.words.stopped.c

# Figure out which items in the vector are not blank
not.blanks.v <- which(can.words.stopped.c !="")
not.blanks.v

# Retain only non-blank items
can.words.v <- can.words.v[not.blanks.v]
class(can.words.v)
can.words.v

# Frequency table
can.freqs.t <- table(can.words.v)
sorted.can.freqs.t <- sort(can.freqs.t, decreasing = T)
sorted.can.freqs.t
sorted.can.freqs.t[1:10]


--------------------------------------------

# Convert vector of character strings to a corpus of text using 'VectorSource' and 'Corpus'
require(tm)
txt <- VectorSource(can_lines)
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
