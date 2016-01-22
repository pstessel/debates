if(grepl("apple", R.Version()$platform)) {
  setwd("/Volumes/HD2/Users/pstessel/Documents/git_repos/debates/texts")
} else {
  setwd("C:/Users/pstessel/Documents/repos/debates/texts/")
  }

rm(list=ls(all = TRUE))

require(dplyr)
require(stringr)

# Read in text
debate.v <- scan("r_121515.1.txt", what="character", sep="\n")

debate.df <- data.frame(do.call('rbind', strsplit(as.character(debate.v),':',fixed=TRUE)))
class(debate.df)
debate.df


# Remove moderators and audience questions
mods <- list("BLITZER", "HEWITT", "BASH", "QUESTION")
mods
debate.df <- debate.df[ ! debate.df$X1 %in% mods, ]

candidates <- as.list(unique(debate.df$X1))
candidates

x <- c("TRUMP")

#words <- function(x){

  can_lines <- debate.df[which(debate.df$X1==(x)),]
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
  s <- summary(can.words.v)

  # Determine which items in the vector are not blank
  not.blanks.v <- which(can.words.v !="")
  not.blanks.v

  # Retain only non-blank items
  can.words.v <- can.words.v[not.blanks.v]
  class(can.words.v)
  can.words.v
  can.words.len <- length(can.words.v)
  tot.can.words <- can.words.len
  tot.can.words
  can.words.len
  can.words.len <- can.words.len*10

  # Frequency table
  can.freqs.t <- table(can.words.v)
  sorted.can.freqs.t <- sort(can.freqs.t, decreasing = T)
  sorted.can.freqs.t[1:10]

  # Remove stopwords
  require(tm)

  can.words.c <- as.character(can.words.v)
  can.words.stopped.c <- removeWords(can.words.c, stopwords('SMART'))
  can.words.stopped.c
  # removeNumbers(can.words.stopped.c)

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
  a <- as.data.frame(sorted.can.freqs.t[1:10])
  b <- as.data.frame(sorted.can.freqs.t[1:10]/can.words.len)

  rownames(a)

plot(b, type = "b", xaxt="n",
     main = paste(x,"'s Words", collapse = " "),
     xlab = "Top 10 Words",
     ylab = "Relative Frequency")
axis(1, at=1:10, labels = rownames(b))
axis(2, at=seq(0, 1, by=.01))

#}

#lapply(candidates, words)


a
?plot
?rownames

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

?removeWords
getTransformations()
