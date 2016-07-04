#Raw debate transcripts obtained from http://www.presidency.ucsb.edu/debates.php

if(grepl("PHSs-MacBook-Air.local", Sys.info()["nodename"])) {
  setwd("/Users/PHS/Documents/Git_Repos/debates")
} else if(grepl("iMac", Sys.info()["nodename"])) {
  setwd("/Volumes/HD2/Users/pstessel/Documents/git_repos/debates")
} else {
  setwd("C:/Users/pstessel/Documents/repos/debates")
}
getwd()

rm(list=ls(all = TRUE)) #Clear environment
dev.off(dev.list()["RStudioGD"]) #Clear devices
cat("\014") #Clear console

require(dplyr)
require(stringr)

party <- ("d_")
debate.date <- ("030916")
txt.path <- ("texts/") # path for text files
img.path <- ("images/") # path for image files

# Read in text
debate.v <- scan(paste(txt.path,party,debate.date,".txt",
                       collapse = "", sep = ""),
                 what="character", sep="\n")
debate.v

Encoding(debate.v) <- "UTF-8"
iconv(debate.v, "latin1", "UTF-8",sub='') ## replace any non UTF-8 by ''
debate.v

debate.v <- str_replace_all(debate.v,"[^[:graph:]]", " ")
debate.v

d1 <- gsub("(^\\w*):", "~\\1|", debate.v, perl = T)
d1
d2 <- gsub(":", "", d1)
d2
d3 <- gsub("\\r*|\\n*|\\t*", "", d2)
d3
d4 <- gsub("~(.*?)\\|", "\\1:", d3, perl = T)
d4
d5 <- gsub("\\[.*?\\]", "", debate.v, perl = T) #remove bracketed words (and brackets)
d5

# Figure out which items in the vector are not blank
not.blanks.d5 <- which(d5 !="")
not.blanks.d5

# Retain only non-blank items
d6 <- d5[not.blanks.d5]
d6
class(d6)

d7 <- gsub("^\\s+|\\s+$", "", d6)
d7

debate.df <- data.frame(do.call('rbind',
                                strsplit(as.character(d7),
                                         ':',fixed = TRUE)))
class(debate.df)
debate.df

# Remove moderators and audience questions
mods <- list("RAMOS", "SALINAS", "TUMULTY")
mods
debate.df <- debate.df[ ! debate.df$X1 %in% mods, ]

candidates <- as.list(unique(debate.df$X1))
candidates

debate.df$X1

x <- c("CLINTON")
who <- x
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
  can.lower <- gsub("[[:digit:]]", " ", can.lower)
  can.words.l <- strsplit(can.lower, "\\s+")
  can.words.l

  # Convert list of candidate words to a vector
  can.words.v <- unlist(can.words.l)
  can.words.v
  length(can.words.v) # get vector length

  # Determine which items in the vector are not blank
  not.blanks.v <- which(can.words.v !="")
  not.blanks.v

  # Retain only non-blank items
  can.words.v <- can.words.v[not.blanks.v]
  class(can.words.v)
  can.words.v
  can.words.len <- length(can.words.v)
  tot.can.words <- can.words.len # total number of candidate words
  tot.can.words
  can.words.len

  # Frequency table of candidate words
  can.freqs.t <- table(can.words.v)
  can.freqs.t
  sorted.can.freqs.t <- sort(can.freqs.t, decreasing = T) # sort words in descending order
  sorted.can.freqs.t[1:10] # get the top ten words

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

  #Number of unique words used by candidate (excluding stop words)
  can.unique.v <- unique(can.words.v)
  can.unique.v
  length(can.unique.v)

  # Frequency table
  can.freqs.t <- table(can.words.v)
  can.freqs.t
  sorted.can.freqs.t <- sort(can.freqs.t, decreasing = T)
  sorted.can.freqs.t
  sorted.can.freqs.t[1:10]

  a <- as.data.frame(sorted.can.freqs.t[1:10])
  b <- as.data.frame((sorted.can.freqs.t[1:10]/can.words.len))

 # [MAKE THIS A BAR PLOT AND TURN ON SIDE]
  png(paste("top_10_words_",who,".png"))
  plot(b, type = "b", xaxt="n",
      main = paste(x,"'s Words", collapse = ""),
      xlab = "Top 10 Words",
      ylab = "Relative Frequency")
  axis(1, at=1:10, labels = rownames(b))
  axis(2, at=seq(0, 1, by=.1))
  dev.off()

  require(qdap)
  x <- can.unique.v
  mean.syllables <- mean(syllable_sum(x))
mean.syllables



#===================================================================

library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

--------------------------------------------

# Convert vector of character strings to a corpus of text using 'VectorSource' and 'Corpus'
require(tm)
  wordCorpus <- Corpus(VectorSource(can.words.v))
  wordCorpus <- tm_map(wordCorpus, removePunctuation)
  wordCorpus <- tm_map(wordCorpus, removeNumbers)
  wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
  #wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
  wordCorpus <- tm_map(wordCorpus, removeWords, c("amp"))
  wordCorpus <- tm_map(wordCorpus, stripWhitespace)


  png(paste("wordcloud_",party,debate.date,"_",who,".png", collapse = ""))
  pal <- brewer.pal(9, "Blues")
  pal <- pal[-(1:4)]
  set.seed(123)
  wordcloud(words = wordCorpus, scale = c(3,0.1), max.words = 100, random.order = F,
            rot.per = 0.25, use.r.layout = F, colors = pal)
  dev.off()


# Sentiment Analysis ------------------------------------------------------
  library(syuzhet)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(reshape2)
  library(dplyr )

  mySentiment <- get_nrc_sentiment(can.words.v)

  head(mySentiment)

  words <- (cbind(can.words.v, mySentiment))
  class(words)

# Look at the sentiment scores for the eight emotions from the NRC lexicon
sentimentTotals <- data.frame(colSums(words[,c(2:9)]))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
  max <- max(sentimentTotals$count)
  min <- min(sentimentTotals$count)
  sentimentTotals$norm <- (sentimentTotals$count-min)/(max-min)
  sentimentTotals$norm
  sum <- sum(sentimentTotals$count)
  rownames(sentimentTotals) <- NULL
  ggplot(data = sentimentTotals, aes(x = sentiment, y = count/sum)) +
    geom_bar(aes(fill = sentiment), stat = "identity") +
    theme(legend.position = "none") +
    xlab("Sentiment") + ylab("Relative Value") +
    # ggtitle("Total Sentiment Score for Trump Debate Words\n2/4/16")
    ggtitle(paste("Total Sentiment Score for", who,"'s Words\n",debate.date,collapse = ""))
  ggsave(paste("sentiment_",party,debate.date,who,".png", collapse=""))
# END ---------------------------------------------------------------------

?ggplot
