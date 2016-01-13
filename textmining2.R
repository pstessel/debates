# 2016 State of the Union Analysis

setwd("C:/Users/pstessel/Documents/repos/debates/texts")

require(stringr)

sou <- readLines("sou_2016.txt")
sou

sou <- paste(sou, collapse ='')
sou

td.1 <- gsub("(\\v+|\\f+|\\n+ |\\r+|\\t+|\\s+)", " ", sou)
td.1 <- gsub("[[:space:]]", " ", sou)
td.1
td.2 <- gsub("(\\(Applause\\.\\)|\\(Laughter\\.\\))", "", td.1)
td.2
td.3 <- gsub("'", "", td.2)
td.3

text.d <- td.3; rm(td.1, td.2, td.3)


# Convert vector of character strings to a corpus of text using 'VectorSource' and 'Corpus'
require(tm)
txt <- VectorSource(text.d); rm(text.d)
txt.corpus <- Corpus(txt); rm(txt)
inspect(txt.corpus)

txt.corpus <- tm_map(txt.corpus, tolower)
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
#inspect(tdm[,1])
inspect(tdm)

tdm <- order(tdm)
?order

findFreqTerms(x=tdm, lowfreq=8, highfreq=Inf)
