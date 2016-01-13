# http://www.unt.edu/rss/class/Jon/Benchmarks/TextMining_L_JDS_Jan2014.pdf

policy.HTML.page <- readLines("http://www.weil.com/people/garrett-charon")
length(policy.HTML.page)
policy.HTML.page
policy.HTML.page[256:258]
id.1 <- 1 + which(policy.HTML.page == "            <div class=\"intro\">")
id.2 <- id.1 + 2
text.data <- policy.HTML.page[id.1:id.2]
text.data

td.1 <- gsub(pattern = "<p>", replacement = "", x = text.data)
td.1
td.2 <- gsub("&nbsp;", replacement = "", x = td.1)
td.2
td.3 <- gsub("&(rs|rd|ls|ld)quo;", "", td.2)
td.3
td.4 <- gsub("<em>", "", td.3)
td.4
td.5 <- gsub("</em>", "", td.4)
td.5
td.6 <- gsub("</p>", "", td.5)
td.6
td.7 <- gsub("</div>", "", td.6)
td.7
td.8 <- gsub(" (\\t+|\\s+)", " ", td.7)
td.8

text.d <- td.8; rm(text.data, td.1, td.2, td.3, td.4, td.5, td.6, td.7, td.8)

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
inspect(tdm[,1])

findFreqTerms(x=tdm, lowfreq=2, highfreq=Inf)
