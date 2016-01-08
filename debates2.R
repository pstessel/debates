setwd("C:/Users/pstessel/Documents/repos/debates/texts/")

require(dplyr)

rm(list=ls(all = TRUE))

# Read in text
debate.v <- scan("r_121515.1.txt", what="character", sep="\n")
debate.v


debate.df <- data.frame(do.call('rbind', strsplit(as.character(debate.v),':',fixed=TRUE)))

trump_lines <- debate.df[which(debate.df$X1=="TRUMP"),]
trump_lines$X1 <- NULL
trump_lines$X2 <- as.character(trump_lines$X2)
class(trump_lines)
trump_lines.v <- as.vector(trump_lines)
trump_lines.l <- as.list(trump_lines)

trump.lines.lower.v <- tolower(trump_lines.v)




















class(new_table)
new_table <- rename(new_table, c(V1="speaker", V2="lines"))

trump_lines <- subset(new_table, speaker="TRUMP", select="lines")

# Pull out the spoken lines
speakers <- debate.v[grepl("^[^\\s]\\w+:", debate.v)]
speakers

# Split off the speaker's names
speaker_split <- debate.v(speakers, split = ":")
speaker_split

# Get names
speaker_names <- sapply(speaker_split, "[", 1L)
speaker_names

# And what they said (collapsing because their lines may have had other colons that we lost):
speaker_parts <- sapply(speaker_split, function(x) paste(x[-1L], collapse = ":"))
unlist(speaker_parts)
speaker_parts

trump <- which(speaker_names == "TRUMP")

(unlist(strsplit(speaker_parts[trump], split = "")) == " ")


write.csv(new_table, file = "new_table.csv")
