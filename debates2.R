setwd("/Volumes/HD2/Users/pstessel/Documents/Git_Repos/debates")

rm(list=ls(all = TRUE))

# Read in text
debate.v <- scan("texts/r_121515.1.txt", what="character", sep="\n")
debate.v

# Pull out the spoken lines
speakers <- debate.v[grepl("^[^\\s]\\w+:", debate.v)]
speakers

# Split off the speaker's names
speaker_split <- strsplit(speakers, split = ":")
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
