# clear_global_environment
rm(list=ls(all=T))

# set_working_directory
setwd("~/repos/debates")
getwd()

debate <- scan("r_121515.txt", what = "character", sep = "\n")

speakers <- debate[grepl("^[^\\s]\\w+:", debate)]

speaker_split <- strsplit(speakers, split = ":")
speaker_split

speaker_names <- sapply(speaker_split, "[", 1L)
speaker_names

speaker_parts <- sapply(speaker_split, function(x) paste(x[-1L], collapse = ":"))
speaker_parts

trump <- which(speaker_names == "TRUMP")

trump_words <- unlist(strsplit(speaker_parts[trump], split = " "))
trump_words

x <- c("abc", "def", "cba a", "aa")
m <- regexpr("a+", x, perl = T)
m