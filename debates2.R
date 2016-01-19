setwd("/Volumes/HD2/Users/pstessel/Documents/git_repos/debates/texts/")

require(dplyr)

rm(list=ls(all = TRUE))

# Read in text
debate.v <- scan("r_121515.1.txt", what="character", sep="\n")
debate.v


debate.v <- do.call('rbind', strsplit(as.character(debate.v),':',fixed=TRUE))
debate.v

trump_lines.v <- debate.v[which(debate.v$X1=="TRUMP"),]
trump_lines.df
trump_lines.df$X1 <- NULL
trump_lines.df$X2 <- as.character(trump_lines$X2)
trump_lines.df


trump_lines.v <- paste(trump_lines.df, collapse=" ")
trump_lines.v

trump.lines.lower.v <- tolower(trump_lines.v)
trump.lines.lower.v

not.blanks.v <- which(trump.lines.lower.v !="")
not.blanks.v

# Total number of trump words
length(trump.lines.lower.v)

write.csv(new_table, file = "new_table.csv")
