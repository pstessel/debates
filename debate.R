# clear_global_environment
rm(list=ls(all=T))

# set_working_directory
setwd("~/repos/debates/texts")
getwd()

debate.v <- scan("r_121515.txt", what="character", sep="\n")

gsub("(^.*)(:){1}(.+\n\n)", "", debate.v)
?gsub

debate.1 <- grep("(.+\n\n)*", "", debate.v)
debate.1[[1]]

# trim leading and trailing spaces from the lines
debate.v <- stri_trim(debate.v)

# get rid of empty lines
debate.v <- debate.v[nzchar(debate.v)]
debate.v

# remove stage directions
stage_dir_rx <- exactly(
  OPEN_BRACKET %R%
    one_or_more(printable()) %R%
    "]"
)
is_stage_dir_line <- stri_detect_regex(debate.v, stage_dir_rx)
debate.v <- debate.v[!is_stage_dir_line]
is_stage_dir_line

# match lines containing "character.dialogue"
character_dialogue_rx <- START %R%
  optional(capture(one_or_more(alpha()) %R% lookahead(":"))) %R%
  optional(":") %R%
  zero_or_more(space()) %R%
  capture(one_or_more(printable()))
matches <- stri_match_first_regex(debate.v, character_dialogue_rx)
character_dialogue_rx

# store the matches in a data.table (needed for the roll functionality);
# a line number key column is also needed shortly
debate_data <- data.table(
  line_number = seq_len(nrow(matches)),
  character = matches[,2],
  dialogue = matches[,3]
)