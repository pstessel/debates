# clear_global_environment
rm(list=ls(all=T))

# set_working_directory
setwd("~/repos/contracts")

# change_blank_cells_to_NA

dat1 <- read.csv("data/marsh.csv", header=T, na.strings=c(""," ","NA","NaN","0","\na"))

ds <- dat1

# set_column_names_to_lower_case
names(ds) <- tolower(names(ds))

# report missing values in a data.frame
marsh.missing <- sapply(ds, function(x) sum(is.na(x)))

class(marsh.missing)

write.csv(marsh.missing, "data/marsh.missing")

################################################################################
# PREPARE CHARTS: MONTHS-BY-REVIEWER                                           #
################################################################################

require(dplyr)
require(ggplot2)

# Remove recoreds without date.received
dates <- ds[which(!is.na(ds$date.received)),]

# Create a new variable 'count' with a value of 1 if there is a date and 0 if not
# Not strictly necessary if code above is implemented

dates$count <- ifelse(!is.na(dates$date.received), 1, 0)

# Group contracts by responsible.marsh.attorney.paralegal then month
x <- group_by(dates, responsible.marsh.attorney.paralegal, month)

# Create a new data structure 'contracts.month' grouping by reviewers then month
# then summarising the groupings by the sum of 'count'
# then sorting the data by attorney.paralegal (which = first name)
contracts.month <- dates %>%
  group_by(responsible.marsh.attorney.paralegal, month) %>%
  summarise(count = sum(count)) %>%
  arrange(responsible.marsh.attorney.paralegal)

glimpse(contracts.month)
summary.stats <- summary(contracts.month)
class(contracts.month)
summary.stats

# Small multiples of contracts by attorney by month ------------
contracts.month$month <- factor(contracts.month$month, levels=month.abb)

# Creates a Small Multiples bar plot of each attorney giving their contract
# amounts by month.
ggplot(data=contracts.month, aes(x=month, y=count)) +
  # geom_point(stat = "identity") +
  geom_bar(stat = "identity") +
  facet_wrap(~responsible.marsh.attorney.paralegal, ncol = 7) +
  # facet_wrap(~attorney.paralegal, ncol = 6) +
  ggtitle("Marsh 2015 Contracts: July 1 - November 30") +
  theme(plot.title = element_text(family="Arial", face="bold", size=9, hjust=0, color="#555555")) +
  theme(axis.text.x = element_text(angle=90))

