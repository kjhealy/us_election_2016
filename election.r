### US State Election Data http://uselectionatlas.org/RESULTS/data.php?year=2016&datatype=national&def=1&f=0&off=0&elect=0


library(htmltab)
library(rvest)
library(tidyverse)

## Clean formattings
strip_pct <- function(x) {
    gsub("\\%", "", x)
}

strip_comma <- function(x) {
    gsub(",", "", x)
}

clean_col <- function(x) {
    o <- strip_pct(x)
    strip_comma(o)
}


url <- "http://uselectionatlas.org/RESULTS/data.php?year=2016&datatype=national&def=1&f=0&off=0&elect=0"

election <- htmltab(doc = url, which = "//*[(@id = 'data')]")

page <- read_html(url)
save(page, file = "data/page")


election_dirty <- page %>% html_node("#datatable") %>% html_table()


## Remove unneeded columns
election_dirty <- election_dirty[,-c(1:2)]

## Clean colnames
dirty_names <- election_dirty[1,]
clean_names <- c("state", "ev_dem", "ev_rep", "ev_oth",
                              "total_vote", "rank_clinton", "rank_trump", "rank_johnson",
                              "vote_margin", "pct_margin",
                              "pct_clinton", "pct_trump", "pct_johnson", "pct_other",
                              "clinton_vote", "trump_vote", "johnson_vote", "other_vote")

length(dirty_names)
length(clean_names)

colnames(election_dirty) <- clean_names

## Check
cbind(as.character(dirty_names), clean_names)

## Remove blank rows
election_dirty <- election_dirty[!apply(election_dirty== "", 1, all),]

## Remove first rows (old varnames)
election_dirty <- election_dirty[2:nrow(election_dirty),]

## Remove last row (trailing and total)
election_dirty <- election_dirty[1:(nrow(election_dirty)-2),]

## Normalize DC name
election_dirty$state <- stringr::str_replace(election_dirty$state, "D. C.", "District of Columbia")

## Clean columns of formatting
election_dirty <- election_dirty %>% mutate_all(clean_col)

## Convert all cols except "state" to numeric
election_dirty <- election_dirty %>% mutate_each("as.numeric", -state)



election_dirty <- election_dirty %>% mutate(r_points = pct_trump - pct_clinton,
                                d_points = pct_clinton - pct_trump)

## fips
fips <- read_csv("data/state-fips-master.csv")

ind <- match(election$state, fips$state.name)

election_dirty$st <- fips$state.abbr[ind]
election_dirty$st[9] <- "DC"
election_dirty$fips <- fips$fips[ind]
election_dirty$fips[9] <- 11




election <- election_dirty %>%
    select(state, st, fips, total_vote, vote_margin,
           pct_margin, r_points, d_points, pct_clinton, pct_trump, pct_johnson, pct_other, clinton_vote,
           trump_vote, johnson_vote, other_vote, ev_dem, ev_rep, ev_oth)

election <- as_tibble(election)

## Save csv
write_csv(election, "data/election_state_16.csv")


## Send object over to socviz
save(election, file = "~/Source/socviz/data/election.rda")
