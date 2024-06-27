library(rvest)
library(dplyr)
library(tidyverse)


# URL for squad statistics
SquadStatsUrl <- "https://fbref.com/en/comps/Big5/Big-5-European-Leagues-Stats"

# Read the HTML content of the page
page <- read_html(SquadStatsUrl)

# Extract tables from the page
tables <- page %>% html_table()

# Access the table of interest
big5table <- tables[[1]]