library(rvest)
library(dplyr)
library(tidyverse)

# URL for miscellaneous squad statistics
MiscStatsurl <- "https://fbref.com/en/comps/Big5/misc/squads/Big-5-European-Leagues-Stats"

# Read the HTML content of the page
page <- read_html(MiscStatsurl)

# Extract tables from the page
tables <- page %>% html_table()

# Access the table of interest
europe5miscStats <- tables[[1]]

# Set column names from the first row
colnames(europe5miscStats) <- europe5miscStats[1, ]

# Remove the first row
europe5miscStats <- europe5miscStats[-1, ]

# Convert all columns except "Squad" and "Comp" to numeric in europe5miscStats
europe5miscStats <- europe5miscStats %>%
  mutate(across(-c(Squad, Comp), as.numeric))