library(rvest)
library(dplyr)
library(tidyverse)

# URL for defensive actions statistics
DefensiveActionsUrl <- "https://fbref.com/en/comps/Big5/defense/squads/Big-5-European-Leagues-Stats"

# Read the HTML content of the page
page <- read_html(DefensiveActionsUrl)

# Extract tables from the page
tables <- page %>% html_table()

# Access the table of interest (usually the first table on the page)
EuropeDefensiveActions <- tables[[1]]

EuropeDefensiveActions <- EuropeDefensiveActions[-1, ]

# Use the second row of the Dataframe as the header
colnames(EuropeDefensiveActions) <- c("\Rank", "Squad", "Competition", "# Players", "90s Played", "Tackles", "Tackles Won", 
                                      "Defensive Third Tackles", "Midfield Third Tackles", "Attacking Third Tackles", 
                                      "Tackles vs Dribbles", "Dribbled Past", "Tackle Success Rate %", "Pressures", 
                                      "Pressures Success Rate %", "Blocks", "Blocked Shots", "Blocked Shots on Target", 
                                      "Interceptions", "Tackles + Interceptions", "Clearances", "Errors")