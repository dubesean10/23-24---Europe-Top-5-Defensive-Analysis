library(rvest)
library(dplyr)
library(tidyverse)

# create complete big 5 europe stats df

completeBig5EuropeStats <- europe5miscStats %>%
  left_join(big5table, by = c("Squad" = "Squad")) %>%
  left_join(EuropeDefensiveActions, by = c("Squad" = "Squad")) %>%
  select(Rank, Squad, Comp, `# Pl`, W, D, L, GF, GA, Pts, xG, xGA, Attendance,
         CrdY, CrdR, `2CrdY`, Fls, Fld, Int, Tackles, `Tackles Won`, `Defensive Third Tackles`,
         `Midfield Third Tackles`, `Attacking Third Tackles`, `Tackles vs Dribbles`,
         `Dribbled Past`, `Tackle Success Rate %`, Pressures, `Pressures Success Rate %`,
         Blocks, `Blocked Shots`, `Blocked Shots on Target`, Interceptions,
         `Tackles + Interceptions`, Clearances)

completeBig5EuropeStats<- completeBig5EuropeStats %>%
  mutate(across(-c(Squad, Comp, Attendance), as.numeric))

