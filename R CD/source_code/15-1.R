# ---------
#  15-1
# ---------
# ---------------------------------------------------------- #

# 載入 dplyr
library(dplyr)
library(tibble)

# 建立一個擁有兩個直行的tibble
sportLeague <- tibble(sport=c("Hockey", "Baseball", "Football"),
                      league=c("NHL", "MLB", "NFL"))
  
# 建立一個擁有一個直行的tibble
trophy <- tibble(trophy=c("Stanley Cup", "Commissioner's Trophy",
                          "Vince Lombardi Trophy"))

# 將它們合併成一個tibble
trophies1 <- bind_cols(sportLeague, trophy)

# 使用tribble建立另一個tibble(逐列建立的捷徑)
trophies2 <- tribble(
              ~sport, ~league, ~trophy,
              "Basketball", "NBA", "Larry O'Brien Championship Trophy",
              "Golf", "PGA", "Wanamaker Trophy"
              )

# 將它們合併成一個tibble 
trophies <- bind_rows(trophies1, trophies2)
trophies
          
# ---------------------------------------------------------- #