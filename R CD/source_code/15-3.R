# ---------
#  15-3
# ---------
# ---------------------------------------------------------- #

library(readr)
emotion <- read_tsv('http://www.jaredlander.com/data/reaction.txt')
emotion

# ---------------------------------------------------------- #

library(tidyr)
emotion %>%
    gather(key=Type,value=Measurement,Age,BMI,React,Regulate)

# ---------------------------------------------------------- #

library(tidyr)
emotionLong <- emotion %>%
    gather(key=Type,value=Measurement,Age,BMI,React,Regulate) %>%
    arrange(ID)

head(emotionLong,20)

# ---------------------------------------------------------- #

emotion %>%
    gather(key=Type,value=Measurement,-ID,-Test,-Gender)

identical(
        emotion %>%
          gather(key=Type,value=Measurement,-ID,-Test,-Gender),
        emotion %>%
          gather(key=Type,value=Measurement,Age,BMI,React,Regulate)
        )

# ---------------------------------------------------------- #

emotionLong %>%
        spread(key=Type,value=Measurement)

# ---------------------------------------------------------- #