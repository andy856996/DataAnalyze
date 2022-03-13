# ---------
#  12-7
# ---------
# ---------------------------------------------------------- #

summarize(diamonds, mean(price))
diamonds %>% summarize(mean(price))

# ---------------------------------------------------------- #

diamonds %>%
  summarize(AvgPrice=mean(price),
  MedianPrice=median(price),
  AvgCarat=mean(carat))

# ---------------------------------------------------------- #