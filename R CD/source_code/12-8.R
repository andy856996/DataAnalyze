# ---------
#  12-8
# ---------
# ---------------------------------------------------------- #

diamonds %>%
  group_by(cut) %>%
  summarize(AvgPrice=mean(price))

# ---------------------------------------------------------- #

diamonds %>%
  group_by(cut) %>%
  summarize(AvgPrice=mean(price), SumCarat=sum(carat))

diamonds %>%
  group_by(cut, color) %>%
  summarize(AvgPrice=mean(price), SumCarat=sum(carat))

# ---------------------------------------------------------- #