# ---------
#  12-9
# ---------
# ---------------------------------------------------------- #

diamonds %>%
  group_by(cut) %>%
  summarize(AvgPrice=mean(price), SumCarat=sum(carat)) %>%
  arrange(AvgPrice)

diamonds %>%
  group_by(cut) %>%
  summarize(AvgPrice=mean(price), SumCarat=sum(carat)) %>%
  arrange(desc(AvgPrice))

# ---------------------------------------------------------- #