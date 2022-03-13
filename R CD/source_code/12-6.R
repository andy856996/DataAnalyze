# ---------
#  12-6
# ---------
# ---------------------------------------------------------- #

diamonds %>% mutate(price/carat)

# ---------------------------------------------------------- #

diamonds %>% select(carat, price) %>% mutate(price/carat)

# ---------------------------------------------------------- #

diamonds %>% select(carat, price) %>% mutate(Ratio=price/carat)

# ---------------------------------------------------------- #

diamonds %>%
  select(carat, price) %>%
  mutate(Ratio=price/carat, Double=Ratio*2)


# ---------------------------------------------------------- #

library(magrittr)
diamonds2 <- diamonds
diamonds2

# ---------------------------------------------------------- #

diamonds2 %<>%
  select(carat, price) %>%
  mutate(Ratio=price/carat, Double=Ratio*2)

diamonds2

# ---------------------------------------------------------- #

diamonds2 <- diamonds2 %>%
              mutate(Quadruple=Double*2)

diamonds2

