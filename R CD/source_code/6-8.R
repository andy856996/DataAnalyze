# ---------
#  6-8
# ---------
# ---------------------------------------------------------- #

library(jsonlite)
pizza <- fromJSON('http://www.jaredlander.com/data/PizzaFavorites.json')
pizza

# ---------------------------------------------------------- #

class(pizza)
class(pizza$Name)
class(pizza$Details)
class(pizza$Details[[1]])

# ---------------------------------------------------------- #