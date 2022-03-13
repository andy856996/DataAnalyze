# ---------
#  6-7
# ---------
# ---------
#  6-7-1
# ---------
# ---------------------------------------------------------- #

library(XML)
theURL <- "http://www.jaredlander.com/2012/02/another-kind-of-super-bowl-pool/"
bowlPool <- readHTMLTable(theURL, which = 1, header = FALSE,
                          stringsAsFactors = FALSE)
bowlPool

# ---------------------------------------------------------- #
# ---------
#  6-7-2
# ---------
# ---------------------------------------------------------- #
library(rvest)
ribalta <- read_html('http://www.jaredlander.com/data/ribalta.html')
class(ribalta)
ribalta

# ---------------------------------------------------------- #

ribalta %>% html_nodes('ul') %>% html_nodes('span')

# ---------------------------------------------------------- #

ribalta %>% html_nodes('.street')

# ---------------------------------------------------------- #

ribalta %>% html_nodes('.street') %>% html_text()

# ---------------------------------------------------------- #

ribalta %>% html_nodes('#longitude') %>% html_attr('value')

# ---------------------------------------------------------- #

ribalta %>%
  html_nodes('table.food-items') %>%
  magrittr::extract2(5) %>%
  html_table()
