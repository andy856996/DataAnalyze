# ---------
#  6-1
# ---------
# ---------------------------------------------------------- #

theUrl <- "http://www.jaredlander.com/data/Tomato%20First.csv"
tomato <- read.table (file = theUrl, header = TRUE, sep = ",")

# ---------------------------------------------------------- #

head(tomato)

# ---------------------------------------------------------- #

x <- 10:1
y <- -4:5
q <- c("Hockey", "Football", "Baseball", "Curling", "Rugby",
       "Lacrosse", "Basketball", "Tennis", "Cricket", "Soccer")
theDF <- data.frame(First=x, Second=y, Sport=q, stringsAsFactors=FALSE)
theDF$Sport

# ---------------------------------------------------------- #
# ---------
#  6-1-1
# ---------
# ---------------------------------------------------------- #

library(readr)
theUrl <- "http://www.jaredlander.com/data/TomatoFirst.csv"
tomato2 <- read_delim(file=theUrl, delim=',')

# ---------------------------------------------------------- #

tomato2

# ---------------------------------------------------------- #
# ---------
#  6-1-2
# ---------
# ---------------------------------------------------------- #

library(data.table)
theUrl <- "http://www.jaredlander.com/data/TomatoFirst.csv"
tomato3 <- fread(input=theUrl, sep=',', header=TRUE)

# ---------------------------------------------------------- #

head(tomato3)

# ---------------------------------------------------------- #