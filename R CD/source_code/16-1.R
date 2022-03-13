# ---------
#  16-1
# ---------
# ---------------------------------------------------------- #

paste("Hello", "Jared", "and others")

# ---------------------------------------------------------- #

paste("Hello", "Jared", "and others", sep = "/")

# ---------------------------------------------------------- #

paste(c("Hello", "Hey", "Howdy"), c("Jared", "Bob", "David"))

# ---------------------------------------------------------- #

paste("Hello", c("Jared", "Bob", "David"))
paste("Hello", c("Jared", "Bob", "David"), c("Goodbye", "Seeya"))

# ---------------------------------------------------------- #

vectorOfText <- c("Hello", "Everyone", "out there", ".")
paste(vectorOfText, collapse = " ")
paste(vectorOfText, collapse = "*")

# ---------------------------------------------------------- #