# ---------
#  16-2
# ---------
# ---------------------------------------------------------- #

person <- "Jared"
partySize <- "eight"
waitTime <- 25

paste("Hello ", person, ", your party of ", partySize,
      " will be seated in ", waitTime, " minutes.", sep="")

# ---------------------------------------------------------- #

sprintf("Hello %s, your party of %s will be seated in %s minutes",
         person, partySize, waitTime)

# ---------------------------------------------------------- #

sprintf("Hello %s, your party of %s will be seated in %s minutes",
        c("Jared", "Bob"), c("eight", 16, "four", 10), waitTime)

# ---------------------------------------------------------- #