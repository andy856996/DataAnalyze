# ---------
#  23-4
# ---------
# ---------------------------------------------------------- #

library(rpart)
creditTree <- rpart(Credit ~ CreditAmount + Age +
                    CreditHistory + Employment, data = credit)

# ---------------------------------------------------------- #

creditTree

# ---------------------------------------------------------- #

library(rpart.plot)
rpart.plot(creditTree, extra = 4)

# ---------------------------------------------------------- #