# ---------
#  18-4
# ---------
# ---------------------------------------------------------- #

tipAnova <- aov(tip ~ day - 1, tips)

# ---------------------------------------------------------- #

tipIntercept <- aov(tip ~ day, tips)
tipAnova$coefficients
tipIntercept$coefficients

# ---------------------------------------------------------- #

summary(tipAnova)

# ---------------------------------------------------------- #

tipsByDay <- ddply(tips, "day", summarize,
                   tip.mean=mean(tip), tip.sd=sd(tip),
                   Length=NROW(tip),
                   tfrac=qt(p=.90, df=Length-1),
                   Lower=tip.mean - tfrac*tip.sd/sqrt(Length),
                   Upper=tip.mean + tfrac*tip.sd/sqrt(Length)
                   )

ggplot(tipsByDay, aes(x=tip.mean, y=day)) + geom_point() +
       geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.3)

# ---------------------------------------------------------- #

nrow(tips)
NROW(tips)
nrow(tips$tip)
NROW(tips$tip)

# ---------------------------------------------------------- #