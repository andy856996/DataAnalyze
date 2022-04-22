# ---------
#  18-4
# ---------
# ---------------------------------------------------------- #

tipAnova <- aov(total_bill ~ day - 1, tips)

# ---------------------------------------------------------- #

tipIntercept <- aov(total_bill ~ day, tips)
tipAnova$coefficients
tipIntercept$coefficients

# ---------------------------------------------------------- #

summary(tipAnova)

# ---------------------------------------------------------- #

tipsByDay <- ddply(tips, "day", summarize,
                   total_bill.mean=mean(total_bill), total_bill.sd=sd(total_bill),
                   Length=NROW(total_bill),
                   tfrac=qt(p=.90, df=Length-1),
                   Lower=total_bill.mean - tfrac*total_bill.sd/sqrt(Length),
                   Upper=total_bill.mean + tfrac*total_bill.sd/sqrt(Length)
                   )

ggplot(tipsByDay, aes(x=total_bill.mean, y=day)) + geom_point() +
       geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.3)

# ---------------------------------------------------------- #

nrow(tips)
NROW(tips)
nrow(tips$tip)
NROW(tips$tip)

# ---------------------------------------------------------- #