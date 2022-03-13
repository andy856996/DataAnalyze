# ---------
#  21-2
# ---------
# ---------------------------------------------------------- #

house2 <- lm(ValuePerSqFt ~ Units * SqFt + Boro, data=housing)
house3 <- lm(ValuePerSqFt ~ Units + SqFt * Boro + Class,
             data=housing)
house4 <- lm(ValuePerSqFt ~ Units + SqFt * Boro + SqFt*Class,
             data=housing)
house5 <- lm(ValuePerSqFt ~ Boro + Class, data=housing)

# ---------------------------------------------------------- #

multiplot(house1, house2, house3, house4, house5, pointSize = 2)

# ---------------------------------------------------------- #

anova(house1, house2, house3, house4, house5)

# ---------------------------------------------------------- #

AIC(house1, house2, house3, house4, house5)
BIC(house1, house2, house3, house4, house5)

# ---------------------------------------------------------- #

# ミ穝じ跑计,ㄤValuePerSqFt琌150夹跑计
housing$HighValue <- housing$ValuePerSqFt >= 150

# ミ碭家
high1 <- glm(HighValue ~ Units + SqFt + Boro,
             data=housing, family=binomial(link="logit"))
high2 <- glm(HighValue ~ Units * SqFt + Boro,
             data=housing, family=binomial(link="logit"))
high3 <- glm(HighValue ~ Units + SqFt * Boro + Class,
             data=housing, family=binomial(link="logit"))
high4 <- glm(HighValue ~ Units + SqFt * Boro + SqFt*Class,
             data=housing, family=binomial(link="logit"))
high5 <- glm(HighValue ~ Boro + Class,
             data=housing, family=binomial(link="logit"))

# ノANOVA(熬畉キよ㎝,deviance),AIC㎝BICㄓ禘耞家
anova(high1, high2, high3, high4, high5)
AIC(high1, high2, high3, high4, high5)
BIC(high1, high2, high3, high4, high5)

# ---------------------------------------------------------- #