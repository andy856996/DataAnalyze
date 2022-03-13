# ---------
#  19-2
# ---------
# ---------------------------------------------------------- #

housing <- read.table("http://www.jaredlander.com/data/housing.csv",
                      sep = ",", header = TRUE,
                      stringsAsFactors = FALSE)

# ---------------------------------------------------------- #

names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt",
                    "SqFt", "Income", "IncomePerSqFt", "Expense",
                    "ExpensePerSqFt", "NetIncome", "Value",
                    "ValuePerSqFt", "Boro")

head(housing)

# ---------------------------------------------------------- #

ggplot(housing, aes(x=ValuePerSqFt)) +
       geom_histogram(binwidth=10) + labs(x="Value per Square Foot")

# ---------------------------------------------------------- #

ggplot(housing, aes(x=ValuePerSqFt, fill=Boro)) +
       geom_histogram(binwidth=10) + labs(x="Value per Square Foot")
ggplot(housing, aes(x=ValuePerSqFt, fill=Boro)) +
       geom_histogram(binwidth=10) + labs(x="Value per Square Foot") +
       facet_wrap(~Boro)

# ---------------------------------------------------------- #

ggplot(housing, aes(x=SqFt)) + geom_histogram()
ggplot(housing, aes(x=Units)) + geom_histogram()
ggplot(housing[housing$Units < 1000, ],
       aes(x=SqFt)) + geom_histogram()
ggplot(housing[housing$Units < 1000, ],
       aes(x=Units)) + geom_histogram()

# ---------------------------------------------------------- #

ggplot(housing, aes(x = SqFt, y = ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x = Units, y = ValuePerSqFt)) + geom_point()
ggplot(housing[housing$Units < 1000, ], aes(x = SqFt,
                                            y = ValuePerSqFt)) + geom_point()
ggplot(housing[housing$Units < 1000, ], aes(x = Units,
                                            y = ValuePerSqFt)) + geom_point()

# 有多少種建築是要被移除的?
sum(housing$Units >= 1000)

# ---------------------------------------------------------- #

# 把它們(單位個數超過1000的建築)移除掉
housing <- housing[housing$Units < 1000, ]

# ---------------------------------------------------------- #

# 繪製ValuePerSqFt對SqFt的散佈圖
ggplot(housing, aes(x=SqFt, y=ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x=log(SqFt), y=ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x=SqFt, y=log(ValuePerSqFt))) + geom_point()
ggplot(housing, aes(x=log(SqFt), y=log(ValuePerSqFt))) +
       geom_point()

# ---------------------------------------------------------- #

# 繪製ValuePerSqFt對Units的散佈圖
ggplot(housing, aes(x=Units, y=ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x=log(Units), y=ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x=Units, y=log(ValuePerSqFt))) + geom_point()
ggplot(housing, aes(x=log(Units), y=log(ValuePerSqFt))) +
       geom_point()

# ---------------------------------------------------------- #

house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data = housing)
summary(house1)

# ---------------------------------------------------------- #

house1$coefficients
coef(house1)

# 跟使用coef的結果一樣
coefficients(house1)

require(coefplot)
coefplot(house1)
house2 <- lm(ValuePerSqFt ~ Units * SqFt + Boro, data = housing)
house3 <- lm(ValuePerSqFt ~ Units:SqFt + Boro, data = housing)
house2$coefficients
house3$coefficients
coefplot(house2)
coefplot(house3)

# ---------------------------------------------------------- #

house4 <- lm(ValuePerSqFt ~ SqFt * Units * Income, housing)
house4$coefficients

# ---------------------------------------------------------- #

house5 <- lm(ValuePerSqFt ~ Class * Boro, housing)
house5$coefficients

# ---------------------------------------------------------- #

coefplot(house1, sort='mag') + scale_x_continuous(limits=c(-.25, .1))
coefplot(house1, sort='mag') + scale_x_continuous(limits=c(-.0005, .0005))

# ---------------------------------------------------------- #

house1.b <- lm(ValuePerSqFt ~ scale(Units) + scale(SqFt) + Boro,
               data=housing)
coefplot(house1.b, sort='mag')

# ---------------------------------------------------------- #

house6 <- lm(ValuePerSqFt ~ I(SqFt/Units) + Boro, housing)
house6$coefficients

# ---------------------------------------------------------- #

house7 <- lm(ValuePerSqFt ~ (Units + SqFt)^2, housing)
house7$coefficients

house8 <- lm(ValuePerSqFt ~ Units * SqFt, housing)
identical(house7$coefficients, house8$coefficients)

house9 <- lm(ValuePerSqFt ~ I(Units + SqFt)^2, housing)
house9$coefficients

# ---------------------------------------------------------- #

# 也是從coefplot套件
multiplot(house1, house2, house3)

# ---------------------------------------------------------- #

housingNew <- read.table("http://www.jaredlander.com/data/
      housingNew.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# ---------------------------------------------------------- #

# 用新資料來做預測和建立95%信賴區間
housePredict <- predict(house1, newdata = housingNew, se.fit = TRUE,
                        interval = "prediction", level = .95)

# 顯示預測值和根據標準誤差建立的信賴區間上界和下界
head(housePredict$fit)

# 顯示預測值的標準誤差
head(housePredict$se.fit)

# ---------------------------------------------------------- #