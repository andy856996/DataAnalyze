# ---------
#  21-1
# ---------
# ---------------------------------------------------------- #

# 讀取資料
housing <- read.table("data/housing.csv", sep=",", header=TRUE,
                      stringsAsFactors=FALSE)

# 替資料取名
names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt",
                    "SqFt", "Income", "IncomePerSqFt", "Expense",
                    "ExpensePerSqFt", "NetIncome", "Value",
                    "ValuePerSqFt", "Boro")

# 移除一些離群值
housing <- housing[housing$Units < 1000, ]
head(housing)

# 建立模型
house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data=housing)
summary(house1)

# 視覺化模型
library(coefplot)
coefplot(house1)

# ---------------------------------------------------------- #

library(ggplot2)

# 檢視被鞏固後的lm模型長什麼樣子
head(fortify(house1))

# 儲存一個圖到一個物件
# 可以發現到我們用新建立的直排作為x和y軸
# x軸為.fitted和y軸為.resid
h1 <- ggplot(aes(x=.fitted, y=.resid), data = house1) +
             geom_point() +
             geom_hline(yintercept = 0) +
             geom_smooth(se = FALSE) +
             labs(x="Fitted Values", y="Residuals")

# 顯示該圖
h1

# ---------------------------------------------------------- #

h1 + geom_point(aes(color = Boro))

# ---------------------------------------------------------- #

# 通過內建函數繪圖
plot(house1, which=1)

# ---------------------------------------------------------- #

# 同樣的圖,但根據Boro填上了顏色
plot(house1, which=1, col=as.numeric(factor(house1$model$Boro)))

# 對圖的一些說明
legend("topright", legend=levels(factor(house1$model$Boro)), pch=1,
       col=as.numeric(factor(levels(factor(house1$model$Boro)))),
       text.col=as.numeric(factor(levels(factor(house1$model$Boro)))),
       title="Boro")

# ---------------------------------------------------------- #

plot(house1, which = 2)
ggplot(house1, aes(sample = .stdresid)) + stat_qq() + geom_abline()

# ---------------------------------------------------------- #

ggplot(house1, aes(x = .resid)) + geom_histogram()

# ---------------------------------------------------------- #