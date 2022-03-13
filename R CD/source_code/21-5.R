# ---------
#  21-5
# ---------
# ---------------------------------------------------------- #

# 最小的模型為虛無模型,基本上就是一個直線平均
nullModel <- lm(ValuePerSqFt ~ 1, data=housing)

# 我們能接受的最大模型
fullModel <- lm(ValuePerSqFt ~ Units + SqFt*Boro + Boro*Class,
                data=housing)

# 嘗試不同的模型
# 從nullModel開始
# 不能超越fullModel
# 運行一個雙向(both)的迭代
houseStep <- step(nullModel,
                  scope=list(lower=nullModel, upper=fullModel),
                  direction="both")

# 顯示被挑選的模型
houseStep

# ---------------------------------------------------------- #