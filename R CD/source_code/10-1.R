# ---------
#  10-1
# ---------
# ---------------------------------------------------------- #

for (i in 1:10)
{
   print(i)
}

# ---------------------------------------------------------- #

print(1:10)

# ---------------------------------------------------------- #

# 建立一個含有水果名稱的vector
fruit <- c("apple", "banana", "pomegranate")

# 建立一個變數(亦為vector)以儲存水果名稱的長度,先儲存NA值作為開始
fruitLength <- rep(NA, length(fruit))

# 把它顯示出來,全部為NA值
fruitLength

# 替它取名
names(fruitLength) <- fruit

# 再次顯示,還是NA值
fruitLength

# 對fruit(水果名稱)做出迭代,每次把名稱長度都存入vector裏
for (a in fruit)
{
   fruitLength[a] <- nchar(a)
}

# 把長度顯示出來
fruitLength

# ---------------------------------------------------------- #

# 只需要呼叫nchar函數
fruitLength2 <- nchar(fruit)

# 替它取名
names(fruitLength2) <- fruit

# 把它顯示出
fruitLength2

# ---------------------------------------------------------- #

identical(fruitLength, fruitLength2)

# ---------------------------------------------------------- #