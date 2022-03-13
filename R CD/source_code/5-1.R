# ---------
#  5-1
# ---------
# ---------------------------------------------------------- #

x <- 10:1
y <- -4:5
q <- c("Hockey", "Football", "Baseball", "Curling", "Rugby",
       "Lacrosse", "Basketball", "Tennis", "Cricket", "Soccer")
theDF <- data.frame(x, y, q)
theDF

# ---------------------------------------------------------- #

theDF <- data.frame(First = x, Second = y, Sport = q)
theDF

# ---------------------------------------------------------- #

nrow(theDF)
ncol(theDF)
dim(theDF)

# ---------------------------------------------------------- #

names(theDF)
names(theDF)[3]

# ---------------------------------------------------------- #

rownames(theDF)
rownames(theDF) <- c("One", "Two", "Three", "Four", "Five", "Six",
                     "Seven", "Eight", "Nine", "Ten")
rownames(theDF)

# 把它們設回通用的標引(index)
rownames(theDF) <- NULL
rownames(theDF)

# ---------------------------------------------------------- #

head(theDF)
head(theDF, n = 7)
tail(theDF)

# ---------------------------------------------------------- #

class(theDF)

# ---------------------------------------------------------- #

theDF$Sport

# ---------------------------------------------------------- #

theDF[3, 2]

# ---------------------------------------------------------- #

# 第三橫排,第二到第三直排
theDF[3, 2:3]

# 第二直排,第三和第五橫排
# 由於只選了一直排,其將回傳一個向量(vector)
# 因此直排名稱將不被顯示
theDF[c(3, 5), 2]

# 第三和第五橫排,第二到三直排
theDF[c(3, 5), 2:3]

# ---------------------------------------------------------- #

# 所有第三直排的元素
# 由於只是單一直排,因此回傳一個向量(vector)
theDF[, 3]

# 所有第二到第三直排的元素
theDF[, 2:3]

# 所有第二橫排的元素
theDF[2, ]

# 所有第二到第四橫排的元素
theDF[2:4, ]

# ---------------------------------------------------------- #

theDF[, c("First", "Sport")]

# ---------------------------------------------------------- #

# 只顯示"Sport"直排
# 只有單一個直排,所以回傳一個向量vector(且為因素,factor)
theDF[, "Sport"]
class(theDF[, "Sport"])

# 只指定顯示"Sport"直排
# 回傳單一直排的data.frame
theDF["Sport"]
class(theDF["Sport"])

# 只顯示"Sport"直排
# 此也vector(且為因素,factor)
theDF[["Sport"]]
class(theDF[["Sport"]])

# ---------------------------------------------------------- #

theDF[, "Sport", drop = FALSE]
class(theDF[, "Sport", drop = FALSE])
theDF[, 3, drop = FALSE]
class(theDF[, 3, drop = FALSE])

# ---------------------------------------------------------- #

newFactor <- factor(c("Pennsylvania", "New York", "New Jersey", "New York",
                      "Tennessee", "Massachusetts", "Pennsylvania", "New York"))
model.matrix(~newFactor - 1)
attr(,"assign")
attr(,"contrasts")
attr(,"contrasts")$newFactor

# ---------------------------------------------------------- #