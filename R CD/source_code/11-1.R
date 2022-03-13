# ---------
#  11-1
# ---------
# ---------
#  11-1-1
# ---------
# ---------------------------------------------------------- #

# 建立matrix
theMatrix <- matrix(1:9, nrow = 3)

# 每一橫排的總和
apply(theMatrix, 1, sum)

# 每一直排的總和
apply(theMatrix, 2, sum)

# ---------------------------------------------------------- #

rowSums(theMatrix)
colSums(theMatrix)

# ---------------------------------------------------------- #

theMatrix[2, 1] <- NA
apply(theMatrix, 1, sum)
apply(theMatrix, 1, sum, na.rm = TRUE)
rowSums(theMatrix)
rowSums(theMatrix, na.rm = TRUE)

# ---------------------------------------------------------- #
# ---------
#  11-1-2
# ---------
# ---------------------------------------------------------- #

theList <- list(A = matrix(1:9, 3), B = 1:5, C = matrix(1:4, 2), D = 2)
lapply(theList, sum)

# ---------------------------------------------------------- #

sapply(theList, sum)

# ---------------------------------------------------------- #

theNames <- c("Jared", "Deb", "Paul")
lapply(theNames, nchar)

# ---------------------------------------------------------- #
# ---------
#  11-1-3
# ---------
# ---------------------------------------------------------- #

## 建立兩個list
firstList <- list(A = matrix(1:16, 4), B = matrix(1:16, 2), C = 1:5)
secondList <- list(A = matrix(1:16, 4), B = matrix(1:16, 8), C = 15:1)

# 元素對元素的檢測, 看它們是否相同
mapply(identical, firstList, secondList)

## 建立一個簡單的函數把各元素的橫排的數量(長度)加起來
simpleFunc <- function(x, y)
{
   NROW(x) + NROW(y)
}

# 把函數應用到那兩個list
mapply(simpleFunc, firstList, secondList)

# ---------------------------------------------------------- #