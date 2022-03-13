# ---------
#  5-2
# ---------
# ---------------------------------------------------------- #

# 建立三個元素的list
list(1, 2, 3)

# 建立一個元素的list,且其唯一的 元素為一個含有三個元素的vector
list(c(1, 2, 3))

# 建立兩個元素的list
# 第一個元素為含有三個元素的vector
# 第二個元素為含有五個元素的vector
(list3 <- list(c(1, 2, 3), 3:7))

# 兩個元素的list
# 第一元素為 data.frame
# 第二元素為含有10個元素的vector
list(theDF, 1:10)

# 三個元素的list
# 第一個為data.frame
# 第二個為vector
# 第三個為含有兩個vector的list,名為list3
list5 <- list(theDF, 1:10, list3)
list5

# ---------------------------------------------------------- #

names(list5)
names(list5) <- c("data.frame", "vector", "list")
names(list5)
list5

# ---------------------------------------------------------- #

list6 <- list(TheDataFrame = theDF, TheVector = 1:10, TheList = list3)
names(list6)
list6

# ---------------------------------------------------------- #

(emptyList <- vector(mode = "list", length = 4))

# ---------------------------------------------------------- #

list5[[1]]
list5[["data.frame"]]

# ---------------------------------------------------------- #

list5[[1]]$Sport
list5[[1]][, "Second"]
list5[[1]][, "Second", drop = FALSE]

# ---------------------------------------------------------- #

# 勘查其長度
length(list5)

# 附加第四個元素,並不給於名稱
list5[[4]] <- 2
length(list5)

# 附加第五個元素,並給於名稱
list5[["NewElement"]] <- 3:6
length(list5)
names(list5)
list5

# ---------------------------------------------------------- #