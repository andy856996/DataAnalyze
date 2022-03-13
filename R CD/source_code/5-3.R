# ---------
#  5-3
# ---------
# ---------------------------------------------------------- #

# 建立一個5x2 matrix
A <- matrix(1:10, nrow = 5)

# 建立另一個5x2 matrix
B <- matrix(21:30, nrow = 5)

# 建立另一個5x2 matrix
C <- matrix(21:40, nrow = 2)

A
B
C

nrow(A)
ncol(A)
dim(A)

# 把它們加起來
A + B

# 把它們互相乘起來
A * B

# 勘查元素是否一樣
A == B

# ---------------------------------------------------------- #

A %*% t(B)

# ---------------------------------------------------------- #

colnames(A)
rownames(A)
colnames(A) <- c("Left", "Right")
rownames(A) <- c("1st", "2nd", "3rd", "4th", "5th")

colnames(B)
rownames(B)
colnames(B) <- c("First", "Second")
rownames(B) <- c("One", "Two", "Three", "Four", "Five")

colnames(C)
rownames(C)
colnames(C) <- LETTERS[1:10]
rownames(C) <- c("Top", "Bottom")

# ---------------------------------------------------------- #

t(A)
A %*% C

# ---------------------------------------------------------- #