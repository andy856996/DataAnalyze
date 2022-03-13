# ---------
#  17-1
# ---------
# ---------------------------------------------------------- #

# 從標準0-1常態分佈抽取10個值
rnorm(n = 10)

# 從期望值為100,標準差為20的常態分配抽取10個值
rnorm(n = 10, mean = 100, sd = 20)

# ---------------------------------------------------------- #

randNorm10 <- rnorm(10)
randNorm10
dnorm(randNorm10)
dnorm(c(-1, 0, 1))

# ---------------------------------------------------------- #

# 生成常態變數
randNorm <- rnorm(30000)

# 計算它們的分佈
randDensity <- dnorm(randNorm)

# 載入ggplot2
require(ggplot2)

# 把它們畫出來
ggplot(data.frame(x = randNorm, y = randDensity)) + aes(x = x, y = y) +
       geom_point() + labs(x = "Random Normal Variables", y = "Density")

# ---------------------------------------------------------- #

pnorm(randNorm10)
pnorm(c(-3, 0, 3))
pnorm(-1)

# ---------------------------------------------------------- #

pnorm(1) - pnorm(0)
pnorm(1) - pnorm(-1)

# ---------------------------------------------------------- #

# 第一行指令運做了好一些事情
# 其做法是要建立一個 ggplot2物件,以方便之後再加上其它層次
# 這就是為何我們把它存入p
# 我們將randNorm和randDensity存入一個data.frame
# 在函數外面宣告x和y軸以給予更大的彈性
# 之後我們用geom_line()加入線條
# x和y軸則由labs(x="x", y="Density")標籤或命名
p <- ggplot(data.frame(x=randNorm, y=randDensity)) + aes(x=x, y=y) +
            geom_line() + labs(x="x", y="Density")

# 繪製p所帶來的會是一個漂亮的分佈圖
# 為了要建立曲線下的陰影面積,我們先計算該面積
# 生成從最左邊到-1一序列的數字
neg1Seq <- seq(from=min(randNorm), to=-1, by=.1)

# 對該序列建立data.frame,並設為x
# 該序列的分佈則被設為y
lessThanNeg1 <- data.frame(x=neg1Seq, y=dnorm(neg1Seq))
head(lessThanNeg1)

# 將此與最左邊和最右邊的終點做合併
# 其高度為0
lessThanNeg1 <- rbind(c(min(randNorm), 0),
                        lessThanNeg1,
                        c(max(lessThanNeg1$x), 0))

# 以幾何(polygon)定義該陰影部份
p + geom_polygon(data=lessThanNeg1, aes(x=x, y=y))

# 建立從-1到1的序列
neg1Pos1Seq <- seq(from=-1, to=1, by=.1)

# 將該序列建立為data.frame,並設為x
# 該序列的分佈則為y
neg1To1 <- data.frame(x=neg1Pos1Seq, y=dnorm(neg1Pos1Seq))
head(neg1To1)

# 將此與最左邊和最右邊的終點做合併
# 其高度為0
neg1To1 <- rbind(c(min(neg1To1$x), 0),
                   neg1To1,
                   c(max(neg1To1$x), 0))

# 以幾何(polygon)定義該陰影部份
p + geom_polygon(data=neg1To1, aes(x=x, y=y))

# ---------------------------------------------------------- #

randProb <- pnorm(randNorm)
ggplot(data.frame(x=randNorm, y=randProb)) + aes(x=x, y=y) +
       geom_point() + labs(x="Random Normal Variables", y="Probability")

# ---------------------------------------------------------- #

randNorm10
qnorm(pnorm(randNorm10))
all.equal(randNorm10, qnorm(pnorm(randNorm10)))

# ---------------------------------------------------------- #