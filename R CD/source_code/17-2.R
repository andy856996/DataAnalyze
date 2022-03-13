# ---------
#  17-2
# ---------
# ---------------------------------------------------------- #

rbinom(n = 1, size = 10, prob = 0.4)

# ---------------------------------------------------------- #

rbinom(n = 1, size = 10, prob = 0.4)
rbinom(n = 5, size = 10, prob = 0.4)
rbinom(n = 10, size = 10, prob = 0.4)

# ---------------------------------------------------------- #

rbinom(n = 1, size = 1, prob = 0.4)
rbinom(n = 5, size = 1, prob = 0.4)
rbinom(n = 10, size = 1, prob = 0.4)

# ---------------------------------------------------------- #

binomData <- data.frame(Successes = rbinom(n = 10000, size = 10,
                                           prob = 0.3))
ggplot(binomData, aes(x = Successes)) + geom_histogram(binwidth = 1)

# ---------------------------------------------------------- #

# 建立一個持有兩個直排和10,000橫排的data.frame 
# 第一直排為Successes,記錄的是10,000組試驗的成功次數
# 第二直排記錄每組試驗次數(Size),每組次數為5
binom5 <- data.frame(Successes=rbinom(n=10000, size=5,
                                      prob=.3), Size=5)
dim(binom5)
head(binom5)

# 跟之前的一樣,還是10,000橫排
# 這次從不同試驗次數的分佈抽取成功次數
# 現在10,000橫排的Size皆為10
binom10 <- data.frame(Successes=rbinom(n=10000, size=10,
                                       prob=.3), Size=10)
dim(binom10)
head(binom10)
binom100 <- data.frame(Successes=rbinom(n=10000, size=100,
                                        prob=.3), Size=100)
binom1000 <- data.frame(Successes=rbinom(n=10000, size=1000,
                                         prob=.3), Size=1000)

# 把它們都合併在一起
binomAll <- rbind(binom5, binom10, binom100, binom1000)
dim(binomAll)
head(binomAll, 10)
tail(binomAll, 10)

# 現在繪圖
# 直方圖只需設定x軸
# 圖的分層(拆解)依據為Size的值
# 這些值為5, 10, 100, 1000
ggplot(binomAll, aes(x=Successes)) + geom_histogram() +
       facet_wrap(~ Size, scales="free")

# ---------------------------------------------------------- #

# 10次試驗裏三次成功的機率
dbinom(x = 3, size = 10, prob = 0.3)

# 10次試驗裏三次或更少次成功的機率
pbinom(q = 3, size = 10, prob = 0.3)

# 該兩個函數也可以執行向量化運算
dbinom(x = 1:10, size = 10, prob = 0.3)
pbinom(q = 1:10, size = 10, prob = 0.3)

# ---------------------------------------------------------- #

qbinom(p = 0.3, size = 10, prob = 0.3)
qbinom(p = c(0.3, 0.35, 0.4, 0.5, 0.6), size = 10, prob = 0.3)

# ---------------------------------------------------------- #