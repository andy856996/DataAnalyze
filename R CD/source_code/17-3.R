# ---------
#  17-3
# ---------
# ---------------------------------------------------------- #

# 從5個不同的泊松分佈各生成10,000個樣本
pois1 <- rpois(n=10000, lambda=1)
pois2 <- rpois(n=10000, lambda=2)
pois5 <- rpois(n=10000, lambda=5)
pois10 <- rpois(n=10000, lambda=10)
pois20 <- rpois(n=10000, lambda=20)
pois <- data.frame(Lambda.1=pois1, Lambda.2=pois2,
                   Lambda.5=pois5, Lambda.10=pois10, Lambda.20=pois20)

# 載入reshape2套件以溶化資料,讓繪圖的工作容易一些
library(reshape2)

# 將資料溶化成長的格式
pois <- melt(data=pois, variable.name="Lambda", value.name="x")

# 載入stringr套件以處理新直排的名稱
library(stringr)

# 處理Lambda(清除掉不必要的字元),讓它只顯示Lambda的數值
pois$Lambda <- as.factor(as.numeric(str_extract(string=pois$Lambda,
                                                pattern="\\d+")))
head(pois)
tail(pois)

# ---------------------------------------------------------- #

library(ggplot2)
ggplot(pois, aes(x=x)) + geom_histogram(binwidth=1) +
       facet_wrap(~ Lambda) + ggtitle("Probability Mass Function")

# ---------------------------------------------------------- #

ggplot(pois, aes(x=x)) +
   geom_density(aes(group=Lambda, color=Lambda, fill=Lambda),
                adjust=4, alpha=1/2) +
   scale_color_discrete() + scale_fill_discrete() +
   ggtitle("Probability Mass Function")

# ---------------------------------------------------------- #