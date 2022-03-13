# ---------
#  18-3
# ---------
# ---------------------------------------------------------- #

head(tips)

# 服務員的性別
unique(tips$sex)

# 每周各天
unique(tips$day)

# ---------------------------------------------------------- #
# ---------
#  18-3-1
# ---------
# ---------------------------------------------------------- #

t.test(tips$tip, alternative = "two.sided", mu = 2.5)

# ---------------------------------------------------------- #

## 建立一個t分佈
randT <- rt(30000, df=NROW(tips)-1)

# 得到t統計量和其它相關訊息
tipTTest <- t.test(tips$tip, alternative="two.sided", mu=2.50)

# 繪製圖
ggplot(data.frame(x=randT)) +
       geom_density(aes(x=x), fill="grey", color="grey") +
       geom_vline(xintercept=tipTTest$statistic) +
       geom_vline(xintercept=mean(randT) + c(-2, 2)*sd(randT), linetype=2)

# ---------------------------------------------------------- #

t.test(tips$tip, alternative = "greater", mu = 2.5)

# ---------------------------------------------------------- #
# ---------
#  18-3-2
# ---------
# ---------------------------------------------------------- #

# 首先對各組計算變異數;
# 使用formula介面
# 計算每個性別小費的變異數
aggregate(tip ~ sex, data=tips, var)

# 現在檢測小費的分佈是否為常態
shapiro.test(tips$tip)
shapiro.test(tips$tip[tips$sex == "Female"])
shapiro.test(tips$tip[tips$sex == "Male"])

# 用目測可以判斷所有檢測都不通過
ggplot(tips, aes(x=tip, fill=sex)) +
       geom_histogram(binwidth=.5, alpha=1/2)
ansari.test(tip ~ sex, tips)

# ---------------------------------------------------------- #

# 設定var.equal=TRUE將進行標準的雙樣本t檢定
# 設定var.equal=FALSE(預設)則進行Welch檢定
t.test(tip ~ sex, data = tips, var.equal = TRUE)

# ---------------------------------------------------------- #

library(plyr)
tipSummary <- ddply(tips, "sex", summarize,
                    tip.mean=mean(tip), tip.sd=sd(tip),
                    Lower=tip.mean - 2*tip.sd/sqrt(NROW(tip)),
                    Upper=tip.mean + 2*tip.sd/sqrt(NROW(tip)))
tipSummary

# ---------------------------------------------------------- #

ggplot(tipSummary, aes(x=tip.mean, y=sex)) + geom_point() +
       geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.2)

# ---------------------------------------------------------- #
# ---------
#  18-3-3
# ---------
# ---------------------------------------------------------- #

data(father.son, package='UsingR')
head(father.son)
t.test(father.son$fheight, father.son$sheight, paired = TRUE)

# ---------------------------------------------------------- #

heightDiff <- father.son$fheight - father.son$sheight
ggplot(father.son, aes(x=fheight - sheight)) +
       geom_density() +
       geom_vline(xintercept=mean(heightDiff)) +
       geom_vline(xintercept=mean(heightDiff) +
                  2*c(-1, 1)*sd(heightDiff)/sqrt(nrow(father.son)),
                  linetype=2)

# ---------------------------------------------------------- #