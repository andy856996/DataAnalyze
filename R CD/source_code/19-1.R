# ---------
#  19-1
# ---------
# ---------------------------------------------------------- #

data(father.son, package='UsingR')
library(ggplot2)
head(father.son)
ggplot(father.son, aes(x=fheight, y=sheight)) + geom_point() +
       geom_smooth(method="lm") + labs(x="Fathers", y="Sons")

# ---------------------------------------------------------- #

heightsLM <- lm(sheight ~ fheight, data = father.son)
heightsLM

# ---------------------------------------------------------- #

summary(heightsLM)

# ---------------------------------------------------------- #
# ---------
#  19-1-1
# ---------
# ---------------------------------------------------------- #

data(tips, package = "reshape2")
head(tips)
tipsAnova <- aov(tip ~ day - 1, data = tips)

# 把-1放在formula裏是不要讓截距項涵蓋在模型裏;
# 類別變數將自動地被設定成每個level都會有一個迴歸係數
tipsLM <- lm(tip ~ day - 1, data = tips)
summary(tipsAnova)
summary(tipsLM)

# ---------------------------------------------------------- #

# 首先手動計算平均數和信賴區間(CI)
library(plyr)
tipsByDay <- ddply(tips, "day", summarize,
                   tip.mean=mean(tip), tip.sd=sd(tip),
                   Length=NROW(tip),
                   tfrac=qt(p=.90, df=Length-1),
                   Lower=tip.mean - tfrac*tip.sd/sqrt(Length),
                   Upper=tip.mean + tfrac*tip.sd/sqrt(Length)
                   )

# 現在從tipsLM的摘要表(summary)把它們抽取出來
tipsInfo <- summary(tipsLM)
tipsCoef <- as.data.frame(tipsInfo$coefficients[, 1:2])
tipsCoef <- within(tipsCoef, {
   Lower <- Estimate - qt(p=0.90, df=tipsInfo$df[2]) * `Std. Error`
   Upper <- Estimate + qt(p=0.90, df=tipsInfo$df[2]) * `Std. Error`
   day <- rownames(tipsCoef)
   })

# 把它們繪製出來
ggplot(tipsByDay, aes(x=tip.mean, y=day)) + geom_point() +
       geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.3) +
       ggtitle("Tips by day calculated manually")

ggplot(tipsCoef, aes(x=Estimate, y=day)) + geom_point() +
       geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.3) +
       ggtitle("Tips by day calculated from regression model")

# ---------------------------------------------------------- #