# ---------
#  22-2
# ---------
# ---------------------------------------------------------- #

download.data('http://jaredlander.com/data/ideo.rdata','data/ideo.rdata')
load("data/ideo.rdata")
head(ideo)

# ---------------------------------------------------------- #

## 建立一些模型
library(dplyr)
results <- ideo %>%
   # 依年度對資料分群
   group_by(Year) %>%
   # 對每群資料建立模型
   do(Model=glm(Vote ~ Race + Income + Gender + Education,
                 data=.,
                 family=binomial(link="logit")))

# 模型儲存在一個list直行中，因此我們視它為一個直行
# 對直行命名
names(results$Model) <- as.character(results$Year)
results

# ---------------------------------------------------------- #

library(coefplot)

# 得到係數的訊息
voteInfo <- multiplot(results, coefficients="Raceblack", plot=FALSE)
head(voteInfo)

# 將視窗限制到(-20, 10)才繪圖
multiplot(results$Model, 
          coefficients="Raceblack", secret.weapon=TRUE)
    coord_flip(xlim=c(-20, 10))

# ---------------------------------------------------------- #

resultsB <- ideo %>%
      # 依年度對資料進行分群
      group_by(Year) %>%
      # 對每群資料建立模型
      do(Model=arm::bayesglm(Vote ~ Race + Income + Gender + Education,
                               data=.,
                               family=binomial(link="logit"),
                               prior.scale=2.5, prior.df=1))
# 對list命名
names(resultsB$Model) <- as.character(resultsB$Year)

# 建立係數圖
multiplot(resultsB, coefficients="Raceblack", secret.weapon=TRUE)

# ---------------------------------------------------------- #