# ---------
#  24-2
# ---------
# ---------------------------------------------------------- #

# 載入reshape2
library(reshape2)

# 將data.frame轉換為寬的格式
gdpCast <- dcast(Year ~ Country,
                 data=gdp[, c("Country", "Year", "PerCapGDP")],
                 value.var="PerCapGDP")
head(gdpCast)

# 移除掉前十個橫排,原因是德國(Germany)並沒有 <-這個部份應該有問題,好像是少了點什麼
# 將它轉換為時間序列
gdpTS <- ts(data=gdpCast[, -1], start=min(gdpCast$Year),
            end=max(gdpCast$Year))

# 用內建繪圖功能來畫圖和加入說明
plot(gdpTS, plot.type="single", col=1:8)
legend("topleft", legend=colnames(gdpTS), ncol=2, lty=1,
       col=1:8, cex=.9)

# ---------------------------------------------------------- #

gdpTS <- gdpTS[, which(colnames(gdpTS) != "Germany")]

# ---------------------------------------------------------- #

numDiffs <- ndiffs(gdpTS)
numDiffs

gdpDiffed <- diff(gdpTS, differences=numDiffs)
plot(gdpDiffed, plot.type="single", col=1:7)
legend("bottomleft", legend=colnames(gdpDiffed), ncol=2, lty=1,
       col=1:7, cex=.9)

# ---------------------------------------------------------- #

library(vars)

# 建立模型
gdpVar <- VAR(gdpDiffed, lag.max = 12)

# 所挑選的位階(order)
gdpVar$p

# 每個模型的名稱
names(gdpVar$varresult)

# 每個模型其實都是lm物件
class(gdpVar$varresult$Canada)
class(gdpVar$varresult$Japan)

# 每個模型都有各自的係數
head(coef(gdpVar$varresult$Canada))
head(coef(gdpVar$varresult$Japan))

library(coefplot)
coefplot(gdpVar$varresult$Canada)
coefplot(gdpVar$varresult$Japan)

# ---------------------------------------------------------- #

predict(gdpVar, n.ahead = 5)

# ---------------------------------------------------------- #