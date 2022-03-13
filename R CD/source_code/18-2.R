# ---------
#  18-2
# ---------
# ---------------------------------------------------------- #

library(ggplot2)
head(economics)

# ---------------------------------------------------------- #

cor(economics$pce, economics$psavert)

# ---------------------------------------------------------- #

# 用cor計算相關係數
cor(economics$pce, economics$psavert)

## 計算用來找出相關係數的每個部份
xPart <- economics$pce - mean(economics$pce)
yPart <- economics$psavert - mean(economics$psavert)
nMinusOne <- (nrow(economics) - 1)
xSD <- sd(economics$pce)
ySD <- sd(economics$psavert)

# 應用相關係數的公式
sum(xPart * yPart) / (nMinusOne * xSD * ySD)

# ---------------------------------------------------------- #

cor(economics[, c(2, 4:6)])

# ---------------------------------------------------------- #

GGally::ggpairs(economics[, c(2, 4:6)], params = list(labelSize = 8))

# ---------------------------------------------------------- #

# 載入reshape套件以溶化資料
require(reshape2)

# 載入scales套件以增添繪圖功能
require(scales)

# 建立相關係數矩陣
econCor <- cor(economics[, c(2, 4:6)])

# 把它溶化成長的格式
econMelt <- melt(econCor, varnames=c("x", "y"),
                 value.name="Correlation")

# 依據相關係數做排序
econMelt <- econMelt[order(econMelt$Correlation), ]

# 顯示溶化後的資料
econMelt

## 用ggplot繪圖
# 用x和y設為x和y軸作為圖的初始建立
ggplot(econMelt, aes(x=x, y=y)) +
      # 畫上磚塊(方塊),依據相關係數(Correlation)填上顏色
      geom_tile(aes(fill=Correlation)) +
      # 以三層色彩漸層(color gradient)的顏色對磚塊填上顏色
      # 靜音(muted)紅作為最低點, 白色為中間, 鋼鐵藍作為最高點
      # 顏色說明為一條不設有刻度(ticks)的色帶(colorbar), 其高度為10行
      # limits則指定所填上的尺度範圍為從-1到1
      scale_fill_gradient2(low=muted("red"), mid="white",
                           high="steelblue",
                           guide=guide_colorbar(ticks=FALSE, barheight=10),
                           limits=c(-1, 1)) +
      # 使用最簡單的主題(minimal theme)以確保圖中沒多餘的東西
      theme_minimal() +
      # 將x和y標籤留空
      labs(x=NULL, y=NULL)

# ---------------------------------------------------------- #

m <- c(9, 9, NA, 3, NA, 5, 8, 1, 10, 4)
n <- c(2, NA, 1, 6, 6, 4, 1, 1, 6, 7)
p <- c(8, 4, 3, 9, 10, NA, 3, NA, 9, 9)
q <- c(10, 10, 7, 8, 4, 2, 8, 5, 5, 2)
r <- c(1, 9, 7, 6, 5, 6, 2, 7, 9, 10)

# 把它們合併在一起
theMat <- cbind(m, n, p, q, r)

# ---------------------------------------------------------- #

cor(theMat, use = "everything")
cor(theMat, use = "all.obs")

# ---------------------------------------------------------- #

cor(theMat, use = "complete.obs")
cor(theMat, use = "na.or.complete")

# 只用含有完整資料的橫排計算相關係數
cor(theMat[c(1, 4, 7, 9, 10), ])

# 比較"complete.obs"和手動用指定的橫排來計算相關係數
# 結果應該是要一樣的
identical(cor(theMat, use = "complete.obs"),
              cor(theMat[c(1, 4, 7, 9, 10), ]))

# ---------------------------------------------------------- #

# 完整的相關係數矩陣
cor(theMat, use = "pairwise.complete.obs")

# 只算m對n的結果,再跟該矩陣的結果做比較
cor(theMat[, c("m", "n")], use = "complete.obs")

# 只算m對p的結果,再跟該矩陣的結果做比較
cor(theMat[, c("m", "p")], use = "complete.obs")

# ---------------------------------------------------------- #

data(tips, package = "reshape2")
head(tips)
GGally::ggpairs(tips)

# ---------------------------------------------------------- #

require(RXKCD)
getXKCD(which = "552")

# ---------------------------------------------------------- #

cov(economics$pce, economics$psavert)
cov(economics[, c(2, 4:6)])

# 檢測cov和cor*sd*sd的結果是否一樣
identical(cov(economics$pce, economics$psavert),
              cor(economics$pce, economics$psavert) *
              sd(economics$pce) * sd(economics$psavert))

# ---------------------------------------------------------- #