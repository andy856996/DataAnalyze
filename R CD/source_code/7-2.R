# ---------
#  7-2
# ---------
# ---------
#  7-2-1
# ---------
# ---------------------------------------------------------- #

ggplot(data = diamonds) + geom_histogram(aes(x = carat))

# ---------------------------------------------------------- #

ggplot(data = diamonds) + geom_density(aes(x = carat), fill = "grey50")

# ---------------------------------------------------------- #
# ---------
#  7-2-2
# ---------
# ---------------------------------------------------------- #

ggplot(diamonds, aes(x = carat, y = price)) + geom_point()

# ---------------------------------------------------------- #

# 把初始的ggplot物件存入變數
g <- ggplot(diamonds, aes(x = carat, y = price))

# ---------------------------------------------------------- #

g + geom_point(aes(color = color))

# ---------------------------------------------------------- #

g + geom_point(aes(color = color)) + facet_wrap(~color)
g + geom_point(aes(color = color)) + facet_grid(cut ~ clarity)


# ---------------------------------------------------------- #

ggplot(diamonds, aes(x = carat)) + geom_histogram() + facet_wrap(~color)

# ---------------------------------------------------------- #
# ---------
#  7-2-3
# ---------
# ---------------------------------------------------------- #

ggplot(diamonds, aes(y = carat, x = 1)) + geom_boxplot()

# ---------------------------------------------------------- #

ggplot(diamonds, aes(y = carat, x = cut)) + geom_boxplot()

# ---------------------------------------------------------- #

ggplot(diamonds, aes(y = carat, x = cut)) + geom_violin()

# ---------------------------------------------------------- #
# ---------
#  7-2-4
# ---------
# ---------------------------------------------------------- #

ggplot(economics, aes(x = date, y = pop)) + geom_line()

# ---------------------------------------------------------- #

# 載入lubridate套件
require(lubridate)

## 建立year(年份)和month(月份)變數
economics$year <- year(economics$date)

# label引數設為TRUE表示結果中的月份要以月份的名稱顯示,而非數字 
economics$month <- month(economics$date, label=TRUE)

# 採取部份資料
# which函數所回傳的將會是該檢測結果為TRUE所對應的位置(或標引)
# 即年份大於2000的觀測值的位置將被回傳
econ2000 <- economics[which(economics$year >= 2000), ]

# 載入scales套件以更好地格式化圖中的軸
library(scales)

# 建立圖的基底
g <- ggplot(econ2000, aes(x=month, y=pop))

# 以不同顏色的線來代表不同的年份
# group外觀引數將把資料分成很多群
g <- g + geom_line(aes(color=factor(year), group=year))

# 把說明命名為"Year"
g <- g + scale_color_discrete(name="Year")

# 格式化y-軸
g <- g + scale_y_continuous(labels=comma)

# 新增標題和x-,y-軸的標籤
g <- g + labs(title="Population Growth", x="Month", y="Population")

# 畫圖
g

# ---------------------------------------------------------- #
# ---------
#  7-2-5
# ---------
# ---------------------------------------------------------- #

library(ggthemes)

# 繪圖並存如g2
g2 <- ggplot(diamonds, aes(x=carat, y=price)) +
      geom_point(aes(color=color))

# 使用一些主題
g2 + theme_economist() + scale_colour_economist()
g2 + theme_excel() + scale_colour_excel()
g2 + theme_tufte()
g2 + theme_wsj()

# ---------------------------------------------------------- #