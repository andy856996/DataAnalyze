# ---------
#  24-1
# ---------
# ---------------------------------------------------------- #

# 載入世界銀行API套件
library(WDI)

# 抽取資料
gdp <- WDI(country=c("US", "CA", "GB", "DE", "CN", "JP", "SG", "IL"),
           indicator=c("NY.GDP.PCAP.CD", "NY.GDP.MKTP.CD"),
           start=1960, end=2011)

# 對其命名
names(gdp) <- c("iso2c", "Country", "Year", "PerCapGDP", "GDP")

# ---------------------------------------------------------- #

head(gdp)

library(ggplot2)
library(scales)

# 人均GDP
ggplot(gdp, aes(Year, PerCapGDP, color=Country, linetype=Country)) +
       geom_line() + scale_y_continuous(label=dollar)

require(useful)

# GDP絕對值
ggplot(gdp, aes(Year, GDP, color=Country, linetype=Country)) +
       geom_line() +
       scale_y_continuous(label=multiple_format(extra=dollar,
                                                multiple="M"))
   
# ---------------------------------------------------------- #

# 抽取美國的資料
us <- gdp$PerCapGDP[gdp$Country == "United States"]

# 將它轉換為時間序列
us <- ts(us, start = min(gdp$Year), end = max(gdp$Year))
us
plot(us, ylab = "Per Capita GDP", xlab = "Year")

# ---------------------------------------------------------- #

acf(us)
pacf(us)

# ---------------------------------------------------------- #

x <- c(1, 4, 8, 2, 6, 6, 5, 3)

# 一階差分
diff(x, differences = 1)

# 二階差分
diff(x, differences = 2)

# 等同一階差分
diff(x, lag = 1)

# 找出元素和其之前第二個元素的差
diff(x, lag = 2)

# ---------------------------------------------------------- #

library(forecast)
ndiffs(x = us)
plot(diff(us, 2))

# ---------------------------------------------------------- #

usBest <- auto.arima(x = us)
usBest

# ---------------------------------------------------------- #

acf(usBest$residuals)
pacf(usBest$residuals)

# ---------------------------------------------------------- #

coef(usBest)

# ---------------------------------------------------------- #

# 估計未來5年的GDP,並將其標準差包含在結果裏
predict(usBest, n.ahead = 5, se.fit = TRUE)

# ---------------------------------------------------------- #

# 估計未來5年的結果
theForecast <- forecast(object = usBest, h = 5)

# 將其畫出
plot(theForecast)

# ---------------------------------------------------------- #