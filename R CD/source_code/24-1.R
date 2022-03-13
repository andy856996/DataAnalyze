# ---------
#  24-1
# ---------
# ---------------------------------------------------------- #

# ���J�@�ɻȦ�API�M��
library(WDI)

# ������
gdp <- WDI(country=c("US", "CA", "GB", "DE", "CN", "JP", "SG", "IL"),
           indicator=c("NY.GDP.PCAP.CD", "NY.GDP.MKTP.CD"),
           start=1960, end=2011)

# ���R�W
names(gdp) <- c("iso2c", "Country", "Year", "PerCapGDP", "GDP")

# ---------------------------------------------------------- #

head(gdp)

library(ggplot2)
library(scales)

# �H��GDP
ggplot(gdp, aes(Year, PerCapGDP, color=Country, linetype=Country)) +
       geom_line() + scale_y_continuous(label=dollar)

require(useful)

# GDP�����
ggplot(gdp, aes(Year, GDP, color=Country, linetype=Country)) +
       geom_line() +
       scale_y_continuous(label=multiple_format(extra=dollar,
                                                multiple="M"))
   
# ---------------------------------------------------------- #

# ������ꪺ���
us <- gdp$PerCapGDP[gdp$Country == "United States"]

# �N���ഫ���ɶ��ǦC
us <- ts(us, start = min(gdp$Year), end = max(gdp$Year))
us
plot(us, ylab = "Per Capita GDP", xlab = "Year")

# ---------------------------------------------------------- #

acf(us)
pacf(us)

# ---------------------------------------------------------- #

x <- c(1, 4, 8, 2, 6, 6, 5, 3)

# �@���t��
diff(x, differences = 1)

# �G���t��
diff(x, differences = 2)

# ���P�@���t��
diff(x, lag = 1)

# ��X�����M�䤧�e�ĤG�Ӥ������t
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

# ���p����5�~��GDP,�ñN��зǮt�]�t�b���G��
predict(usBest, n.ahead = 5, se.fit = TRUE)

# ---------------------------------------------------------- #

# ���p����5�~�����G
theForecast <- forecast(object = usBest, h = 5)

# �N��e�X
plot(theForecast)

# ---------------------------------------------------------- #