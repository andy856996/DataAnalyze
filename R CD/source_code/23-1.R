# ---------
#  23-1
# ---------
# ---------------------------------------------------------- #

load("data/wifi.rdata")
head(wifi)

# ---------------------------------------------------------- #

library(ggplot2)
ggplot(wifi, aes(x=x, y=y, color=Distance)) + geom_point() +
       scale_color_gradient2(low="blue", mid="white", high="red",
                             midpoint=mean(wifi$Distance))

# ---------------------------------------------------------- #

# 指定用根號模型
# 以網格的中心作為起始值
wifiMod1 <- nls(Distance ~ sqrt((betaX - x)^2 + (betaY - y)^2),
                data = wifi, start = list(betaX = 50, betaY = 50))
summary(wifiMod1)

# ---------------------------------------------------------- #

ggplot(wifi, aes(x = x, y = y, color = Distance)) + geom_point() +
       scale_color_gradient2(low = "blue", mid = "white", high = "red",
                             midpoint = mean(wifi$Distance)) +
       geom_point(data = as.data.frame(t(coef(wifiMod1))),
                  aes(x = betaX, y = betaY), size = 5, color = "green")

# ---------------------------------------------------------- #