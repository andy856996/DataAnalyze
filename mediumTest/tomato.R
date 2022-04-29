rm(list=ls())
library(ggplot2)
#圓番茄 價格 來源 甜 酸 顏色 質地 總體 平均值 總計 平均值
#https://www.jaredlander.com/data/tomatofirst.csv
data_tomato <- read.csv("https://www.jaredlander.com/data/TomatoFirst.csv", header=TRUE, sep=",", fileEncoding = 'BIG5')
head(data_tomato)

#shapiro.test(data_tomato$Round)標籤檢定無意義
#shapiro.test(data_tomato$Tomato)非數值不能檢定
shapiro.test(data_tomato$Price)
#shapiro.test(data_tomato$Source)非數值不能檢定
shapiro.test(data_tomato$Sweet)
shapiro.test(data_tomato$Acid)
shapiro.test(data_tomato$Color)
shapiro.test(data_tomato$Texture)

data(tips , package = "reshape2")
head(tips)



T_tomato <- t.test(data_tomato$Price, alternative = "two.sided", mu = 2.5)
data_tomato[!is.na(data_tomato)]
