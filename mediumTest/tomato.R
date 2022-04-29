rm(list=ls())
library(ggplot2)
#圓番茄 價格 來源 甜 酸 顏色 質地 總體 平均值 總計 平均值
#https://www.jaredlander.com/data/tomatofirst.csv
data_tomato <- read.csv("https://www.jaredlander.com/data/TomatoFirst.csv", header=TRUE, sep=",", fileEncoding = 'BIG5')
data_tomato[is.na(data_tomato$Price),"Price"] = mean(data_tomato$Price,na.rm = 1) #用平均數補充income的缺失值
head(data_tomato)
## 檢定常態性##
#shapiro.test(data_tomato$Round)標籤檢定無意義
#shapiro.test(data_tomato$Tomato)非數值不能檢定
shapiro.test(data_tomato$Price)
#shapiro.test(data_tomato$Source)非數值不能檢定
shapiro.test(data_tomato$Sweet)
shapiro.test(data_tomato$Acid)
shapiro.test(data_tomato$Color)
shapiro.test(data_tomato$Texture)

#var.test(data_tomato$Sweet, data_tomato$Texture)
#ansari.test(data_tomato$Sweet, data_tomato$Texture)
## 分析相關性  ##
cor(data_tomato$Sweet, data_tomato$Texture)
cor(data_tomato$Price, data_tomato$Texture)
cor(data_tomato$Sweet, data_tomato$Color)
cor(data_tomato$Sweet, data_tomato$Acid)


