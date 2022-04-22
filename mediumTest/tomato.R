rm(list=ls())
library(ggplot2)
#圓番茄 價格 來源 甜 酸 顏色 質地 總體 平均值 總計 平均值
#https://www.jaredlander.com/data/tomatofirst.csv
data_tomato <- read.csv("https://www.jaredlander.com/data/TomatoFirst.csv", header=TRUE, sep=",", fileEncoding = 'BIG5')
head(data_tomato)
t.test(data_tomato$Price, alternative = "two.sided", mu = 2.5)
data_tomato[!is.na(data_tomato)]
