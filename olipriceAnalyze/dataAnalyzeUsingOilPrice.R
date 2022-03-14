rm(list = ls())#clear all data
#set working directory#
setwd("C:/Users/andy856996/Desktop/DataAnalyze/olipriceAnalyze")
#import library#
library(data.table)
#start the program#
theUrl <- "crude-oil-price.csv"
oilprice <- fread(input=theUrl, sep=',', header=TRUE)
#----------------------------#
head(oilprice)
#不適合繪製直方圖
hist(oilprice$price,main="oil price",xlab = "price",ylab = "num")

