rm(list = ls())#clear all data
library(data.table)
library(quantmod)
library(TTR)
library(tidyquant)
#取得股價資訊
getSymbols('BTC-USD')
getSymbols('AAPL')
#印出資訊其中包含open、high、low、close、volume、adjusted

#繪製K線
chartSeries(AAPL["2020-01-01::2020-06-01"],theme="black")
chartSeries(`BTC-USD`["2020-01-01::2020-06-01"],theme="white")
