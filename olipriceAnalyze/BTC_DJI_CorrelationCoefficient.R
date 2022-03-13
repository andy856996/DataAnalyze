rm(list = ls())#clear all data
#set working directory#
setwd("C:/Users/andy8/Desktop/DataAnalyze/olipriceAnalyze")
#import library#
library(data.table)
library(quantmod)
library(TTR)
library(tidyquant)
#�p����ѩMBTC�����Y��#
Btc_USD_price_time <- c("BTC-USD") %>%
  tq_get(get = "stock.price", from = "2022-01-01", to = "2022-02-22")
DJI_stock <- c("^DJI") %>% 
  tq_get(get = "stock.price", from = "2022-01-01", to = "2022-02-22")

Btc_USD_high_price = Btc_USD_price_time$high
#���Ѱ���]���S���}�L�ҥH�S�����
DJI_stock_high_price = DJI_stock$high