rm(list = ls())#clear all data
#set working directory#
setwd("C:/Users/andy856996/Desktop/DataAnalyze/olipriceAnalyze")
#import library#
library(data.table)
library(quantmod)
library(TTR)
library(tidyquant)
#計算美股和BTC相關係數#
Btc_USD_price_time <- c("BTC-USD") %>%
  tq_get(get = "stock.price", from = "2019-01-01", to = "2022-02-22")
DJI_stock <- c("^DJI") %>% 
  tq_get(get = "stock.price", from = "2019-01-01", to = "2022-02-22")

Btc_USD_high_price = Btc_USD_price_time$high
Btc_USD_date = Btc_USD_price_time$date
#美股假日因為沒有開盤所以沒有資料
DJI_stock_high_price = DJI_stock$high
DJI_stock_date = DJI_stock$date

sameDate2 = match(DJI_stock_date,Btc_USD_date)

Btc_USD_high_price_mod = Btc_USD_high_price[sameDate2]

cor(Btc_USD_high_price_mod,DJI_stock_high_price)
