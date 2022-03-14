rm(list = ls())#clear all data
#set working directory#
setwd("C:/Users/andy8/Desktop/DataAnalyze/olipriceAnalyze")
#import library#
library(data.table)
library(quantmod)
library(TTR)
library(tidyquant)
#計算美股和BTC相關係數#
dateFrom = "2015-01-01"
dateTo = "2022-02-22"
Btc_USD_price_time <- c("BTC-USD") %>%
  tq_get(get = "stock.price", from = dateFrom, to = dateTo)

DJI_stock <- c("^DJI") %>% 
  tq_get(get = "stock.price", from = dateFrom, to = dateTo)

SP_stock <- c("^GSPC") %>% 
  tq_get(get = "stock.price", from = dateFrom, to = dateTo)

Btc_USD_high_price = Btc_USD_price_time$high
Btc_USD_date = Btc_USD_price_time$date
#美股假日因為沒有開盤所以沒有資料
DJI_stock_high_price = DJI_stock$high
DJI_stock_date = DJI_stock$date
#美股假日因為沒有開盤所以沒有資料
SP500_stock_high_price = SP_stock$high
SP500_stock_date = SP_stock$date

sameDate2 = match(DJI_stock_date,Btc_USD_date)
sameDate3 = match(SP500_stock_date,Btc_USD_date)

Btc_USD_high_price_mod1 = Btc_USD_high_price[sameDate2]
Btc_USD_high_price_mod2 = Btc_USD_high_price[sameDate3]
#BTC對DJI之相關係數
cor(Btc_USD_high_price_mod1,DJI_stock_high_price)
#BTC對SP500之相關係數
cor(Btc_USD_high_price_mod2,SP500_stock_high_price)
