rm(list = ls())#clear all data
library(data.table)
library(quantmod)
library(TTR)
library(tidyquant)
#���o�ѻ���T
getSymbols('BTC-USD')
getSymbols('AAPL')
#�L�X��T�䤤�]�topen�Bhigh�Blow�Bclose�Bvolume�Badjusted

#ø�sK�u
chartSeries(AAPL["2020-01-01::2020-06-01"],theme="black")
chartSeries(`BTC-USD`["2020-01-01::2020-06-01"],theme="white")