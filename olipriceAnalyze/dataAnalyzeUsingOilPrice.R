library(data.table)
theUrl <- "crude-oil-price.csv"
oilprice <- fread(input=theUrl, sep=',', header=TRUE)
#----------------------------#
head(oilprice)