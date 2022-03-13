library(data.table)
theUrl <- "crude-oil-price.csv"
oilprice <- fread(input=theUrl, sep=',', header=TRUE)
#----------------------------#
head(oilprice)
#不適合繪製直方圖
#hist(oilprice$price,main="oil price",xlab = "price",ylab = "num")
